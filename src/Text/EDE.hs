{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | A (mostly logicless) textual templating language with similar syntax to
-- <https://github.com/Shopify/liquid Liquid> or <http://jinja.pocoo.org/docs/ Jinja2>.
--
-- (ED-E is a character from Fallout New Vegas, pronounced iː-diː-iː, or Eddie.)
module Text.EDE
    (
    -- * How to use this library
    -- $usage

    -- * Parsing and Rendering
    -- $parsing_and_rendering
      Template

    -- ** Parsing
    , parse
    , parseIO
    , parseFile
    , parseFileWith
    , parseWith

    -- ** Includes
    -- $resolvers
    , Resolver
    , includeMap
    , includeFile

    -- ** Filters
    , defaultFilters

    -- ** Rendering
    , render
    , renderWith

    -- ** Either Variants
    , eitherParse
    , eitherParseFile
    , eitherParseWith
    , eitherRender
    , eitherRenderWith

    -- ** Results and Errors
    -- $results
    , Delta  (..)
    , Result (..)
    , eitherResult
    , result
    , success
    , failure
    -- * Input
    -- $input
    , fromValue
    , fromPairs
    , (.=)

    -- * Version
    , version

    -- * Syntax
    , Delim
    , Syntax
    , delimRender
    , delimComment
    , delimBlock

    , defaultSyntax
    , alternateSyntax

    -- ** Comments
    -- $comments

    -- ** Raw
    -- $raw

    -- ** Variables
    -- $variables

    -- ** Conditionals
    -- $conditionals

    -- ** Case Analysis
    -- $case

    -- ** Loops
    -- $loops

    -- ** Includes
    -- $includes

    -- ** Filters
    -- $filters
    ) where

import           Control.Monad
import           Data.Aeson                   ((.=))
import           Data.Aeson.Types             (Object)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Foldable                (foldrM)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.List.NonEmpty           (NonEmpty(..))
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder       (toLazyText)
import           Data.Version                 (Version)
import qualified Paths_ede                    as Paths
import           System.Directory
import           System.FilePath
import           Text.EDE.Filters
import qualified Text.EDE.Internal.Eval       as Eval
import qualified Text.EDE.Internal.Parser     as Parser
import           Text.EDE.Internal.Syntax
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.ANSI.Leijen (string)
import           Text.Trifecta.Delta

-- | ED-E Version.
version :: Version
version = Paths.version

-- FIXME: detect include/import loops

-- | Parse Lazy 'LText.Text' into a compiled 'Template'.
--
-- Because this function is pure and does not resolve @include@s,
-- encountering an @include@ expression during parsing will result in an 'Error'.
--
-- See 'parseFile' or 'parseWith' for mechanisms to deal with @include@
-- dependencies.
parse :: ByteString -- ^ Strict 'ByteString' template definition.
      -> Result Template
parse = join . parseWith defaultSyntax (includeMap mempty) "Text.EDE.parse"

-- | Parse 'Text' into a compiled 'Template'.
--
-- This function handles all @include@ expressions as 'FilePath's and performs
-- recursive loading/parsing.
parseIO :: FilePath   -- ^ Parent directory for relatively pathed includes.
        -> ByteString -- ^ Strict 'ByteString' template definition.
        -> IO (Result Template)
parseIO p = parseWith defaultSyntax (includeFile p) "Text.EDE.parse"

-- | Load and parse a 'Template' from a file.
--
-- This function handles all @include@ expressions as 'FilePath's and performs
-- recursive loading/parsing, with pathing of @include@s relatively to the
-- target (unless absolute paths are used).
parseFile :: FilePath -- ^ Path to the template to load and parse.
          -> IO (Result Template)
parseFile = parseFileWith defaultSyntax

-- | /See:/ 'parseFile'.
parseFileWith :: Syntax   -- ^ Delimiters and parsing options.
              -> FilePath -- ^ Path to the template to load and parse.
              -> IO (Result Template)
parseFileWith s p = loadFile p >>= result failure
    (parseWith s (includeFile (takeDirectory p)) (Text.pack p))

-- | Parse a 'Template' from a Strict 'ByteString' using a custom function for
-- resolving @include@ expressions.
--
-- Two custom @include@ resolvers are supplied:
--
-- * 'includeMap'
--
-- * 'includeFile'
--
-- 'parseFile' for example, is defined as: 'parseWith' 'includeFile'.
parseWith :: Monad m
          => Syntax     -- ^ Delimiters and parsing options.
          -> Resolver m -- ^ Function to resolve includes.
          -> Text       -- ^ Strict 'Text' name.
          -> ByteString -- ^ Strict 'ByteString' template definition.
          -> m (Result Template)
parseWith o f n = result failure resolve . Parser.runParser o n
  where
    resolve (u, is) = do
        r <- foldrM include (Success (Map.singleton n u)) (Map.toList is)
        result failure
               (success . Template n u)
               r

    -- Presuming self is always in self's includes, see singleton above.
    -- FIXME: utilise the list of deltas for failures
    include (_, _)    (Failure  e) = failure e
    include (k, d:|_) (Success ss) = f o k d >>=
        result failure (success . mappend ss . _tmplIncl)

-- | 'HashMap' resolver for @include@ expressions.
--
-- The 'identifier' component of the @include@ expression is treated as a lookup
-- key into the supplied 'HashMap'.
-- If the 'identifier' doesn't exist in the 'HashMap', an 'Error' is returned.
includeMap :: Monad m
           => HashMap Text Template -- ^ A 'HashMap' of named 'Template's.
           -> Resolver m            -- ^ Resolver for 'parseWith'.
includeMap ts _ k _
    | Just v <- Map.lookup k ts = success v
    | otherwise = failure ("unable to resolve " <> string (Text.unpack k))
      -- FIXME: utilise deltas in error messages

-- | 'FilePath' resolver for @include@ expressions.
--
-- The 'identifier' component of the @include@ expression is treated as a relative
-- 'FilePath' and the template is loaded and parsed using 'parseFile'.
-- If the 'identifier' doesn't exist as a valid 'FilePath', an 'Error' is returned.
includeFile :: FilePath -- ^ Parent directory for relatively pathed includes.
            -> Resolver IO
includeFile p o k _ = loadFile f >>= result failure (parseWith o inc k)
    where
      inc = includeFile (takeDirectory f)

      f | Text.null k = Text.unpack k
        | otherwise   = p </> Text.unpack k

loadFile :: FilePath -> IO (Result ByteString)
loadFile p = do
    e <- doesFileExist p
    if not e
        then failure ("file " <> string p <> " doesn't exist.")
        else BS.readFile p >>= success

-- | Render an 'Object' using the supplied 'Template'.
render :: Template -- ^ Parsed 'Template' to render.
       -> Object   -- ^ Bindings to make available in the environment.
       -> Result LText.Text
render = renderWith defaultFilters

-- | Render an 'Object' using the supplied 'Template'.
renderWith :: HashMap Text Binding -- ^ Filters to make available in the environment.
           -> Template             -- ^ Parsed 'Template' to render.
           -> Object               -- ^ Bindings to make available in the environment.
           -> Result LText.Text
renderWith fs (Template _ u ts) = fmap toLazyText . Eval.render ts fs u

-- | /See:/ 'parse'
eitherParse :: ByteString -> Either String Template
eitherParse = eitherResult . parse

-- | /See:/ 'parseFile'
eitherParseFile :: FilePath -> IO (Either String Template)
eitherParseFile = fmap eitherResult . parseFile

-- | /See:/ 'parseWith'
eitherParseWith :: (Functor m, Monad m)
                => Syntax
                -> Resolver m
                -> Text
                -> ByteString
                -> m (Either String Template)
eitherParseWith o f n = fmap eitherResult . parseWith o f n

-- | /See:/ 'render'
eitherRender :: Template
             -> Object
             -> Either String LText.Text
eitherRender t = eitherResult . render t

-- | /See:/ 'renderWith'
eitherRenderWith :: HashMap Text Binding
                 -> Template
                 -> Object
                 -> Either String LText.Text
eitherRenderWith fs t = eitherResult . renderWith fs t

-- $usage
--
-- A simple example of parsing and rendering 'Text' containing a basic conditional
-- expression and variable interpolation follows.
--
-- First the 'Template' is defined:
--
-- >>> let tmpl = "{% if var %}\nHello, {{ var }}!\n{% else %}\nnegative!\n{% endif %}\n" :: Data.ByteString.ByteString
--
-- Then an 'Object' is defined containing the environment which will be
-- available to the 'Template' during rendering:
--
-- >>> let env = fromPairs [ "var" .= "World" ] :: Object
--
-- Note: the 'fromPairs' function above is a wrapper over Aeson's 'object'
-- which removes the 'Value' constructor, exposing the delicious 'HashMap' underneath.
--
-- Finally the environment is applied to the 'Template':
--
-- >>> render tmpl env :: Result Text
-- > Success "Hello, World!"
--
-- In this manner, 'Template's can be pre-compiled to the internal AST and
-- the cost of parsing can be amortised if the same 'Template' is rendered multiple times.
--
-- Another example, this time rendering a 'Template' from a file:
--
-- > import qualified Data.Text.Lazy as LText
-- > import           Text.EDE
-- >
-- > main :: IO ()
-- > main = do
-- >     r <- eitherParseFile "template.ede"
-- >     either error print $ r >>= (`eitherRender` env)
-- >   where
-- >     env = fromPairs
-- >         [ "text" .= "Some Text."
-- >         , "int"  .= 1
-- >         , "list" .= [5..10]
-- >         ]
--
-- Please see the <#syntax syntax> section for more information about available
-- statements and expressions.

-- $parsing_and_rendering
--
-- Parsing and rendering require two separate steps intentionally so that the
-- more expensive (and potentially impure) action of parsing and resolving
-- @include@s can be embedded and re-used in a pure fashion.
--
-- * Parsing tokenises the input and converts it to an internal AST representation,
-- resolving @include@s using a custom function. The result is a compiled template
-- which can be cached for future use.
--
-- * Rendering takes a 'HashMap' of custom 'Fun's (functions available in the
-- template context), an 'Object' as the binding environment, and a parsed
-- 'Template' to subsitute the values into.
-- The result is a Lazy 'LText.Text' value containing the rendered output.

-- $resolvers
--
-- The 'Resolver' used to resolve @include@ expressions determines the purity
-- of 'Template' parsing.
--
-- For example, using the 'includeFile' 'Resolver' means parsing is restricted
-- to 'IO', while pre-caching a 'HashMap' of 'Template's and supplying them to
-- 'parseWith' using 'includeMap' offers a pure variant for @include@ resolution.

-- $results
--
-- The 'Result' of a 'parse' or 'render' steps can be inspected or analysed using
-- 'result' as follows:
--
-- >>> result failure success $ render tmpl env
--
-- If you're only interested in dealing with errors as strings, and the positional
-- information contained in 'Meta' is not of use you can use the convenience functions
-- 'eitherParse', 'eitherRender', or convert a 'Result' to 'Either' using 'eitherResult'.
--
-- >>> either failure success $ eitherParse tmpl

-- $input
--
-- 'fromPairs' (or 'fromValue') is a wrapper around Aeson's 'object' function which
--  safely strips the outer 'Value' constructor, providing the correct type
-- signature for input into 'render'.
--
-- It is used in combination with the re-exported '.=' as follows:
--
-- >>> render (fromPairs [ "foo" .= "value", "bar" .= 1 ]) :: Template -> Result Text

-- $comments #syntax#
--
-- Comments are ignored by the parser and omitted from the rendered output.
--
-- > {# singleline comment #}
--
-- > {#
-- >    multiline
-- >    comment
-- > #}
--

-- $raw
--
-- You can disable template processing for blocks of text using the @raw@ section:
--
-- > {% raw %}
-- > Some {{{ handlebars }}} or {{ mustache }} or {{ jinja2 }} output tags etc.
-- > {% endraw %}
--
-- This can be used to avoid parsing expressions which would otherwise be considered
-- valid @ED-E@ syntax.

-- $variables
--
-- Variables are substituted directly for their 'Buildable' representation.
-- An error is raised if the varaible being substituted is not a literal type
-- (ie. an 'Array' or 'Object') or doesn't exist in the supplied environment.
--
-- > {{ var }}
--
-- Nested variable access is also supported for variables which resolve to an 'Object'.
-- Dot delimiters are used to chain access through multiple nested 'Object's.
-- The right-most accessor must resolve to a 'Buildable' type as with the previous
-- non-nested variable access.
--
-- > {{ nested.var.access }}

-- $conditionals
--
-- A conditional is introduced and completed with the section syntax:
--
-- > {% if var %}
-- >    ... consequent expressions
-- > {% else %}
-- >    ... alternate expressions
-- > {% endif %}
--
-- The value of @{{ var }}@ determines the branch that is rendered by the template with
-- the else branch being optional.
--
-- In the case of a literal it conforms directly to the supported boolean or relation logical
-- operators from Haskell.
-- If a variable is singuarly used its existence determines the result of the predicate,
-- the exception to this rule is boolean values which will be substituted into the
-- expression if they exist in the supplied environment.
--
-- The following logical expressions are supported as predicates in conditional statements
-- with parameters type checked and an error raised if the left and right
-- hand sides are not type equivalent.
--
-- * @And@: '&&'
--
-- * @Or@: '||'
--
-- * @Equal@: '=='
--
-- * @Not Equal@: '/='
--
-- * @Greater@: '>'
--
-- * @Greater Or Equal@: '>='
--
-- * @Less@: '<'
--
-- * @Less Or Equal@: '<='
--
-- * @Negation@: '!'

-- $case
--
-- To pattern match a literal or variable, you can use the @case@ statement:
--
-- > {% case var %}
-- > {% when "a" %}
-- >    .. matched expressions
-- > {% when "b" %}
-- >    .. matched expressions
-- > {% else %}
-- >    .. alternate expressions
-- > {% endcase %}

-- $loops
--
-- Iterating over an 'Array' or 'Object' can be acheived using the 'for ... in' section syntax.
-- Attempting to iterate over any other type will raise an error.
--
-- Example:
--
-- > {% for var in list %}
-- >     ... iteration expression
-- > {% else %}
-- >     ... alternate expression
-- > {% endfor %}
--
-- The iteration branch is rendering per item with the else branch being (which is optional)
-- being rendered if the @{{ list }}@ variable is empty.
--
-- When iterating over an 'Object', a stable sort using key equivalence is applied, 'Array's
-- are unmodified.
--
-- The resulting binding within the iteration expression (in this case, @{{ var }}@) is
-- an 'Object' containing the following keys:
--
-- * @key        :: Text@: They key if the loop target is an 'Object'
--
-- * @value      :: a@: The value of the loop target
--
-- * @loop       :: Object@: Loop metadata.
--
-- * @length     :: Int@: Length of the loop
--
-- * @index      :: Int@: Index of the iteration
--
-- * @index0     :: Int@: Zero based index of the iteration
--
-- * @remainder  :: Int@: Remaining number of iterations
--
-- * @remainder0 :: Int@: Zero based remaining number of iterations
--
-- * @first      :: Bool@: Is this the first iteration?
--
-- * @last       :: Bool@: Is this the last iteration?
--
-- * @odd        :: Bool@: Is this an odd iteration?
--
-- * @even       :: Bool@: Is this an even iteration?
--
-- For example:
--
-- > {% for item in items %}
-- >     {{ item.index }}:{{ item.value }}
-- >     {% if !item.last %}
-- >
-- >     {% endif %}
-- > {% endfor %}
--
-- Will render each item with its (1-based) loop index as a prefix, separated
-- by a blank newline, without a trailing at the end of the document.

-- $includes
--
-- Includes are a way to reduce the amount of noise in large templates.
-- They can be used to abstract out common snippets and idioms into partials.
--
-- If 'parseFile' or the 'includeFile' resolver is used, templates will be loaded
-- from 'FilePath's, for example:
--
-- > {% include "/var/tmp/partial.ede" %}
--
-- Loads @partial.ede@ from the file system.
--
-- By default, the current environment is made available to the included template,
-- but this can be overriden by specifying a specific binding to make available:
--
-- > {% include "/var/tmp/partial.ede" with value %}
--
-- Will ensure only the key @value@ (and descendents) is available in the
-- partial's environment.
--
-- Includes can also be resolved using pure 'Resolver's such as 'includeMap',
-- which will treat the @include@ expression's identifier as a 'HashMap' key:
--
-- > {% include "arbitrary_key" %}
--
-- Uses 'Map.lookup' to find @arbitrary_key@ in the 'HashMap' supplied to 'includeMap'.

-- $filters
--
-- Filters are typed functions which can be applied to variables and literals.
-- An example of rendering a lower cased boolean would be:
--
-- > {{ true | show | lower }}
--
-- The input is on the LHS and chained filters (delimited by '|') are on the RHS,
-- with filters being applied left associatively.
--
-- /See:/ "Text.EDE.Filters".
