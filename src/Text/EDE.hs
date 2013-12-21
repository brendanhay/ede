{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Text.EDE
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A (mostly logicless) textual templating language in the same
-- vein as <https://github.com/Shopify/liquid Liquid> or <http://jinja.pocoo.org/docs/ Jinja2>.
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
    , parseFile
    , parseWith

    -- ** Rendering
    , render
    , renderWith

    -- ** Either Variants
    , eitherParse
    , eitherParseFile
    , eitherParseWith
    , eitherRender
    , eitherRenderWith

    -- * Results and Errors
    -- $results
    , Meta   (..)
    , Result (..)
    , eitherResult
    , result
    , success
    , failure

    -- * Includes

    -- * Filters
    , Fun    (..)
    , TType  (..)
    , defaultFilters

    -- * Convenience
    -- ** Data.Aeson
    -- $input
    , fromPairs
    , (.=)

    -- * Syntax
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
import           Data.Aeson                 ((.=))
import           Data.Aeson.Types           (Object)
import           Data.Foldable              (foldrM)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (toLazyText)
import qualified Data.Text.Lazy.IO          as LText
import           System.Directory
import qualified Text.EDE.Internal.Compiler as Compiler
import           Text.EDE.Internal.Filters  as Filters
import qualified Text.EDE.Internal.Parser   as Parser
import           Text.EDE.Internal.Types

-- -- | Parse 'Text' into a compiled template.
-- --
-- -- Because this function is pure and does not resolve @include@ expressions,
-- -- it is required that the caller supplies a 'HashMap' of 'Template's at
-- -- render time using 'renderWith' if 'include's are used, otherwise an error
-- -- will be returned.

-- -- FIXME: make the lookup of include identifiers safe
-- --   ie: if the target is some key but it also happened to be a file unintentionally
-- --   how could this be made more robust?

-- -- | Read and parse a file into a compiled template.
-- --
-- -- If 'True' is specified as the first parameter, 'parseFile' performs
-- -- all template resolution and parsing recursively, with non-existent templates
-- -- and parse errors returning an error immediately.
-- --
-- -- If you wish to defer loading of missing includes, or supply additional
-- -- precompiled/named includes at runtime (using 'renderWith' rather than
-- -- exclusively using file paths) you should specify 'False' as the first parameter,
-- -- or use 'parse' and 'renderWith' directly.

parse :: LText.Text -- ^ Lazy 'Data.Text.Lazy.Text' template definition.
      -> Result Template
parse = join . parseWith (includeMap mempty) "Text.EDE.parse"

parseFile :: FilePath -- ^ Path to the file to load.
          -> IO (Result Template)
parseFile p = do
    e <- doesFileExist p
    if not e
        then failure (mkMeta p) ["file " ++ p ++ " doesn't exist."]
        else LText.readFile p >>= parseWith includeFile (Text.pack p)

parseWith :: Monad m
          => (Text -> Meta -> m (Result Template)) -- ^ Function to resolve includes.
          -> Text                                  -- ^ Template name.
          -> LText.Text                            -- ^ Lazy 'Data.Text.Lazy.Text' template definition.
          -> m (Result Template)
parseWith f n = result failure resolve . Parser.runParser (Text.unpack n)
  where
    resolve (u, is) = do
        r <- foldrM include (Success $ Map.singleton n u) $ Map.toList is
        result failure
             (success . Template n u)
             r

    include (_, _) (Error m xs) = failure m xs
    include (k, m) (Success ss) = do
        r <- f k m
        result failure
               -- Shouldn't need to insert self, presuming self
               -- is always in self's includes.
               (success . mappend ss . tmplIncl)
               r

includeMap :: Monad m
           => HashMap Text Template
           -> Text
           -> Meta
           -> m (Result Template)
includeMap ts k m
    | Just v <- Map.lookup k ts = success v
    | otherwise = failure m ["unable to resolve " ++ Text.unpack k]

includeFile :: Text
            -> Meta
            -> IO (Result Template)
includeFile k = const (parseFile $ Text.unpack k)

-- | Render an 'Object' using the supplied 'Template'.
render :: Template -- ^ Parsed 'Template' to render.
       -> Object   -- ^ Bindings to make available in the environment.
       -> Result LText.Text
render = renderWith defaultFilters

-- | Render an 'Object' using the supplied 'Template'.
renderWith :: HashMap Text Fun -- ^ Filters to make available in the environment.
           -> Template         -- ^ Parsed 'Template' to render.
           -> Object           -- ^ Bindings to make available in the environment.
           -> Result LText.Text
renderWith fs (Template _ u ts) = fmap toLazyText . Compiler.render fs ts u

-- | See: 'parse'
eitherParse :: LText.Text
            -> Either String Template
eitherParse = eitherResult . parse

-- | See: 'parseFile'
eitherParseFile :: FilePath
                -> IO (Either String Template)
eitherParseFile = fmap eitherResult . parseFile

-- | See: 'parseWith'
eitherParseWith :: (Functor m, Monad m)
                => (Text -> Meta -> m (Result Template))
                -> Text
                -> LText.Text
                -> m (Either String Template)
eitherParseWith f n = fmap eitherResult . parseWith f n

-- | See: 'render'
eitherRender :: Template
             -> Object
             -> Either String LText.Text
eitherRender t = eitherResult . render t

-- | See: 'renderWith'
eitherRenderWith :: HashMap Text Fun
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
-- >>> let tmpl = parse "{% if var %}Hello, {{ var }}!{% else %}negative!{% endif %}" :: Result Template
--
-- Then an 'Object' is defined containing the environment which will be
-- available to the 'Template' during rendering:
--
-- >>> let env = render $ fromPairs [ "var" .= "World" ] :: Template -> Result Text
--
-- Finally the environment is applied to the 'Template':
--
-- >>> tmpl >>= env :: Result Text
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
-- >     tmpl <- LText.readFile "template.ede"
-- >     either error print $ eitherParse tmpl >>= eitherRender env
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
-- more expensive result of parsing can be embedded, and/or reused.
--
-- * Parsing tokenises the input and converts it to an internal AST representation,
-- which can be cached for future use.
--
-- * Rendering takes an 'Object' as the environment and a parsed 'Template'
-- to subsitute the values into.

-- $results
--
-- Unsuccessful 'parse' or 'render' steps can be inspected or analysed using
-- 'result'.
--
-- >>> result failure success $ parse tmpl >>= render env
--
-- If you're only interested in dealing with errors as strings, and the positional
-- information contained in 'Meta' is not of use you can use the convenience functions
-- 'eitherParse', 'eitherRender', or convert a 'Result' to 'Either' using 'eitherResult'.
--
-- >>> either failure success $ eitherParse tmpl >>= eitherRender env

-- $input
--
-- 'fromPairs' is a wrapper around Aeson's 'object' function which safely strips the outer
-- 'Value' constructor, providing the correct type signature for input into 'render'.
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
-- with parameters being type checked and an error is raised if the left and right
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
-- TODO

-- $filters
--
-- Filters are typed functions which can be applied to variables and literals.
-- An example of rendering a lower cased boolean would be:
--
-- > {{ True | show | lower }}
--
-- The input is on the LHS and chained filters (delimited by '|') are on the RHS,
-- with filters being applied left associatively.
--
-- Available filters:
--
-- * @show :: a -> Text@: Convert a value to 'Text' using its 'Show' instance.
--
-- * @lower :: Text -> Text@: Lower case a textual value.
--
-- * @upper :: Text -> Text@: Upper case a textual value.
--
-- * @lowerFirst :: Text -> Text@: Lower case the first character of a textual value.
--
-- * @upperFirst :: Text -> Text@: Upper case the first character of a textual value.
--
-- * @listLength :: Array -> Integer@: Get the length of an 'Array'.
--
-- * @mapLength :: Object -> Integer@: Get the length of an 'Object'.
--
