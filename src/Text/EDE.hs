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

    -- ** Basic
    , parse
    , render

    -- ** IO and Includes
    , parseFile

    -- ** Configurable
    -- $configuration
    , parseAs
    , renderWith

    -- ** Either Variants
    , eitherParse
    , eitherParseFile
    , eitherParseAs
    , eitherRender
    , eitherRenderWith

    -- * Results and Errors
    -- $results
    , Meta   (..)
    , Result (..)
    , eitherResult
    , result

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

import           Data.Aeson                 ((.=))
import           Data.Aeson.Types           (Object)
import           Data.Foldable              (foldl', foldrM)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
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
import           Text.Parsec                (SourceName)

-- | Parse 'Text' into a compiled template.
--
-- This function is pure and does not resolve include expressions. This requires
-- that the caller supplies a 'HashMap' of 'Template's at render time
-- using 'renderWith'.
parse :: LText.Text -- ^ Lazy 'Data.Text.Lazy.Text' template definition.
      -> Result Template
parse = parseAs "Text.EDE.parse"

-- | Render an 'Object' using the supplied 'Template'.
--
-- Rendering a 'Template' which has been been parsed used the pure variants of
-- 'parse', and contains
render :: Template
       -> Object
       -> Result LText.Text
render = renderWith defaultFilters Map.empty

-- | Read and parse a file into a compiled template.
--
-- If 'True' is specified as the first parameter, 'parseFile' performs
-- all template resolution and parsing recursively, with non-existent templates
-- and parse errors returning an error immediately.
--
-- If you wish to defer loading of includes, or supply additional precompiled/named
-- includes at runtime (rather than exclusively using file paths) you should
-- specify 'False' as the first parameter, or use 'parse' and 'renderWith' directly.
parseFile :: Bool     -- ^ Error if target @include@ tag is a non-existent file.
          -> FilePath -- ^ Path to the template to load.
          -> IO (Result Template)
parseFile strict f = do
    p <- doesFileExist f
    if not p
        then failure (mkMeta f) ["file " ++ f ++ " doesn't exist."]
        else LText.readFile f >>= result failure resolve . parseAs f
  where
    resolve t@(Template e _) =
        template Map.empty (Text.pack f) t >>=
            result failure (success . Template e)

    template us k (Template u is) =
        let initial = Success $ Map.insert k (Resolved u) us
        in  foldrM includes initial $ Map.toList is

    includes _                     (Error m xs) = failure m xs
    includes (k, r@(Resolved   _)) (Success us) = success $ Map.insert k r us
    includes (k, r@(Unresolved _)) (Success us) = do
        let f' = Text.unpack k
        p <- doesFileExist f'
        if not p && strict
            then success $ Map.insert k r us
            else parseFile strict f' >>= result failure (template us k)

    failure m = return . Error m
    success   = return . Success

-- | Parse 'Text' into a compiled template.
parseAs :: SourceName -- ^ Descriptive name for error messages.
        -> LText.Text -- ^ Lazy 'Data.Text.Lazy.Text' template definition.
        -> Result Template
parseAs = Parser.runParser

-- | Render an 'Object' using the supplied 'Template'.
renderWith :: HashMap Text Fun      -- ^ Filters
           -> HashMap Text Template -- ^ Additional Templates
           -> Template              -- ^ Parsed Template
           -> Object                -- ^ Environment
           -> Result LText.Text
renderWith fs ts (Template e is) o =
    fmap toLazyText $ resolve >>= \es -> Compiler.render fs es e o
  where
    resolve = Map.traverseWithKey g
        . foldl' (Map.unionWith f) is
        $ Map.map tmplIncl ts

    f (Unresolved _) b = b
    f a (Unresolved _) = a
    f a _              = a

    g _ (Resolved   u) = Success u
    g k (Unresolved m) =
        Error m ["include target " ++ Text.unpack k ++ " is unresolved."]

-- | See: 'parse'
eitherParse :: LText.Text
            -> Either String Template
eitherParse = eitherResult . parse

-- | See: 'parseFile'
eitherParseFile :: Bool
                -> FilePath
                -> IO (Either String Template)
eitherParseFile strict = fmap eitherResult . parseFile strict

-- | See: 'parseAs'
eitherParseAs :: SourceName
              -> LText.Text
              -> Either String Template
eitherParseAs n = eitherResult . parseAs n

-- | See: 'render'
eitherRender :: Template
             -> Object
             -> Either String LText.Text
eitherRender t = eitherResult . render t

-- | See: 'renderWith'
eitherRenderWith :: HashMap Text Fun
                 -> HashMap Text Template
                 -> Template
                 -> Object
                 -> Either String LText.Text
eitherRenderWith fs ts t = eitherResult . renderWith fs ts t

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
