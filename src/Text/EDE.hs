{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

    -- * Parsing and rendering
    -- $parsing_and_rendering
      Template
    , parse
    , render
    , eitherParse
    , eitherRender

    -- * Results and errors
    -- $results
    , Meta   (..)
    , Result (..)
    , eitherResult
    , result

    -- * Convenience
    -- ** Data.Aeson
    -- $input
    , fromPairs
    , (.=)

    -- ** Data.Text.Lazy.Builder
    -- $output
    , toLazyText

    -- * Syntax
    -- ** Comments
    -- $comments

    -- ** Variables
    -- $variables

    -- ** Conditionals
    -- $conditionals

    -- *** Logic
    -- $logic

    -- ** Loops
    -- $loops

    -- *** Context
    -- $context

    -- ** Debugging
    -- $debugging
    ) where

import           Data.Aeson                 (object, (.=))
import           Data.Aeson.Types           (Array, Object, Pair, Value(..))
import           Data.Text.Buildable        (Buildable)
import           Data.Text.Lazy             (Text)
import           Data.Text.Lazy.Builder     (Builder, toLazyText)
import qualified Text.EDE.Internal.Compiler as Compiler
import qualified Text.EDE.Internal.Parser   as Parser
import           Text.EDE.Internal.Types

-- | A valid parsed and compiled template.
newtype Template = Template { template :: UExp }
    deriving (Eq, Ord)

-- | Parse 'Text' into a compiled template.
parse :: Text -> Result Template
parse = fmap Template . Parser.runParser

-- | Parse 'Text' into a compiled template,
-- and convert the 'Result' using 'eitherResult'.
eitherParse :: Text -> Either String Template
eitherParse = eitherResult . parse

-- | Render an 'Object' using the supplied 'Template'.
render :: Object -> Template -> Result Builder
render o = (`Compiler.render` o) . template

-- | Render an 'Object' using the supplied 'Template',
-- and convert the 'Result' using 'eitherResult'.
eitherRender :: Object -> Template -> Either String Builder
eitherRender o = eitherResult . render o

-- | Create an 'Object' from a list of name/value 'Pair's.
-- See 'Aeson''s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs = (\(Object o) -> o) . object

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
-- >>> let env = render $ fromPairs [ "var" .= "World" ] :: Template -> Result Builder
--
-- Finally the environment is applied to the 'Template':
--
-- >>> tmpl >>= env :: Result Builder
-- > Success "Hello, World!"
--
-- In this manner, 'Template's can be pre-compiled to the internal AST and
-- the cost of parsing can be amortised if the same 'Template' is rendered multiple times.
--
-- Please see the <#syntax syntax> section for more information.

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
-- >>> render (fromPairs [ "foo" .= "value", "bar" .= 1 ]) :: Template -> Result Builder

-- $output
--
-- The successful result of rendering an 'Object' environment and 'Template' is
-- a lazy 'Builder' which can be converted to 'Text' using the re-exported 'toLazyText'.

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
-- The value of @{{ var }}@ determines the branch that is rendered by the template.
--
-- In the case of a literal it conforms directly to the supported boolean or relation logical
-- operators from Haskell.
-- If a variable is singuarly used it's existence determines the result of the predicate,
-- the exception to this rule is boolean values which will be substituted into the
-- expression if they exist in the supplied environment.
--

-- $logic
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

-- $loops
--

-- $context
--
-- The following variables are available inside a loop expression by applying an
-- accessor to the bound variable:
--
-- * @length :: Int@: length of the loop
--
-- * @index :: Int@: index of the iteration
--
-- * @index0 :: Int@: zero based index of the iteration
--
-- * @remainder :: Int@: remaining number of iterations
--
-- * @remainder0 :: Int@: zero based remaining number of iterations
--
-- * @first :: Bool@: is this the first iteration?
--
-- * @last :: Bool@: is this the last iteration?
--
-- * @odd :: Bool@: is this an odd iteration?
--
-- * @even :: Bool@: is this an even iteration?
--
-- For example:
--
-- > {% for item in items %}
-- >     {# render the value of the binding #}
-- >     {{ item.index }}:{{ item.value }}
-- >     {% if item.last %}
-- >       the end.
-- >     {% endif %}
-- > {% endfor %}
--
-- Will render each item with it's (1-based) loop index as a prefix and the
-- text literal 'the end.' only once after then rest of the loop items have been rendered.

-- $debugging
--
