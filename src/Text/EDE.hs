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

    -- *** Boolean Logic
    -- $boolean_logic

    -- *** Relational Logic
    -- $relational_logic

    -- *** Negation
    -- $negation

    -- ** Case Statements
    -- $case_statements

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
-- > >>> let tmpl = parse "{% if var %}Hello, {{ var }}!{% else %}negative!{% endif %}" :: Result Template
--
-- Then an 'Object' is defined containing the environment which will be
-- available to the 'Template' during rendering:
--
-- > >>> let env = render $ fromPairs [ "var" .= "World" ] :: Template -> Result Builder
--
-- Finally the environment is applied to the 'Template':
--
-- > >>> tmpl >>= env :: Result Builder
-- > Success "Hello, World!"
--
-- In this manner, 'Template's can be pre-compiled to the internal AST and
-- the cost of parsing can be amortised if the same 'Template' is rendered multiple times.

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
-- > >>> result failure success $ parse tmpl >>= render env
--
-- If you're only interested in dealing with errors as strings, and the positional
-- information contained in 'Meta' is not of use you can use the convenience functions
-- 'eitherParse', 'eitherRender', or convert a 'Result' to 'Either' using 'eitherResult'.
--
-- > >>> either failure success $ eitherParse tmpl >>= eitherRender env

-- $input
--
-- 'fromPairs' is a wrapper around Aeson's 'object' function which safely strips the outer
-- 'Value' constructor, providing the correct type signature for input into 'render'.
--
-- It is used in combination with the re-exported '.=' as follows:
--
-- > >>> render (fromPairs [ "foo" .= "value", "bar" .= 1 ]) :: Template -> Result Builder

-- $output
--
-- The successful result of rendering an 'Object' environment and 'Template' is
-- a lazy 'Builder' which can be converted to 'Text' using the re-exported 'toLazyText'.

-- $comments
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
-- (ie. an 'Array' or 'Object').
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

-- $boolean_logic
--

-- $relational_logic
--

-- $negation
--

-- $case_statements
--

-- $loops
--

-- $context
--

-- $debugging
--
