{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- (ED-E is a character from Fallout New Vegas, pronounced 'Eddie'.)
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
    , Id
    , includeMap
    , includeFile

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
    , delimPragma
    , delimInline
    , delimComment
    , delimBlock

    , defaultSyntax
    , alternateSyntax

    -- ** Pragmas
    -- $pragmas

    -- ** Expressions
    -- $expressions

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

    -- ** Raw
    -- $raw

    -- ** Comments
    -- $comments

    -- ** Let Expressions
    -- $let
    ) where

import           Control.Monad
import           Data.Aeson                   ((.=))
import           Data.Aeson.Types             (Object)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Foldable                (foldrM)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.List.NonEmpty           (NonEmpty (..))
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder       (toLazyText)
import           Data.Version                 (Version)
import qualified Paths_ede                    as Paths
import           System.Directory
import           System.FilePath
import qualified Text.EDE.Internal.Eval       as Eval
import qualified Text.EDE.Internal.Parser     as Parser
import           Text.EDE.Internal.Quoting    (Term)
import           Text.EDE.Internal.Syntax
import           Text.EDE.Internal.Types      hiding ((</>))
import           Data.Text.Prettyprint.Doc    (Pretty (..))
import           Text.Trifecta.Delta

-- | ED-E Version.
version :: Version
version = Paths.version

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
           => HashMap Id Template -- ^ A 'HashMap' of named 'Template's.
           -> Resolver m          -- ^ Resolver for 'parseWith'.
includeMap ts _ k _
    | Just v <- Map.lookup k ts = success v
    | otherwise = failure ("unable to resolve " <> pretty (Text.unpack k))
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
        then failure ("file " <> pretty p <> " doesn't exist.")
        else BS.readFile p >>= success

-- | Render an 'Object' using the supplied 'Template'.
render :: Template -- ^ Parsed 'Template' to render.
       -> Object   -- ^ Bindings to make available in the environment.
       -> Result LText.Text
render = renderWith mempty

-- | Render an 'Object' using the supplied 'Template'.
renderWith :: HashMap Id Term -- ^ Filters to make available in the environment.
           -> Template        -- ^ Parsed 'Template' to render.
           -> Object          -- ^ Bindings to make available in the environment.
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
eitherRenderWith :: HashMap Id Term
                 -> Template
                 -> Object
                 -> Either String LText.Text
eitherRenderWith fs t = eitherResult . renderWith fs t

-- $usage
--
-- A simple example of parsing and rendering 'Text' containing a basic conditional
-- expression and variable interpolation follows.
--
-- First the 'Template' is defined and parsed in the 'Result' monad:
--
-- >>> tmpl <- parse "{% if var %}\nHello, {{ var }}!\n{% else %}\nnegative!\n{% endif %}\n" :: Result Template
--
-- Then an 'Object' is defined containing the environment which will be
-- available to the 'Template' during rendering:
--
-- >>> let env = fromPairs [ "var" .= "World" ] :: Object
--
-- Note: the 'fromPairs' function above is a wrapper over Aeson's 'object'
-- which removes the outer 'Object' 'Value' constructor, exposing the underlying 'HashMap'.
--
-- Then, the 'Template' is rendered using the 'Object' environment:
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
-- Please see the syntax section for more information about available
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

-- #syntax#
--
-- $pragmas
--
-- Syntax can be modified either via the arguments to 'parseWith' or alternatively
-- by specifying the delimiters via an @EDE_SYNTAX@ pragma.
--
-- /Note:/ The pragmas must start on line1. Subsequently encountered
-- pragmas are parsed as textual template contents.
--
-- For example:
--
-- > {! EDE_SYNTAX pragma=("{*", "*}") inline=("#@", "@#") comment=("<#", "#>") block=("$$", "$$") !}
-- > {* EDE_SYNTAX block=("#[", "]#")  *}
-- > ...
--
-- Would result in the following syntax:
--
-- * Pragmas: @{* ... *}@
--
-- * Inline: @\#\@ ... \@\#@
--
-- * Comment: @\<\# ... \#>@
--
-- * Block: @\#[ ... ]\#@
--
-- /Note:/ @EDE_SYNTAX@ pragmas only take effect for the current template, not
-- child includes. If you want to override the syntax for all templates use 'parseWith'
-- and custom 'Syntax' settings.

-- $expressions
--
-- Expressions behave as any simplistic programming language with a variety of
-- prefix, infix, and postifx operators available. (/See:/ "Text.EDE.Filters")
--
-- A rough overview of the expression grammar:
--
-- > expression ::= literal | identifier | '|' filter
-- > filter     ::= identifier
-- > identifier ::= [a-zA-Z_]{1}[0-9A-Za-z_']*
-- > object     ::= '{' pairs '}'
-- > pairs      ::= string ':' literal | string ':' literal ',' pairs
-- > array      ::= '[' elements ']'
-- > elements   ::= literal | literal ',' elements
-- > literal    ::= object | array | boolean | number | string
-- > boolean    ::= true | false
-- > number     ::= integer | double
-- > string     ::= "char+|escape"

--
-- /Note:/
--
-- * Identifiers are named similarly to Haskell's rules.
--
-- * Booleans are lowered cased.
--
-- * The string quoting and escaping follows Haskell's rules.
--
-- * The Numeric format shares the same characteristics as the <http://json.org/ JSON specification.>

-- $variables
--
-- Variables are substituted directly for their renderable representation.
-- An error is raised if the varaible being substituted is not a literal type
-- (ie. an 'Array' or 'Object') or doesn't exist in the supplied environment.
--
-- > {{ var }}
--
-- Nested variable access is also supported for variables which resolve to an 'Object'.
-- Dot delimiters are used to chain access through multiple nested 'Object's.
-- The right-most accessor must resolve to a renderable type as with the previous
-- non-nested variable access.
--
-- > {{ nested.var.access }}

-- $conditionals
--
-- A conditional is introduced and completed with the section syntax:
--
-- > {% if <expr1> %}
-- >    ... consequent expressions
-- > {% elif <expr2> %}
-- >    ... consequent expressions
-- > {% elif <expr3> %}
-- >    ... consequent expressions
-- > {% else %}
-- >    ... alternate expressions
-- > {% endif %}
--
-- The boolean result of the @expr@ determines the branch that is rendered by
-- the template with multiple (or none) elif branches supported, and the
-- else branch being optional.
--
-- In the case of a literal it conforms directly to the supported boolean or relation logical
-- operators from Haskell.
-- If a variable is singularly used its existence determines the result of the predicate;
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
-- * @Not Equal@: @!=@ (/See:/ '/=')
--
-- * @Greater@: '>'
--
-- * @Greater Or Equal@: '>='
--
-- * @Less@: '<'
--
-- * @Less Or Equal@: '<='
--
-- * @Negation@: @!@ (/See:/ 'not')
--
-- /See:/ "Text.EDE.Filters"

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
--
-- Patterns take the form of @variables@, @literals@, or the wild-card
-- '@_@' pattern (which matches anything).

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
--
-- Valid loop targets are 'Object's, 'Array's, and 'String's, with only 'Object's
-- having an available @{{ <var>.key }}@ in scope.

-- $includes
--
-- Includes are a way to reduce the amount of noise in large templates.
-- They can be used to abstract out common snippets and idioms into partials.
--
-- If 'parseFile' or the 'includeFile' resolver is used, templates will be loaded
-- using 'FilePath's. (This is the default.)
--
-- For example:
--
-- > {% include "/var/tmp/partial.ede" %}
--
-- Loads @partial.ede@ from the file system.
--
-- The current environment is made directly available to the included template.
-- Additional bindings can be created (/See:/ @let@) which will be additionally
-- available only within the include under a specific identifier:
--
-- > {% include "/var/tmp/partial.ede" with some_number = 123 %}
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
-- The input is on the LHS and chained filters (delimited by the pipe operator @|@)
-- are on the RHS, with filters being applied postfix, left associatively.
--
-- /See:/ "Text.EDE.Filters"

-- $raw
--
-- You can disable template processing for blocks of text using the @raw@ section:
--
-- > {% raw %}
-- > Some {{{ handlebars }}} or {{ mustache }} or {{ jinja2 }} output tags etc.
-- > {% endraw %}
--
-- This can be used to avoid parsing expressions which would otherwise be
-- considered valid @ED-E@ syntax.

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

-- $let
--
-- You can also bind an identifier to values which will be available within
-- the following expression scope.
--
-- For example:
--
-- > {% let var = false %}
-- > ...
-- > {{ var }}
-- > ...
-- > {% endlet %}
