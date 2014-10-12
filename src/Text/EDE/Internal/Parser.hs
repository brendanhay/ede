{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Parser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Parser
    ( Includes
    , runParser
    ) where

import           Control.Applicative
import           Control.Arrow              (second)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.ByteString            (ByteString)
import           Data.ByteString.UTF8       as UTF8
import           Data.Char                  (isSpace)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Read
import           Text.EDE.Internal.AST
import qualified Text.EDE.Internal.Syntax   as Syntax
import           Text.EDE.Internal.Types
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import qualified Text.Trifecta              as Tri
import           Text.Trifecta              hiding (Result(..), render)
import           Text.Trifecta.Delta

type Includes = HashMap Text (HashSet Delta)

type Parse m =
    ( Monad m
    , MonadState Includes m
    , TokenParsing m
    , DeltaParsing m
    , LookAheadParsing m
    )

runParser :: String -> ByteString -> Result (Exp, Includes)
runParser n = res . parseByteString (runStateT (document <* eof) mempty) pos
  where
    pos = Directed (UTF8.fromString n) 0 0 0 0

    res (Tri.Success x) = Success x
    res (Tri.Failure e) = Failure e

document :: Parse m => m Exp
document = eapp <$> position <*> many expr
  where
    expr = choice
        [ render
        , ifelif
        , cases
        , loop
        , include
        , binding
        , fragment
        ]

render :: Parse m => m Exp
render = between (symbol "{{") (string "}}") term

fragment :: Parse m => m Exp
fragment = notFollowedBy end >> ELit <$> position <*> txt
  where
    txt = LText . Text.pack <$> manyTill1 anyChar (lookAhead end <|> eof)
    end = void (try (char '{') >> oneOf "{#%")

ifelif :: Parse m => m Exp
ifelif = eif
    <$> branch "if"
    <*> many (branch "elif")
    <*> else'
    <*  exit "endif"
  where
    branch k = (,) <$> block k term <*> document

cases :: Parse m => m Exp
cases = ecase
    <$> block "case" term
    <*> many
        ((,) <$> block "when" pattern
             <*> document)
    <*> else'
    <*  exit "endcase"

loop :: Parse m => m Exp
loop = do
    d <- position
    uncurry (ELoop d)
        <$> block "for"
            ((,) <$> identifier
                 <*> (keyword "in" *> variable))
        <*> document
        <*> else'
        <*  exit "endfor"

include :: Parse m => m Exp
include = do
    d <- position
    block "include" $ do
        k <- stringLiteral
        id %= Map.insertWith (<>) k (Set.singleton d)
        EIncl d k <$> optional (keyword "with" *> term)

binding :: Parse m => m Exp
binding = do
    d <- position
    uncurry (ELet d)
        <$> block "let"
            ((,) <$> identifier
                 <*> (symbol "=" *> term))
        <*> document
        <*  exit "endlet"

block :: Parse m => Text -> m a -> m a
block k = between (try (symbol "{%" *> keyword k)) (string "%}")

else' :: Parse m => m (Maybe Exp)
else' = optional (block "else" (pure ()) *> document)

exit :: Parse m => Text -> m ()
exit k = block k (pure ())

term :: Parse m => m Exp
term = buildExpressionParser table expr
  where
    table =
        [ [prefix "!"]
        , [infix' "*", infix' "/"]
        , [infix' "-", infix' "+"]
        , [infix' "==", infix' "!=", infix' ">", infix' ">=", infix' "<", infix' "<="]
        , [infix' "&&"]
        , [infix' "||"]
        , [filter' "|"]
        ]

    prefix n = Prefix (efun <$> operator n <*> pure (Id n))

    infix' n = Infix (do
        d <- operator n
        return $ \l r ->
            EApp d (efun d (Id n) l) r) AssocLeft

    filter' n = Infix (do
        d <- operator n
        i <- try (lookAhead identifier)
        return $ \l _ ->
            efun d i l) AssocLeft

    expr = parens term <|> apply EVar variable <|> apply ELit literal

pattern :: Parse m => m Pat
pattern = PWild <$ char '_' <|> PVar <$> variable <|> PLit <$> literal

literal :: Parse m => m Lit
literal = LBool <$> boolean <|> LNum <$> number <|> LText <$> stringLiteral

number :: Parse m => m Scientific
number = either fromIntegral fromFloatDigits <$> integerOrDouble

boolean :: Parse m => m Bool
boolean = textSymbol "true " *> return True
      <|> textSymbol "false" *> return False

operator :: Parse m => Text -> m Delta
operator n = position <* reserveText Syntax.operator n

keyword :: Parse m => Text -> m Delta
keyword k = position <* try (reserveText Syntax.keyword k)

variable :: Parse m => m Var
variable = Var <$> (NonEmpty.fromList <$> sepBy1 identifier (char '.'))

identifier :: Parse m => m Id
identifier = Id <$> ident Syntax.variable

manyTill1 :: (Monad m, Alternative m) => m a -> m end -> m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

apply :: Parse m => (Delta -> a -> b) -> m a -> m b
apply f p = f <$> position <*> p
