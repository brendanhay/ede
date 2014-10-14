{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           Control.Lens               hiding (both, noneOf)
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (nub)
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Debug.Trace
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Syntax
import           Text.EDE.Internal.Types
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import qualified Text.Trifecta              as Tri
import           Text.Trifecta              hiding (Result(..), render, spaces)
import           Text.Trifecta.Delta

-- FIXME: add 'raw' tag
-- whitespace
-- comments

type Includes = HashMap Text (NonEmpty Delta)

data Env = Env
    { _syntax  :: !Syntax
    , _includes :: Includes
    }

makeLenses ''Env

type Parse m =
    ( Monad m
    , MonadState Env m
    , TokenParsing m
    , DeltaParsing m
    , LookAheadParsing m
    )

runParser :: Syntax -> Text -> ByteString -> Result (Exp, Includes)
runParser o n = res . parseByteString (runStateT (document <* eof) env) pos
  where
    env = Env o mempty
    pos = Directed (Text.encodeUtf8 n) 0 0 0 0

    res (Tri.Success x) = Success (_includes `second` x)
    res (Tri.Failure e) = Failure e

document :: Parse m => m Exp
document = eapp <$> position <*> many (statement <|> render <|> fragment)

render :: Parse m => m Exp
render = between renderStart renderEnd term

fragment :: Parse m => m Exp
fragment = ELit <$> position <*> pack (notFollowedBy cond >> try l0 <|> l1)
  where
    l0 = manyTill1 (noneOf "\n") (cond <|> eof)
    l1 = manyEndBy1 anyChar newline

    cond = () <$ lookAhead (renderStart <|> try blockStart)

    pack = fmap (LText . Text.pack)

statement :: Parse m => m Exp
statement = trace "statement" $ choice
    [ ifelif
    , cases
    , loop
    , include
    , binding
    ]

block :: Parse m => String -> m a -> m a
block k = between (try (blockStart *> keyword k)) (try rstrip <|> blockEnd)
  where
    rstrip = blockEnd <* skipMany (oneOf "\t ") <* newline

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
        includes %= Map.insertWith (<>) k (d:|[])
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

else' :: Parse m => m (Maybe Exp)
else' = optional (block "else" (pure ()) *> document)

exit :: Parse m => String -> m ()
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

    prefix n = Prefix (efun <$> operator n <*> pure n)

    infix' n = Infix (do
        d <- operator n
        return $ \l r ->
            EApp d (efun d n l) r) AssocLeft

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
boolean = symbol "true " *> return True
      <|> symbol "false" *> return False

operator :: Parse m => Text -> m Delta
operator n = position <* reserveText operatorStyle n

keyword :: Parse m => String -> m Delta
keyword k = position <* try (reserve keywordStyle k)

variable :: Parse m => m Var
variable = Var <$> (NonEmpty.fromList <$> sepBy1 identifier (char '.'))

identifier :: Parse m => m Id
identifier = ident variableStyle

comments :: Parse m => m ()
comments = do
    (start, end) <- syntax' delimComment
    skipSome (try (string start) *> go (nub (start ++ end)) end)
  where
    go both end = ()
        <$  try (string end)
        <|> comments               *> go both end
        <|> skipSome (noneOf both) *> go both end
        <|> oneOf both             *> go both end
        <?> "end of comment"

apply :: Parse m => (Delta -> a -> b) -> m a -> m b
apply f p = f <$> position <*> p

manyTill1 :: Alternative m => m a -> m b -> m [a]
manyTill1 p end = (:) <$> p <*> manyTill p end

manyEndBy1 :: Alternative m => m a -> m a -> m [a]
manyEndBy1 p end = go
  where
    go = (:[]) <$> end <|> (:) <$> p <*> go

renderStart, commentStart, blockStart :: Parse m => m String
renderStart  = syntax' (delimRender  . _1) >>= symbol
commentStart = syntax' (delimComment . _1) >>= string
blockStart   = do
    d <- syntax' (delimBlock   . _1)
    c <- column <$> position
    if c == 0
        then skipMany (oneOf "\t ") *> symbol d
        else symbol d

renderEnd, blockEnd :: Parse m => m String
renderEnd = syntax' (delimRender . _2) >>= string
blockEnd  = syntax' (delimBlock  . _2) >>= string

syntax' :: MonadState Env m => Getter Syntax a -> m a
syntax' l = gets (view (syntax.l))
