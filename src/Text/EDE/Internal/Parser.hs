{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

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
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Syntax
import           Text.EDE.Internal.Types
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import qualified Text.Trifecta              as Tri
import           Text.Trifecta              hiding (Result(..), render)
import           Text.Trifecta.Delta

-- FIXME: add 'raw' tag
-- whitespace
-- comments

type Includes = HashMap Text (NonEmpty Delta)

data Env = Env
    { _options  :: !Syntax
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
render = between renderStart renderEnd term

fragment :: Parse m => m Exp
fragment = notFollowedBy anyStart >> ELit
    <$> position
    <*> (LText . Text.pack <$> manyTill1 anyChar (lookAhead anyStart <|> eof))

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

block :: Parse m => String -> m a -> m a
block k = between (try (start *> keyword k)) end
  where
    start = lstrip blockStart
    end   = rtrim  blockEnd

    lstrip p = do
        c <- columnByte <$> position
        n <- fmap fromIntegral . BS.findIndex retain <$> line
        if n < Just c
           then p
           else skipMany (oneOf "\t ") *> p

    rtrim p = p <* try (skipMany (oneOf "\t ") >> newline)

    retain 32 = False
    retain 9  = False
    retain _  = True

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

manyTill1 :: (Monad m, Alternative m) => m a -> m b -> m [a]
manyTill1 p e = liftM2 (:) p (manyTill p e)

apply :: Parse m => (Delta -> a -> b) -> m a -> m b
apply f p = f <$> position <*> p

anyStart :: Parse m => m ()
anyStart = void . try $ renderStart <|> blockStart -- <|> commentStart

renderStart, blockStart :: Parse m => m ()
renderStart  = syntax (delimRender._1)  >>= void . symbol
--commentStart = syntax (delimComment._1) >>= void . symbol
blockStart   = syntax (delimBlock._1)   >>= void . symbol

renderEnd, blockEnd :: Parse m => m ()
renderEnd  = syntax (delimRender._2)  >>= void . string
--commentEnd = syntax (delimComment._2) >>= void . string
blockEnd   = syntax (delimBlock._2)   >>= void . string

syntax :: MonadState Env m => Getter Syntax a -> m a
syntax l = gets (view (options.l))
