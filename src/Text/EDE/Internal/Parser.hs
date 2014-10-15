{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Text.EDE.Internal.Parser where

import           Control.Applicative
import           Control.Lens               hiding (both, noneOf)
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import           Data.Char                  (isSpace)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Text.EDE.Internal.Syntax
import           Text.EDE.Internal.Types
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Style    (buildSomeSpaceParser)
import qualified Text.Trifecta              as Tri
import           Text.Trifecta              hiding (Parser, Result(..), spaces)
import           Text.Trifecta.Delta

-- FIXME: make include always have 'scope' as the parent, and inject
-- the 'with <ident> = <expr>' as <ident> into the top-level (not scope)

-- FIXME: the numerous 'try' calls were added during development,
-- these should now be reduced where possible.

data Env = Env
    { _settings :: !Syntax
    , _includes :: HashMap Text (NonEmpty Delta)
    }

makeLenses ''Env

type Parser m =
    ( Monad m
    , MonadState Env m
    , TokenParsing m
    , DeltaParsing m
    , LookAheadParsing m
    )

newtype EDE a = EDE { runEDE :: Tri.Parser a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , Parsing
        , CharParsing
        , DeltaParsing
        , LookAheadParsing
        )

instance TokenParsing EDE where
  nesting (EDE m) = EDE (nesting m)
  {-# INLINE nesting #-}

  someSpace = skipMany (satisfy $ \c -> c /= '\n' && isSpace c)
  {-# INLINE someSpace #-}

  semi = EDE semi
  {-# INLINE semi #-}

  highlight h (EDE m) = EDE (highlight h m)
  {-# INLINE highlight #-}

runParser :: Syntax
          -> Text
          -> ByteString
          -> Result (Exp, HashMap Text (NonEmpty Delta))
runParser o n = res . parseByteString (runEDE run) pos
  where
    run = runStateT (document <* eof) (Env o mempty)
    pos = Directed (Text.encodeUtf8 n) 0 0 0 0

    res (Tri.Success x) = Success (_includes `second` x)
    res (Tri.Failure e) = Failure e

document :: Parser m => m Exp
document = eapp <$> position <*> many (statement <|> substitute <|> fragment)

substitute :: Parser m => m Exp
substitute = between subl subr term

fragment :: Parser m => m Exp
fragment = ELit <$> position <*> pack (notFollowedBy end0 >> try line0 <|> line1)
  where
    line0 = manyTill1 (noneOf "\n") (try (lookAhead end0) <|> eof)
    line1 = manyEndBy1 anyChar newline

    end0 = void (subl <|> blockl <|> try end1)
    end1 = multiLine (pure ()) (manyTill1 anyChar (lookAhead blockr))

statement :: Parser m => m Exp
statement = choice
    [ ifelif
    , cases
    , loop
    , include
    , binding
    , raw
    , comment
    ]

block :: Parser m => String -> m a -> m a
block k p = try (multiLine (keyword k) p) <|> singleLine (keyword k) p

multiLine :: Parser m => m b -> m a -> m a
multiLine s = between (try (lstrip blockl *> s)) (rstrip blockr)

singleLine :: Parser m => m b -> m a -> m a
singleLine s = between (try (blockl *> s)) blockr

ifelif :: Parser m => m Exp
ifelif = eif
    <$> branch "if"
    <*> many (branch "elif")
    <*> else'
    <*  exit "endif"
  where
    branch k = (,) <$> block k term <*> document

cases :: Parser m => m Exp
cases = ecase
    <$> block "case" term
    <*> many
        ((,) <$> block "when" pattern
             <*> document)
    <*> else'
    <*  exit "endcase"

loop :: Parser m => m Exp
loop = do
    d <- position
    uncurry (ELoop d)
        <$> block "for"
            ((,) <$> identifier
                 <*> (keyword "in" *> variable))
        <*> document
        <*> else'
        <*  exit "endfor"

include :: Parser m => m Exp
include = do
    d <- position
    block "include" (incl d)
  where
    incl d = do
        k <- stringLiteral
        includes %= Map.insertWith (<>) k (d:|[])
        ($ EIncl d k) . maybe id (uncurry (ELet d)) <$> scope

    scope = optional $
        (,) <$> (keyword "with" *> identifier)
            <*> (symbol  "="    *> term)

binding :: Parser m => m Exp
binding = do
    d <- position
    uncurry (ELet d)
        <$> block "let"
            ((,) <$> identifier
                 <*> (symbol "=" *> term))
        <*> document
        <*  exit "endlet"

raw :: Parser m => m Exp
raw = ELit
   <$> position
   <*> (start *> pack (manyTill anyChar (lookAhead end)) <* end)
  where
    start = block "raw" (pure ())
    end   = exit "endraw"

-- FIXME: this is due to the whitespace sensitive nature of the parser making
-- it difficult to do what most applicative parsers do by skipping comments
-- as part of the whitespace.
comment :: Parser m => m Exp
comment = ELit
    <$> position
    <*> pure (LText mempty)
    <*  (try (lstrip (rstrip go)) <|> go)
  where
    go = (commentStyle <$> commentl <*> commentr) >>=
        buildSomeSpaceParser (fail "whitespace significant")

else' :: Parser m => m (Maybe Exp)
else' = optional (block "else" (pure ()) *> document)

exit :: Parser m => String -> m ()
exit k = block k (pure ())

term :: Parser m => m Exp
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

pattern :: Parser m => m Pat
pattern = PWild <$ char '_' <|> PVar <$> variable <|> PLit <$> literal

literal :: Parser m => m Lit
literal = LBool <$> boolean <|> LNum <$> number <|> LText <$> stringLiteral

number :: Parser m => m Scientific
number = either fromIntegral fromFloatDigits <$> integerOrDouble

boolean :: Parser m => m Bool
boolean = symbol "true " *> return True
      <|> symbol "false" *> return False

operator :: Parser m => Text -> m Delta
operator n = position <* reserveText operatorStyle n

keyword :: Parser m => String -> m Delta
keyword k = position <* try (reserve keywordStyle k)

variable :: Parser m => m Var
variable = Var <$> (NonEmpty.fromList <$> sepBy1 identifier (char '.'))

identifier :: Parser m => m Id
identifier = ident variableStyle

spaces :: Parser m => m ()
spaces = skipMany (oneOf "\t ")

apply :: Parser m => (Delta -> a -> b) -> m a -> m b
apply f p = f <$> position <*> p

manyTill1 :: Alternative m => m a -> m b -> m [a]
manyTill1 p end = (:) <$> p <*> manyTill p end

manyEndBy1 :: Alternative m => m a -> m a -> m [a]
manyEndBy1 p end = go
  where
    go = (:[]) <$> end <|> (:) <$> p <*> go

pack :: Functor f => f String -> f Lit
pack = fmap (LText . Text.pack)

lstrip :: Parser m => m a -> m a
lstrip p = do
    c <- column <$> position
    if c == 0
        then spaces *> p
        else fail "left whitespace removal failed"

rstrip :: Parser m => m a -> m a
rstrip p = p <* spaces <* newline

commentl, commentr :: MonadState Env m => m String
commentl = syntax (delimComment._1)
commentr = syntax (delimComment._2)

subl, subr :: Parser m => m String
subl = syntax (delimSubstitute._1) >>= symbol
subr = syntax (delimSubstitute._2) >>= string

blockl, blockr :: Parser m => m String
blockl = syntax (delimBlock._1) >>= symbol
blockr = syntax (delimBlock._2) >>= string

syntax :: MonadState Env m => Getter Syntax a -> m a
syntax l = gets (view (settings.l))
