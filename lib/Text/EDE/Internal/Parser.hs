{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Text.EDE.Internal.Parser
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
module Text.EDE.Internal.Parser where

import Control.Applicative (Alternative ((<|>)))
import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens ((%=))
import qualified Control.Lens as Lens
import Control.Monad (MonadPlus, void)
import Control.Monad.State.Strict (MonadState, StateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (lift)
import Data.Aeson.Types (Array, Object, Value (..))
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import Text.EDE.Internal.AST
import Text.EDE.Internal.Syntax
import Text.EDE.Internal.Types
import qualified Text.Parser.Expression as Expression
import Text.Parser.LookAhead (lookAhead)
import qualified Text.Parser.LookAhead as LookAhead
import Text.Parser.Token.Style (buildSomeSpaceParser)
import Text.Trifecta (DeltaParsing, TokenParsing)
import qualified Text.Trifecta as Trifecta
import Text.Trifecta.Delta (Delta)
import qualified Text.Trifecta.Delta as Trifecta.Delta

data Env = Env
  { _settings :: !Syntax,
    _includes :: HashMap Text (NonEmpty Delta)
  }

$(Lens.makeLenses ''Env)

instance HasSyntax Env where
  syntax = settings

type Parser m =
  ( Monad m,
#if MIN_VERSION_base(4,13,0)
    MonadFail m,
#endif
    MonadState Env m,
    Trifecta.TokenParsing m,
    Trifecta.DeltaParsing m,
    LookAhead.LookAheadParsing m,
    Trifecta.Errable m
  )

newtype EDE a = EDE {runEDE :: Trifecta.Parser a}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
#if MIN_VERSION_base(4,13,0)
      MonadFail,
#endif
      MonadPlus,
      Trifecta.Parsing,
      Trifecta.CharParsing,
      Trifecta.DeltaParsing,
      LookAhead.LookAheadParsing,
      Trifecta.Errable
    )

instance TokenParsing EDE where
  nesting (EDE m) =
    EDE $ Trifecta.nesting m
  {-# INLINE nesting #-}

  someSpace =
    EDE $ Trifecta.skipMany (Trifecta.satisfy $ \c -> c /= '\n' && Char.isSpace c)
  {-# INLINE someSpace #-}

  semi =
    EDE Trifecta.semi
  {-# INLINE semi #-}

  highlight h (EDE m) = EDE (Trifecta.highlight h m)
  {-# INLINE highlight #-}

instance Trifecta.Errable (StateT Env EDE) where
  raiseErr = lift . Trifecta.raiseErr

runParser ::
  Syntax ->
  Text ->
  ByteString ->
  Result (Exp Delta, HashMap Text (NonEmpty Delta))
runParser o n = res . Trifecta.parseByteString (runEDE run) pos
  where
    run = State.runStateT (pragma *> document <* Trifecta.eof) (Env o mempty)

    pos = Trifecta.Delta.Directed (Text.Encoding.encodeUtf8 n) 0 0 0 0

    res = \case
      Trifecta.Success x -> Success (Bifunctor.second _includes x)
      Trifecta.Failure e -> Failure (Trifecta._errDoc e)

pragma :: Parser m => m ()
pragma =
  void . Trifecta.many $ do
    !xs <- pragmal *> Trifecta.symbol "EDE_SYNTAX" *> Trifecta.sepBy field spaces <* trimr pragmar

    mapM_ (uncurry Lens.assign) xs
  where
    field =
      (,)
        <$> setter <* Trifecta.symbol "="
        <*> Trifecta.parens delim

    delim =
      (,)
        <$> Trifecta.stringLiteral <* Trifecta.symbol ","
        <*> Trifecta.stringLiteral

    setter =
      pragmak "pragma" *> pure delimPragma
        <|> pragmak "inline" *> pure delimInline
        <|> pragmak "comment" *> pure delimComment
        <|> pragmak "block" *> pure delimBlock

document :: Parser m => m (Exp Delta)
document =
  eapp
    <$> Trifecta.position
    <*> Trifecta.many (statement <|> inline <|> fragment)

inline :: Parser m => m (Exp Delta)
inline = Trifecta.between inlinel inliner term

fragment :: Parser m => m (Exp Delta)
fragment =
  ann (ELit <$> pack (Trifecta.notFollowedBy end0 >> Trifecta.try line0 <|> line1))
  where
    line0 = manyTill1 (Trifecta.noneOf "\n") (Trifecta.try (lookAhead end0) <|> Trifecta.eof)
    line1 = manyEndBy1 Trifecta.anyChar Trifecta.newline

    end0 = void (inlinel <|> blockl <|> Trifecta.try end1)
    end1 = multiLine (pure ()) (manyTill1 Trifecta.anyChar (lookAhead blockr))

statement :: Parser m => m (Exp Delta)
statement =
  Trifecta.choice
    [ ifelif,
      cases,
      loop,
      include,
      binding,
      raw,
      comment
    ]

block :: Parser m => String -> m a -> m a
block k p =
  Trifecta.try (multiLine (keyword k) p)
    <|> singleLine (keyword k) p

multiLine :: Parser m => m b -> m a -> m a
multiLine s =
  Trifecta.between (Trifecta.try (triml blockl *> s)) (trimr blockr)

singleLine :: Parser m => m b -> m a -> m a
singleLine s =
  Trifecta.between (Trifecta.try (blockl *> s)) blockr

ifelif :: Parser m => m (Exp Delta)
ifelif =
  eif
    <$> branch "if"
    <*> Trifecta.many (branch "elif")
    <*> else'
    <* exit "endif"
  where
    branch k = (,) <$> block k term <*> document

cases :: Parser m => m (Exp Delta)
cases =
  ecase
    <$> block "case" term
    <*> Trifecta.many
      ( (,) <$> block "when" pattern
          <*> document
      )
    <*> else'
    <* exit "endcase"

loop :: Parser m => m (Exp Delta)
loop = do
  (i, v) <-
    block
      "for"
      ( (,) <$> identifier
          <*> (keyword "in" *> collection)
      )
  d <- document
  eempty v (Comonad.extract v :< ELoop i v d)
    <$> else'
    <* exit "endfor"

include :: Parser m => m (Exp Delta)
include = block "include" $ do
  d <- Trifecta.position
  k <- Trifecta.stringLiteral
  includes %= HashMap.insertWith (<>) k (d :| [])
  elet <$> scope <*> pure (d :< EIncl k)
  where
    scope =
      Trifecta.optional $
        (,) <$> (keyword "with" *> identifier)
          <*> (Trifecta.symbol "=" *> term)

binding :: Parser m => m (Exp Delta)
binding =
  elet . Just
    <$> block
      "let"
      ( (,) <$> identifier
          <*> (Trifecta.symbol "=" *> term)
      )
    <*> document
    <* exit "endlet"

raw :: Parser m => m (Exp Delta)
raw = ann (ELit <$> body)
  where
    body = start *> pack (Trifecta.manyTill Trifecta.anyChar (lookAhead end)) <* end
    start = block "raw" (pure ())
    end = exit "endraw"

-- FIXME: this is due to the whitespace sensitive nature of the parser making
-- it difficult to do what most applicative parsers do by skipping comments
-- as part of the whitespace.
comment :: Parser m => m (Exp Delta)
comment = ann (ELit <$> pure (String mempty) <* (Trifecta.try (triml (trimr go)) <|> go))
  where
    go =
      (commentStyle <$> commentl <*> commentr)
        >>= buildSomeSpaceParser (fail "whitespace significant")

else' :: Parser m => m (Maybe (Exp Delta))
else' = Trifecta.optional (block "else" (pure ()) *> document)

exit :: Parser m => String -> m ()
exit k = block k (pure ())

pattern :: Parser m => m Pat
pattern =
  PWild <$ Trifecta.char '_'
    <|> PVar <$> variable
    <|> PLit <$> literal

term :: Parser m => m (Exp Delta)
term =
  chainl1' term0 (Trifecta.try filter') (Trifecta.symbol "|" *> pure efilter)
    <|> term0

term0 :: Parser m => m (Exp Delta)
term0 = Expression.buildExpressionParser table expr
  where
    table =
      [ [prefix "!"],
        [infix' "*", infix' "/"],
        [infix' "-", infix' "+"],
        [infix' "==", infix' "!=", infix' ">", infix' ">=", infix' "<", infix' "<="],
        [infix' "&&"],
        [infix' "||"]
      ]

    prefix n = Expression.Prefix (efun <$ operator n <*> pure n)

    infix' n =
      Expression.Infix
        ( do
            d <- operator n
            pure $ \l r ->
              d :< EApp (efun n l) r
        )
        Expression.AssocLeft

    expr =
      Trifecta.parens term
        <|> ann (EVar <$> variable)
        <|> ann (ELit <$> literal)

filter' :: Parser m => m (Id, [Exp Delta])
filter' =
  (,)
    <$> identifier
    <*> (Trifecta.parens (Trifecta.commaSep1 term) <|> pure [])

collection :: Parser m => m (Exp Delta)
collection = ann (EVar <$> variable <|> ELit <$> col)
  where
    col =
      Object <$> object
        <|> Array <$> array
        <|> String <$> Trifecta.stringLiteral

literal :: Parser m => m Value
literal =
  Bool <$> bool
    <|> Number <$> number
    <|> String <$> Trifecta.stringLiteral
    <|> Object <$> object
    <|> Array <$> array

number :: Parser m => m Scientific
number =
  either fromIntegral Scientific.fromFloatDigits
    <$> Trifecta.integerOrDouble

bool :: Parser m => m Bool
bool =
  Trifecta.symbol "true" *> pure True
    <|> Trifecta.symbol "false" *> pure False

object :: Parser m => m Object
object = HashMap.fromList <$> Trifecta.braces (Trifecta.commaSep pair)
  where
    pair =
      (,)
        <$> (Trifecta.stringLiteral <* Trifecta.spaces)
        <*> (Trifecta.char ':' *> Trifecta.spaces *> literal)

array :: Parser m => m Array
array = Vector.fromList <$> Trifecta.brackets (Trifecta.commaSep literal)

operator :: Parser m => Text -> m Delta
operator n = Trifecta.position <* Trifecta.reserveText operatorStyle n

keyword :: Parser m => String -> m Delta
keyword k = Trifecta.position <* Trifecta.try (Trifecta.reserve keywordStyle k)

variable :: (Monad m, TokenParsing m) => m Var
variable =
  Var <$> (NonEmpty.fromList <$> Trifecta.sepBy1 identifier (Trifecta.char '.'))

identifier :: (Monad m, TokenParsing m) => m Id
identifier = Trifecta.ident variableStyle

spaces :: (Monad m, TokenParsing m) => m ()
spaces = Trifecta.skipMany (Trifecta.oneOf "\t ")

manyTill1 :: Alternative m => m a -> m b -> m [a]
manyTill1 p end = (:) <$> p <*> Trifecta.manyTill p end

manyEndBy1 :: Alternative m => m a -> m a -> m [a]
manyEndBy1 p end = go
  where
    go = (: []) <$> end <|> (:) <$> p <*> go

chainl1' :: Alternative m => m a -> m b -> m (a -> b -> a) -> m a
chainl1' l r op = scan
  where
    scan = flip id <$> l <*> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> r <*> rst <|> pure id

ann :: (DeltaParsing m, Functor f) => m (f (Fix f)) -> m (Cofree f Delta)
ann p = cofreeFix <$> Trifecta.position <*> (Fix <$> p)

pack :: Functor f => f String -> f Value
pack = fmap (String . Text.pack)

triml :: Parser m => m a -> m a
triml p = do
  c <- Trifecta.Delta.column <$> Trifecta.position
  if c == 0
    then Trifecta.spaces *> p
    else fail "left whitespace removal failed"

trimr :: Parser m => m a -> m a
trimr p = p <* Trifecta.spaces <* Trifecta.newline

pragmak :: Parser m => String -> m ()
pragmak = Trifecta.reserve pragmaStyle

pragmal, pragmar :: Parser m => m String
pragmal = left delimPragma >>= Trifecta.symbol
pragmar = right delimPragma >>= Trifecta.string

commentl, commentr :: MonadState Env m => m String
commentl = left delimComment
commentr = right delimComment

inlinel, inliner :: Parser m => m String
inlinel = left delimInline >>= Trifecta.symbol
inliner = right delimInline >>= Trifecta.string

blockl, blockr :: Parser m => m String
blockl = left delimBlock >>= Trifecta.symbol
blockr = right delimBlock >>= Trifecta.string

left, right :: MonadState s m => Lens.Getter s Delim -> m String
left d = State.gets (fst . Lens.view d)
right d = State.gets (snd . Lens.view d)
