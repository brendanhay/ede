{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Text.EDE.Internal.Parser
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Parser where

import           Control.Applicative
import           Control.Comonad            (extract)
import           Control.Comonad.Cofree
import           Control.Lens               hiding ((:<), both, noneOf, op)
import           Control.Monad.State.Strict
import           Data.Aeson.Types           (Array, Object, Value (..))
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import           Data.Char                  (isSpace)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Scientific
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Vector                as Vector
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Syntax
import           Text.EDE.Internal.Types
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Style    (buildSomeSpaceParser)
import           Text.Trifecta              hiding (Parser, Result (..), spaces)
import qualified Text.Trifecta              as Tri
import           Text.Trifecta.Delta

data Env = Env
    { _settings :: !Syntax
    , _includes :: HashMap Text (NonEmpty Delta)
    }

makeLenses ''Env

instance HasSyntax Env where
    syntax = settings

type Parser m =
    ( Monad m
#if MIN_VERSION_base(4,13,0)
    , MonadFail m
#endif
    , MonadState Env m
    , TokenParsing m
    , DeltaParsing m
    , LookAheadParsing m
    , Errable m
    )

newtype EDE a = EDE { runEDE :: Tri.Parser a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
#if MIN_VERSION_base(4,13,0)
        , MonadFail
#endif
        , MonadPlus
        , Parsing
        , CharParsing
        , DeltaParsing
        , LookAheadParsing
        , Errable
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

instance Errable (StateT Env EDE) where
    raiseErr = lift . raiseErr

runParser :: Syntax
          -> Text
          -> ByteString
          -> Result (Exp Delta, HashMap Text (NonEmpty Delta))
runParser o n = res . parseByteString (runEDE run) pos
  where
    run = runStateT (pragma *> document <* eof) (Env o mempty)
    pos = Directed (Text.encodeUtf8 n) 0 0 0 0

    res (Tri.Success x) = Success (_includes `second` x)
    res (Tri.Failure e) = Failure $ _errDoc e

pragma :: Parser m => m ()
pragma = void . many $ do
    !xs <- pragmal *> symbol "EDE_SYNTAX" *> sepBy field spaces <* trimr pragmar
    mapM_ (uncurry assign) xs
  where
    field = (,) <$> setter <* symbol "=" <*> parens delim

    delim = (,) <$> stringLiteral <* symbol "," <*> stringLiteral

    setter = pragmak "pragma"  *> pure delimPragma
         <|> pragmak "inline"  *> pure delimInline
         <|> pragmak "comment" *> pure delimComment
         <|> pragmak "block"   *> pure delimBlock

document :: Parser m => m (Exp Delta)
document = eapp <$> position <*> many (statement <|> inline <|> fragment)

inline :: Parser m => m (Exp Delta)
inline = between inlinel inliner term

fragment :: Parser m => m (Exp Delta)
fragment = ann (ELit <$> pack (notFollowedBy end0 >> try line0 <|> line1))
  where
    line0 = manyTill1 (noneOf "\n") (try (lookAhead end0) <|> eof)
    line1 = manyEndBy1 anyChar newline

    end0 = void (inlinel <|> blockl <|> try end1)
    end1 = multiLine (pure ()) (manyTill1 anyChar (lookAhead blockr))

statement :: Parser m => m (Exp Delta)
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
multiLine s = between (try (triml blockl *> s)) (trimr blockr)

singleLine :: Parser m => m b -> m a -> m a
singleLine s = between (try (blockl *> s)) blockr

ifelif :: Parser m => m (Exp Delta)
ifelif = eif
    <$> branch "if"
    <*> many (branch "elif")
    <*> else'
    <*  exit "endif"
  where
    branch k = (,) <$> block k term <*> document

cases :: Parser m => m (Exp Delta)
cases = ecase
    <$> block "case" term
    <*> many
        ((,) <$> block "when" pattern
             <*> document)
    <*> else'
    <*  exit "endcase"

loop :: Parser m => m (Exp Delta)
loop = do
    (i, v) <- block "for"
        ((,) <$> identifier
             <*> (keyword "in" *> collection))
    d      <- document
    eempty v (extract v :< ELoop i v d)
        <$> else'
        <*  exit "endfor"

include :: Parser m => m (Exp Delta)
include = block "include" $ do
    d <- position
    k <- stringLiteral
    includes %= Map.insertWith (<>) k (d:|[])
    elet <$> scope <*> pure (d :< EIncl k)
  where
    scope = optional $
        (,) <$> (keyword "with" *> identifier)
            <*> (symbol  "="    *> term)

binding :: Parser m => m (Exp Delta)
binding = elet . Just
    <$> block "let"
        ((,) <$> identifier
             <*> (symbol "=" *> term))
    <*> document
    <*  exit "endlet"

raw :: Parser m => m (Exp Delta)
raw = ann (ELit <$> body)
  where
    body  = start *> pack (manyTill anyChar (lookAhead end)) <* end
    start = block "raw" (pure ())
    end   = exit "endraw"

-- FIXME: this is due to the whitespace sensitive nature of the parser making
-- it difficult to do what most applicative parsers do by skipping comments
-- as part of the whitespace.
comment :: Parser m => m (Exp Delta)
comment = ann (ELit <$> pure (String mempty) <* (try (triml (trimr go)) <|> go))
  where
    go = (commentStyle <$> commentl <*> commentr) >>=
        buildSomeSpaceParser (fail "whitespace significant")

else' :: Parser m => m (Maybe (Exp Delta))
else' = optional (block "else" (pure ()) *> document)

exit :: Parser m => String -> m ()
exit k = block k (pure ())

pattern :: Parser m => m Pat
pattern = PWild <$ char '_' <|> PVar <$> variable <|> PLit <$> literal

term :: Parser m => m (Exp Delta)
term = chainl1' term0 (try filter') (symbol "|" *> pure efilter) <|> term0

term0 :: Parser m => m (Exp Delta)
term0 = buildExpressionParser table expr
  where
    table =
        [ [prefix "!"]
        , [infix' "*", infix' "/"]
        , [infix' "-", infix' "+"]
        , [infix' "==", infix' "!=", infix' ">", infix' ">=", infix' "<", infix' "<="]
        , [infix' "&&"]
        , [infix' "||"]
        ]

    prefix n = Prefix (efun <$ operator n <*> pure n)

    infix' n = Infix (do
        d <- operator n
        return $ \l r ->
            d :< EApp (efun n l) r) AssocLeft

    expr = parens term
       <|> ann (EVar <$> variable)
       <|> ann (ELit <$> literal)

filter' :: Parser m => m (Id, [Exp Delta])
filter' = (,) <$> identifier <*> (parens (commaSep1 term) <|> pure [])

collection :: Parser m => m (Exp Delta)
collection = ann (EVar <$> variable <|> ELit <$> col)
  where
    col = Object <$> object
      <|> Array  <$> array
      <|> String <$> stringLiteral

literal :: Parser m => m Value
literal = Bool   <$> bool
      <|> Number <$> number
      <|> String <$> stringLiteral
      <|> Object <$> object
      <|> Array  <$> array

number :: Parser m => m Scientific
number = either fromIntegral fromFloatDigits <$> integerOrDouble

bool :: Parser m => m Bool
bool = symbol "true" *> return True <|> symbol "false" *> return False

object :: Parser m => m Object
object = Map.fromList <$> braces (commaSep pair)
  where
    pair = (,)
        <$> (stringLiteral <* spaces)
        <*> (char ':' *> spaces *> literal)

array :: Parser m => m Array
array = Vector.fromList <$> brackets (commaSep literal)

operator :: Parser m => Text -> m Delta
operator n = position <* reserveText operatorStyle n

keyword :: Parser m => String -> m Delta
keyword k = position <* try (reserve keywordStyle k)

variable :: (Monad m, TokenParsing m) => m Var
variable = Var <$> (NonEmpty.fromList <$> sepBy1 identifier (char '.'))

identifier :: (Monad m, TokenParsing m) => m Id
identifier = ident variableStyle

spaces :: (Monad m, TokenParsing m) => m ()
spaces = skipMany (oneOf "\t ")

manyTill1 :: Alternative m => m a -> m b -> m [a]
manyTill1 p end = (:) <$> p <*> manyTill p end

manyEndBy1 :: Alternative m => m a -> m a -> m [a]
manyEndBy1 p end = go
  where
    go = (:[]) <$> end <|> (:) <$> p <*> go

chainl1' :: Alternative m => m a -> m b -> m (a -> b -> a) -> m a
chainl1' l r op = scan
  where
    scan = flip id <$> l <*> rst
    rst  = (\f y g x -> g (f x y)) <$> op <*> r <*> rst <|> pure id

ann :: (DeltaParsing m, Functor f) => m (f (Mu f)) -> m (Cofree f Delta)
ann p = cofree <$> position <*> (Mu <$> p)

pack :: Functor f => f String -> f Value
pack = fmap (String . Text.pack)

triml :: Parser m => m a -> m a
triml p = do
    c <- column <$> position
    if c == 0
        then spaces *> p
        else fail "left whitespace removal failed"

trimr :: Parser m => m a -> m a
trimr p = p <* spaces <* newline

pragmak :: Parser m => String -> m ()
pragmak = reserve pragmaStyle

pragmal, pragmar :: Parser m => m String
pragmal = left  delimPragma >>= symbol
pragmar = right delimPragma >>= string

commentl, commentr :: MonadState Env m => m String
commentl = left  delimComment
commentr = right delimComment

inlinel, inliner :: Parser m => m String
inlinel = left  delimInline >>= symbol
inliner = right delimInline >>= string

blockl, blockr :: Parser m => m String
blockl = left  delimBlock >>= symbol
blockr = right delimBlock >>= string

left, right :: MonadState s m => Getter s Delim -> m String
left  d = gets (fst . view d)
right d = gets (snd . view d)
