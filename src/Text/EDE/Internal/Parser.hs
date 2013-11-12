{-# LANGUAGE TupleSections #-}

-- Module      : Text.EDE.Internal.Parser
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Parser where

import           Control.Applicative     ((<$>), (<*>), (<*), (*>), pure)
import           Control.Monad
import           Data.Foldable           (foldr')
import           Data.Monoid
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types hiding (ident)
import qualified Text.Parsec             as Parsec
import           Text.Parsec             hiding (runParser, parse)
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy   (Parser)

-- FIXME:
-- make the parsing more strict, error on invalid sections
--   rather than consuming as fragments
-- support negation of exprs with parens

runParser :: String -> LText.Text -> Result UExp
runParser name = either ParseError Success . Parsec.runParser template () name

template :: Parser UExp
template = pack $ manyTill expression (try eof)

expression :: Parser UExp
expression = choice [loop, conditional, fragment]

fragment :: Parser UExp
fragment = try variable <|> do
    skipMany $ comments <* optional newline
    UFrag <$> meta <*> (FBld . fromString <$> manyTill1 anyChar stop)
  where
    stop = try . lookAhead $ next <|> eof
    next = void (char '{' >> oneOf "{%#")

variable :: Parser UExp
variable = do
    m <- meta
    v <- between (symbol "{{") (string "}}") ident
    return . UFrag m $ FVar m v

loop :: Parser UExp
loop = do
    m <- meta
    (b, i) <- try $ section ((,)
        <$> (reserved "for" >> binding)
        <*> (reserved "in"  >> ident))
    c <- consequent end
    a <- alternative end
    end
    return $ ULoop m b i c a
  where
    end = keyword "endfor"

conditional :: Parser UExp
conditional = UCond
    <$> meta
    <*> try (section $ reserved "if" >> pre)
    <*> consequent end
    <*> alternative end
     <* end
  where
    pre = try operation <|> (UVar <$> meta <*> ident)
    end = keyword "endif"

consequent :: Parser () -> Parser UExp
consequent end = pack . manyTill expression . try . lookAhead $
    try (keyword "else") <|> end

alternative :: Parser () -> Parser UExp
alternative end = pack . option mempty $
    try (keyword "else") >> manyTill expression (try $ lookAhead end)

term :: Parser UExp
term = try variable <|> literal

keyword :: String -> Parser ()
keyword = ("keyword" ??) . section . reserved

section :: Parser a -> Parser a
section p = "section" ??
    between (symbol "{%") (string "%}") p <* optional newline

ident :: Parser Ident
ident = "ident" ?? Ident . map Text.pack <$> sepBy1 identifier (symbol ".")

binding :: Parser Bind
binding = "binding" ?? try pattern <|> nominal
  where
    pattern = Bind
        <$> meta
        <*> (symbol "(" *> key <* symbol ",")
        <*> (Just <$> (key <* symbol ")"))

    nominal = Bind
        <$> meta
        <*> key
        <*> pure Nothing

    key = Text.pack <$> identifier

literal :: Parser UExp
literal = "literal" ?? choice
    [ try bool
    , try integer
    , try double
    , text
    ]

bool :: Parser UExp
bool = parse "bool" UBool (try true <|> false)

integer :: Parser UExp
integer = parse "integer" UInt integerLiteral

double :: Parser UExp
double = parse "double" UDbl doubleLiteral

text :: Parser UExp
text = parse "text" (\m -> UText m . Text.pack) stringLiteral

parse :: String -> (Meta -> a -> b) -> Parser a -> Parser b
parse name f p = do
    m <- meta
    (f m <$> p) <?> name

operation :: Parser UExp
operation = buildExpressionParser ops term <?> "operation"
  where
    ops = [ [Prefix $ op "!" UNeg]
          , [Infix (bin "&&" And)          AssocLeft]
          , [Infix (bin "||" Or)           AssocLeft]
          , [Infix (rel "==" Equal)        AssocLeft]
          , [Infix (rel "/=" NotEqual)     AssocLeft]
          , [Infix (rel ">"  Greater)      AssocLeft]
          , [Infix (rel ">=" GreaterEqual) AssocLeft]
          , [Infix (rel "<"  Less)         AssocLeft]
          , [Infix (rel "<=" LessEqual)    AssocLeft]
          ]

    bin o = op o . flip UBin
    rel o = op o . flip URel

    op o f = f <$> (reservedOp o >> meta)

true, false :: Parser Bool
true  = res "true"  True
false = res "false" False

res :: String -> a -> Parser a
res s = (reserved s >>) . return

meta :: Parser Meta
meta = do
    p <- getPosition
    return $ Meta (sourceName p) (sourceLine p) (sourceColumn p)

pack :: Parser [UExp] -> Parser UExp
pack = fmap (foldr' (<>) mempty)

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)
