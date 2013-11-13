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

import           Control.Applicative     ((<$>), (<*>), (<*), (*>))
import           Control.Monad
import           Data.Foldable           (foldr')
import           Data.Monoid
import qualified Data.Text               as Text
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Builder
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types
import qualified Text.Parsec             as Parsec
import           Text.Parsec             hiding (Error, runParser, parse)
import           Text.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy   (Parser)

-- FIXME:
-- make the parsing more strict, error on invalid sections
--   rather than consuming as fragments
-- support negation of exprs with parens

runParser :: Text -> Result UExp
runParser = either failure Success . Parsec.runParser template () "ede"
  where
    failure e = Error
        (positionMeta $ errorPos e)
        (map messageString $ errorMessages e)

template :: Parser UExp
template = pack $ manyTill expression (try eof)

expression :: Parser UExp
expression = choice [loop, conditional, fragment]

fragment :: Parser UExp
fragment = try var <|> bld
  where
    var = between (symbol "{{") (string "}}") variable
    bld = do
        skipMany $ comments <* optional newline
        UBld <$> meta <*> (fromString <$> manyTill1 anyChar stop)

    stop = try . lookAhead $ next <|> eof
    next = void (char '{' >> oneOf "{%#")

variable :: Parser UExp
variable = pack $ sepBy1 ident (char '.')

ident :: Parser UExp
ident = UVar <$> meta <*> (Id . Text.pack <$> identifier)

loop :: Parser UExp
loop = do
    m <- meta
    (b, i) <- try $ section ((,)
        <$> (reserved "for" >> binding)
        <*> (reserved "in"  >> variable))
    c <- consequent end
    a <- alternative end
    end
    return $ ULoop m b i c a
  where
    end = keyword "endfor"

binding :: Parser UExp
binding = "binding" ?? try pattern <|> ident
  where
    pattern = UApp
        <$> meta
        <*> (symbol "(" *> ident <* symbol ",")
        <*> (ident <* symbol ")")

conditional :: Parser UExp
conditional = UCond
    <$> meta
    <*> try (section $ reserved "if" >> try (operation <|> variable))
    <*> consequent end
    <*> alternative end
     <* end
  where
    end = keyword "endif"

consequent :: Parser () -> Parser UExp
consequent end = pack . manyTill expression . try . lookAhead $
    try (keyword "else") <|> end

alternative :: Parser () -> Parser UExp
alternative end = pack . option mempty $
    try (keyword "else") >> manyTill expression (try $ lookAhead end)

keyword :: String -> Parser ()
keyword = ("keyword" ??) . section . reserved

section :: Parser a -> Parser a
section p = "section" ??
    between (symbol "{%") (string "%}") p <* optional newline

operation :: Parser UExp
operation = buildExpressionParser ops term <?> "operation"
  where
    term = try variable <|> literal

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

literal :: Parser UExp
literal = do
    m <- meta
    "literal" ?? choice
        [ try $ UBool m <$> (try true <|> false)
        , try $ either (UInt m) (UDbl m) <$> numberLiteral
        , UText m . Text.pack <$> stringLiteral
        ]

true, false :: Parser Bool
true  = res "true"  True
false = res "false" False

res :: String -> a -> Parser a
res s = (reserved s >>) . return

meta :: Parser Meta
meta = positionMeta <$> getPosition

positionMeta :: SourcePos -> Meta
positionMeta p = Meta (sourceName p) (sourceLine p) (sourceColumn p)

pack :: Parser [UExp] -> Parser UExp
pack = fmap (foldr' (\a b -> UApp (metadata a) a b) UNil)

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)