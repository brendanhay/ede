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
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy   (Parser)

-- FIXME:
-- add support for whitespace removal/preservation via -/+

runParser :: Text -> Result UExp
runParser = either failure Success . Parsec.runParser template () "ede"
  where
    failure e = Error
        (positionMeta $ errorPos e)
        [show e]

template :: Parser UExp
template = pack . manyTill expression $ try eof

expression :: Parser UExp
expression = choice [raw, fragment, substitution, conditional, loop, case']

raw :: Parser UExp
raw = do
    try $ keyword "raw"
    UBld <$> meta
         <*> (fromString <$> manyTill1 anyChar (try $ lookAhead end <|> eof))
          <* end
  where
    end = keyword "endraw"

fragment :: Parser UExp
fragment = do
    "comment" ?? skipMany (comments <* optional newline)
    try $ lookAhead (next >> fail "") <|> return ()
    UBld <$> meta
         <*> (fromString <$> manyTill1 anyChar end)
  where
    end  = try . lookAhead $ next <|> eof
    next = void $ char '{' >> oneOf "{%#"

substitution :: Parser UExp
substitution = "substitution" ?? try (between (symbol "{{") (string "}}") term)

variable :: Parser UExp
variable = "variable" ??
    filtered (pack $ sepBy1 (UVar <$> meta <*> ident) (char '.'))

filtered :: Parser UExp -> Parser UExp
filtered p = try f <|> p
  where
    f = do
        p' <- p
        f' <- try (symbol "|") *> sepBy1 (UFun <$> meta <*> ident) (symbol "|")
        pack . return . reverse $ p' : f'

ident :: Parser Id
ident = Id . Text.pack <$> identifier

conditional :: Parser UExp
conditional = UCond
    <$> meta
    <*> try (section $ reserved "if" >> (try operator <|> variable))
    <*> consequent end
    <*> alternative end
     <* end
  where
    end = keyword "endif"

case' :: Parser UExp
case' = UCase
    <$> meta
    <*> try (control "case" <* manyTill anyChar (try . lookAhead $ symbol "{%"))
    <*> many1 ((,)
        <$> try (control "when")
        <*> consequent (try (control "when" >> return ()) <|> end))
    <*> alternative end
     <* end
  where
    control n = section $ reserved n >> term

    end = keyword "endcase"

loop :: Parser UExp
loop = do
    m <- meta
    uncurry (ULoop m)
        <$> (try $ section ((,)
            <$> (reserved "for" >> ident)
            <*> (reserved "in"  >> variable)))
        <*> consequent end
        <*> alternative end
         <* end
  where
    end = keyword "endfor"

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

operator :: Parser UExp
operator = buildExpressionParser ops term <?> "operator"
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

term :: Parser UExp
term = try literal <|> variable

literal :: Parser UExp
literal = filtered $ do
    m <- meta
    "literal" ?? choice
        [ try $ UBool m <$> (try true <|> false)
        , try $ either (UInt m) (UDbl m) <$> numberLiteral
        , UText m . Text.pack <$> stringLiteral
        ]

true, false :: Parser Bool
true  = res "True"  True
false = res "False" False

res :: String -> a -> Parser a
res s = (reserved s >>) . return

meta :: Parser Meta
meta = positionMeta <$> getPosition

positionMeta :: SourcePos -> Meta
positionMeta p = Meta (sourceName p) (sourceLine p) (sourceColumn p)

pack :: Parser [UExp] -> Parser UExp
pack = fmap (foldr' (\a b -> UApp (_meta a) a b) UNil)

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)
