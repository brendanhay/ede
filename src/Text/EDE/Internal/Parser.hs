{-# LANGUAGE TupleSections #-}

module Text.EDE.Internal.Parser where

import           Control.Applicative    ((<$>), (<*>), (<*), (*>), pure)
import           Control.Monad
import           Data.Foldable          (foldr')
import           Data.Monoid
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import           Data.Text.Lazy.Builder
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types     hiding (ident)
import qualified Text.Parsec            as Parsec
import           Text.Parsec            hiding (runParser, parse)
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy  (Parser)

-- FIXME:
-- support negation of exprs with parens
-- add metadata to idents

runParser :: String -> LText -> Either ParseError UExp
runParser = Parsec.runParser template ()

template :: Parser UExp
template = foldr' (<>) mempty <$> manyTill expression (try eof)

expression :: Parser UExp
expression = try $ choice [loop, conditional, variable, fragment]

loop :: Parser UExp
loop = do
    m <- meta
    b <- section ((,)
        <$> (reserved "for" >> binding)
        <*> (reserved "in"  >> ident))
    uncurry (ULoop m) b
        <$> consequent
        <*> alternative
         <* keyword "endfor"

conditional :: Parser UExp
conditional = UCond
    <$> meta
    <*> section (reserved "if" >> p)
    <*> consequent
    <*> alternative
     <* keyword "endif"
  where
    p = try operation <|> (UVar <$> meta <*> ident)

variable :: Parser UExp
variable = UVar
    <$> meta
    <*> between (symbol "{{") (string "}}") ident

fragment :: Parser UExp
fragment = UFrag
    <$> meta
    <*> (fromString <$> manyTill1 anyChar (try . lookAhead $ eof <|> next))
  where
    next = void $ char '{' >> oneOf "{%#"

consequent :: Parser UExp
consequent = foldr' (<>) mempty <$> many expression

alternative :: Parser UExp
alternative = option mempty . try $ keyword "else" >> consequent

term :: Parser UExp
term = try variable <|> literal

section :: Parser a -> Parser a
section p = ("section" ??) . try $
    between (symbol "{%") (string "%}") p <* optional newline

keyword :: String -> Parser ()
keyword = ("keyword" ??) . section . reserved

ident :: Parser Ident
ident = "ident" ?? Ident . Text.pack <$> identifier

binding :: Parser Bind
binding = "binding" ?? try pattern <|> nominal
  where
    pattern = Bind
        <$> meta
        <*> (symbol "(" *> ident <* symbol ",")
        <*> (Just <$> (ident <* symbol ")"))

    nominal = Bind
        <$> meta
        <*> ident
        <*> pure Nothing

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
text = parse "text" (\m -> UText m . LText.pack) stringLiteral

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

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)
