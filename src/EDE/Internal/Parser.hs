{-# LANGUAGE TupleSections #-}

module EDE.Internal.Parser where

import           Control.Applicative    ((<$>), (<*>), (<*), (*>), pure)
import           Control.Monad
import           Data.Foldable          (foldl', foldr')
import           Data.Monoid
import qualified Data.Text              as Text
import           Data.Text.Lazy.Builder
import           Text.Parsec            hiding (parse)
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy  (Parser)
import           EDE.Internal.Lexer    as Lexer
import           EDE.Internal.Types

-- FIXME:
-- Support negation of exprs with parens

parse :: LText -> Either ParseError UExp
parse = runParser template () "parse"

template :: Parser UExp
template = foldr' (<>) mempty <$> manyTill expression (try eof)

expression :: Parser UExp
expression = try $ choice [loop, conditional, variable, fragment]

loop :: Parser UExp
loop = uncurry ULoop
    <$> section ((,)
        <$> (reserved "for" >> binding)
        <*> (reserved "in"  >> ident))
    <*> consequent
    <*> alternative
     <* keyword "endfor"

conditional :: Parser UExp
conditional = UCond
    <$> section (reserved "if" >> (try operation <|> fmap UVar ident))
    <*> consequent
    <*> alternative
     <* keyword "endif"

variable :: Parser UExp
variable = UVar <$> between (symbol "{{") (string "}}") ident

fragment :: Parser UExp
fragment = pack <$> manyTill1 anyChar (try . lookAhead $ eof <|> next)
  where
    pack = UFrag . fromString
    next = void $ char '{' >> oneOf "{%#"

consequent :: Parser UExp
consequent = foldr' (<>) mempty <$> many expression

alternative :: Parser UExp
alternative = option mempty . try $ keyword "else" >> consequent

operation :: Parser UExp
operation = buildExpressionParser ops term <?> "expression"
  where
    ops = [ [Prefix            $ bNot UNeg]
          , [Infix (bAnd       $ UBin And) AssocLeft]
          , [Infix (bOr        $ UBin Or) AssocLeft]
          , [Infix (rEq        $ URel Equal) AssocLeft]
          , [Infix (rNotEq     $ URel NotEqual) AssocLeft]
          , [Infix (rGreater   $ URel Greater) AssocLeft]
          , [Infix (rGreaterEq $ URel GreaterEqual) AssocLeft]
          , [Infix (rLess      $ URel Less) AssocLeft]
          , [Infix (rLessEq    $ URel LessEqual) AssocLeft]
          ]

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
binding = "binding" ?? try pattern <|> (Bind <$> ident <*> pure Nothing)
  where
    pattern = do
        k <- symbol "(" *> ident <* symbol ","
        v <- ident <* symbol ")"
        return $ Bind k (Just v)

literal :: Parser UExp
literal = "literal" ?? choice
    [ try bool
    , try integer
    , try double
    , text
    ]

bool :: Parser UExp
bool = "bool" ?? UBool <$> (try true <|> false)

integer :: Parser UExp
integer = "integer" ?? UInt <$> integerLiteral

double :: Parser UExp
double = "double" ?? UDbl <$> doubleLiteral

text :: Parser UExp
text = "text" ?? UText . Text.pack <$> stringLiteral

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)
