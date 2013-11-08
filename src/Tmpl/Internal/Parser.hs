{-# LANGUAGE TupleSections #-}

module Tmpl.Internal.Parser where

import           Control.Applicative   ((<$>), (<*>), (<*), (*>), pure)
import           Control.Monad
import           Data.Foldable         (foldl', foldr')
import           Data.Monoid
import qualified Data.Text             as Text
import           Text.Parsec           hiding (parse)
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy (Parser)
import           Tmpl.Internal.Lexer   as Lexer
import           Tmpl.Internal.Types

-- FIXME:
-- Support negation of exprs with parens

parse :: LText -> Either ParseError UExpr
parse = runParser template () "parse"

template :: Parser UExpr
template = foldr' (<>) mempty <$> manyTill expression (try eof)

expression :: Parser UExpr
expression = try $ choice [loop, conditional, variable, fragment]

loop :: Parser UExpr
loop = uncurry ULoop
    <$> section ((,)
        <$> (reserved "for" >> binding)
        <*> (reserved "in"  >> ident))
    <*> consequent
    <*> alternative
     <* keyword "endfor"

conditional :: Parser UExpr
conditional = UCond
    <$> section (reserved "if" >> (try operation <|> fmap UVar ident))
    <*> consequent
    <*> alternative
     <* keyword "endif"

variable :: Parser UExpr
variable = UVar <$> between (symbol "{{") (string "}}") ident

fragment :: Parser UExpr
fragment = pack <$> manyTill1 anyChar (try . lookAhead $ eof <|> next)
  where
    pack = UText . Text.pack
    next = void $ char '{' >> oneOf "{%#"

consequent :: Parser UExpr
consequent = foldr' (<>) mempty <$> many expression

alternative :: Parser UExpr
alternative = option mempty . try $ keyword "else" >> consequent

operation :: Parser UExpr
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

term :: Parser UExpr
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

literal :: Parser UExpr
literal = "literal" ?? choice
    [ try bool
    , try integer
    , try double
    , text
    ]

bool :: Parser UExpr
bool = "bool" ?? UBool <$> (try true <|> false)

integer :: Parser UExpr
integer = "integer" ?? UInt <$> integerLiteral

double :: Parser UExpr
double = "double" ?? UDbl <$> doubleLiteral

text :: Parser UExpr
text = "text" ?? UText . Text.pack <$> stringLiteral

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)
