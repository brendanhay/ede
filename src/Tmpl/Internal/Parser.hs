{-# LANGUAGE TupleSections #-}

module Tmpl.Internal.Parser where

import           Control.Applicative   ((<$>), (<*>), (<*), (*>), pure)
import           Control.Monad
import qualified Data.Text             as Text
import           Text.Parsec           hiding (parse)
import           Text.Parsec.Expr
import           Text.Parsec.Text.Lazy (Parser)
import           Tmpl.Internal.Lexer   as Lexer
import           Tmpl.Internal.Types

-- FIXME:
-- Support hashmap loops / destructuring
-- Support negation of exprs with parens

parse :: LText -> Either ParseError [Expr]
parse = runParser template () "parse"

template :: Parser [Expr]
template = manyTill expression $ try eof

expression :: Parser Expr
expression = try $ choice [loop, conditional, variable, fragment]

loop :: Parser Expr
loop = uncurry ELoop
    <$> section ((,)
        <$> (reserved "for" >> binding)
        <*> (reserved "in"  >> ident))
    <*> consequent
    <*> alternative
     <* keyword "endfor"

conditional :: Parser Expr
conditional = ECond
    <$> section (reserved "if" >> (try operation <|> fmap EVar ident))
    <*> consequent
    <*> alternative
     <* keyword "endif"

variable :: Parser Expr
variable = EVar <$> between (symbol "{{") (string "}}") ident

fragment :: Parser Expr
fragment = pack <$> manyTill1 anyChar (try . lookAhead $ eof <|> next)
  where
    pack = ELit . LText . Text.pack
    next = void $ char '{' >> oneOf "{%#"

consequent :: Parser [Expr]
consequent = many expression

alternative :: Parser [Expr]
alternative = option [] . try $ keyword "else" >> consequent

operation :: Parser Expr
operation = buildExpressionParser ops term <?> "expression"
  where
    ops = [ [Prefix $ bNot ENeg]
          , [Infix (bAnd       $ EBin And) AssocLeft]
          , [Infix (bOr        $ EBin Or) AssocLeft]
          , [Infix (rEq        $ ERel Equal) AssocLeft]
          , [Infix (rNotEq     $ ERel NotEqual) AssocLeft]
          , [Infix (rGreater   $ ERel Greater) AssocLeft]
          , [Infix (rGreaterEq $ ERel GreaterEqual) AssocLeft]
          , [Infix (rLess      $ ERel Less) AssocLeft]
          , [Infix (rLessEq    $ ERel LessEqual) AssocLeft]
          ]

term :: Parser Expr
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

literal :: Parser Expr
literal = "literal" ?? ELit <$> choice
    [ try bool
    , try number
    , try character
    , text
    ]

bool :: Parser Literal
bool = "bool" ?? LBool <$> (try true <|> false)

number :: Parser Literal
number = "number" ?? either LInt LDoub <$> naturalOrFloat

character :: Parser Literal
character = "character" ?? LChar <$> charLiteral

text :: Parser Literal
text = "text" ?? LText . Text.pack <$> stringLiteral

manyTill1 :: Stream s m t
          => ParsecT s u m a
          -> ParsecT s u m b
          -> ParsecT s u m [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

infix 0 ??

(??) :: String -> ParsecT s u m a -> ParsecT s u m a
(??) = flip (<?>)
