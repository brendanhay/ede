module Tmpl.Internal.Lexer where

import           Data.Functor.Identity
import           Data.Text.Lazy        (Text)
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Text.Lazy (Parser)
import           Text.Parsec.Token     (GenTokenParser)
import qualified Text.Parsec.Token     as Parsec

identifier :: Parser String
identifier = Parsec.identifier lexer

reserved :: String -> Parser ()
reserved = Parsec.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Parsec.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Parsec.stringLiteral lexer

charLiteral :: Parser Char
charLiteral = Parsec.charLiteral lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Parsec.naturalOrFloat lexer

symbol :: String -> Parser String
symbol = Parsec.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Parsec.whiteSpace lexer

true, false :: Parser Bool
true  = res "true" True
false = res "false" False

bNot, bAnd, bOr :: a -> Parser a
bNot = op "!"
bAnd = op "&&"
bOr  = op "||"

rEq, rNotEq, rGreater, rGreaterEq, rLess, rLessEq :: a -> Parser a
rEq        = op "=="
rNotEq     = op "!="
rGreater   = op ">"
rGreaterEq = op ">="
rLess      = op "<"
rLessEq    = op "<="

res, op :: String -> a -> Parser a
res s = (reserved s >>) . return
op  s = (reservedOp s >>) . return

lexer :: GenTokenParser Text () Identity
lexer = Parsec.makeTokenParser rules

rules :: GenLanguageDef Text u Identity
rules = Parsec.LanguageDef
    { Parsec.commentStart    = "{#"
    , Parsec.commentEnd      = "#}"
    , Parsec.commentLine     = ""
    , Parsec.nestedComments  = False
    , Parsec.identStart      = letter <|> char '_'
    , Parsec.identLetter     = alphaNum <|> oneOf "_'"
    , Parsec.opStart         = Parsec.opLetter rules
    , Parsec.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Parsec.caseSensitive   = False
    , Parsec.reservedNames   = reservedNames
    , Parsec.reservedOpNames = reservedOps
    }

reservedNames :: [String]
reservedNames = ["if", "endif", "for", "in", "endfor", "else", "true", "false"]

reservedOps :: [String]
reservedOps = [">", ">=", "<", "=<", "==", "!", "!=", "||", "&&"]
