-- Module      : Text.EDE.Internal.Lexer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Lexer where

import           Control.Monad
import           Data.Functor.Identity
import           Data.List             (nub)
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

numberLiteral :: Parser (Either Integer Double)
numberLiteral = Parsec.naturalOrFloat lexer

symbol :: String -> Parser String
symbol = Parsec.symbol lexer

comments :: Parser ()
comments = try (string start) >> run
  where
    run =  (void . try $ string end)
       <|> (comments >> run)
       <|> (skipMany1 (noneOf startEnd) >> run)
       <|> (oneOf startEnd >> run)
       <?> "end of comment"

    startEnd = nub $ end ++ start

    start = Parsec.commentStart rules
    end   = Parsec.commentEnd rules

whiteSpace :: Parser ()
whiteSpace = Parsec.whiteSpace lexer

lexer :: GenTokenParser Text () Identity
lexer = Parsec.makeTokenParser rules

rules :: GenLanguageDef Text u Identity
rules = Parsec.LanguageDef
    { Parsec.commentStart    = "{#"
    , Parsec.commentEnd      = "#}"
    , Parsec.commentLine     = ""
    , Parsec.nestedComments  = False
    , Parsec.identStart      = letter <|> char '_'
    , Parsec.identLetter     = alphaNum <|> oneOf "_'-"
    , Parsec.opStart         = Parsec.opLetter rules
    , Parsec.opLetter        = oneOf "!>=</|&"
    , Parsec.caseSensitive   = False
    , Parsec.reservedOpNames = operators
    , Parsec.reservedNames   = names
    }

operators :: [String]
operators = [">", ">=", "<", "<=", "==", "/=", "!", "||", "&&"]

names :: [String]
names =
    [ "if"
    , "endif"
    , "for"
    , "in"
    , "endfor"
    , "else"
    , "true"
    , "false"
    , "case"
    , "when"
    , "endcase"
    , "raw"
    , "endraw"
    ]
