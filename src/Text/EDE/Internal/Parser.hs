{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE.Internal.Parser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Parser where

import           Bound
import           Control.Applicative
import           Data.List                      (elemIndex)
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Read                 as Read
import           Text.EDE.Internal.Lexer.Tokens
import           Text.EDE.Internal.AST
import qualified Text.Parsec                    as Parsec
import           Text.Parsec                    (Parsec, (<?>), getState, try)
import           Text.Parsec.Combinator

type Parser = Parsec [Token] ParserState

data ParserState = ParserState
    { stateShow :: Token -> String
    , stateName :: String
    }

-- runParser :: (Token -> String)
--           -> String
--           -> Parser a
--           -> [Token]
--           -> Either ParseError a
runParser p = Parsec.runParser (p <* eof) (ParserState show "parse") "parse"

document :: Parser (Exp Text)
document = foldl1 (EApp . EApp (EVar "<>")) <$> many1
    (fragment <|> substitution <|> sections)

fragment :: Parser (Exp a)
fragment = expect "a textual fragment" $ do
    (m, txt) : cs <- many1 (capture KFrag <|> whitespace <|> newline)
    return . ELit . LText $
        case cs of
            [] -> txt
            _  -> Text.concat (txt : map snd cs)

whitespace :: Parser (Meta, Text)
whitespace = expect "whitespace" $ capture KWhiteSpace

newline :: Parser (Meta, Text)
newline = expect "a newline" $ (, "\n") <$> atom KNewLine

substitution :: Parser (Exp Text)
substitution = expect "substitution" $ atom KIdentL *> term <* atom KIdentR

sections :: Parser (Exp Text)
sections = choice
    [ assign
    , match
    ]

section :: String -> Parser a -> Parser a
section n p = try (atom KSectionL *> p <* atom KSectionR) <?> n

assign :: Parser (Exp Text)
assign = elet
    <$> section "assign" (atom KAssign *> decls)
    <*> (document <|> blank)

match :: Parser (Exp Text)
match = ECase
    <$> section "case" (atom KCase *> term)
    <*> many1 a
    <*  section "endcase" (atom KEndCase)
  where
    a = alt
        <$> section "when" (atom KWhen *> pattern0)
        <*> document

conditional :: Parser (Exp Text)
conditional = do
    p <- section "if" (atom KIf *> term)
    e <- document
    n <- 

    section "endif" (atom KEndIf)
  where
--    branch n a = section n (atom a *> term) <$>


    a = (,)
        <$> section "elsif" (atom KElseIf *> term)
        <*> document

-- if a
-- x
-- elsif b
-- y
-- else
-- z
-- end

-- ECase a [(True, x), (False, ECase b ...)]

-- conditional

-- loop

-- include

decls :: Parser [(Text, Exp Text)]
decls = sepBy1 ((,) <$> identifier <* atom KEquals <*> term) (atom KNewLine)

term :: Parser (Exp Text)
term = term0 <|> term1

term0 :: Parser (Exp Text)
term0 = EVar <$> identifier <|> literal

term1 :: Parser (Exp Text)
term1 = foldl1 EApp <$> some term0

pattern0 :: Parser (P Text)
pattern0 = (varp <$> identifier) <|> (wildp <$ atom KUnderscore)

pattern1 :: Parser (P Text)
pattern1 = asp <$> try (identifier <* atom KAt) <*> pattern1 <|> pattern0

identifier :: Parser Text
identifier = snd <$> capture KIdent

literal :: Parser (Exp a)
literal = boolean <|> string <|> integer

boolean :: Parser (Exp a)
boolean = expect "a boolean" $ ELit . LBool <$> (true <|> false)
  where
    true  = atom KTrue  >> return True
    false = atom KFalse >> return False

string :: Parser (Exp a)
string = expect "a string" $ ELit . LText . snd <$> capture KText

integer :: Parser (Exp a)
integer = expect "an integer" $ do
    (_, txt) <- capture KNum
    either (fail . mappend "unexpected error parsing number: ")
           (uncurry parse)
           (Read.signed Read.decimal txt)
  where
    parse n "" = return $ ELit (LInteger n)
    parse n rs = fail $ "leftovers after parsing number: " ++ show (n, rs)

blank :: Parser (Exp Text)
blank = return $ ELit (LText "")

parens :: Parser a -> Parser a
parens p = atom KParenL *> p <* atom KParenR

expect = flip (<?>)

capture :: Capture -> Parser (Meta, Text)
capture x = token f
  where
    f (TC m y t) | x == y = Just (m, t)
    f _                   = Nothing

atom :: Atom -> Parser Meta
atom x = token f
  where
    f (TA m y) | x == y = Just m
    f _                 = Nothing

token :: (Token -> Maybe a) -> Parser a
token f = stateShow <$> getState >>= \g -> Parsec.token g tokenSourcePos f
