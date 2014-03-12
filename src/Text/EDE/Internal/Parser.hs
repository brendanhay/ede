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

import           Control.Applicative
import           Data.Foldable                  (foldl')
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Read                 as Read
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Lexer.Tokens
import           Text.Parsec                    (Parsec, (<?>), getState, try)
import qualified Text.Parsec                    as Parsec
import           Text.Parsec.Combinator
import           Text.Parsec.Error

type Parser = Parsec [Token] ParserState

data ParserState = ParserState
    { stateShow :: Token -> String
    , stateName :: String
    }

runParser :: String -> Parser a -> [Token] -> Either ParseError a
runParser src p = Parsec.runParser (p <* eof) (ParserState show src) src

document :: Parser (Exp Text)
document = foldl1 (EApp . EApp (EVar "<>")) <$> many1
    (fragment <|> substitution <|> sections)

fragment :: Parser (Exp a)
fragment = do
    (_, txt) : cs <- many1 (capture KFrag <|> whitespace <|> newline)
    return . ELit . LText $
        case cs of
            [] -> txt
            _  -> Text.concat (txt : map snd cs)
    <?> "a textual fragment"

whitespace :: Parser (Meta, Text)
whitespace = capture KWhiteSpace
    <?> "whitespace"

newline :: Parser (Meta, Text)
newline = (, "\n") <$> atom KNewLine
    <?> "a newline"

substitution :: Parser (Exp Text)
substitution = atom KIdentL *> term <* atom KIdentR
    <?> "substitution"

sections :: Parser (Exp Text)
sections = choice
    [ assign
    , match
    , conditional
    ] <?> "a section"

section :: String -> Parser a -> Parser a
section n p = try (atom KSectionL *> p <* atom KSectionR) <?> n

assign :: Parser (Exp Text)
assign = elet
    <$> section "assign" (atom KAssign *> decls)
    <*> (document <|> blank)

match :: Parser (Exp Text)
match = ECase
    <$> section "case" (atom KCase *> term)
    <*> many1 (alt <$> section "when" (atom KWhen *> pattern) <*> document)
    <*  section "endcase" (atom KEndCase)

conditional :: Parser (Exp Text)
conditional = foldl' (\a (x, y) -> a . eif x y)
    <$> (eif <$> section "if" (atom KIf *> term) <*> document)
    <*> many ((,) <$> section "elsif" (atom KElseIf *> term) <*> document)
    <*> (section "else" (atom KElse) *> document <|> blank)
    <*  section "endif" (atom KEndIf)

-- loop
-- include

decls :: Parser [(Text, Exp Text)]
decls = sepBy1 ((,) <$> identifier <* atom KEquals <*> term) (atom KNewLine)
    <?> "a declaration"

term :: Parser (Exp Text)
term = term0 <|> term1
    <?> "a term"

term0 :: Parser (Exp Text)
term0 = EVar <$> identifier <|> literal

term1 :: Parser (Exp Text)
term1 = foldl1 EApp <$> some term0

pattern :: Parser (Pattern Text)
pattern = (varp <$> identifier) <|> (wildp <$ atom KUnderscore)
    <?> "a pattern"

pattern0 :: Parser (Pattern Text)
pattern0 = asp <$> try (identifier <* atom KAt) <*> pattern0 <|> pattern

identifier :: Parser Text
identifier = snd <$> capture KIdent
    <?> "an identifier"

literal :: Parser (Exp a)
literal = boolean <|> string <|> integer

boolean :: Parser (Exp a)
boolean = ELit . LBool <$>
    (atom KTrue *> return True <|> atom KFalse *> return False)
    <?> "a boolean"

string :: Parser (Exp a)
string = ELit . LText . snd <$> capture KText
    <?> "a string"

integer :: Parser (Exp a)
integer = do
    (_, txt) <- capture KNum
    either (fail . mappend "unexpected error parsing number: ")
           (uncurry parse)
           (Read.signed Read.decimal txt)
    <?> "an integer"
  where
    parse n "" = return $ ELit (LInteger n)
    parse n rs = fail $ "leftovers after parsing number: " ++ show (n, rs)

blank :: Parser (Exp Text)
blank = return $ ELit (LText "")

parens :: Parser a -> Parser a
parens p = atom KParenL *> p <* atom KParenR

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
