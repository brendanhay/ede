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
import           Control.Arrow
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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let m  = Meta "m" 1 1
-- >>> let tc = TC m
-- >>> let ta = TA m
-- >>> let parse p = either (error . show) id . runParser "m" p

type Parser = Parsec [Token] ParserState

data ParserState = ParserState
    { stateShow :: Token -> String
    , stateName :: String
    }

runParser :: String -> Parser a -> [Token] -> Either ParseError a
runParser src p = Parsec.runParser p (ParserState show src) src

document :: Parser (Exp Meta)
document = do
    x  <- p
    xs <- many p <* eof
    return $ foldl' (\a e -> eapp (meta a) [evar (meta a) "<>", a, e]) x xs
  where
    p = fragment <|> substitution <|> sections

fragment :: Parser (Exp Meta)
fragment = do
    (m, txt) : cs <- many1 (capture KFrag <|> whitespace <|> newline)
    return . ELit m . LText $
        case cs of
            [] -> txt
            _  -> Text.concat (txt : map snd cs)
    <?> "a textual fragment"

-- |
-- >>> parse (snd <$> whitespace) [tc KWhiteSpace "   "]
-- "   "
whitespace :: Parser (Meta, Text)
whitespace = capture KWhiteSpace
    <?> "whitespace"

-- |
-- >>> parse (snd <$> newline) [ta KNewLine]
-- "\n"
newline :: Parser (Meta, Text)
newline = (, "\n") <$> atom KNewLine
    <?> "a newline"

-- |
-- >>> parse substitution [ta KIdentL, ta KTrue, ta KIdentR]
-- ELit m:1:1 (LBool True)
substitution :: Parser (Exp Meta)
substitution = atom KIdentL *> term <* atom KIdentR
    <?> "substitution"

sections :: Parser (Exp Meta)
sections = choice
    [ -- assign
    ] <?> "a section"

section :: String -> Parser a -> Parser a
section n p = try (atom KSectionL *> p <* atom KSectionR) <?> n

-- assign :: Parser (Exp Meta)
-- assign = elet
--     <$> section "assign" (atom KAssign *> decls)
--     <*> (document <|> blank)

-- loop
-- include

-- decls :: Parser [(Text, Exp Meta)]
-- decls = sepBy1 ((,) <$> identifier <* atom KEquals <*> term) (atom KNewLine)
--     <?> "a declaration"

term :: Parser (Exp Meta)
term = term0 <|> term1
    <?> "a term"

term0 :: Parser (Exp Meta)
term0 = uncurry EVar <$> variable <|> literal

term1 :: Parser (Exp Meta)
term1 = eapp <$> position <*> some term0

variable :: Parser (Meta, Var)
variable = second Var <$> capture KIdent
    <?> "a variable"

literal :: Parser (Exp Meta)
literal = boolean <|> string <|> number

-- |
-- >>> parse boolean [ta KTrue]
-- ELit m:1:1 (LBool True)
-- >>> parse boolean [ta KFalse]
-- ELit m:1:1 (LBool False)
boolean :: Parser (Exp Meta)
boolean = uncurry ELit . second LBool <$>
    ((,True) <$> atom KTrue <|> (,False) <$> atom KFalse)
    <?> "a boolean"

-- |
-- >>> parse string [tc KText "abc"]
-- ELit m:1:1 (LText "abc")
string :: Parser (Exp Meta)
string = uncurry ELit . second LText <$> capture KText
    <?> "a string"

-- |
-- >>> parse number [tc KNum "123"]
-- ELit m:1:1 (LNum 123)
number :: Parser (Exp Meta)
number = do
    (m, txt) <- capture KNum
    either (fail . mappend "unexpected error parsing number: ")
           (uncurry $ parse m)
           (Read.signed Read.decimal txt)
    <?> "an number"
  where
    parse m n "" = return $ ELit m (LNum n)
    parse _ n rs = fail $ "leftovers after parsing number: " ++ show (n, rs)

-- |
-- >>> parse blank []
-- ELit m:1:1 (LText "")
blank :: Parser (Exp Meta)
blank = ELit <$> position <*> pure (LText "")

-- |
-- >>> parse (parens $ snd <$> variable) [ta KParenL, tc KIdent "var", ta KParenR]
-- Var "var"
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

position :: Parser Meta
position = meta <$> Parsec.getPosition
