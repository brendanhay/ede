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
import           Control.Arrow                  (second)
import           Control.Monad
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import           Data.Monoid
import           Data.Scientific
import           Data.Text                      (Text)
import qualified Data.Text.Lazy                 as LText
import qualified Data.Text.Read                 as Read
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Lexer.Tokens
import           Text.EDE.Internal.Types
import qualified Text.Parsec                    as Parsec
import           Text.Parsec                    hiding (Error, (<|>), many, token, newline, string)
import           Text.Parsec.Expr

type Parser = Parsec [Token] ParserState

data ParserState = ParserState
    { stateShow :: Token -> String
    , stateName :: String
    , stateIncl :: HashMap Text Meta
    }

runParser :: String -> [Token] -> Result (Exp, HashMap Text Meta)
runParser n = either msg Success
    . Parsec.runParser tmpl (ParserState show n mempty) n
  where
    msg e = Error (pos (errorPos e)) [show e]
    pos s = Meta (sourceName s) (sourceLine s) (sourceColumn s)

    tmpl = (,) <$> document <* eof <*> (stateIncl <$> getState)

document :: Parser Exp
document = eapp <$> p <*> many p
  where
    p = fragment <|> substitution <|> sections

fragment :: Parser Exp
fragment = do
    (m, txt) : cs <- many1 (capture KFrag <|> whitespace <|> newline)
    return . ELit m . LText $
        case cs of
            [] -> txt
            _  -> LText.concat (txt : map snd cs)
    <?> "a textual fragment"

whitespace :: Parser (Meta, LText.Text)
whitespace = capture KWhiteSpace
    <?> "whitespace"

newline :: Parser (Meta, LText.Text)
newline = (, "\n") <$> atom KNewLine
    <?> "a newline"

substitution :: Parser Exp
substitution = atom KIdentL *> term <* atom KIdentR
    <?> "substitution"

sections :: Parser Exp
sections = choice
    [ ifelsif
    , cases
    , assign
    , loop
    , include
    ] <?> "a section"

section :: String -> Parser a -> Parser a
section n p = try (atom KSectionL *> p <* atom KSectionR) <?> n

ifelsif :: Parser Exp
ifelsif = eif
    <$> ((:) <$> branch "if" KIf <*> many (branch "elsif" KElseIf))
    <*> alternative
    <*  section "endif" (atom KEndIf)
  where
    branch k a = (,)
        <$> section k (atom a *> term)
        <*> document

cases :: Parser Exp
cases = ecase
    <$> section "case" (atom KCase *> term)
    <*> many (alt <$> section "when" (atom KWhen *> pattern) <*> document)
    <*> alternative
    <*  section "endcase" (atom KEndCase)

assign :: Parser Exp
assign = section "assign" $ uncurry ELet
    <$> (atom KAssign *> identifier)
    <*> (atom KEquals *> (Left . snd <$> literal <|> Right . snd <$> identifier))

loop :: Parser Exp
loop = do
    l <- section "for" $ do
        (m, k) <- atom KFor *> identifier
        (_, v) <- atom KIn  *> identifier
        return (ELoop m k v)
    b <- document
    e <- alternative
    void $ section "endfor" (atom KEndFor)
    return (l b e)

include :: Parser Exp
include = section "include" $ do
    (m, n) <- atom KInclude *> capture KText
    v      <- optionMaybe (atom KWith *> term)
    let k = LText.toStrict n
    modifyState $ \s ->
        s { stateIncl = Map.insert k m (stateIncl s) }
    return (EIncl m k v)

alternative :: Parser (Maybe Exp)
alternative = try $ optionMaybe (section "else" (atom KElse *> document))

term :: Parser Exp
term = flip buildExpressionParser term1
    [ [prefix "-", prefix "+"]
    , [binary "*", binary "/"]
    , [binary "+", binary "-"]
    ]
  where
    prefix n = Prefix (operator n >>= \m -> return $ partial m n)
    binary n = Infix  (operator n >>= \m -> return (\f a -> EApp m (partial m n $ f) a)) AssocLeft

    partial m = epartial m . LText.toStrict

term0 :: Parser Exp
term0 = variable <|> uncurry ELit <$> literal

term1 :: Parser Exp
term1 = eapp <$> term0 <*> many term0

pattern :: Parser Pat
pattern = (PWild <$ atom KUnderscore)
      <|> (PVar . snd <$> identifier)
      <|> (PLit . snd <$> literal)
    <?> "a pattern"

operator :: LText.Text -> Parser Meta
operator op = token f <?> "an operator"
  where
    f (TC m y t)
        | KOp == y
        , op  == t = Just m
    f _            = Nothing

-- FIXME: parse nested . identifiers
variable :: Parser Exp
variable = uncurry EVar <$> identifier
    <?> "a variable"

identifier :: Parser (Meta, Id)
identifier = second (Id . LText.toStrict) <$> capture KIdent
    <?> "an identifier"

literal :: Parser (Meta, Lit)
literal = boolean <|> string <|> number
    <?> "a literal"

boolean :: Parser (Meta, Lit)
boolean = ((,LBool True) <$> atom KTrue) <|> ((,LBool False) <$> atom KFalse)
    <?> "a boolean"

string :: Parser (Meta, Lit)
string = second LText <$> capture KText
    <?> "a string"

number :: Parser (Meta, Lit)
number = do
    (m, txt) <- capture KNum
    either (fail . mappend "unexpected error parsing number: ")
           (fmap (m,) . uncurry p)
           (Read.double (LText.toStrict txt))
    <?> "a number"
  where
    p n "" = return $ LNum (fromFloatDigits n)
    p n rs = fail $ "leftovers after parsing number: " ++ show (n, rs)

parens :: Parser a -> Parser a
parens p = atom KParenL *> p <* atom KParenR

capture :: Capture -> Parser (Meta, LText.Text)
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
