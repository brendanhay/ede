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
import           Data.Char                      (isSpace)
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import           Data.List.NonEmpty             (NonEmpty(..))
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                      (Text)
import qualified Data.Text.Lazy                 as LText
import qualified Data.Text.Read                 as Read
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Lexer.Tokens
import           Text.EDE.Internal.Types
import qualified Text.Parsec                    as Parsec
import           Text.Parsec                    hiding (Error, (<|>), many, optional, token, newline, string)
import           Text.Parsec.Error              (errorMessages, showErrorMessages)
import           Text.Parsec.Expr

type Parser = Parsec [Token] ParserState

data ParserState = ParserState
    { stateShow :: Token -> String
    , stateName :: String
    , stateIncl :: HashMap Text Meta
    }

runParser :: String -> [Token] -> Result (Exp, HashMap Text Meta)
runParser n = either err Success
    . Parsec.runParser tmpl (ParserState show n mempty) n
  where
    err e = Error (Parser (meta e) (msg e))

    msg = lines
        . dropWhile isSpace
        . showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
        . errorMessages

    tmpl = (,)
        <$> (document  <|> return (bld (Meta n 0 0))) <* eof
        <*> (stateIncl <$> getState)

document :: Parser Exp
document = eapp <$> p <*> many p
  where
    p = fragment <|> substitution <|> blocks

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
substitution = atom KVarL *> term <* atom KVarR
    <?> "substitution"

blocks :: Parser Exp
blocks = choice
    [ ifelsif
    , cases
    , assign
    , loop
    , include
    ] <?> "a block"

block :: String -> Parser a -> Parser a
block n p = try (atom KBlockL *> p <* atom KBlockR <* trim)
    <?> n
  where
    trim = optional newline
    -- lstrip

ifelsif :: Parser Exp
ifelsif = eif
    <$> branch "if" KIf
    <*> many (branch "elsif" KElseIf)
    <*> alternative
    <*  block "endif" (atom KEndIf)
  where
    branch k a = (,)
        <$> block k (atom a *> term)
        <*> document

cases :: Parser Exp
cases = ecase
    <$> block "case" (atom KCase *> term)
    <*> many (alt <$> block "when" (atom KWhen *> pattern) <*> document)
    <*> alternative
    <*  block "endcase" (atom KEndCase)

assign :: Parser Exp
assign = block "assign" $ ELet
    <$> position
    <*> (atom KAssign *> identifier)
    <*> (atom KEquals *> (Left . snd <$> literal <|> Right <$> variable))

loop :: Parser Exp
loop = do
    l <- block "for" $ do
        k <- atom KFor *> identifier
        v <- atom KIn  *> variable
        return (ELoop (meta k) k v)
    b <- document
    e <- alternative
    void $ block "endfor" (atom KEndFor)
    return (l b e)

include :: Parser Exp
include = block "include" $ do
    (m, n) <- atom KInclude *> capture KText
    v      <- optionMaybe (atom KWith *> term)
    let k = LText.toStrict n
    modifyState $ \s ->
        s { stateIncl = Map.insert k m (stateIncl s) }
    return (EIncl m k v)

alternative :: Parser (Maybe Exp)
alternative = try $ optionMaybe (block "else" (atom KElse *> document))

-- term' :: Parser Exp
-- term' 

-- function :: Parser Exp
-- function = do
--     t  <- term0
--     m  <- operator "|"
--     i  <- identifier
--     return (EApp m (EFun m i) t)

-- filter' = do
--     t  <- term
--     m  <- operator "|"
--     i  <- identifier
--     return (EApp m (EFun m i) t)

term :: Parser Exp
term = flip buildExpressionParser term0
    [ [prefix "!"]
    , [binary "*", binary "/"]
    , [binary "-", binary "+"]
    , [binary "==", binary "!=", binary ">", binary ">=", binary "<", binary "<="]
    , [binary "&&"]
    , [binary "||"]
    , [binary "|"]
    ]
  where
    prefix n = Prefix (operator n >>= \m -> return $ fun m n)
    binary n = Infix (operator n >>= \m -> return (\f a -> EApp m (fun m n $ f) a)) AssocLeft

    fun m = EApp m . EFun m . Id m . LText.toStrict

-- function :: Parser Exp
-- function = eapp
--     <$> (EFun <$ operator "|" <*> position <*> identifier)
--     <*> many term0
--     <?> "a filter"

term0 :: Parser Exp
term0 = eapp <$> term1 <*> many term1

term1 :: Parser Exp
term1 = evar <$> variable <|> uncurry ELit <$> literal

pattern :: Parser Pat
pattern = (PWild <$ atom KUnderscore)
      <|> (PVar <$> variable)
      <|> (PLit . snd <$> literal)
    <?> "a pattern"

operator :: LText.Text -> Parser Meta
operator op = token f <?> "an operator"
  where
    f (TC m y t)
        | KOp == y
        , op  == t = Just m
    f _            = Nothing

variable :: Parser Var
variable = do
    v:vs <- sepBy1 (var <$> identifier) (atom KDot)
    return (sconcat (v :| vs))
    <?> "a variable"

identifier :: Parser Id
identifier = uncurry Id . second LText.toStrict <$> capture KVar
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

position :: Parser Meta
position = meta <$> getPosition
