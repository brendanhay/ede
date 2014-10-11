{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Arrow              (second)
import           Control.Lens
import           Control.Monad.State
import           Data.ByteString            (ByteString)
import           Data.ByteString.UTF8       as UTF8
import           Data.Char                  (isSpace)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashSet               as Set
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Read
import           Text.EDE.Internal.AST
import qualified Text.EDE.Internal.Keywords as Keywords
import qualified Text.EDE.Internal.Style    as Style
import           Text.EDE.Internal.Types
import           Text.Parser.Expression
import           Text.Trifecta              hiding (render)
import           Text.Trifecta.Delta

type Parse m =
    ( Monad m
    , TokenParsing m
    , DeltaParsing m
--    , MonadState (HashMap Text Delta) m
    )

--runParser :: String -> ByteString -> Result (Exp, HashMap Text Delta)
runParser bs =
    print $ parseByteString template (Directed (UTF8.fromString "parse") 0 0 0 0) bs

  -- where
  --   res (Success x) = return x
  --   res (E)

template :: Parse m => m Exp
template = document <* eof

document :: Parse m => m Exp
document = eapp <$> expr <*> many expr
  where
    expr = render <|> block <|> fragment

render :: Parse m => m Exp
render = between (symbol "{{") (symbol "}}") term

block :: Parse m => m Exp
block = choice
    [ ifelif
    , cases
    , loop
    , include
    , binding
    ]

fragment :: Parse m => m Exp
fragment = ELit <$> position <*> (LText <$> txt)
  where
    txt = Text.pack <$> try (some anyChar) <* end
    end = notFollowedBy (char '{' >> oneOf "{#%")

ifelif :: Parse m => m Exp
ifelif = eif
    <$> branch "if"
    <*> many (branch "elif")
    <*> optional else'
    <*  section (keyword "endif")
  where
    branch k = section ((,) <$ keyword k <*> term <*> document)

cases :: Parse m => m Exp
cases = ecase
    <$> section (keyword "case" *> term)
    <*> many
        ((,) <$> section (keyword "when" *> pattern)
             <*> document)
    <*> optional else'
    <*  section (keyword "endcase")

loop :: Parse m => m Exp
loop = do
    d <- position
    uncurry (ELoop d)
        <$> section
            ((,) <$> (keyword "for" *> identifier)
                 <*> (keyword "in"  *> variable))
        <*> document
        <*> optional else'
        <*  section (keyword "endfor")

include :: Parse m => m Exp
include = do
    d  <- position
    k  <- stringLiteral
--    id %= Map.insert k d
    EIncl d k <$> optional (keyword "with" *> term)

binding :: Parse m => m Exp
binding = do
    d <- position
    uncurry (ELet d)
        <$> section
            ((,) <$> (keyword "let" *> identifier)
                 <*> (symbol  "="   *> term))
        <*> document
        <*  section (keyword "endlet")

else' :: Parse m => m Exp
else' = try (section (keyword "else")) *> document

section :: Parse m => m a -> m a
section = between (symbol "{%") (symbol "%}")

term :: Parse m => m Exp
term = buildExpressionParser table expr
  where
    table =
        [ [prefix "!"]
        , [binary "*", binary "/"]
        , [binary "-", binary "+"]
        , [binary "==", binary "!=", binary ">", binary ">=", binary "<", binary "<="]
        , [binary "&&"]
        , [binary "||"]
--        , [filter']
        ]

    prefix n = Prefix (efun <$> operator n <*> pure n)

    binary n = Infix (do
        d <- operator n
        return $ \l r ->
            EApp d (efun d n l) r) AssocLeft

    expr = parens term <|> apply EVar variable <|> apply ELit literal

    -- filter' = Infix (do
    --     m <- operator "|"
    --     i <- try (lookAhead identifier)
    --     return $ \l _ ->
    --         EApp m (EFun m i) l) AssocLeft


pattern :: Parse m => m Pat
pattern = PWild <$ char '_' <|> PVar <$> variable <|> PLit <$> literal

literal :: Parse m => m Lit
literal = LBool <$> boolean <|> LNum <$> number <|> LText <$> stringLiteral

number :: Parse m => m Scientific
number = either fromIntegral fromFloatDigits <$> integerOrDouble

boolean :: Parse m => m Bool
boolean = textSymbol "true " *> return True
      <|> textSymbol "false" *> return False

operator :: Parse m => Text -> m Delta
operator n = position <* reserveText Style.operator n

keyword :: Parse m => Text -> m Delta
keyword k = position <* reserveText Style.keyword k

variable :: Parse m => m Var
variable = Var <$> ((:|) <$> identifier <*> sepBy identifier dot)

identifier :: Parse m => m Id
identifier = Id <$> ident Style.variable

apply :: Parse m => (Delta -> a -> b) -> m a -> m b
apply f p = f <$> position <*> p

-- type Parser = Parsec [Token] ParserState

-- data ParserState = ParserState
--     { stateShow :: Token -> String
--     , stateName :: String
--     , stateIncl :: HashMap Text Meta
--     }

-- runParser :: String -> [Token] -> Result (Exp, HashMap Text Meta)
-- runParser n = either err Success
--     . Parsec.runParser tmpl (ParserState show n mempty) n
--   where
--     err e = Error (Parser (meta e) (msg e))

--     msg = lines
--         . dropWhile isSpace
--         . showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
--         . errorMessages

--     tmpl = (,)
--         <$> (document  <|> return def) <* eof
--         <*> (stateIncl <$> getState)

--     def = bld (Meta n 0 0)

-- document :: Parser Exp
-- document = eapp <$> p <*> many p
--   where
--     p = blocks <|> fragment <|> substitution

-- fragment :: Parser Exp
-- fragment = do
--     (m, txt) : cs <- many1 (capture KFrag <|> whitespace <|> newline)
--     return . ELit m . LText $
--         case cs of
--             [] -> txt
--             _  -> LText.concat (txt : map snd cs)
--     <?> "textual fragment"

-- whitespace :: Parser (Meta, LText.Text)
-- whitespace = capture KWhiteSpace
--     <?> "whitespace"

-- newline :: Parser (Meta, LText.Text)
-- newline = (, "\n") <$> atom KNewLine
--     <?> "a newline"

-- substitution :: Parser Exp
-- substitution = atom KVarL *> term <* atom KVarR
--     <?> "substitution"

-- blocks :: Parser Exp
-- blocks = choice
--     [ ifelsif
--     , cases
--     , binding
--     , loop
--     , include
--     ]

-- block :: String -> Parser a -> Parser a
-- block n p = try (atom KBlockL *> p <* atom KBlockR)
--     <?> "a valid '" ++ n ++ "' block"

-- ifelsif :: Parser Exp
-- ifelsif = eif
--     <$> branch "if" KIf
--     <*> many (branch "elsif" KElseIf)
--     <*> alternative
--     <*  block "endif" (atom KEndIf)
--   where
--     branch k a = (,)
--         <$> block k (atom a *> term)
--         <*> document

-- cases :: Parser Exp
-- cases = ecase
--     <$> block "case" (atom KCase *> term)
--     <*> many (alt <$> block "when" (atom KWhen *> pattern) <*> document)
--     <*> alternative
--     <*  block "endcase" (atom KEndCase)

-- binding :: Parser Exp
-- binding = uncurry elet
--     <$> block "let"
--         ((,) <$> (atom KLet    *> identifier)
--              <*> (atom KEquals *> term))
--     <*> document
--     <*  block "endlet" (atom KEndLet)

-- loop :: Parser Exp
-- loop = uncurry eloop
--     <$> block "for"
--         ((,) <$> (atom KFor *> identifier)
--              <*> (atom KIn  *> variable))
--     <*> document
--     <*> alternative
--     <*  block "endfor" (atom KEndFor)

-- include :: Parser Exp
-- include = block "include" $ do
--     (m, n) <- atom KInclude *> capture KText <?> "template identifier"
--     v      <- optionMaybe (atom KWith *> term)
--     let k = LText.toStrict n
--     modifyState $ \s ->
--         s { stateIncl = Map.insert k m (stateIncl s) }
--     return (EIncl m k v)

-- alternative :: Parser (Maybe Exp)
-- alternative = try $ optionMaybe (block "else" (atom KElse *> document))

-- term :: Parser Exp
-- term = buildExpressionParser table term0
--   where
--     table =
--         [ [prefix "!"]
--         , [binary "*", binary "/"]
--         , [binary "-", binary "+"]
--         , [binary "==", binary "!=", binary ">", binary ">=", binary "<", binary "<="]
--         , [binary "&&"]
--         , [binary "||"]
--         , [filter']
--         ]

--     binary n = Infix (do
--         m <- operator n
--         return $ \l r ->
--             EApp m (fun m n $ l) r) AssocLeft

--     prefix n = Prefix (do
--         m <- operator n
--         return (fun m n))

--     fun m = EApp m . EFun m . Id m . LText.toStrict

--     filter' = Infix (do
--         m <- operator "|"
--         i <- try (lookAhead identifier)
--         return $ \l _ ->
--             EApp m (EFun m i) l) AssocLeft

-- term0 :: Parser Exp
-- term0 = evar <$> variable <|> uncurry ELit <$> literal

-- pattern :: Parser Pat
-- pattern = (PWild <$ atom KUnderscore)
--       <|> (PVar <$> variable)
--       <|> (PLit . snd <$> literal)
--     <?> "a pattern"

-- operator :: LText.Text -> Parser Meta
-- operator op = token f <?> "an operator"
--   where
--     f (TC m y t)
--         | KOp == y
--         , op  == t = Just m
--     f _            = Nothing

-- variable :: Parser Var
-- variable = do
--     v:vs <- sepBy1 (var <$> identifier) (atom KDot)
--     return (sconcat (v :| vs))
--     <?> "a variable"

-- identifier :: Parser Id
-- identifier = uncurry Id . second LText.toStrict <$> capture KVar
--     <?> "an identifier"

-- literal :: Parser (Meta, Lit)
-- literal = boolean <|> string <|> number
--     <?> "a literal"

-- boolean :: Parser (Meta, Lit)
-- boolean = ((,LBool True) <$> atom KTrue) <|> ((,LBool False) <$> atom KFalse)
--     <?> "a boolean"

-- string :: Parser (Meta, Lit)
-- string = second LText <$> capture KText
--     <?> "a string"

-- number :: Parser (Meta, Lit)
-- number = do
--     (m, txt) <- capture KNum
--     either (fail . mappend "unexpected error parsing number: ")
--            (fmap (m,) . uncurry p)
--            (Read.double (LText.toStrict txt))
--     <?> "a number"
--   where
--     p n "" = return $ LNum (fromFloatDigits n)
--     p n rs = fail $ "leftovers after parsing number: " ++ show (n, rs)

-- capture :: Capture -> Parser (Meta, LText.Text)
-- capture x = token f
--   where
--     f (TC m y t) | x == y = Just (m, t)
--     f _                   = Nothing

-- atom :: Atom -> Parser Meta
-- atom x = token f
--   where
--     f (TA m y) | x == y = Just m
--     f _                 = Nothing

-- token :: (Token -> Maybe a) -> Parser a
-- token f = stateShow <$> getState >>= \g -> Parsec.token g tokenSourcePos f

-- position :: Parser Meta
-- position = meta <$> getPosition
