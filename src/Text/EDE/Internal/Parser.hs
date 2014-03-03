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

import           Control.Applicative     ((<$>), (<*), (*>))
import           Control.Arrow
import           Control.Monad
import           Data.Foldable           (foldl')
import           Data.List               (intersperse)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Read          as Read
import           Data.Tuple
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Lexer
import qualified Text.Parsec             as Parsec
import           Text.Parsec             hiding (runParser)

type Parser = Parsec [Token] ParserState

data ParserState = ParserState
    { stateShow :: Token -> String
    , stateName :: String
    }

runParser :: (Token -> String)
          -> String
          -> Parser a
          -> [Token]
          -> Either ParseError a
runParser f name parser = Parsec.runParser parser (ParserState f name) name

-- FIXME: meta positioning for mappends is incorrect
pDoc :: Parser (Exp Meta)
pDoc = do
    x  <- p
    xs <- many p <* eof
    return $ foldl' (\a e -> eapp (meta a) [evar (meta a) "<>", a, e]) x xs
  where
    p = pConstruct <|> pIdent <|> pFrag

        -- alternative associativty:
        -- return $ foldr (\e a -> eapp [ebound "mappend", a, e]) x xs

-- FIXME: handle whitespace tokens, newlines
pFrag :: Parser (Exp Meta)
pFrag = do
    (m, txt) : cs <- many1 p
    return $ case cs of
        [] -> etext m txt
        _  -> etext m $ Text.concat (txt : map snd cs)
  where
    p = choice
        [ pCapture KFrag <?> "a textual fragment"
        , pCapture KWhiteSpace <?> "whitespace"
        , ((,"\n") <$> pAtom KNewLine) <?> "a newline"
        ]

pIdent :: Parser (Exp Meta)
pIdent = pAtom KIdentL *> pExp <* pAtom KIdentR

pConstruct :: Parser (Exp Meta)
pConstruct = choice
    [ -- assign <name> = <exp>
      -- try $ do
      --   (_, m) <- pTokM KSectionL
      --   n      <- pTok  KAssign *> pId
      --   b      <- pTok  (KOp "=") *> pExp <* pTok KSectionR
      --   return (elet n b, m)

    --   -- capture <name> ...
    -- , try $ do
    --     (_, m) <- pTokM KSectionL
    --     n      <- pTok  KCapture *> pId
    --     b      <- pTok  KSectionR *> pExp <* pSection KEndCapture
    --     return (ELet m (UName n) b, m)

    --   -- if [<alt>]
    -- , try $ do
    --     (_, m) <- pTokM KSectionL
    --     s      <- pTok  KIf *> pExp
    --     c      <- pTok  KSectionR *> pExp
    --     as     <- pAlts KElseIf KEndIf
    --     return (ECond m (ACond s c : as), m)

    --   -- case <exp> [<alt>]
    -- , try $ do
    --     (_, m) <- pTokM KSectionL
    --     s      <- pTok  KCase *> pExp <* pTok KSectionR
    --     as     <- pAlts KWhen KEndCase
    --     return (ECase m s as, m)

    --   -- for <name> in <exp>
    -- , try $ do
    --     (_, m) <- pTokM KSectionL
    --     n      <- pTok  KFor *> pId
    --     s      <- pTok  KIn  *> pExp <* pTok KSectionR
    --     ma     <- optionMaybe (pDefault KEndCase)
    --     return (ELoop m (UName n) s ma, m)

    --   -- include <exp> [with <exp>]
    -- , try $ do
    --      (_, m) <- pTokM KSectionL
    --      n      <- pTok  KInclude *> pId
    --      mw     <- optionMaybe (pTok KWith >> pExp) <* pTok KSectionR
    --      return (EIncl m (UName n) mw, m)

      -- APP1 APP2
--      pApp pTerm
    ] <?> "an expression"

-- pAlts :: TokAtom -> TokAtom -> Parser [Alt]
-- pAlts begin end = (<?> "an alternate expression") $
--         try pCons
--     <|> try ((:[]) <$> pDefault end)
--     <|> pEnd
--   where
--     pCons = do
--         s <- (pTok KSectionL >> pTok begin) *> pExp <* pTok KSectionR
--         c <- pExp
--         (Alt s c :) <$> pAlts begin end

--     pEnd = pSection end >> return []

-- pDefault :: TokAtom -> Parser Alt
-- pDefault end = ADefault <$> (pSection KElse *> pExp <* pSection end)
--     <?> "an else expression"

-- FIXME: needs to handle whitespace / newline control
-- just do as monoids fornow

pSection :: Atom -> Parser Meta
pSection k = begin *> pAtom k <* end
  where
    begin = optional (line >> white) >> pAtom KSectionL
    end   = pAtom KSectionR >> optional (white >> line)

    white = many (pCapture KWhiteSpace)
    line  = pAtom KNewLine

pExp :: Parser (Exp Meta)
pExp = pApp (pOp <|> pTerm)

pTerm :: Parser (Exp Meta)
pTerm = pLiteral <|> pVar

pOp :: Parser (Exp Meta)
pOp = do
    x      <- try pTerm
    (m, o) <- pCapture KOp
    y      <- pTerm
    return (eapp m [evar m o, x, y])

pApp :: Parser (Exp Meta) -> Parser (Exp Meta)
pApp p = do
    x <- p
    (eapp (meta x) . (x :) <$> many1 p) <|> return x

pVar :: Parser (Exp Meta)
pVar = uncurry evar <$> pCapture KIdent

pLiteral :: Parser (Exp Meta)
pLiteral = choice
    [ try text    <?> "a string"
    , try boolean <?> "a boolean"
    , integer     <?> "an integer"
    ]
  where
    text = uncurry etext <$> pCapture KText

    boolean = true <|> false

    true  = (`ebool` True)  <$> pAtom KTrue
    false = (`ebool` False) <$> pAtom KFalse

    integer = do
        (m, txt) <- pCapture KNum
        case Read.signed Read.decimal txt of
            Right (x, rs)
                | Text.null rs -> return (einteger m x)
                | otherwise    -> fail $ "leftovers after parsing number: " ++ show (txt, rs)
            Left  e            -> fail $ "unexpected error parsing number:" ++ e

pCapture :: Capture -> Parser (Meta, Text)
pCapture x = pMaybe f
  where
    f (TC m y t) | x == y = Just (m, t)
    f _                   = Nothing

pAtom :: Atom -> Parser Meta
pAtom x = pMaybe f
  where
    f (TA m y) | x == y = Just m
    f _                 = Nothing

pMaybe :: (Token -> Maybe a) -> Parser a
pMaybe f = pShow >>= \g -> token g tokenSourcePos f

pShow :: Parser (Token -> String)
pShow = stateShow <$> getState
