{-# LANGUAGE TupleSections #-}

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
import qualified Data.Text               as Text
import           Data.Text               (Text)
import qualified Data.Text.Read          as Read
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

-- pDoc :: Parser Exp
-- pDoc = f $ pFragM <|> pIdentM <|> pExpM
--   where
--     f :: Parser (Exp, Meta) -> Parser Exp
--     f p = do
--         (x, _) <- p
--         xs     <- map fst <$> many1 p <|> return []
--         return $ foldl' (\a e -> eapp [ebound "<>", a, e]) x xs
--         -- alternative associativty:
--         -- return $ foldr (\e a -> eapp [ebound "mappend", a, e]) x xs

-- pFragM :: Parser (Exp, Meta)
-- pFragM = do
--     (c, m) <- pTokMaybeM f <?> "a fragment"
--     cs     <- manyTill (pTokMaybeM f) (try . lookAhead $ void (pTokMaybeM g) <|> eof)
--     return (elit (LText . Text.pack $ c : map fst cs), m)
--   where
--     f (KFrag c) = Just c
--     f _         = Nothing

--     g (KFrag _) = Nothing
--     g _         = Just ()

-- pIdentM :: Parser (Exp, Meta)
-- pIdentM = do
--     (_, m) <- pTokM KIdentL
--     (v, _) <- pAtomM <* pTok KIdentR
--     return (v, m)

-- pExp :: Parser Exp
-- pExp = fst <$> pExpM

-- pExpM :: Parser (Exp a, Meta)
-- pExpM = choice
--     [ -- assign <name> = <exp>
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
--      pAppM pAtomM
--    ] <?> "an expression"

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

-- pSection :: TokAtom -> Parser ()
-- pSection k = do
--     show   <- pTokShow
--     (_, m) <- pTokM KSectionL <?> "the start of a section"
--     pTok k <?> ('a' : ' ' : show (Token (KAtom k) m))
--     pTok KSectionR <?> "the end of a section"

-- pAppM :: Parser (Exp, Meta) -> Parser (Exp, Meta)
-- pAppM p = do
--     (x, m) <- p
--     res    <- choice [eapp . (x:) . map fst <$> many1 p, return x]
--     return (res, m)

-- pAtomM :: Parser (Exp, Meta)
-- pAtomM = choice
--     [ -- (EXP)
--       do (_, m) <- pTokM KParenL
--          t      <- pExp <* pTok KParenR
--          return (t, m)

--       -- literals
--     , do (l, m) <- pLitM
--          return (elit l, m)

--       -- variables
--     , pFreeM
--     ]

-- pBound, pFree :: Parser Exp
-- pBound = fst <$> pBoundM
-- pFree  = fst <$> pFreeM

-- pBoundM, pFreeM :: Parser (Exp, Meta)
-- pBoundM = first ebound <$> pIdM
-- pFreeM  = first efree  <$> pIdM

-- pId :: Parser Id
-- pId = fst <$> pIdM

-- pIdM :: Parser (Id, Meta)
-- pIdM = pTokMaybeM f <?> "a variable"
--   where
--     f (KPrim (KVar n)) = Just n
--     f _                = Nothing

pLiteral :: Parser (Exp Meta)
pLiteral = choice
    [ try string  <?> "a string"
    , try boolean <?> "a boolean"
    , integer     <?> "an integer"
    ]
  where
    string  = uncurry etext <$> pCapture KText

    boolean = true <|> false
    true    = (`ebool` True)  <$> pAtom KTrue
    false   = (`ebool` False) <$> pAtom KFalse

    integer = do
        (m, txt) <- pCapture KNum
        case Read.signed Read.decimal txt of
            Right (x, rs)
                | Text.null rs -> return $! einteger m x
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
