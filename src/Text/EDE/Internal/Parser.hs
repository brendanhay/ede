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
import           Data.Functor.Identity
import           Prelude                 hiding (show)
import           Safe                    (readMay)
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types
import qualified Text.Parsec             as Parsec
import           Text.Parsec             hiding (runParser)

type Parser a = ParsecT [Token] ParserState Identity a

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

pExp :: Parser Exp
pExp = choice
    [ -- assign <name> = <exp>
      try $ do
        (_, m) <- pTokM KSectionL
        n      <- pTok  KAssign *> pVar
        b      <- pTok  (KOp "=") *> pExp <* pTok KSectionR
        return $ ELet m (UName n) b

      -- capture <name> ...
    , try $ do
        (_, m) <- pTokM KSectionL
        n      <- pTok  KCapture *> pVar
        b      <- pTok  KSectionR *> pExp <* pSection KEndCapture
        return $ ELet m (UName n) b

      -- if [<alt>]
    , try $ do
        (_, m) <- pTokM KSectionL
        s      <- pTok  KIf *> pExp
        c      <- pTok  KSectionR *> pExp
        as     <- pAlts KElseIf KEndIf
        return $ ECond m (ACond s c : as)

      -- case <exp> [<alt>]
    , try $ do
        (_, m) <- pTokM KSectionL
        s      <- pTok  KCase *> pExp <* pTok KSectionR
        as     <- pAlts KWhen KEndCase
        return $ ECase m s as

      -- for <name> in <exp>
    , try $ do
        (_, m) <- pTokM KSectionL
        n      <- pTok  KFor *> pVar
        s      <- pTok  KIn  *> pExp <* pTok KSectionR
        ma     <- optionMaybe (pDefault KEndCase)
        return $ ELoop m (UName n) s ma

      -- include <exp> [with <exp>]
    , try $ do
         (_, m) <- pTokM KSectionL
         n      <- pTok  KInclude *> pVar
         mw     <- optionMaybe (pTok KWith >> pExp) <* pTok KSectionR
         return $ EIncl m (UName n) mw

      -- APP1 APP2
    , pApp
    ]
        <?> "an expression"

pAlts :: TokAtom -> TokAtom -> Parser [Alt]
pAlts begin end = (<?> "an alternate expression") $
        try pCons
    <|> try ((:[]) <$> pDefault end)
    <|> pEnd
  where
    pCons = do
        s <- (pTok KSectionL >> pTok begin) *> pExp <* pTok KSectionR
        c <- pExp
        (ACond s c :) <$> pAlts begin end

    pEnd = pSection end >> return []

pDefault :: TokAtom -> Parser Alt
pDefault end = ADefault <$> (pSection KElse *> pExp <* pSection end)
    <?> "an else expression"

pSection :: TokAtom -> Parser ()
pSection k = do
    show   <- pTokShow
    (_, m) <- pTokM KSectionL <?> "the start of a section"
    pTok k <?> ('a' : ' ' : show (Token (KAtom k) m))
    pTok KSectionR <?> "the end of a section"

pApp :: Parser Exp
pApp = do
    (x1, _) <- pAtomM
    choice
        [ foldl' (\x (x', m) -> EApp m x x') x1 <$> many1 pAtomM
        , return x1
        ] <?> "an expression or application"

pAtomM :: Parser (Exp, Meta)
pAtomM = choice
    [ -- (EXP)
      do (_, m) <- pTokM KParenL
         t      <- pExp <* pTok KParenR
         return (t, m)

      -- literals
    , do (l, m) <- pLitM
         return (ELit m l, m)

      -- variables
    , do (v, m) <- pVarM
         return (EVar m (UName v), m)
    ]

applyM :: Parser (a, Meta) -> (Meta -> a -> b) -> Parser (b, Meta)
applyM parser f = do
    (x, m) <- parser
    return (f m x, m)

pVar :: Parser String
pVar = fst <$> pVarM

pVarM :: Parser (String, Meta)
pVarM = pTokMaybeM f <?> "a variable"
  where
    f (KPrim (KVar n)) = Just n
    f _                = Nothing

pLitM :: Parser (Lit, Meta)
pLitM = try bool <|> literal
  where
    bool = first LBool <$> pTokMaybeM h <?> "a boolean"

    literal = do
        (l, m) <- pTokMaybeM f <?> "a string or numeric literal"
        (,m) <$> g m l

    f (KPrim (KLit x)) = Just x
    f _                = Nothing

    g _ (KText  s) = return $ LText s
    g m n@(KNum x) = do
        show <- pTokShow
        maybe (fail $ "unexpected " ++ show (Token (KPrim (KLit n)) m))
              (return . LNum)
              (readMay x) <?> "a valid numeric literal"

    h (KAtom KTrue)  = Just True
    h (KAtom KFalse) = Just False
    h _              = Nothing

pTok :: TokAtom -> Parser ()
pTok = void . pTokM

pTokM :: TokAtom -> Parser (Tok, Meta)
pTokM x = pTokMaybeM $ \y -> if KAtom x == y then Just y else Nothing

pTokMaybeM  :: (Tok -> Maybe a) -> Parser (a, Meta)
pTokMaybeM f = do
    show <- pTokShow
    token show tokenSourcePos $ \x -> (, tokenPos x) <$> f (tokenTok x)

pTokShow :: Parser (Token -> String)
pTokShow = stateShow <$> getState
