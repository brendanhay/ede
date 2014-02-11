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

import           Control.Applicative     ((<$>))
import           Control.Arrow
import           Control.Monad
import           Data.Foldable           (foldl')
import           Data.Functor.Identity
import           Safe                    (readMay)
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types
import qualified Text.Parsec             as P
import           Text.Parsec             hiding (SourcePos, runParser)
import           Text.Show.Pretty        (ppShow)

type Parser a = ParsecT [Token] ParserState Identity a

type ParserState = String

runParser :: String -> Parser a -> [Token] -> Either ParseError a
runParser name parser = P.runParser parser name name

pExp :: Parser Exp
pExp = choice
    [ -- assign <name> = <exp>
      try $ do
        (_, p) <- pTokSP KSectionL
        pTok KAssign
        (n, _) <- pVarSP
        pTok (KOp "=")
        b      <- pExp
        pTok KSectionR
        return $ ELet p (UName n) b

      -- capture <name> ...
    , try $ do
        (_, p) <- pTokSP KSectionL
        pTok KCapture
        (n, _) <- pVarSP
        pTok KSectionR
        b      <- pExp
        pSection KEndCapture
        return $ ELet p (UName n) b

      -- if [<alt>]
    , try $ do
        (_, p) <- pTokSP KSectionL
        pTok KIf
        s      <- pExp
        pTok KSectionR
        c      <- pExp
        as     <- pAlts KElseIf KEndIf
        return $ ECond p (ACond s c : as)

      -- case <exp> [<alt>]
    , try $ do
        (_, p) <- pTokSP KSectionL
        pTok KCase
        s      <- pExp
        pTok KSectionR
        as     <- pAlts KWhen KEndCase
        return $ ECase p s as

      -- for <name> in <exp>
    , try $ do
        (_, p) <- pTokSP KSectionL
        pTok KFor
        (n, _) <- pVarSP
        pTok KIn
        s      <- pExp
        pTok KSectionR
        ma     <- optionMaybe (pDefault KEndCase)
        return $ ELoop p (UName n) s ma

      -- include <exp> [with <exp>]
    , try $ do
         (_, p) <- pTokSP KSectionL
         pTok KInclude
         (n, _) <- pVarSP
         mw     <- optionMaybe $ pTok KWith >> pExp
         pTok KSectionR
         return $ EIncl p (UName n) mw

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
        pTok KSectionL
        pTok begin
        s <- pExp
        pTok KSectionR
        c <- pExp
        (ACond s c :) <$> pAlts begin end

    pEnd = pSection end >> return []

pDefault :: TokAtom -> Parser Alt
pDefault end = (<?> "an else expression") $ do
    pSection KElse
    a <- pExp
    pSection end
    return $ ADefault a

pSection :: TokAtom -> Parser ()
pSection k = (pTok KSectionL >> pTok k >> pTok KSectionR) <?> ("a" ++ ppShow k)

pApp :: Parser Exp
pApp = do
    (x1, _) <- pAtomSP
    choice
        [ foldl' (\x (x', p) -> EApp p x x') x1 <$> many1 pAtomSP
        , return x1
        ] <?> "an expression or application"

pAtomSP :: Parser (Exp, Meta)
pAtomSP = choice
    [ -- (EXP)
      do (_, p) <- pTokSP KParenL
         t      <- pExp
         pTok KParenR
         return (t, p)

      -- literals
    , do (l, p) <- pLitSP
         return (ELit p l, p)

      -- variables
    , do (v, p) <- pVarSP
         return (EVar p (UName v), p)
    ]

pVarSP :: Parser (String, Meta)
pVarSP = pTokMaybe f <?> "a variable"
  where
    f (KP (KVar n)) = Just n
    f _             = Nothing

pLitSP :: Parser (Lit, Meta)
pLitSP = try bool <|> literal
  where
    bool = first LBool <$> pTokMaybe h <?> "a boolean"

    literal = do
        (l, p) <- pTokMaybe f <?> "a string or numeric literal"
        (,p) <$> g l

    f (KP (KLit x)) = Just x
    f _             = Nothing

    g (KText  s) = return $ LText s
    g n@(KNum x) =
        maybe (fail $ "unexpected " ++ ppShow n)
              (return . LNum)
              (readMay x) <?> "a valid numeric literal"

    h (KA KTrue)  = Just True
    h (KA KFalse) = Just False
    h _           = Nothing

pTok :: TokAtom -> Parser ()
pTok = void . pTokSP

pTokSP :: TokAtom -> Parser (Tok, Meta)
pTokSP x = pTokMaybe $ \y -> if (KA x) == y then Just y else Nothing

pTokMaybe  :: (Tok -> Maybe a) -> Parser (a, Meta)
pTokMaybe f = token (ppShow . tokenTok) takeSourcePos g
  where
    g x = (, tokenPos x) <$> f (tokenTok x)
