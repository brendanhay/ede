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
import           Control.Arrow
import           Control.Monad
import           Data.Foldable           (foldl')
import           Data.Functor.Identity
import           Data.Tuple
import           Safe                    (readMay)
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types
import qualified Text.Parsec             as P
import           Text.Parsec             hiding (SourcePos, runParser)
import           Text.Parsec.Error

type Parser a = ParsecT [Token Tok] ParserState Identity a

type ParserState = String

runParser :: String      -- ^ Source name for error messages.
          -> Parser a    -- ^ Parser to run.
          -> [Token Tok] -- ^ Tokens to parse.
          -> Either ParseError a
runParser name parser = P.runParser parser name name


--pDocument :: Parser Document

pExp :: Parser (Exp SourcePos)
pExp = P.choice
    [ -- assign <name> = <exp>
      do p <- snd <$> pTokSP KSectionL
         pTok KAssign
         n <- UName <$> pVar
         pTokSP (KOp "=")
         b <- pExp
         pTok KSectionR
         return $ ELet p n b

      -- application
    , do pExpApp
    ]

pExpApp :: Parser (Exp SourcePos)
pExpApp = do
    (x1, _) <- pExpAtomSP
    P.choice
        [ foldl' (\x (x', p) -> EApp p x x') x1 <$> P.many1 pExpAtomSP
        , return x1
        ] <?> "an expression or application"

pExpAtomSP :: Parser (Exp SourcePos, SourcePos)
pExpAtomSP = P.choice
    [ -- (EXP)
      do (_, p) <- pTokSP KParenL
         t      <- pExp
         pTok KParenR
         return (t, p)

      -- literals
    , do (l, p) <- pLitSP
         return (ELit p l, p)
    ]

pVar :: Parser String
pVar = fst <$> pTokMaybe f <?> "a variable"
  where
    f (KP (KVar n)) = Just n
    f _             = Nothing

pLitSP :: Parser (Lit, SourcePos)
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
        maybe (fail $ "unexpected " ++ show n)
              (return . LNum)
              (readMay x) <?> "a valid numeric literal"

    h (KA KTrue)  = Just True
    h (KA KFalse) = Just False
    h _           = Nothing

pTok :: TokAtom -> Parser ()
pTok = void . pTokSP

pTokSP :: TokAtom -> Parser (Tok, SourcePos)
pTokSP x = pTokMaybe $ \y -> if (KA x) == y then Just y else Nothing

pTokMaybe  :: (Tok -> Maybe a) -> Parser (a, SourcePos)
pTokMaybe f = P.token (show . tokenTok) takeSourcePos g
  where
    g x = (, tokenPos x) <$> f (tokenTok x)
