{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Text.EDE.Internal.Lexer.Tokens
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Lexer.Tokens where

import Text.EDE.Internal.Types
import Text.Parsec.Pos
import Text.PrettyPrint.Leijen.Text

data Token = Token
    { tokenTok :: Tok
    , tokenPos :: Meta
    } deriving (Eq)

instance Show Token where
    show = prettyString

instance Pretty Token where
    pretty Token{..} = pretty tokenPos <+> pretty tokenTok

tokenSourcePos :: Token -> SourcePos
tokenSourcePos t = let Meta src l c = tokenPos t in newPos src l c

data Tok
    = KNewLine
    | KJunk Char
    | KFrag Char
    | KAtom !TokAtom
    | KPrim !TokPrim
      deriving (Eq)

instance Pretty Tok where
    pretty t = case t of
        KNewLine -> "newline"
        KJunk c  -> "junk"     <+> squotes (pretty c)
        KFrag c  -> "fragment" <+> squotes (pretty c)
        KAtom a  -> pretty a
        KPrim p  -> pretty p

data TokAtom
    = KSectionL
    | KSectionR
    | KIdentL
    | KIdentR

    | KParenL
    | KParenR
    | KComma

    | KOp !String

    | KTrue
    | KFalse

    | KElse
    | KIf
    | KElseIf
    | KEndIf
    | KCase
    | KWhen
    | KEndCase
    | KFor
    | KIn
    | KEndFor
    | KInclude
    | KWith
    | KAssign
    | KCapture
    | KEndCapture
    | KRaw
    | KEndRaw
      deriving (Eq)

data TokFamily = Symbol | Keyword

instance Pretty TokFamily where
    pretty Symbol  = "symbol"
    pretty Keyword = "keyword"

instance Pretty TokAtom where
    pretty t = pretty fm <+> x
      where
        (fm, x) = case t of
            KSectionL   -> (Symbol, "{%")
            KSectionR   -> (Symbol, "%}")

            KIdentL     -> (Symbol, "{{")
            KIdentR     -> (Symbol, "}}")

            KParenL     -> (Symbol, char '(')
            KParenR     -> (Symbol, char ')')
            KComma      -> (Symbol, char ',')

            KOp s       -> (Symbol, pretty s)

            KTrue       -> (Keyword, "true")
            KFalse      -> (Keyword, "false")

            KElse       -> (Keyword, "else")
            KIf         -> (Keyword, "if")
            KElseIf     -> (Keyword, "elif")
            KEndIf      -> (Keyword, "endif")
            KCase       -> (Keyword, "case")
            KWhen       -> (Keyword, "when")
            KEndCase    -> (Keyword, "endcase")
            KFor        -> (Keyword, "for")
            KIn         -> (Keyword, "in")
            KEndFor     -> (Keyword, "endfor")
            KInclude    -> (Keyword, "include")
            KWith       -> (Keyword, "with")
            KAssign     -> (Keyword, "assign")
            KCapture    -> (Keyword, "capture")
            KEndCapture -> (Keyword, "endcapture")
            KRaw        -> (Keyword, "raw")
            KEndRaw     -> (Keyword, "endraw")

data TokPrim
    = KVar !String
    | KLit !TokLit
      deriving (Eq)

instance Pretty TokPrim where
    pretty (KVar n) = "variable" <+> dquotes (pretty n)
    pretty (KLit l) = "literal"  <+> pretty l

data TokLit
    = KText !String
    | KNum  !String
      deriving (Eq)

instance Pretty TokLit where
    pretty (KText s) = dquotes $ pretty s
    pretty (KNum  n) = pretty n
