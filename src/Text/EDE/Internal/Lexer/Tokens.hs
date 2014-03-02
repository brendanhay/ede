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

import Data.Text               (Text)
import Text.EDE.Internal.Types
import Text.Parsec.Pos

data Token
    = TC !Meta !Capture !Text
    | TA !Meta !Atom
      deriving (Show)

tokenMeta :: Token -> Meta
tokenMeta (TC m _ _) = m
tokenMeta (TA m _)   = m

tokenSourcePos :: Token -> SourcePos
tokenSourcePos t = let Meta src row col = tokenMeta t in newPos src row col

eof :: Token -> Bool
eof (TA _ KEOF) = True
eof _           = False

data Capture
    = KNum
    | KIdent
    | KText
    | KOp
    | KFrag
    | KWhiteSpace
      deriving (Eq, Show)

data Atom
    = KExpL
    | KExpR
    | KParenL
    | KParenR
    | KComma

    | KNewLine
    | KEOF

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
      deriving (Eq, Show)
