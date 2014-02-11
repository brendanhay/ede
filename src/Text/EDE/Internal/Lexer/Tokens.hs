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

data Tok
    = KJunk String
    | KM !TokMeta
    | KA !TokAtom
    | KP !TokPrim
      deriving (Eq, Show)

data TokMeta
    = KNewLine
    | KCommentStart
    | KCommentEnd
    | KCommentUnterminated
      deriving (Eq, Show)

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
    | KEqual
    | KCapture
    | KCycle
      deriving (Eq, Show)

data TokPrim
    = KVar !String
    | KLit !TokLit
      deriving (Eq, Show)

data TokLit
    = KText !String
    | KNum  !String
      deriving (Eq, Show)
