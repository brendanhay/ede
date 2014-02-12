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
import Text.Parsec
import Text.Parsec.Pos

-- data Delim = Delim !Char !Char

-- data Language = Language
--     { defSectionStart :: Delim
--     , defSectionEnd   :: Delim
--     , defCommentStart :: Delim
--     , defCommentEnd   :: Delim
--     , defFilterDelim  :: !Char
--     }

data Token = Token
    { tokenTok :: Tok
    , tokenPos :: Meta
    } deriving (Eq, Show)

tokenLine :: Token -> Int
tokenLine = metaLine . tokenPos

tokenCol :: Token -> Int
tokenCol = metaCol . tokenPos

takeSourcePos :: Token -> SourcePos
takeSourcePos t =
    let Meta src line col = tokenPos t
    in  newPos src line col

data Tok
    = KJunk Char
    | KFrag Char
    | KNewLine
    | KA !TokAtom
    | KP !TokPrim
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
    | KEndCapture
      deriving (Eq, Show)

data TokPrim
    = KVar !String
    | KLit !TokLit
      deriving (Eq, Show)

data TokLit
    = KText !String
    | KNum  !String
      deriving (Eq, Show)
