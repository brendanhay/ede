-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Types where

import           Data.Scientific (Scientific)
import qualified Text.Parsec.Pos as P

-- data Delim = Delim !Char !Char

-- data Language = Language
--     { defSectionStart :: Delim
--     , defSectionEnd   :: Delim
--     , defCommentStart :: Delim
--     , defCommentEnd   :: Delim
--     , defFilterDelim  :: !Char
--     }

data SourcePos = SourcePos
    { sourceName :: String
    , sourceLine :: Int
    , sourceCol  :: Int
    } deriving (Eq, Show)

data Token t = Token
    { tokenTok :: t
    , tokenPos :: SourcePos
    } deriving (Eq, Show)

tokenLine :: Token t -> Int
tokenLine = sourceLine . tokenPos

tokenCol :: Token t -> Int
tokenCol = sourceCol . tokenPos

takeSourcePos :: Token k -> P.SourcePos
takeSourcePos t =
    let SourcePos src line col = tokenPos t
    in  P.newPos src line col

data Lit
    = LText !String
    | LBool !Bool
    | LNum  !Scientific
      deriving (Show)

data Bound
    = UName !String
    | UPrim !String !Type
      deriving (Show)

data Exp a
    = EVar  !a !Bound
    | ELit  !a !Lit
    | ELet  !a !Bound   !(Exp a)
    | EApp  !a !(Exp a) !(Exp a)
    | ECond !a ![Alt a]
    | ECase !a !(Exp a) ![Alt a]
    | ELoop !a !Bound   !(Exp a) (Maybe (Alt a))
    | EIncl !a !Bound   (Maybe (Exp a))
      deriving (Show)

data Alt a
    = ACond    !(Exp a) !(Exp a)
    | ADefault !(Exp a)
      deriving (Show)

data Type
    = TVar !Bound
    | TApp !Type !Type
    | TBot
      deriving (Show)
