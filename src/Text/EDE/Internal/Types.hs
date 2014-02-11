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

-- data Bind
--     = BName !Type
--       -- ^ Named variable in the environment.
--     | BNone !Type
--       -- ^ Variable with no uses in the body doesn't need a name.
--     | BAnon !Type
--       -- ^ Nameless variable on the deBruijn stack.

data Lit
    = LText !String
    | LBool !Bool
    | LNum  !Scientific
      deriving (Show)

data Bound
    = UName !String
      -- ^ Named variable that should be in the environment.
    | UPrim !String !Type
      -- ^ Named primitive with it's attached type.
      deriving (Show)

data Exp a
    = EVar  !a !Bound
      -- ^ Variable.
    | ELit  !a !Lit
      -- ^ Literal.
    | ELet  !a !Bound   !(Exp a)
      -- ^ Variable bindings.
    | EApp  !a !(Exp a) !(Exp a)
      -- ^ Application.
    | ECond !a !(Exp a) ![Alt a]
      -- ^ Conditionals.
    | ELoop !a !Bound   !(Exp a) !(Alt a)
      -- ^ Loops.
    | EIncl !a !Lit     !Bound
      -- ^ Includes
      deriving (Show)

data Alt a
    = ACond !(Exp a)
    | ADefault
      deriving (Show)

data Type
    = TVar !Bound
      -- ^ Variable.
    | TApp !Type !Type
      -- ^ Application.
    | TBot
      -- ^ Bottom!
      deriving (Show)
