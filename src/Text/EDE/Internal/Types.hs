{-# LANGUAGE RecordWildCards #-}

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

import Data.Scientific              (Scientific)
import Data.String
import Data.Text                    (Text)
import Text.PrettyPrint.Leijen.Text

prettyString :: Pretty a => a -> String
prettyString = show . renderCompact . pretty

data Meta = Meta
    { metaName :: String
    , metaLine :: Int
    , metaCol  :: Int
    } deriving (Eq)

instance Show Meta where
    show = prettyString

instance Pretty Meta where
    pretty Meta{..} =
          fromString metaName
       <> char '('
       <> pretty metaLine
       <> char ','
       <> pretty metaCol
       <> char ')'

data Ann a = Ann
    { annType :: Type
    , annTail :: a
    } deriving (Show)

data Bind
    = BNone !Type
    | BName !String !Type
    | BAnon !Type
      deriving (Show)

data Bound
    = UName  !String
    | UIndex !Int
      deriving (Show)

data Type
    = TVar !Bound
    | TApp !Type !Type
    | TText
    | TBool
    | TNum
    | TBot
      deriving (Show)

data Exp a
    = EVar  !a !Bound
    | ELit  !a !Lit
    | ELet  !a !Bind    !(Exp a)
    | EApp  !a !(Exp a) !(Exp a)
    | ECond !a [Alt a]
    | ECase !a !(Exp a) [Alt a]
    | ELoop !a !Bind    !(Exp a) (Maybe (Alt a))
    | EIncl !a !Bind    (Maybe (Exp a))
      deriving (Show)

data Alt a
    = ACond    !(Exp a) !(Exp a)
    | ADefault !(Exp a)
      deriving (Show)

data Lit
    = LText !Text
    | LBool !Bool
    | LNum  !Scientific
      deriving (Show)
