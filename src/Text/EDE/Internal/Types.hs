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

data Lit
    = LText !String
    | LBool !Bool
    | LNum  !Scientific
      deriving (Show)

data Bound
    = UName !String
    | UPrim !String !Type
      deriving (Show)

data Exp
    = EVar  Meta !Bound
    | ELit  Meta !Lit
    | ELet  Meta !Bound !Exp
    | EApp  Meta !Exp   !Exp
    | ECond Meta [Alt]
    | ECase Meta !Exp   [Alt]
    | ELoop Meta !Bound !Exp (Maybe Alt)
    | EIncl Meta !Bound (Maybe Exp)
      deriving (Show)

data Alt
    = ACond    !Exp !Exp
    | ADefault !Exp
      deriving (Show)

data Type
    = TVar !Bound
    | TApp !Type !Type
    | TBot
      deriving (Show)
