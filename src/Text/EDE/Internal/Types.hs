{-# LANGUAGE StandaloneDeriving #-}

-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Types where

import Data.Text              (Text)
import Data.Text.Buildable
import Data.Text.Lazy.Builder

data Meta = Meta
    { source :: !String
    , row    :: !Int
    , column :: !Int
    } deriving (Eq, Ord, Show)

data Result a
    = Error Meta [String]
    | Success a
      deriving (Eq, Ord, Show)

instance Functor Result where
    fmap _ (Error m e) = Error m e
    fmap f (Success x) = Success $ f x

instance Monad Result where
    return          = Success
    Error m e >>= _ = Error m e
    Success a >>= k = k a

newtype Id = Id Text
    deriving (Eq, Ord, Show)

instance Buildable Id where
    build (Id i) = build i

data UExp
    = UNil
    | UText !Meta !Text
    | UBool !Meta !Bool
    | UInt  !Meta !Integer
    | UDbl  !Meta !Double
    | UBld  !Meta !Builder
    | UVar  !Meta !Id
    | UApp  !Meta !UExp  !UExp
    | UNeg  !Meta !UExp
    | UBin  !Meta !BinOp !UExp !UExp
    | URel  !Meta !RelOp !UExp !UExp
    | UCond !Meta !UExp  !UExp !UExp
    | ULoop !Meta !UExp  !UExp !UExp !UExp
      deriving (Eq, Ord, Show)

data BinOp = And | Or
    deriving (Eq, Ord, Show)

data RelOp
    = Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
      deriving (Eq, Ord, Show)

result :: (Meta -> [String] -> b) -> (a -> b) -> Result a -> b
result f _ (Error m e) = f m e
result _ g (Success x) = g x

metadata :: UExp -> Meta
metadata u = case u of
    UNil            -> (Meta "metadata" 0 0)
    UText m _       -> m
    UBool m _       -> m
    UInt  m _       -> m
    UDbl  m _       -> m
    UVar  m _       -> m
    UBld  m _       -> m
    UApp  m _ _     -> m
    UNeg  m _       -> m
    UBin  m _ _ _   -> m
    URel  m _ _ _   -> m
    UCond m _ _ _   -> m
    ULoop m _ _ _ _ -> m
