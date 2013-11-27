{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

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

import           Data.Aeson              hiding (Result, Success, Error)
import           Data.List               (intercalate)
import           Data.Text               (Text)
import           Data.Text.Buildable
import           Data.Text.Format        (Format, format)
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder

-- | Meta information describing the source position of an expression or error.
data Meta = Meta
    { _source :: !String
    , _row    :: !Int
    , _column :: !Int
    } deriving (Eq, Ord)

instance Show Meta where
    show _ = ""

-- | The result of running parsing or rendering steps.
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

-- | Perform a case analysis on a 'Result'.
result :: (Meta -> [String] -> b) -- ^ Function to apply to the 'Error' parameters.
       -> (a -> b)                -- ^ Function to apply to the 'Success' case.
       -> Result a                -- ^ The 'Result' to map over.
       -> b
result f _ (Error m e) = f m e
result _ g (Success x) = g x

-- | Convert a 'Result' to an 'Either' with the 'Left' case holding a formatted
-- error message, and 'Right' being the successful result over which 'Result' is paramterised.
eitherResult :: Result a -> Either String a
eitherResult = result f Right
  where
    f Meta{..} e = Left . concat $
        [ "ED-E error position: "
        , concat [_source, ":(", show _row, ",", show _column, ")"]
        , ", messages: " ++ intercalate ", " e
        ]

newtype Id = Id Text
    deriving (Eq, Ord, Show)

instance Buildable Id where
    build (Id i) = build i

data Filter where
    (:|:) :: (a -> a) -> TType a -> Filter

data TType a where
    TNil  :: TType ()
    TText :: TType Text
    TBool :: TType Bool
    TInt  :: TType Integer
    TDbl  :: TType Double
    TBld  :: TType Builder
    TMap  :: TType Object
    TList :: TType Array

deriving instance Show (TType a)

data UExp
    = UNil
    | UText !Meta !Text
    | UBool !Meta !Bool
    | UInt  !Meta !Integer
    | UDbl  !Meta !Double
    | UBld  !Meta !Builder
    | UVar  !Meta !Id
    | UFil  !Meta !UExp  !Text
    | UApp  !Meta !UExp  !UExp
    | UNeg  !Meta !UExp
    | UBin  !Meta !BinOp !UExp !UExp
    | URel  !Meta !RelOp !UExp !UExp
    | UCond !Meta !UExp  !UExp !UExp
    | ULoop !Meta !Id    !UExp !UExp !UExp
      deriving (Eq, Ord, Show)

-- FIXME:
-- {% raw %} {% endraw %}
-- {% case <value> %}
-- {% when <value> %}
-- {% else %}
-- {% endcase %}
-- {% let <name> = <value> %}

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

throwError :: Params ps => Meta -> Format -> ps -> Result a
throwError m f = Error m . (:[]) . LText.unpack . format f

mkMeta :: String -> Meta
mkMeta n = Meta n 0 0

_meta :: UExp -> Meta
_meta u = case u of
    UNil            -> mkMeta "_meta"
    UText m _       -> m
    UBool m _       -> m
    UInt  m _       -> m
    UDbl  m _       -> m
    UBld  m _       -> m
    UVar  m _       -> m
    UFil  m _ _     -> m
    UApp  m _ _     -> m
    UNeg  m _       -> m
    UBin  m _ _ _   -> m
    URel  m _ _ _   -> m
    UCond m _ _ _   -> m
    ULoop m _ _ _ _ -> m
