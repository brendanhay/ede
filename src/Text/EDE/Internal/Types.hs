{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

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

import           Control.Applicative
import           Control.Monad
import           Data.Aeson              hiding (Result, Success, Error)
import           Data.Aeson.Types        (Pair)
import           Data.HashMap.Strict     (HashMap)
import           Data.List               (intercalate)
import           Data.Monoid
import           Data.Scientific
import           Data.Text               (Text)
import           Data.Text.Buildable
import           Data.Text.Format        (Format, format)
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Text -> Meta -> m (Result Template)

-- | A parsed and compiled template.
data Template = Template
    { tmplName :: !Text
    , tmplExpr :: UExp
    , tmplIncl :: HashMap Text UExp
    } deriving (Eq)

-- | Meta information describing the source position of an expression or error.
data Meta = Meta
    { metaSource :: !String
    , metaRow    :: !Int
    , metaColumn :: !Int
    } deriving (Eq, Ord)

instance Show Meta where
    show _ = ""

-- | The result of running parsing or rendering steps.
data Result a
    = Error !Meta [String]
    | Success a
      deriving (Eq, Ord, Show)

instance Functor Result where
    fmap _ (Error m e) = Error m e
    fmap f (Success x) = Success $ f x
    {-# INLINE fmap #-}

instance Monad Result where
    return          = Success
    {-# INLINE return #-}
    Error m e >>= _ = Error m e
    Success a >>= k = k a
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Monoid a => Monoid (Result a) where
    mempty  = Success mempty
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Convert a 'Result' to an 'Either' with the 'Left' case holding a formatted
-- error message, and 'Right' being the successful result over which 'Result' is paramterised.
eitherResult :: Result a -> Either String a
eitherResult = result f Right
  where
    f Meta{..} e = Left . concat $
        [ "ED-E error position: "
        , concat [metaSource, ":(", show metaRow, ",", show metaColumn, ")"]
        , ", messages: " ++ intercalate ", " e
        ]

-- | Perform a case analysis on a 'Result'.
result :: (Meta -> [String] -> b) -- ^ Function to apply to the 'Error' parameters.
       -> (a -> b)                -- ^ Function to apply to the 'Success' case.
       -> Result a                -- ^ The 'Result' to map over.
       -> b
result f _ (Error m e) = f m e
result _ g (Success x) = g x

-- | Convenience for returning a successful 'Result'.
success :: Monad m => a -> m (Result a)
success = return . Success

-- | Convenience for returning an error 'Result'.
failure :: Monad m => Meta -> [String] -> m (Result a)
failure m = return . Error m

newtype Id = Id Text
    deriving (Eq, Ord, Show)

instance Buildable Id where
    build (Id i) = build i
    {-# INLINE build #-}

data Fun where
    Fun :: (Eq a, Eq b) => TType a -> TType b -> (a -> b) -> Fun

instance Eq Fun where
    _ == _ = False

data TType a where
    TNil  :: TType ()
    TText :: TType Text
    TBool :: TType Bool
    TNum  :: TType Scientific
    TBld  :: TType Builder
    TMap  :: TType Object
    TList :: TType Array
    TFun  :: TType Fun

deriving instance Show (TType a)

data UExp
    = UNil
    | UText !Meta !Text
    | UBool !Meta !Bool
    | UNum  !Meta !Scientific
    | UBld  !Meta !Builder
    | UVar  !Meta !Id
    | UFun  !Meta !Id
    | UApp  !Meta !UExp !UExp
    | UNeg  !Meta !UExp
    | UBin  !Meta !BinOp !UExp !UExp
    | URel  !Meta !RelOp !UExp !UExp
    | UCond !Meta !UExp !UExp !UExp
    | UCase !Meta !UExp [(UExp, UExp)] !UExp
    | ULoop !Meta !Id !UExp !UExp !UExp
    | UIncl !Meta !Text (Maybe UExp)
      deriving (Eq, Ord, Show)

-- FIXME:
-- {% assign ... %}
-- {% capture ... %}

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
    UNum  m _       -> m
    UBld  m _       -> m
    UVar  m _       -> m
    UFun  m _       -> m
    UApp  m _ _     -> m
    UNeg  m _       -> m
    UBin  m _ _ _   -> m
    URel  m _ _ _   -> m
    UCond m _ _ _   -> m
    UCase m _ _ _   -> m
    ULoop m _ _ _ _ -> m
    UIncl m _ _     -> m

-- | Create an 'Object' from a list of name/value 'Pair's.
-- See 'Aeson''s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs = (\(Object o) -> o) . object
