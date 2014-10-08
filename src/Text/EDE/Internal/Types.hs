{-# LANGUAGE FlexibleInstances  #-}
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
import           Data.Monoid             hiding ((<>))
import           Data.Scientific
import           Data.Semigroup
import           Data.Text               (Text)
import           Data.Text.Buildable
import           Data.Text.Format        (Format, format)
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Text -> Meta -> m (Result Template)

instance Monad m => Semigroup (Resolver m) where
    (<>) f g = \x y -> liftM2 mplus (f x y) (g x y)
    {-# INLINE (<>) #-}

-- | A parsed and compiled template.
data Template = Template
    { tmplName :: !Text
    , tmplExpr :: Exp
    , tmplIncl :: HashMap Text Exp
    } deriving (Eq)

-- | Meta information describing the source position of an expression or error.
data Meta = Meta !String !Int !Int
    deriving (Eq, Show)

class Metadata a where
    meta :: a -> Meta

instance Metadata Meta where
    meta = id

-- | The result of running parsing or rendering steps.
data Result a
    = Error !Meta [String]
    | Success a
      deriving (Eq, Show)

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

instance Alternative Result where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) a@(Success _) _ = a
    (<|>) _ b             = b
    {-# INLINE (<|>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus = (<|>)
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
    f (Meta n r c) e = Left . concat $
        [ "ED-E error position: "
        , concat [n, ":(", show r, ",", show c, ")"]
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
    deriving (Eq, Show)

instance Buildable Id where
    build (Id i) = build i
    {-# INLINE build #-}

data Fun where
    Fun :: (Eq a, Eq b) => Type a -> Type b -> (a -> b) -> Fun

instance Eq Fun where
    _ == _ = False

data Type a where
    TNil  :: Type ()
    TText :: Type LText.Text
    TBool :: Type Bool
    TNum  :: Type Scientific
    TBld  :: Type Builder
    TMap  :: Type Object
    TList :: Type Array
    TFun  :: Type Fun
    TVar  :: Type a

deriving instance Show (Type a)

-- data BinOp = And | Or
--     deriving (Eq, Show)

-- data RelOp
--     = Equal
--     | NotEqual
--     | Greater
--     | GreaterEqual
--     | Less
--     | LessEqual
--       deriving (Eq, Show)

-- data Op
--     = ONeg !Exp
--     | OBin !BinOp !Exp !Exp
--     | ORel !RelOp !Exp !Exp
--       deriving (Eq, Show)

data Lit
    = LBool !Bool
    | LNum  !Scientific
    | LText LText.Text
      deriving (Eq, Show)

data Pat
    = PWild
    | PVar Id
    | PLit Lit
      deriving (Eq, Show)

type Alt = (Pat, Exp)

data Exp
--    = ENil
    = ELit  !Meta !Lit
    | EBld  !Meta !Builder
    | EVar  !Meta !Id
--    EFun  !Meta !Id
    | EApp  !Meta !Exp  !Exp
--    | EOp   !Meta !Op
    | ELet  !Meta !Id   (Either Lit Id)
    | ECase !Meta !Exp  [Alt]
    | ELoop !Meta !Id   !Id  !Exp (Maybe Exp)
    | EIncl !Meta !Text (Maybe Exp)
      deriving (Eq, Show)

instance Metadata Exp where
    meta x = case x of
--        ENil            -> mkMeta "Exp.meta"
        ELit  m _       -> m
        EBld  m _       -> m
        EVar  m _       -> m
--        EFun  m _       -> m
        EApp  m _ _     -> m
--        EOp   m _       -> m
        ELet  m _ _     -> m
        ECase m _ _     -> m
        ELoop m _ _ _ _ -> m
        EIncl m _ _     -> m

-- FIXME:
-- {% assign ... %}
-- {% capture ... %}

throwError :: Params ps => Meta -> Format -> ps -> Result a
throwError m f = Error m . (:[]) . LText.unpack . format f

mkMeta :: String -> Meta
mkMeta n = Meta n 0 0

-- | Create an 'Object' from a list of name/value 'Pair's.
-- See 'Aeson''s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs = (\(Object o) -> o) . object
