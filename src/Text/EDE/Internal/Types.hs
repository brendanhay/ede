{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

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
import           Control.Lens
import           Control.Monad
import           Data.Aeson                   hiding (Result, Success, Error)
import           Data.Aeson.Types             (Pair)
import           Data.Foldable
import           Data.HashMap.Strict          (HashMap)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Monoid                  hiding ((<>))
import           Data.Scientific
import           Data.Semigroup
import           Data.Text                    (Text)
import           Data.Text.Buildable
import           Data.Text.Format             (Format, format)
import           Data.Text.Format.Params      (Params)
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import           Data.Traversable
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), Doc, vsep)
import           Text.Trifecta.Delta

-- | The result of running parsing or rendering steps.
data Result a
    = Success a
    | Failure Doc
      deriving (Show, Functor, Foldable, Traversable)

makePrisms ''Result

instance Applicative Result where
    pure = Success
    {-# INLINE pure #-}
    Success f <*> Success x  = Success (f x)
    Success _ <*> Failure e  = Failure e
    Failure e <*> Success _  = Failure e
    Failure e <*> Failure e' = Failure (vsep [e, e'])
    {-# INLINE (<*>) #-}

instance Alternative Result where
    Success x <|> Success _  = Success x
    Success x <|> Failure _  = Success x
    Failure _ <|> Success x  = Success x
    Failure e <|> Failure e' = Failure (vsep [e, e'])
    {-# INLINE (<|>) #-}
    empty = Failure mempty
    {-# INLINE empty #-}

instance Show a => Pretty (Result a) where
    pretty (Success x) = pretty (show x)
    pretty (Failure e) = pretty e

-- | Convert a 'Result' to an 'Either' with the 'Left' case holding a formatted
-- error message, and 'Right' being the successful result over which 'Result' is paramterised.
-- eitherResult :: Result a -> Either String a
-- eitherResult = result (Left . show) Right

-- -- | Perform a case analysis on a 'Result'.
-- result :: (Error -> b) -- ^ Function to apply to the 'Error' case.
--        -> (a -> b)     -- ^ Function to apply to the 'Success' case.
--        -> Result a     -- ^ The 'Result' to map over.
--        -> b
-- result _ g (Success x) = g x
-- result f _ (Error   e) = f e

-- -- | Convenience for returning a successful 'Result'.
-- success :: Monad m => a -> m (Result a)
-- success = return . Success

-- -- | Convenience for returning an error 'Result'.
-- failure :: Monad m => Doc -> m (Result a)
-- failure = return . Error

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Text -> Delta -> m (Result Template)

instance Applicative m => Semigroup (Resolver m) where
    (f <> g) x y = liftA2 (<|>) (f x y) (g x y)
    {-# INLINE (<>) #-}

-- | A parsed and compiled template.
data Template = Template !Text !Exp (HashMap Text Exp)
    deriving (Eq)

data Quoted
    = QLit !Value
    | QLam (Quoted -> Result Quoted)

instance Show Quoted where
    show (QLit v) = show v
    show _        = "<function>"

instance Eq Quoted where
    QLit a == QLit b = a == b
    _      == _      = False

-- data Type a where
--     TNil  :: Type ()
--     TText :: Type LText.Text
--     TBool :: Type Bool
--     TNum  :: Type Scientific
--     TBld  :: Type Builder
--     TMap  :: Type Object
--     TList :: Type Array
--     TFun  :: Type Quoted

-- deriving instance Show (Type a)

-- typeof :: Value -> String
-- typeof = \case
--     Null     -> show TNil
--     Bool   _ -> show TBool
--     Number _ -> show TNum
--     Object _ -> show TMap
--     Array  _ -> show TList
--     String _ -> show TText

-- data TExp = forall a. Eq a => a ::: Type a

-- instance Show TExp where
--     show (_ ::: t) = show t

newtype Id = Id Text
    deriving (Eq, Show)

-- instance HasDelta Id where
--     delta = idDelta

-- instance Buildable Id where
--     build i = mconcat
--         ["`", build (idName i), "` (line ", build l, ", column ", build c, ")"]
--       where
--         Delta _ l c = idDelta i
--     {-# INLINE build #-}

newtype Var = Var (NonEmpty Id)
    deriving (Eq, Show, Semigroup)

-- instance HasDelta Var where
--     delta (Var is) = delta (NonEmpty.head is)

-- FIXME: implement constructors, remove hardcoded bool keywords, etc.
data Lit
    = LBool !Bool
    | LNum  !Scientific
    | LText !Text
      deriving (Eq, Show)

data Pat
    = PWild
    | PVar !Var
    | PLit !Lit
      deriving (Eq, Show)

type Alt = (Pat, Exp)

data Exp
    = ELit  !Delta !Lit
    | EVar  !Delta !Var
    | EFun  !Delta !Id
    | EApp  !Delta !Exp  !Exp
    | ELet  !Delta !Id   !Exp  !Exp
    | ECase !Delta !Exp  [Alt]
    | ELoop !Delta !Id   !Var  !Exp (Maybe Exp)
    | EIncl !Delta !Text (Maybe Exp)
      deriving (Eq, Show)

instance HasDelta Exp where
    delta = \case
        ELit  d _       -> d
        EVar  d _       -> d
        EFun  d _       -> d
        EApp  d _ _     -> d
        ELet  d _ _ _   -> d
        ECase d _ _     -> d
        ELoop d _ _ _ _ -> d
        EIncl d _ _     -> d

-- throwError :: Params ps => ([String] -> Error) -> Format -> ps -> Result a
-- throwError f fmt = Error . f . (:[]) . LText.unpack . format fmt

-- | Create an 'Object' from a list of name/value 'Pair's.
-- See 'Aeson''s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs = (\(Object o) -> o) . object
