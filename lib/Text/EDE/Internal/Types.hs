{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
module Text.EDE.Internal.Types where

import Control.Applicative (Alternative (empty, (<|>)))
import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree (Cofree)
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Object, Pair, Value (..))
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Bifunctor as Bifunctor
import Data.Type.Coercion (coerceWith, sym)
#endif
import qualified Data.Functor.Classes as Functor.Classes
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Doc, Pretty (..))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
#else
import Data.Text.Prettyprint.Doc (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP
#endif
import Text.Trifecta.Delta (Delta, HasDelta)
import qualified Text.Trifecta.Delta as Trifecta.Delta

type AnsiDoc = Doc PP.AnsiStyle

class AnsiPretty a where
  apretty :: a -> AnsiDoc

-- | Convenience wrapper for Pretty instances.
newtype PP a = PP {unPP :: a}

pp :: AnsiPretty (PP a) => a -> AnsiDoc
pp = apretty . PP

(</>) :: Doc ann -> Doc ann -> Doc ann
x </> y = x <> PP.softline <> y

bold :: AnsiDoc -> AnsiDoc
bold = PP.annotate PP.bold

red :: AnsiDoc -> AnsiDoc
red = PP.annotate (PP.color PP.Red)

instance AnsiPretty (PP Text) where
  apretty = pretty . Text.unpack . unPP

instance AnsiPretty (PP Value) where
  apretty (PP v) =
    case v of
      Null -> "Null"
      Bool _ -> "Bool"
      Number _ -> "Scientific"
      Object _ -> "Object"
      Array _ -> "Array"
      String _ -> "String"

-- | The result of running parsing or rendering steps.
data Result a
  = Success a
  | Failure AnsiDoc
  deriving (Show, Functor, Foldable, Traversable)

$(Lens.makePrisms ''Result)

instance Monad Result where
  return = Success
  {-# INLINE return #-}

  Success x >>= k = k x
  Failure e >>= _ = Failure e
  {-# INLINE (>>=) #-}

instance Applicative Result where
  pure = return
  {-# INLINE pure #-}

  Success f <*> Success x = Success (f x)
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e <*> Failure e' = Failure (PP.vsep [e, e'])
  {-# INLINE (<*>) #-}

instance Alternative Result where
  Success x <|> Success _ = Success x
  Success x <|> Failure _ = Success x
  Failure _ <|> Success x = Success x
  Failure e <|> Failure e' = Failure (PP.vsep [e, e'])
  {-# INLINE (<|>) #-}

  empty = Failure mempty
  {-# INLINE empty #-}

instance Show a => AnsiPretty (Result a) where
  apretty (Success x) = pretty (show x)
  apretty (Failure e) = e

-- | Convert a 'Result' to an 'Either' with the 'Left' case holding a
-- formatted error message, and 'Right' being the successful result over
-- which 'Result' is paramterised.
eitherResult :: Result a -> Either String a
eitherResult = result (Left . show) Right

-- | Perform a case analysis on a 'Result'.
result ::
  -- | Function to apply to the 'Failure' case.
  (AnsiDoc -> b) ->
  -- | Function to apply to the 'Success' case.
  (a -> b) ->
  -- | The 'Result' to map over.
  Result a ->
  b
result _ g (Success x) = g x
result f _ (Failure e) = f e

-- | Convenience for returning a successful 'Result'.
success :: Monad m => a -> m (Result a)
success = return . Success

-- | Convenience for returning an error 'Result'.
failure :: Monad m => AnsiDoc -> m (Result a)
failure = return . Failure

type Delim = (String, String)

data Syntax = Syntax
  { _delimPragma :: !Delim,
    _delimInline :: !Delim,
    _delimComment :: !Delim,
    _delimBlock :: !Delim
  }

$(Lens.makeClassy ''Syntax)

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Syntax -> Id -> Delta -> m (Result Template)

-- instance Applicative m => Semigroup (Resolver m) where
--   (f <> g) o k d = liftA2 (<|>) (f o k d) (g o k d) -- Haha!
--   {-# INLINE (<>) #-}

-- | A parsed and compiled template.
data Template = Template
  { _tmplName :: !Text,
    _tmplExp :: !(Exp Delta),
    _tmplIncl :: HashMap Id (Exp Delta)
  }
  deriving (Eq)

type Id = Text

newtype Var = Var (NonEmpty Id)
  deriving (Eq)

instance AnsiPretty Var where
  apretty (Var is) =
    PP.hcat
      . PP.punctuate "."
      . map (PP.annotate PP.bold . pp)
      . reverse
      $ NonEmpty.toList is

instance Show Var where
  show = show . apretty

data Collection where
  Col :: Foldable f => Int -> f (Maybe Text, Value) -> Collection

data Pat
  = PWild
  | PVar !Var
  | PLit !Value
  deriving (Eq, Show)

type Alt a = (Pat, a)

data ExpF a
  = ELit !Value
  | EVar !Var
  | EFun !Id
  | EApp !a !a
  | ELet !Id !a !a
  | ECase !a [Alt a]
  | ELoop !Id !a !a
  | EIncl !Text
  deriving (Eq, Show, Functor)

instance Functor.Classes.Eq1 ExpF where
  liftEq _ (ELit a) (ELit b) = a == b
  liftEq _ (EVar a) (EVar b) = a == b
  liftEq _ (EFun a) (EFun b) = a == b
  liftEq c (EApp a1 a2) (EApp b1 b2) = a1 `c` b1 && a2 `c` b2
  liftEq c (ELet a0 a1 a2) (ELet b0 b1 b2) = a0 == b0 && a1 `c` b1 && a2 `c` b2
  liftEq c (ECase a as) (ECase b bs) = a `c` b && (List.all (uncurry altEq) $ zip as bs)
    where
      altEq (pA, a') (pB, b') = pA == pB && a' `c` b'
  liftEq c (ELoop a0 a1 a2) (ELoop b0 b1 b2) = a0 == b0 && a1 `c` b1 && a2 `c` b2
  liftEq _ (EIncl a) (EIncl b) = a == b
  liftEq _ _ _ = False

type Exp = Cofree ExpF

instance HasDelta (Exp Delta) where
  delta = Comonad.extract

-- | Unwrap a 'Value' to an 'Object' safely.
--
-- See Aeson\'s documentation for more details.
fromValue :: Value -> Maybe Object
fromValue (Object o) = Just o
fromValue _ = Nothing

-- | Create an 'Object' from a list of name/value 'Pair's.
--
-- See Aeson\'s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs xs =
  case Aeson.object xs of
    Object o -> o
    _other -> mempty

#if MIN_VERSION_aeson(2,0,0)
fromKey :: Key -> Text
fromKey =
  case Key.coercionToText of
    Just textCoercion -> coerceWith textCoercion
    Nothing -> Key.toText
{-# INLINEABLE fromKey #-}

toKey :: Text -> Key
toKey =
  case Key.coercionToText of
    Just textCoercion -> coerceWith (sym textCoercion)
    Nothing -> Key.fromText
{-# INLINEABLE toKey #-}

fromKeyMap :: KeyMap Value -> HashMap Id Value
fromKeyMap =
  case KeyMap.coercionToHashMap of
    Just hashMapCoercion ->
      HashMap.mapKeys fromKey . coerceWith (sym hashMapCoercion)
    Nothing ->
      HashMap.fromList . map (Bifunctor.first fromKey) . KeyMap.toList
{-# INLINEABLE fromKeyMap #-}

toKeyMap :: HashMap Id Value -> KeyMap Value
toKeyMap =
  case KeyMap.coercionToHashMap of
    Just hashMapCoercion ->
      coerceWith hashMapCoercion . HashMap.mapKeys toKey
    Nothing ->
      KeyMap.fromList . map (Bifunctor.first toKey) . HashMap.toList
{-# INLINEABLE toKeyMap #-}
#endif
