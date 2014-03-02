{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.Hashable
import           Data.Monoid
import           Data.Text           (Text)

data Meta = Meta
    { metaName :: String
    , metaRow  :: Int
    , metaCol  :: Int
    } deriving (Eq, Show)

data Ann a = Ann
    { annType :: Polytype
    , annTail :: a
    } deriving (Show)

type Id = String

newtype Var = Var Id
    -- = Bound Id
    -- | Free  Id
      deriving (Eq, Show)

data Lit
    = LNum  Integer
    | LText Text
    | LBool Bool
      deriving (Eq, Show)

data Exp a
    = ELit a Lit
    | EVar a Var             -- ^ x
    | EAbs a Var     (Exp a) -- ^ \x. e
    | EApp a (Exp a) (Exp a) -- ^ e1 e2
      deriving (Eq, Show)

data TCon
    = TNum
    | TText
    | TBool
      deriving (Eq, Show)

newtype TVar = TypeVar Id deriving (Eq, Ord, Show, Hashable)

data TKind = Mono | Poly

-- | Types, indexed by their kind: Monotype or Polytype.
--   Only Polytypes can have foralls.
data Type :: TKind -> * where
    TCon    :: TCon   -> Type a
    TVar    :: TVar   -> Type a                 -- ^ alpha
    TExists :: TVar   -> Type a                 -- ^ alpha^
    TForall :: TVar   -> Type Poly -> Type Poly -- ^ forall alpha. A
    TFun    :: Type a -> Type a    -> Type a    -- ^ A -> B

deriving instance Show (Type a)
deriving instance Eq   (Type a)

type Polytype = Type Poly
type Monotype = Type Mono

-- | Is the type a Monotype?
monotype :: Type a -> Maybe Monotype
monotype typ = case typ of
    TCon c      -> Just $ TCon c
    TVar v      -> Just $ TVar v
    TForall _ _ -> Nothing
    TExists v   -> Just $ TExists v
    TFun t1 t2  -> TFun <$> monotype t1 <*> monotype t2

-- | Any type is a Polytype since Monotype is a subset of Polytype
polytype :: Type a -> Polytype
polytype typ = case typ of
    TCon c      -> TCon c
    TVar v      -> TVar v
    TForall v t -> TForall v t
    TExists v   -> TExists v
    TFun t1 t2  -> TFun (polytype t1) (polytype t2)

-- | The free type variables in a type
freeTVars :: Type a -> HashSet TVar
freeTVars typ = case typ of
    TCon _      -> mempty
    TVar v      -> Set.singleton v
    TForall v t -> Set.delete v (freeTVars t)
    TExists v   -> Set.singleton v
    TFun t1 t2  -> freeTVars t1 <> freeTVars t2

data Elem
    = CVar          Var  Polytype -- ^ x : A
    | CForall       TVar          -- ^ alpha
    | CExists       TVar          -- ^ alpha^
    | CExistsSolved TVar Monotype -- ^ alpha^ = tau
    | CMarker       TVar          -- ^ |> alpha^
      deriving (Eq, Show)

newtype Context = Context [Elem]
    deriving (Show)

instance Monoid Context where
    mempty = Context []
    mappend (Context a) (Context b) = Context (b ++ a)
