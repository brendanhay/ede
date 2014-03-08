{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE RecordWildCards            #-}

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
    } deriving (Eq)

instance Show Meta where
    show Meta{..} = metaName <> ":" <> show metaRow <> ":" <> show metaCol

data Ann a = Ann
    { annType :: Polytype
    , annTail :: a
    } deriving (Show)

type Id = Text

data Var
    = VBound Id
    | VFree  Id
      deriving (Eq, Show)

newtype Bind = Bind Id deriving (Eq, Show)

bind :: Bind -> Var
bind (Bind i) = VBound i

data Lit
    = LNum  Integer
    | LText Text
    | LBool Bool
      deriving (Eq, Show)

data Exp a
    = ELit a Lit
    | EVar a Var
    | EAbs a Bind    (Exp a)
    | EApp a (Exp a) (Exp a)
    | ELet a Bind    (Exp a) (Exp a)
      deriving (Eq, Show)

data Pat
    = PWildcard
    | PVar Id
    | PLit Lit
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

class Metadata a where
    meta :: a -> Meta

instance Metadata Meta where
    meta = id

instance Metadata a => Metadata (Ann a) where
    meta = meta . annTail

instance Metadata a => Metadata (Exp a) where
    meta = meta . ann
      where
        ann (ELit a _)     = a
        ann (EVar a _)     = a
        ann (EAbs a _ _)   = a
        ann (EApp a _ _)   = a
        ann (ELet a _ _ _) = a
