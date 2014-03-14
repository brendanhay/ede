{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

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

import Bound
import Bound.Name
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Text           (Text)
import Data.Traversable
import Prelude.Extras

data Meta = Meta !String !Int !Int
    deriving (Eq, Show)

class Metadata a where
    meta :: a -> Meta

type Id = Text

type Bind a = Scope (Name Id a) Exp

data Type a
    = TVar a
    | TApp (Type a) (Type a)
    | TBool
    | TNum
    | TText

data Lit
    = LBool !Bool
    | LNum  !Integer
    | LText !Text
      deriving (Eq, Show)

data Exp a
    = EVar  a
    | ELit  Lit
    | EApp  (Exp a) (Exp a)
    | ELam  (Bind () a)
    | ELet  [Bind Int a] (Bind Int a)
    | ECase (Exp a) [Alt Exp a]
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
    pure  = EVar
    (<*>) = ap

instance Monad Exp where
    EVar  a    >>= f = f a
    ELit  l    >>= _ = ELit  l
    EApp  x y  >>= f = EApp  (x >>= f) (y >>= f)
    ELam  e    >>= f = ELam  (e >>>= f)
    ELet  bs e >>= f = ELet  (map (>>>= f) bs) (e >>>= f)
    ECase e as >>= f = ECase (e >>= f) (map (>>>= f) as)

    return = EVar

instance Eq1   Exp
instance Show1 Exp

data Pat
    = PWild
    | PVar
    | PLit Lit
    | PAs  Pat
      deriving (Eq, Show)

data Path = PLeaf
    deriving (Eq, Show)

leafPath :: Endo Path -> Path
leafPath = flip appEndo PLeaf

paths :: Pat -> [Path]
paths = go mempty
  where
    go p PVar = [leafPath p]
    go _ _    = []

data Alt f a = Alt Pat (Scope Path f a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bound Alt where
    Alt p b >>>= f = Alt p (b >>>= f)

data Binder a = Binder [a] Pat
    deriving (Eq, Show, Functor, Foldable, Traversable)

