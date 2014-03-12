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
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Text           (Text)
import Data.Traversable
import Prelude.Extras

data Meta = Meta !String !Int !Int
    deriving (Eq, Show)

class Metadata a where
    meta :: a -> Meta

data Lit
    = LBool    !Bool
    | LInteger !Integer
    | LText    !Text
      deriving (Eq, Show)

data Exp a
    = EVar  a
    | ELit  !Lit
    | EApp  (Exp a) (Exp a)
    | ELam  !Int    (Pat Exp a)       (Scope Int Exp a)
    | ELet  !Int    [Scope Int Exp a] (Scope Int Exp a)
    | ECase (Exp a) [Alt Exp a]
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
    pure  = EVar
    (<*>) = ap

instance Monad Exp where
    EVar  a      >>= f = f a
    ELit  l      >>= _ = ELit l
    EApp  x y    >>= f = EApp (x >>= f) (y >>= f)
    ELam  n p e  >>= f = ELam n (p >>>= f) (e >>>= f)
    ELet  n bs e >>= f = ELet n (map (>>>= f) bs) (e >>>= f)
    ECase e as   >>= f = ECase (e >>= f) (map (>>>= f) as)

    return = EVar

instance Eq1   Exp
instance Show1 Exp

data Pat f a
    = PVar
    | PLit  Lit
    | PWild
    | PAs   (Pat f a)
    | PCon  String [Pat f a]
    | PView (Scope Int f a) (Pat f a)
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bound Pat where
    PVar       >>>= _ = PVar
    PLit  l    >>>= _ = PLit l
    PWild      >>>= _ = PWild
    PAs   p    >>>= f = PAs (p >>>= f)
    PCon  g ps >>>= f = PCon g (map (>>>= f) ps)
    PView e p  >>>= f = PView (e >>>= f) (p >>>= f)

data Alt f a = Alt !Int (Pat f a) (Scope Int f a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bound Alt where
    Alt n p b >>>= f = Alt n (p >>>= f) (b >>>= f)
