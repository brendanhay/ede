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
import Data.Traversable
import Prelude.Extras

data Exp a
    = EVar  a
    | EApp  (Exp a) (Exp a)
    | ELam  !Int    (Pat Exp a)       (Scope Int Exp a)
    | ELet  !Int    [Scope Int Exp a] (Scope Int Exp a)
    | ECase (Exp a) [Alt Exp a]
      deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative Exp where
    pure  = EVar
    (<*>) = ap

instance Monad Exp where
    EVar  a      >>= f = f a
    EApp  x y    >>= f = EApp (x >>= f) (y >>= f)
    ELam  n p e  >>= f = ELam n (p >>>= f) (e >>>= f)
    ELet  n bs e >>= f = ELet n (map (>>>= f) bs) (e >>>= f)
    ECase e as   >>= f = ECase (e >>= f) (map (>>>= f) as)

    return = EVar

instance Eq1   Exp
instance Ord1  Exp
instance Show1 Exp
instance Read1 Exp

data Pat f a
    = PVar
    | PWild
    | PAs   (Pat f a)
    | PCon  String [Pat f a]
    | PView (Scope Int f a) (Pat f a)
      deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bound Pat where
    PVar       >>>= _ = PVar
    PWild      >>>= _ = PWild
    PAs   p    >>>= f = PAs (p >>>= f)
    PCon  g ps >>>= f = PCon g (map (>>>= f) ps)
    PView e p  >>>= f = PView (e >>>= f) (p >>>= f)

data Alt f a = Alt !Int (Pat f a) (Scope Int f a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bound Alt where
    Alt n p b >>>= f = Alt n (p >>>= f) (b >>>= f)
