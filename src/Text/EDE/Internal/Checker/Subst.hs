{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

-- Module      : Text.EDE.Internal.Checker.Subst
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Subst where

import Text.EDE.Internal.Types

class Subst a where
    type SV a :: *
    subst  :: a -> SV a   -> a -> a
    substs :: [(a, SV a)] -> a -> a

    substs = flip $ foldr (uncurry subst)

-- | Expression substitution: e' x e = [e'/x]e
instance Subst (Exp a) where
    type SV (Exp a) = Var
    subst e' x expr = case expr of
        ELit a l                -> ELit a l
        EVar a x'   | x' == x   -> e'
                    | otherwise -> EVar a x'
        EAbs a x' e | x' == x   -> EAbs a x' e
                    | otherwise -> EAbs a x' (subst e' x e)
        EApp a e1 e2            -> EApp a (subst e' x e1) (subst e' x e2)

-- | Type substitution: A α B = [A/α]B
instance Subst (Type a) where
    type SV (Type a) = TVar
    subst t' v typ = case typ of
        TCon c                   -> TCon c
        TVar v'      | v' == v   -> t'
                     | otherwise -> TVar v'
        TForall v' t | v' == v   -> TForall v' t
                     | otherwise -> TForall v' (subst t' v t)
        TExists v'   | v' == v   -> t'
                     | otherwise -> TExists v'
        TFun t1 t2               -> TFun (subst t' v t1) (subst t' v t2)
