-- Module      : Text.EDE.Internal.Checker.Substitution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Substitution where

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as Set
import           Data.List               (nub, intersect, union)
import           Data.Maybe
import           Data.Monoid
import           Text.EDE.Internal.Types

newtype Subst = Subst { subs :: [(TVar, Type)] }

instance Monoid Subst where
    mempty      = Subst []
    mappend a b = Subst $ [(u, apply a t) | (u, t) <- subs b] ++ subs a

singleton :: TVar -> Type -> Subst
singleton u t = Subst [(u, t)]

merge :: Monad m => Subst -> Subst -> m Subst
merge (Subst s1) (Subst s2)
    | agree `all` intersection = return $ Subst (s1 ++ s2)
    | otherwise                = fail "merge fails"
  where
    intersection = map fst s1 `intersect` map fst s2
    agree v      = apply (Subst s1) (TVar v) == apply (Subst s2) (TVar v)

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [TVar]

instance Types Type where
    apply (Subst s) (TVar u) = fromMaybe (TVar u) $ lookup u s
    apply s (TApp l r)       = apply s l `TApp` apply s r
    apply s t                = t

    tv (TVar u)   = [u]
    tv (TApp l r) = tv l `union` tv r
    tv t          = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv      = nub . concatMap tv

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tv (IsIn i t) = tv t

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall ks qt)      = tv qt

instance Types Assump where
    apply s (i :>: sc) = i :>: apply s sc
    tv (i :>: sc)      = tv sc
