-- Module      : Text.EDE.Internal.Checker.Unification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Unification where

import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as Map
import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as Set
import           Data.Maybe
import           Data.Monoid
import           Text.EDE.Internal.Checker.Substitution
import           Text.EDE.Internal.Types

-- Unification

mgu :: Monad m => Type -> Type -> m Subst
mgu (TApp l r) (TApp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 <> s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return mempty
mgu t1 t2 = fail $ "types do not unify: " ++ show (t1, t2)

varBind :: Monad m => TVar -> Type -> m Subst
varBind u t
    | t == TVar u = return mempty
    | u `elem` tv t = fail "occurs check fails"
    | kind u /= kind t = fail $ "kinds do not match: " ++ show (u, t)
    | otherwise = return (singleton u t)

match :: Monad m => Type -> Type -> m Subst
match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
match (TVar u)   t
    | kind u == kind t = return (singleton u t)
match (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return mempty
match t1 t2 = fail "types do not match"

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred   = lift mgu
matchPred = lift match

lift m (IsIn i t) (IsIn i' t')
    | i == i' = m t t'
    | otherwise = fail "classes differ"
