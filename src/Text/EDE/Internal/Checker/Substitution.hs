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

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as Set
import           Data.Maybe
import           Data.Monoid
import           Prelude                 hiding (lookup)
import           Text.EDE.Internal.Types

-- Substitutions

type Subst = [(TVar, Type)]

nullSubst :: Subst
nullSubst = []

(+->) :: TVar -> Type -> Subst
u +-> t = [(u, t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                   (map fst s1 `intersect` map fst s2)
