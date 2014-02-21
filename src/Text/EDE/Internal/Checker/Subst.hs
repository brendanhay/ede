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

class Substitute a where
    ftv   :: a -> HashSet String
    apply :: a -> Subst -> a

instance Substitute Type where
    ftv (TVar n)     = Set.singleton n
    ftv (TCon _)     = Set.empty
    ftv (TFun t1 t2) = ftv t1 <> ftv t2

    apply (TVar n)     s = fromMaybe (TVar n) (lookup n s)
    apply (TFun t1 t2) s = TFun (apply t1 s) (apply t2 s)
    apply t            _ = t

-- It will occasionally be useful to extend the Substitute methods to lists.
instance Substitute a => Substitute [a] where
    ftv        = Set.unions . map ftv
    apply xs s = map (`apply` s) xs

instance Substitute Scheme where
    ftv   (Scheme vs t)   = ftv t `Set.difference` Set.fromList vs
    apply (Scheme vs t) s = Scheme vs $ apply t (foldr delete s vs)

-- Finite mappings from type variables to types.
newtype Subst = Subst { subMap :: HashMap String Type }
    deriving (Show)

instance Monoid Subst where
    mempty      = Subst Map.empty
    mappend a b = Subst $ Map.map (`apply` a) (subMap b) `Map.union` (subMap a)

singleton :: String -> Type -> Subst
singleton k = Subst . Map.singleton k

fromList :: [(String, Type)] -> Subst
fromList = Subst . Map.fromList

lookup :: String -> Subst -> Maybe Type
lookup k = Map.lookup k . subMap

delete :: String -> Subst -> Subst
delete k = Subst . Map.delete k . subMap
