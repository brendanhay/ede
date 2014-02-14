-- Module      : Text.EDE.Internal.Checker.Env
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Env
    ( Env
    , empty
    , insert
    , extend
    , lookup
    , typeVars
    ) where

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as Set
import qualified Prelude
import           Prelude                 hiding (lookup)
import           Text.EDE.Internal.Types

newtype Env = Env { envMap :: HashMap String (Type, HashSet String) }

empty :: Env
empty = Env Map.empty

insert :: Bind -> Type -> Env -> Env
insert (Bind b) t = Env . Map.insert b (t, Set.empty) . envMap

extend :: Bind -> Type -> Env -> Env
extend (Bind b) t (Env m) = Env $
    Map.insert b (t, typeVars t `Set.difference` set) m
  where
    set = Set.unions
        . map (\(t', s) -> typeVars t' `Set.difference` s)
        $ Map.elems m

lookup :: Bind -> Env -> Maybe Type
lookup (Bind b) = fmap fst . Map.lookup b . envMap

typeVars :: Type -> HashSet String
typeVars (TVar n)    = Set.singleton n
typeVars (TCon _ ts) = Set.unions $ map typeVars ts
typeVars (TLam x y)  = typeVars x `Set.union` typeVars y
