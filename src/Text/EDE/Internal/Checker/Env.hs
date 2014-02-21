{-# LANGUAGE GeneralizedNewtypeDeriving            #-}

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
    , singleton
    , lookup
    , insert
    , extend
    , remove
    , generalise
    ) where

import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Map
import qualified Data.HashSet                    as Set
import           Data.Monoid
import           Prelude                         hiding (lookup)
import           Text.EDE.Internal.Checker.Subst (Substitute(..))
import           Text.EDE.Internal.Types

-- Type environments, called Γ in the text, are mappings from term variables
-- to their respective type schemes.
newtype Env = Env { typeMap :: HashMap Id Scheme }
    deriving (Monoid)

instance Substitute Env where
    ftv       = ftv . Map.elems . typeMap
    apply t s = Env $ Map.map (`apply` s) (typeMap t)

singleton :: Id -> Scheme -> Env
singleton k = Env . Map.singleton k

lookup :: Id -> Env -> Maybe Scheme
lookup k = Map.lookup k . typeMap

extend :: Id -> Type -> Env -> Env
extend k t = mappend (singleton k (Scheme [] t))

insert :: Id -> Scheme -> Env -> Env
insert k s = Env . Map.insert k s . typeMap

-- We define several functions on type environments. The operation Γ\x removes the binding for
-- x from Γ and is called remove.
remove :: Id -> Env -> Env
remove k = Env . Map.delete k . typeMap

-- The function generalize abstracts a type over all type variables which are
-- free in the type but not free in the given type environment.
generalise :: Type -> Env -> Scheme
generalise t env = Scheme (Set.toList $ ftv t `Set.difference` ftv env) t
