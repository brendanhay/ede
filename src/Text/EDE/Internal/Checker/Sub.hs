-- Module      : Text.EDE.Internal.Checker.Sub
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Sub
    ( Sub
    , empty
    , extend
    , lookup
    , substitute
    ) where

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Maybe
import           Prelude                 hiding (lookup)
import           Text.EDE.Internal.Types

newtype Sub = Sub { subMap :: HashMap String Type }

empty :: Sub
empty = Sub Map.empty

extend :: String -> Type -> Sub -> Sub
extend v t = Sub . Map.insert v t . subMap

lookup :: String -> Sub -> Type
lookup v = fromMaybe (TVar v) . Map.lookup v . subMap

substitute :: Type -> Sub -> Type
substitute (TLam x y)  s = TLam (substitute x s) (substitute y s)
substitute (TCon n ts) s = TCon n (map (`substitute` s) ts)
substitute t@(TVar n)  s =
    let t' = lookup n s
    in if t == t'
           then t'
           else substitute t' s
