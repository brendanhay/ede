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
    , singleton
    , depth
    , extend
    , lookup
    ) where

import           Control.Monad
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import qualified Prelude
import           Prelude                 hiding (lookup)
import           Text.EDE.Internal.Types

data Env = Env
    { envMap   :: HashMap String Type
    , envStack :: [Type]
    , envDepth :: !Int
    , envPrim  :: String -> Maybe Type
    }

empty :: Env
empty = Env Map.empty [] 0 (\_ -> Nothing)

singleton :: Bind -> Env
singleton = (`extend` empty)

depth :: Env -> Int
depth = envDepth

extend :: Bind -> Env -> Env
extend (BNone   _) e = e
extend (BName n t) e = e { envMap = Map.insert n t (envMap e) }
extend (BAnon   t) e = e
    { envStack = t : envStack e
    , envDepth = envDepth e + 1
    }

lookup :: Bound -> Env -> Maybe Type
lookup (UName  n) e = Map.lookup n (envMap e) `mplus` envPrim e n
lookup (UIndex i) e = Prelude.lookup i $ zip [0..] (envStack e)
