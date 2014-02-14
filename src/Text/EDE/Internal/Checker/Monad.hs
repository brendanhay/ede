-- Module      : Text.EDE.Internal.Checker.Monad
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Monad where

import           Control.Monad
import           Text.EDE.Internal.Checker.Env (Env)
import qualified Text.EDE.Internal.Checker.Env as Env
import           Text.EDE.Internal.Checker.Sub (Sub)
import qualified Text.EDE.Internal.Checker.Sub as Sub

data Check s a = Check (s -> (s, Either String a))

data State s = State
    { _state :: !s
    , _env   :: !Env
    , _subs  :: !Sub
    }

type CheckM s = Check (State s)

instance Functor (Check s) where
    fmap = liftM

instance Monad (Check s) where
    return x      = Check $ \s -> (s, Right x)
    Check f >>= g = Check $ \s ->
        case f s of
            (s', Left  e) -> (s', Left e)
            (s', Right x) -> runCheck s' (g x)

runCheck :: s -> Check s a -> (s, Either String a)
runCheck s (Check f) = f s

evalCheck :: s -> Check s a -> Either String a
evalCheck s = snd . runCheck s

throw :: String -> Check s a
throw e = Check $ \s -> (s, Left e)

get :: Check s s
get = Check $ \s -> (s, Right s)

put :: s -> Check s ()
put s = Check $ \_ -> (s, Right ())

with :: (Env -> Sub -> (a, Env, Sub)) -> CheckM s a
with f = do
    s          <- get
    let (rs, x, y) = f (_env s) (_subs s)
    put $! s { _env = x, _subs = y }
    return rs
