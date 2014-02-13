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

import Control.Monad

data Check s e a = Check (s -> (s, Either e a))

instance Functor (Check s e) where
    fmap = liftM

instance Monad (Check s err) where
    return x      = Check $ \s -> (s, Right x)
    Check f >>= g = Check $ \s ->
        case f s of
            (s', Left  e) -> (s', Left e)
            (s', Right x)  -> runCheck s' (g x)

runCheck :: s -> Check s e a -> (s, Either e a)
runCheck s (Check f) = f s

evalCheck :: s -> Check s e a -> Either e a
evalCheck s m = snd $ runCheck s m

throw :: e -> Check s e a
throw e = Check $ \s -> (s, Left e)

get :: Check s e s
get = Check $ \s -> (s, Right s)

put :: s -> Check s e ()
put s = Check $ \_ -> (s, Right ())
