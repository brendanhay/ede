{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Applicative
import Control.Monad
import Data.HashMap.Strict   (HashMap)
import Data.Monoid
import Text.EDE.Internal.AST

type Env a = HashMap a Type

data State = State
    { tvars :: [Type]
    , env   :: Env Int
    }

newtype Check a = Check { runCheck :: State -> Either String (State, a) }

instance Functor (Check) where
    fmap = liftM

instance Applicative (Check) where
    pure  = return
    (<*>) = ap

instance Monad (Check) where
    return !x   = Check $ \s -> Right (s, x)
    (>>=) !m !k = Check $ \s ->
        case runCheck m s of
            Left  e       -> Left e
            Right (s', x) -> runCheck (k x) s'

evalCheck :: Check a -> Either String a
evalCheck m = snd <$> runCheck m (State (map TVar [0..]) mempty)

throw :: String -> Check a
throw !e = Check $ const (Left e)

freshTVar :: Check Type
freshTVar = Check $ \s@State{..} -> Right (s { tvars = tail tvars }, head tvars)

get :: Check (Env Int)
get = Check $ \s -> Right (s, env s)

put :: Env Int -> Check ()
put e = Check $ \s -> Right (s { env = e }, ())
