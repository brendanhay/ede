{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections            #-}

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

import           Control.Applicative
import           Control.Monad
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid
import           Text.EDE.Internal.Types

data State = State
    { stateUniq    :: Int
    , stateUnbound :: HashMap Id Type
    } deriving (Show)

newtype Check a = Check { runCheck :: State -> (State, Either String a) }

instance Functor Check where
    fmap = liftM

instance Applicative Check where
    (<*>) = ap
    pure  = return

instance Monad Check where
    return !x   = Check $ \s -> (s, Right x)
    (>>=) !m !k = Check $ \s ->
        case runCheck m s of
            (s', Left  e) -> (s', Left e)
            (s', Right x) -> runCheck (k x) s'

evalCheck :: Check a -> Either String a
evalCheck c = snd . runCheck c $ State (fromEnum 'a') mempty

throw :: String -> Check a
throw e = Check $ \s -> (s, Left e)

next :: Check Type
next = Check $ \s@State{..} ->
    (s { stateUniq = stateUniq + 1 }, Right $ TVar [toEnum stateUniq])

unbound :: Id -> Check Type
unbound k = do
    s <- get
    case Map.lookup k $ stateUnbound s of
        Just t  -> return t
        Nothing -> do
            t  <- next
            s' <- get
            put s' { stateUnbound = Map.insert k t $ stateUnbound s' }
            return t

get :: Check State
get = Check $ \s -> (s, Right s)

put :: State -> Check ()
put s = modify (const s)

modify :: (State -> State) -> Check ()
modify f = Check $ \s -> (f s, Right ())
