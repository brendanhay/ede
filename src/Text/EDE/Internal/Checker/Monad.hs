{-# LANGUAGE BangPatterns #-}

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
import Text.EDE.Internal.Types

data State = State
    -- { varNames  :: [Var]
    -- , tvarNames :: [TVar]
    -- } deriving (Show)

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
evalCheck c = snd . runCheck c $ State
  --   { varNames  = map (Var . ('$':)) ns
  --   , tvarNames = map (TypeVar . ('\'':)) ns
  --   }
  -- where
  --   ns = [1..] >>= flip replicateM ['a'..'z']

throw :: String -> Check a
throw e = Check $ \s -> (s, Left e)

-- -- | Create a fresh variable
-- freshVar :: Check Var
-- freshVar = do
--     v:vs <- varNames <$> get
--     modify $ \s -> s { varNames = vs }
--     return v

-- -- | Create a fresh type variable
-- freshTVar :: Check TVar
-- freshTVar = do
--     v:vs <- tvarNames <$> get
--     modify $ \s -> s { tvarNames = vs }
--     return v

get :: Check State
get = Check $ \s -> (s, Right s)

put :: State -> Check ()
put s = modify (const s)

modify :: (State -> State) -> Check ()
modify f = Check $ \s -> (f s, Right ())
