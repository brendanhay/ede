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

import           Control.Applicative
import           Control.Monad
import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as Map
import           Data.Monoid
import           Text.EDE.Internal.Checker.Class        (ClassEnv)
import           Text.EDE.Internal.Checker.Substitution
import           Text.EDE.Internal.Checker.Unification
import           Text.EDE.Internal.Types

data State = State
    { classes :: ClassEnv
    , globals :: HashMap Id Scheme
    , locals  :: HashMap Id Scheme
    , substs  :: Subst
    , supply  :: [Kind -> TVar]
    }

newtype Check a = Check { runCheck :: State -> (State, Either String a) }

instance Functor Check where
    fmap = liftM

instance Applicative Check where
    (<*>) = ap
    pure  = return

instance Monad Check where
    return !x = Check $ \s -> (s, Right x)
    fail   !e = Check $ \s -> (s, Left e)

    (>>=) !m !k = Check $ \s ->
        case runCheck m s of
            (s', Left  e) -> (s', Left e)
            (s', Right x) -> runCheck (k x) s'

evalCheck :: ClassEnv -> [Assump] -> Check a -> Either String a
evalCheck ce as c = snd . runCheck c $ State
    { classes = ce
    , globals = Map.fromList [(i, s) | i :>: s <- as]
    , locals  = mempty
    , substs  = mempty
    , supply  = map (\i k -> TV [i] k) (cycle ['a'..'z'])
    }

find :: Id -> Check (Maybe Scheme)
find i = Map.lookup i <$> gets locals

-- | Extend the local environment with new assumptions.
extend :: [Assump] -> Check a -> Check a
extend as c = Check $ \s@State{..} ->
    (s, evalCheck classes [i :>: s | (i, s) <- Map.toList globals] c)

-- | Introduce a global variable, or refine an existing one.
global :: Id -> Check Scheme
global i = do
    gs <- gets globals
    -- FIXME: compare supplied scheme with existing
    -- unify
    case Map.lookup i gs of
        Just x -> return x
        Nothing           -> do
             v <- local Star
             let q = (Forall [Star] $ [] :=> v)
             modify $ \s -> s { globals = Map.insert i q gs }
             return q

-- | Introduce a unique local variable.
local :: Kind -> Check Type
local k = do
    f : fs <- gets supply
    modify $ \s -> s { supply = fs }
    return $ TVar (f k)

instantiate :: Scheme -> Check (Qual Type)
instantiate (Forall ks qt) = (`inst` qt) <$> mapM local ks

unify :: Type -> Type -> Check ()
unify t1 t2 = do
    s' <- gets substs
    u  <- mgu (apply s' t1) (apply s' t2)
    modify $ \s -> s { substs = u <> s' }

gets :: (State -> a) -> Check a
gets = (`fmap` get)

get :: Check State
get = Check $ \s -> (s, Right s)

put :: State -> Check ()
put s = modify (const s)

modify :: (State -> State) -> Check ()
modify f = Check $ \s -> (f s, Right ())
