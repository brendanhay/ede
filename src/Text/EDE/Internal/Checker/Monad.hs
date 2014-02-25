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
import Data.Monoid
import Text.EDE.Internal.Checker.Substitution
import Text.EDE.Internal.Checker.Unification
import Text.EDE.Internal.Types

data State = State
    { substitutions :: Subst
    , supply        :: [Kind -> TVar]
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

evalCheck :: Check a -> Either String a
evalCheck c = snd . runCheck c $
    State mempty (map (\i k -> TV [i] k) $ cycle ['a'..'z'])

unify :: Type -> Type -> Check ()
unify t1 t2 = do
    s' <- gets substitutions
    u  <- mgu (apply s' t1) (apply s' t2)
    modify $ \s -> s { substitutions = u <> s' }

freshTVar :: Kind -> Check Type
freshTVar k = do
    f:fs <- gets supply
    modify $ \s -> s { supply = fs }
    return $ TVar (f k)

freshInst :: Scheme -> Check (Qual Type)
freshInst (Forall ks qt) = (`inst` qt) <$> mapM freshTVar ks

gets :: (State -> a) -> Check a
gets = (`fmap` get)

get :: Check State
get = Check $ \s -> (s, Right s)

put :: State -> Check ()
put s = modify (const s)

modify :: (State -> State) -> Check ()
modify f = Check $ \s -> (f s, Right ())
