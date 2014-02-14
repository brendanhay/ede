{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Checker
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable                   (foldrM)
import qualified Data.HashMap.Strict             as Map
import qualified Data.HashSet                    as Set
import           Text.EDE.Internal.Checker.Env   (Env)
import qualified Text.EDE.Internal.Checker.Env   as Env
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Checker.Sub   (Sub)
import qualified Text.EDE.Internal.Checker.Sub   as Sub
import           Text.EDE.Internal.Types

--typeOf :: Exp a -> Either String (Exp (Ann a))
typeOf x = go
  where
    go = evalCheck (State 0 Env.empty Sub.empty) $ do
        a  <- variable
        x' <- principal x a
        subs <- _subs <$> get
        return (x', Sub.substitute a subs)

test :: Exp String
test = ELet m "compose"
    (ELam m "f" (ELit m (LText "dsaas")))
    (EVar m "compose")
  where
    m = "meta"

-- rename :: Type -> Either e Type
rename = go
  where
    go (TVar n)    = TVar . (:[]) <$> name n
    go (TLam x y)  = liftM2 TLam (go x) (go y)
    go (TCon n ts) = TCon n <$> mapM go ts

    name n = do
      (m, i) <- get
      maybe (missing n i m) return (Map.lookup n m)

    missing n i m = do
        let j = toEnum (fromEnum i + 1)
        put (Map.insert n i m, j)
        return j

principal :: Exp a
          -> Type
          -> Check (State Int) (Exp (Ann a))
principal x' t = f x'
  where
    f (ELit a l) = do
        let t' = case l of
                     LText _ -> TCon "string"  []
                     LBool _ -> TCon "boolean" []
                     LNum  _ -> TCon "number"  []

        unify t' t

        return $! ELit (Ann t' a) l

    f (EVar a n) = do
        env  <- _env  <$> get
        subs <- _subs <$> get

        t'   <- maybe (throw $ "Unable to find varible name: " ++ bindName n)
                return
                (Env.lookup n env)

        unify (Sub.substitute t' subs) t

        return $! EVar (Ann t' a) n

    f (ELam a b bdy) = do
        x <- variable
        y <- variable

        unify t (TLam x y)

        with $ \env subs -> ((), Env.insert b x env, subs)

        bdye <- principal bdy y

        return $! ELam (Ann y a) b bdye

    f (EApp a e1 e2) = do
        x   <- variable
        e1' <- principal e1 (TLam x t)
        e2' <- principal e2 x

        return $! EApp (Ann x a) e1' e2'

    f (ELet a n inv bdy) = do
        x    <- variable
        inv' <- principal inv x

        subs <- _subs <$> get

        let bt = Sub.substitute x subs

        with $ \env s -> ((), Env.extend n bt env, s)

        bdy' <- principal bdy bt

        return $! ELet (Ann bt a) n inv' bdy'

unify :: Type -> Type -> Check (State s) ()
unify t1 t2 = do
    s <- _subs <$> get
    f (Sub.substitute t1 s) (Sub.substitute t2 s)
  where
    f (TVar nx) (TVar ny) | nx == ny =
        return ()

    f (TVar n) _ | vs <- Env.typeVars t2, not (Set.member n vs) =
        with $ \env sub -> ((), env, Sub.extend n t2 sub)

    f _ (TVar _) =
        unify t2 t1

    f (TLam x1 y1) (TLam x2 y2) =
        unify y1 y2 >> unify x1 x2

    f (TCon n1 ts1) (TCon n2 ts2) | n1 == n2 =
        mapM_ (uncurry unify) (zip ts1 ts2)

    f a b =
        throw $ "Unable to unify:" ++ show (a, b)

variable :: Check (State Int) Type
variable = do
    s <- get
    put $! s { _state = _state s + 1 }
    return $! TVar [toEnum $ _state s]
