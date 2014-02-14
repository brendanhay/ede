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

typeOf :: Exp a -> Either String Type
typeOf x = go >>= rename
  where
    go = evalCheck 0 $ do
        a    <- variable
        subs <- principal x a Env.empty Sub.empty
        return $! Sub.substitute a subs

test :: Exp String
test = ELet m "compose"
    (ELam m "f"
        (ELam m "g"
            (ELam m "x"
                (EApp m (EVar m "g") (EApp m (EVar m "f") (EVar m "x"))))))
    (EVar m "compose")
  where
    m = "meta"

rename :: Type -> Either e Type
rename = evalCheck (Map.empty, 'a') . go
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
          -> Env
          -> Sub
          -> Check Int String Sub
principal x t env subs = go x
  where
    go (ELit _ l) = (\v -> unify v t subs) $
        case l of
            LText _ -> TCon "string"  []
            LBool _ -> TCon "boolean" []
            LNum  _ -> TCon "number"  []

    go (EVar _ n) =
        maybe (throw $ "Unable to find varible name: " ++ bindName n)
              (\(t', _) -> unify (Sub.substitute t' subs) t subs)
              (Env.lookup n env)

    go (ELam _ y bdy) = do
        a  <- variable
        b  <- variable
        s1 <- unify t (TLam a b) subs

        principal bdy b (Env.insert y a env) s1

    go (EApp _ e1 e2) = do
        a  <- variable
        s1 <- principal e1 (TLam a t) env subs

        principal e2 a env s1

    go (ELet _ n inv bdy) = do
        a  <- variable
        s1 <- principal inv a env subs

        let bt = Sub.substitute a s1

        principal bdy bt (Env.extend n bt env) s1

variable :: Check Int e Type
variable = do
    x <- get
    put (x + 1)
    return $ TVar [toEnum x]


unify :: Type -> Type -> Sub -> Check s String Sub
unify x y s = f (Sub.substitute x s) (Sub.substitute y s)
  where
    f (TVar nx) (TVar ny) | nx == ny =
        return s
    f (TVar n) _ | vs <- Env.typeVars y, not (Set.member n vs) =
        return (Sub.extend n y s)
    f _ (TVar _) =
        unify y x s
    f (TLam x1 y1) (TLam x2 y2) =
        unify x1 x2 =<< unify y1 y2 s
    f (TCon n1 ts1) (TCon n2 ts2) | n1 == n2 =
        foldrM (\(a, b) s' -> unify a b s') s (zip ts1 ts2)
    f a b =
        throw $ "Unable to unify:" ++ show (a, b)
