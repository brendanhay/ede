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
import           Data.IORef
import           Data.List                       (nub, (\\))
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Types

data Expected a = Infer (IORef a) | Check a

typecheck :: Exp a -> Check Sigma
typecheck e = do { ty <- inferSigma e
                 ; zonkType ty }

-- tcRho, and its variants

-- | Invariant: the Rho is always in weak-prenex form
checkRho :: Exp a -> Rho -> Check ()
checkRho expr ty = tcRho expr (Check ty)

inferRho :: Exp a -> Check Rho
inferRho expr = do
    ref <- newTcRef (error "inferRho: empty result")
    tcRho expr (Infer ref)
    readTcRef ref

-- | Invariant: if the second argument is (Check rho),
-- then rho is in weak-prenex form
tcRho :: Exp a -> Expected Rho -> Check ()
tcRho (ELit _ _) exp_ty
  = instSigma intType exp_ty

tcRho (EVar _ v) exp_ty
  = do { v_sigma <- lookupVar v
       ; instSigma v_sigma exp_ty }

tcRho (EApp _ fun arg) exp_ty
  = do { fun_ty <- inferRho fun
       ; (arg_ty, res_ty) <- unifyFun fun_ty
       ; checkSigma arg arg_ty
       ; instSigma res_ty exp_ty }

tcRho (ELam _ var body) (Check exp_ty)
  = do { (var_ty, body_ty) <- unifyFun exp_ty
       ; extendVarEnv var var_ty (checkRho body body_ty) }

tcRho (ELam _ var body) (Infer ref)
  = do { var_ty  <- newTyVarTy
       ; body_ty <- extendVarEnv var var_ty (inferRho body)
       ; writeTcRef ref (var_ty --> body_ty) }

tcRho (ELet _ var rhs body) exp_ty
  = do { var_ty <- inferSigma rhs
       ; extendVarEnv var var_ty (tcRho body exp_ty) }

-- tcRho (Ann body ann_ty) exp_ty
--    = do { checkSigma body ann_ty
--         ; instSigma ann_ty exp_ty }


-- inferSigma and checkSigma
inferSigma :: Exp a -> Check Sigma
inferSigma e
   = do { exp_ty <- inferRho e
        ; env_tys <- getEnvTypes
        ; env_tvs <- getMetaTyVars env_tys
        ; res_tvs <- getMetaTyVars [exp_ty]
        ; let forall_tvs = res_tvs \\ env_tvs
        ; quantify forall_tvs exp_ty }

checkSigma :: Exp a -> Sigma -> Check ()
checkSigma expr sigma
  = do { (skol_tvs, rho) <- skolemise sigma
       ; checkRho expr rho
       ; env_tys <- getEnvTypes
       ; esc_tvs <- getFreeTyVars (sigma : env_tys)
       ; let bad_tvs = filter (`elem` esc_tvs) skol_tvs
       ; check (null bad_tvs)
               "Type not polymorphic enough" }

-- Subsumption checking

-- | Invariant: if the second argument is (Check rho),
-- then rho is in weak-prenex form
instSigma :: Sigma -> Expected Rho -> Check ()
instSigma t1 (Check t2) = unify t1 t2
instSigma t1 (Infer r)  = do { t1' <- instantiate t1
                             ; writeTcRef r t1' }
