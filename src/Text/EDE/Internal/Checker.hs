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

import Control.Monad (msum)
import Data.List     (nub, (\\), intersect, union, partition)

tiLit :: Lit -> TI ([Pred], Type)
tiLit (LInt  _) = do
    v <- newTVar Star
    return ([IsIn "Num" v], v)
tiLit (LChar _) = return ([], tChar)
tiLit (LStr  _) = return ([], tString)
tiLit (LBool _) = return ([], tBool)

tiExpr :: ClassEnv -> [Assump] -> Exp -> TI ([Pred], Type)
tiExpr ce as (Var i) = do
    sc         <- find i as
    (ps :=> t) <- freshInst sc
    return (ps, t)
tiExpr ce as (Const (i:>:sc)) = do
    (ps :=> t) <- freshInst sc
    return (ps, t)
tiExpr ce as (Lit l) = do
    (ps,t) <- tiLit l
    return (ps, t)
tiExpr ce as (Ap e f) = do
    (ps,te) <- tiExpr ce as e
    (qs,tf) <- tiExpr ce as f
    t       <- newTVar Star
    unify (tf `fn` t) te
    return (ps++qs, t)
tiExpr ce as (Let bg e) = do
    (ps, as') <- tiBindGroup ce as bg
    (qs, t)   <- tiExpr ce (as' ++ as) e
    return (ps ++ qs, t)


-----------------------------------------------------------------------------
-- TIMonad:	Type inference monad
-----------------------------------------------------------------------------

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
    return x = TI $ \s n -> (s,n,x)
    TI f >>= g = TI $ \s n ->
        case f s n of
            (s',m,x) -> let TI gx = g x in gx s' m

runTI :: TI a -> a
runTI (TI f) = x
  where
    (s,n,x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s,n,s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extSubst u

extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> (s'@@s, n, ()))

newTVar :: Kind -> TI Type
newTVar k = TI (\s n -> let v = TVar (enumId n) k in (s, n+1, TVar v))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
    ts <- mapM newTVar ks
    return (inst ts qt)

-----------------------------------------------------------------------------
-- TIMain:	Type Inference Algorithm
-----------------------------------------------------------------------------
-- Infer:	Basic definitions for type inference
-----------------------------------------------------------------------------
-- type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)
-----------------------------------------------------------------------------

type Ambiguity = (TVar, [Pred])

ambiguities :: ClassEnv -> [TVar] -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]

numClasses :: [Id]
numClasses = ["Num", "Integral", "Floating", "Fractional",
               "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
               "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
                                   ts = [ t | IsIn i t <- qs ],
                               all ((TVar v)==) ts,
                               any (`elem` numClasses) is,
                               all (`elem` stdClasses) is,
                               t' <- defaults ce,
                               all (entail ce []) [ IsIn i t' | i <- is ] ]

withDefaults :: Monad m
             => ([Ambiguity] -> [Type] -> a)
             -> ClassEnv
             -> [TVar]
             -> [Pred]
             -> m a
withDefaults f ce vs ps
    | any null tss = fail "cannot resolve ambiguity"
    | otherwise = return (f vps (map head tss))
      where vps = ambiguities ce vs ps
            tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [TVar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps ts -> concat (map snd vps))

defaultSubst :: Monad m => ClassEnv -> [TVar] -> [Pred] -> m Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)
