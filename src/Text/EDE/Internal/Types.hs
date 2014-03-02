{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Types where

import "mtl"     Control.Monad.State.Strict
import           Data.IORef
import qualified Data.IntMap                as M
import           Data.List                  (nub)

type Id = String

type TVarName = Int

data Type
    = TBool
    | TInt
    | TVar TVarName
    | TFun Type Type
      deriving (Show, Eq)

infixr 4 -->
(-->) :: Type -> Type -> Type
(-->) = TFun

-- Quantifier should be non-empty
data Scheme = Forall [TVarName] Type
    deriving (Show)

data Ann a = Ann
    { annType :: Type
    , annTail :: a
    } deriving (Show)

data Lit
    = LInt  Integer
    | LBool Bool
      deriving (Show)

data Exp a
    = ELit a Lit
    | EVar a Id
    | EAbs a Id          (Exp a)
    | EApp a (Exp a)     (Exp a)
    | ELet a (Id, Exp a) (Exp a)
      deriving (Show)

-- instance Functor Exp where
--     fmap f (EVar x i)        = EVar (f x) i
--     fmap f (EAbs x i e)      = EAbs (f x) i (fmap f e)
--     fmap f (EApp x a b)      = EApp (f x)   (fmap f a)    (fmap f b)
--     fmap f (ELet x (i, r) b) = ELet (f x)   (i, fmap f r) (fmap f b)

ann :: Exp a -> a
ann e = case e of
    ELit a _   -> a
    EVar a _   -> a
    EAbs a _ _ -> a
    EApp a _ _ -> a
    ELet a _ _ -> a

infixr 4 $$
($$) :: Exp a -> Exp a -> Exp a
a $$ b = EApp (ann a) a b

initial :: TEnv
initial =
    [ ("+", Forall [0] $ TInt --> TInt --> TInt)
    ]

-- Type Environment: associating *would-be* types with `free' term variables
type TEnv = [(Id, Scheme)]

env0 :: TEnv
env0 = []

lkup :: TEnv -> Id -> Scheme
lkup env x = maybe err id $ lookup x env
  where
    err = error $ "Unbound variable " ++ x

ext :: TEnv -> (Id, Scheme) -> TEnv
ext env xt = xt : env

-- Type Variable Environment: associating types with `free' type variables
data TVE = TVE Int (M.IntMap Type) deriving (Show)

-- TVE is the state of a monadic computation
type TVEM = State TVE

-- Allocate a fresh type variable (see the first component of TVE)
newtv :: TVEM Type
newtv = do
    TVE n s <- get
    put (TVE (succ n) s)
    return (TVar n)

tve0 :: TVE
tve0 = TVE 0 M.empty

tvlkup :: TVE -> TVarName -> Maybe Type
tvlkup (TVE _ s) v = M.lookup v s

tvext :: TVE -> (TVarName, Type) -> TVE
tvext (TVE c s) (tv,t) = TVE c $ M.insert tv t s

-- TVE domain predicate: check to see if a TVar is in the domain of TVE
tvdomainp :: TVE -> TVarName -> Bool
tvdomainp (TVE _ s) v = M.member v s

-- Give the list of all type variables that are allocated in TVE but
-- not bound there
tvfree :: TVE -> [TVarName]
tvfree (TVE c s) = filter (\v -> not (M.member v s)) [0..c-1]

-- Type variables are logic variables: hypothetical reasoning
tvsub :: TVE -> Type -> Type
tvsub tve (t1 `TFun` t2) = tvsub tve t1 `TFun` tvsub tve t2
tvsub tve (TVar v) | Just t <- tvlkup tve v = tvsub tve t
tvsub tve t = t

-- `shallow' substitution; check if tv is bound to anything `substantial'
tvchase :: TVE -> Type -> Type
tvchase tve (TVar v) | Just t <- tvlkup tve v = tvchase tve t
tvchase _ t = t

-- The unification. If unification failed, return the reason
unify :: Type -> Type -> TVE -> Either String TVE
unify t1 t2 tve = unify' (tvchase tve t1) (tvchase tve t2) tve

-- If either t1 or t2 are type variables, they are definitely unbound
unify' :: Type -> Type -> TVE -> Either String TVE
unify' TInt TInt = Right
unify' (t1a `TFun` t1r) (t2a `TFun` t2r) = either Left (unify t1r t2r) . unify t1a t2a
unify' (TVar v1) t2 = unifyv v1 t2
unify' t1 (TVar v2) = unifyv v2 t1
unify' t1 t2 = const (Left $ unwords ["constant mismatch:",show t1,"and",
                                      show t2])

-- Unify a free variable v1 with t2
unifyv :: TVarName -> Type -> TVE -> Either String TVE
unifyv v1 (TVar v2) tve =
    if v1 == v2 then Right tve
       else Right (tvext tve (v1, TVar v2)) -- record new constraint
unifyv v1 t2 tve = if occurs v1 t2 tve
                      then Left $ unwords ["occurs check:",show (TVar v1),
                                           "in",show $ tvsub tve t2]
                      else Right (tvext tve (v1,t2))

-- The occurs check: if v appears free in t
occurs :: TVarName -> Type -> TVE -> Bool
occurs v TInt _ = False
occurs v TBool _ = False
occurs v (t1 `TFun` t2) tve = occurs v t1 tve || occurs v t2 tve
occurs v (TVar v2) tve =
    case tvlkup tve v2 of
         Nothing -> v == v2
         Just t  -> occurs v t tve

-- Compute (quite unoptimally) the characteristic function of the set
--  forall tvb \in fv(tve_before). Union fv(tvsub(tve_after,tvb))
tvdependentset :: TVE -> TVE -> (TVarName -> Bool)
tvdependentset tve_before tve_after =
    \tv -> any (\tvb -> occurs tv (TVar tvb) tve_after) tvbs
 where
 tvbs = tvfree tve_before

-- Monadic version of unify
unifyM :: Type -> Type -> (String -> String) -> TVEM ()
unifyM t1 t2 errf = do
  tve <- get
  case unify t1 t2 tve of
       Right tve -> put tve
       Left  err -> fail (errf err)

-- Given a type scheme, that is, the type t and the list of type variables tvs,
-- for every tvs, replace all of its occurrences in t with a fresh
-- type variable.
-- We do that by creating a substitution tve and applying it to t.
instantiate :: Scheme -> TVEM Type
instantiate (Forall tvs t) = do
  tve <- associate_with_freshvars tvs
  return $ tvsub tve t
 where
 associate_with_freshvars [] = return tve0
 associate_with_freshvars (tv:tvs) = do
   tve     <- associate_with_freshvars tvs
   tvfresh <- newtv
   return $ tvext tve (tv,tvfresh)

-- Given a typechecking action ta yielding the type t, return the type
-- scheme quantifying over _truly free_ type variables in t
-- with respect to TVE that existed before the typechecking action began.
-- Let tve_before is TVE before the type checking action is executed,
-- and tve_after is TVE after the action. A type variable tv
-- is truly free if it is free in tve_after and remains free if the
-- typechecking action were executed in any tve extending tve_before
-- with arbitrary binding to type variables free in tve_before.
-- To be more precise, a type variable tv is truly free with respect
-- to tve_before if:
--    tv \not\in domain(tve_after)
--    forall tvb \in fv(tve_before). tv \not\in fv(tvsub(tve_after,tvb))
-- In other words, tv is truly free if it is free and `independent' of
-- tve_before.
--
-- Our goal is to reproduce the behavior in TInfLetI.hs:
-- generalize/instantiate should mimic multiple executions of
-- the typechecking action. That means we should quantify over all
-- type variables created by ta that are independent of the type environment
-- in which the action may be executed.

generalize :: TVEM (Type, Exp (Ann a)) -> TVEM (Scheme, Exp (Ann a))
generalize ta = do
    tve_before <- get      -- type env before ta is executed
    (t, e)     <- ta
    tve_after  <- get      -- type env after ta is executed
    let t'    = tvsub tve_after t
        tvdep = tvdependentset tve_before tve_after
        fv    = filter (not . tvdep) (nub (freevars t'))
    return (Forall fv t', e)

-- Return the list of type variables in t (possibly with duplicates)
freevars :: Type -> [TVarName]
freevars TInt       = []
freevars (t1 `TFun` t2) = freevars t1 ++ freevars t2
freevars (TVar v)     = [v]

-- Type reconstruction: abstract evaluation
-- teval' :: TEnv -> Exp a -> TVEM Type
teval' env (ELit a l) = do
    let t = case l of
                LInt  _ -> TInt
                LBool _ -> TBool
    return (t, ELit (Ann t a) l)

teval' env (EVar a x) = do
    t <- instantiate (lkup env x)
    return (t, EVar (Ann t a) x)

teval' env (EAbs a x e) = do
    tv <- newtv
    (te, e') <- teval' (ext env (x, Forall [] tv)) e -- extend with the monomorphic type
    let t = tv --> te
    return (t, EAbs (Ann t a) x e')

teval' env (EApp a e1 e2) = do
    (t1, e1') <- teval' env e1
    (t2, e2') <- teval' env e2
    t1r <- newtv
    unifyM t1 (t2 `TFun` t1r) id
    return (t1r, EApp (Ann t1r a) e1' e2')

teval' env (ELet a (x, rhs) bdy) = do
    (rhss, rhs') <- generalize $ teval' env rhs     -- This is new, cf TInfLetI.hs
    (bdyt, bdy') <- teval' (ext env (x, rhss)) bdy
    return (bdyt, ELet (Ann bdyt a) (x, rhs') bdy')

-- teval' env (I n) = return TInt
-- teval' env (e1 :+ e2) = do
--     t1 <- teval' env e1
--     t2 <- teval' env e2
--     unifyM t1 TInt ("Trying to add non-integers: " ++)
--     unifyM t2 TInt ("Trying to add non-integers: " ++)
--     return TInt
-- teval' env (IFZ e1 e2 e3) = do
--     t1 <- teval' env e1
--     t2 <- teval' env e2
--     t3 <- teval' env e3
--     unifyM t1 TInt ("Trying to compare a non-integer to 0: " ++)
--     unifyM t2 t3 (\err  -> unwords ["Branches of IFZ have different",
--                                     "types. Unification failed:",err])
--     return t2

-- teval' env (Fix e) = do
--     t  <- teval' env e
--     ta <- newtv
--     tb <- newtv
--     unifyM t ((ta `TFun` tb) `TFun` (ta `TFun` tb))
--            ("Inappropriate type in Fix: " ++)
--     return $ ta `TFun` tb

-- Resolve all type variables, as far as possible, and generalize
-- We assume teval will be used for top-level expressions where generalization
-- is appropriate.
-- teval :: TEnv -> Exp a -> Scheme
-- teval tenv e = let (ts,tve) = runState (generalize $ teval' tenv e) tve0 in ts

-- non-generalizing teval (as before)
tevalng :: TEnv -> Exp a -> Type
tevalng tenv e = let ((t, _), tve) = runState (teval' tenv e) tve0 in tvsub tve t

-- te tenv e = let (t, tve) = runState (teval' tenv e) tve0 in t
