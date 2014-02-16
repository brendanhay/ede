{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections            #-}

-- Module      : Text.EDE.Internal.Checker.MOnad
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Monad where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Monoid
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as Set
import           Data.IORef
import           Data.List               (nub, (\\))
import           Data.Tuple
import           Text.EDE.Internal.Types

-- data Env = Env
--     { envUniq  :: Int                -- Unique supply
--     , envStack :: HashMap Int  Tau
--     , envVars  :: HashMap Name Sigma -- Type environment for term variables
--     } deriving (Show)

-- newtype Check a = Check { runCheck :: Env -> (Env, Either String a) }

-- instance Functor Check where
--     fmap = liftM

-- instance Applicative Check where
--     (<*>) = ap
--     pure  = return

-- instance Monad Check where
--     return !x = Check $ \s -> (s, Right x)

--     (>>=) !m !k = Check $ \s ->
--         case runCheck m s of
--             (s', Left  e) -> (s', Left e)
--             (s', Right x) -> runCheck (k x) s'

-- -- | Run type-check, given an initial environment.
-- evalCheck :: Show a => HashMap Name Sigma -> Check a -> Either String a
-- evalCheck binds chk = unsafePerformIO $ do
--     let x = runCheck chk $ Env 0 Map.empty binds
--     print x
--     return $ snd x

-- throw :: String -> Check a
-- throw e = Check $ \s -> (s, Left e)

-- check :: Bool -> String -> Check ()
-- check True  = const $ return ()
-- check False = throw

-- get :: Check Env
-- get = Check $ \s -> (s, Right s)

-- with :: (Env -> (a, Env)) -> Check a
-- with f = Check (second Right . swap . f)


class Types a where
    ftv   :: a -> HashSet String
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar n)     = Set.singleton n
    ftv TNum         = Set.empty
    ftv TBool        = Set.empty
    ftv (TFun t1 t2) = ftv t1 <> ftv t2

    apply s (TVar n) =
        case Map.lookup n s of
            Nothing -> TVar n
            Just  t -> t
    apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
    apply s t = t

instance Types Scheme where
    ftv     (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
    apply s (Scheme vars t) = Scheme vars $ apply (foldr Map.delete s vars) t

-- It will occasionally be useful to extend the Types methods to lists.
instance Types a => Types [a] where
    ftv   = Set.unions . map ftv
    apply = map . apply

-- Now we define substitutions, which are finite mappings from type variables to types.
type Subst = HashMap String Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Type environments, called Γ in the text, are mappings from term variables
-- to their respective type schemes.
newtype TypeEnv = TypeEnv (HashMap String Scheme)

-- We define several functions on type environments. The operation Γ\x removes the binding for
-- x from Γ and is called remove.
remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

-- The function generalize abstracts a type over all type variables which are
-- free in the type but not free in the given type environment.
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (Set.toList $ ftv t `Set.difference` ftv env) t

data Scheme = Scheme [String] Type

data TIEnv = TIEnv { }

data TIState = TIState
    { tiSupply :: Int
    , tiSubst  :: Subst
    }

type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
    (res, st) <- runStateT (runReaderT (runErrorT t) initTIEnv) initTIState
    return (res, st)
  where
    initTIEnv   = TIEnv { }
    initTIState = TIState { tiSupply = 0, tiSubst = Map.empty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TVar (prefix ++ show (tiSupply s)))

-- The instantiation function replaces all bound type variables in a type
-- scheme with fresh type variables.
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

-- This is the unification function for types. The function varBind attempts
-- to bind a type variable to a type and return that binding as a subsitution,
-- but avoids binding a variable to itself and performs the occurs check.

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return $ s1 `composeSubst` s2
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TNum TNum  = return nullSubst
mgu TBool TBool = return nullSubst
mgu _ _ = throwError "doesn't unify"

varBind :: String -> Type -> TI Subst
varBind u t
    | t == TVar u = return nullSubst
    | u `Set.member` ftv t = throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return (Map.singleton u t)

-- Main type inference

tiLit :: TypeEnv -> Lit -> TI (Subst, Type)
tiLit _ (LNum _)  = return (nullSubst, TNum)
tiLit _ (LBool _) = return (nullSubst, TBool)

ti :: TypeEnv -> Exp a -> TI (Subst, Type)
ti (TypeEnv env) (EVar _ n) =
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable: " ++ n
        Just sigma -> do
            t <- instantiate sigma
            return (nullSubst,t)
ti env (ELit _ l) = tiLit env l
ti env (ELam _ n e) = do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env''        = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
    (s1,t1) <- ti env'' e
    return (s1, TFun (apply s1 tv) t1)
ti env (EApp _ e1 e2) = do
    tv      <- newTyVar "a"
    (s1,t1) <- ti env e1
    (s2,t2) <- ti (apply s1 env) e2
    s3      <- mgu (apply s2 t1) (TFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (ELet _ x e1 e2) = do
    (s1,t1) <- ti env e1
    let TypeEnv env' = remove env x
        t'           = generalize (apply s1 env) t1
        env''        = TypeEnv (Map.insert x t' env')
    (s2,t2) <- ti (apply s1 env'') e2
    return (s1 `composeSubst` s2, t2)

-- This is the main entry point to the type inferencer. It simply calls ti
-- and applies the returned substitution to the returned type.
typeInference :: HashMap String Scheme -> Exp a -> TI Type
typeInference env e = do
    (s, t) <- ti (TypeEnv env) e
    return (apply s t)
