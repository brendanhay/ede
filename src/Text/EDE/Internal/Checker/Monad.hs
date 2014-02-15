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

module Text.EDE.Internal.Checker.Monad
    ( Check
    , evalCheck
    , throw
    , check

    -- Environment manipulation
    , extendVarEnv
    , lookupVar
    , getEnvTypes
    , getFreeTyVars
    , getMetaTyVars

    -- Types and unification
    , newTyVarTy
    , instantiate

    , skolemise

    , zonkType

    , quantify

    , unify
    , unifyFun
    ) where

import System.IO.Unsafe
import Debug.Trace

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.IORef
import           Data.List               (nub, (\\))
import           Data.Tuple
import           Text.EDE.Internal.Types

data Env = Env
    { envUniq :: Int                -- Unique supply
    , envStack :: HashMap Int  (Maybe Tau)
    , envVars  :: HashMap Name Sigma -- Type environment for term variables
    } deriving (Show)

newtype Check a = Check { runCheck :: Env -> (Env, Either String a) }

instance Functor Check where
    fmap = liftM

instance Applicative Check where
    (<*>) = ap
    pure  = return

instance Monad Check where
    return !x = Check $ \s -> (s, Right x)

    (>>=) !m !k = Check $ \s ->
        case runCheck m s of
            (s', Left  e) -> (s', Left e)
            (s', Right x) -> runCheck (k x) s'

-- | Run type-check, given an initial environment.
evalCheck :: [(Name, Sigma)] -> Check a -> Either String a
evalCheck binds chk =
    let (s, r) = runCheck chk $ Env 0 Map.empty (Map.fromList binds)
    in unsafePerformIO $ print s >> return r

throw :: String -> Check a
throw e = Check $ \s -> (s, Left e)

check :: Bool -> String -> Check ()
check True  = const $ return ()
check False = throw

get :: Check Env
get = Check $ \s -> (s, Right s)

with :: (Env -> (a, Env)) -> Check a
with f = Check (second Right . swap . f)

advance :: Check Int
advance = with $ \s ->
    let n = envUniq s
    in (n, s { envUniq = n + 1, envStack = Map.insert n Nothing (envStack s) })

stack :: Check (HashMap Int (Maybe Tau))
stack = envStack <$> get

readTv :: TMeta -> Check (Maybe Tau)
readTv (TM n) = join . Map.lookup n <$> stack

writeTv :: TMeta -> Tau -> Check ()
writeTv (TM n) ty = with $ \s ->
    ((), s { envStack = Map.insert n (Just ty) (envStack s) })

environment :: Check (HashMap Name Sigma)
environment = envVars <$> get
-- getEnv :: Check (HashMap Name Sigma)
-- getEnv = Check $ \env -> return $ Right (var_env env)

-- | Get the types mentioned in the environment
getEnvTypes :: Check [Type]
getEnvTypes = Map.elems <$> environment

extendVarEnv :: Name -> Sigma -> Check a -> Check a
extendVarEnv var ty m = Check $ \s ->
    runCheck m $ s { envVars = Map.insert var ty (envVars s) }

lookupVar :: Name -> Check Sigma
lookupVar n = do
    env <- environment
    maybe (throw $ "Not in scope: " ++ n)
          return
          (Map.lookup n env)

-- -- Creating, reading, writing TMetas

newTyVarTy :: Check Tau
newTyVarTy = TMeta <$> newMetaTVar

newMetaTVar :: Check TMeta
newMetaTVar = TM <$> advance

newSkolemTVar :: TVar -> Check TVar
newSkolemTVar tv = TSkolem (tvarName tv) <$> advance

-- Instantiation

-- | Instantiate the topmost for-alls of the argument type
-- with flexible type variables
instantiate :: Sigma -> Check Rho
instantiate (TForAll tvs ty) = do
    tvs' <- mapM (\_ -> newMetaTVar) tvs
    return (substTy tvs (map TMeta tvs') ty)
instantiate ty = return ty

-- | Performs deep skolemisation, retuning the
-- skolem constants and the skolemised type
skolemise :: Sigma -> Check ([TVar], Rho)
skolemise (TForAll tvs ty) = do -- Rule PRPOLY
    sks1 <- mapM newSkolemTVar tvs
    (sks2, ty') <- skolemise (substTy tvs (map TVar sks1) ty)
    return (sks1 ++ sks2, ty')
skolemise (TFun arg_ty res_ty) = do -- Rule PRFUN
    (sks, res_ty') <- skolemise res_ty
    return (sks, TFun arg_ty res_ty')
skolemise ty =                   -- Rule PRMONO
    return ([], ty)

-- Quantification

-- | Quantify over the specified type variables (all flexible)
quantify :: [TMeta] -> Rho -> Check Sigma
quantify tvs ty = do
    mapM_ bind (tvs `zip` new_bndrs)   -- 'bind' is just a cunning way
    ty' <- zonkType ty                 -- of doing the substitution
    return (TForAll new_bndrs ty')
  where
    used_bndrs = tyVarBndrs ty  -- Avoid quantified type variables in use
    new_bndrs  = take (length tvs) (allBinders \\ used_bndrs)
    bind (tv, name) = writeTv tv (TVar name)

-- | a,b,..z, a1, b1,... z1, a2, b2,...
allBinders :: [TVar]
allBinders =
       [TBound [x] | x <- ['a'..'z']]
    ++ [TBound (x : show i) | i <- [1 :: Integer ..], x <- ['a'..'z']]

-- Getting the free tyvars

-- | This function takes account of zonking, and returns a set
-- (no duplicates) of unbound meta-type variables
getMetaTyVars :: [Type] -> Check [TMeta]
getMetaTyVars tys = do
    tys' <- mapM zonkType tys
    return (metaTvs tys')

-- | This function takes account of zonking, and returns a set
-- (no duplicates) of free type variables
getFreeTyVars :: [Type] -> Check [TVar]
getFreeTyVars tys = do
    tys' <- mapM zonkType tys
    return (freeTyVars tys')

-- Zonking
-- Eliminate any substitutions in the type

zonkType :: Type -> Check Type
zonkType (TForAll ns ty) = do
    ty' <- zonkType ty
    return (TForAll ns ty')
zonkType (TFun arg res) = do
    arg' <- zonkType arg
    res' <- zonkType res
    return (TFun arg' res')
zonkType (TCon tc) =
    return (TCon tc)
zonkType (TVar n) =
    return (TVar n)
zonkType (TMeta tv) = do    -- A mutable type variable
    mb_ty <- readTv tv
    case mb_ty of
        Nothing -> return (TMeta tv)
        Just ty -> do
            ty' <- zonkType ty
            writeTv tv ty' -- "Short out" multiple hops
            return ty'

-- Unification

unify :: Tau -> Tau -> Check ()

unify ty1 ty2
  | badType ty1 || badType ty2  -- Compiler error
  = throw "Panic! Unexpected types in unification:"
--            vcat [ppr ty1, ppr ty2])

unify (TVar tv1)  (TVar tv2)  | tv1 == tv2 = return ()
unify (TMeta tv1) (TMeta tv2) | tv1 == tv2 = return ()

unify (TMeta tv) ty = unifyVar tv ty
unify ty (TMeta tv) = unifyVar tv ty

unify (TFun arg1 res1) (TFun arg2 res2) = unify arg1 arg2 >> unify res1 res2

unify (TCon tc1) (TCon tc2) | tc1 == tc2 = return ()

unify ty1 ty2 = throw "Cannot unify types:" -- ++ show (ty1, ty2))

-- Invariant: tv1 is a flexible type variable
unifyVar :: TMeta -> Tau -> Check ()
unifyVar tv1 ty2 = do       -- Check whether tv1 is bound
    mb_ty1 <- readTv tv1
    case mb_ty1 of
        Just ty1 -> unify ty1 ty2
        Nothing  -> unifyUnboundVar tv1 ty2

-- Invariant: the flexible type variable tv1 is not bound
unifyUnboundVar :: TMeta -> Tau -> Check ()
unifyUnboundVar tv1 ty2@(TMeta tv2) = do
    -- We know that tv1 /= tv2 (else the
    -- top case in unify would catch it)
    mb_ty2 <- readTv tv2
    case mb_ty2 of
        Just ty2' -> unify (TMeta tv1) ty2'
        Nothing  -> writeTv tv1 ty2

unifyUnboundVar tv1 ty2 = do
    tvs2 <- getMetaTyVars [ty2]
    if tv1 `elem` tvs2
        then occursCheckErr tv1 ty2
        else writeTv tv1 ty2

--      (arg,res) <- unifyFunTy fun
-- unifies 'fun' with '(arg -> res)'
unifyFun :: Rho -> Check (Sigma, Rho)
unifyFun (TFun arg res) = return (arg, res)
unifyFun tau            = do
    arg_ty <- newTyVarTy
    res_ty <- newTyVarTy
    unify tau (arg_ty --> res_ty)
    return (arg_ty, res_ty)

-- Raise an occurs-check error
occursCheckErr :: TMeta -> Tau -> Check ()
occursCheckErr tv ty
  = throw "Occurs check for"  -- <+> quotes (ppr tv) <+>
--            text "in:" <+> ppr ty)

-- | Tells which types should never be encountered during unification
badType :: Tau -> Bool
badType (TVar (TBound _)) = True
badType _                 = False
