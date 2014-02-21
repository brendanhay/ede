{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import           Control.Monad.Error
import qualified Data.HashSet            as Set
import           Data.Monoid
import           Data.Tuple
import           Text.EDE.Internal.Types

-- Index unbound _type_ variables,
-- int -> scheme, lookup,

-- Unbound _value_ variables need to introduce an unbound type variable if they
-- don't exist in the environment?

type NameEnv v = [(Name, v)]

type Env = [Value]

iEval :: ITerm -> (NameEnv Value,Env) -> Value
iEval (Ann  e _)  d = cEval e d
iEval (Free  x)   d = case lookup x (fst d) of Nothing ->  (vfree x); Just v -> v
iEval (Bound ii)  d = (snd d) !! ii
iEval (e1 :@: e2) d = vapp (iEval e1 d) (cEval e2 d)

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

cEval :: CTerm -> (NameEnv Value,Env) -> Value
cEval (Inf ii) d = iEval ii d
cEval (Lam e)  d = VLam (\ x -> cEval e (((\(e, d) -> (e,  (x : d))) d)))

cKind :: Context -> Type -> Kind -> Result ()
cKind g (TFree x) Star =
    case lookup x g of
        Just (HasKind Star) -> return ()
        Nothing             -> throwError "unknown identifier"
cKind g (Fun kk kk') Star = do
    cKind g kk  Star
    cKind g kk' Star

iType0 :: Context -> ITerm -> Result Type
iType0 = iType 0

iType :: Int -> Context -> ITerm -> Result Type
iType ii g (Ann e ty) = do
    cKind g ty Star
    cType ii g e ty
    return ty
iType ii g (Free x) =
    case lookup x g of
        Just (HasType ty) -> return ty
        Nothing           -> return $ TFree x -- throwError "unknown identifier"
iType ii g (e1 :@: e2) = do
    si <- iType ii g e1
    case si of
        Fun ty ty' -> do
            cType ii g e2 ty
            return ty'
        _          -> throwError "illegal application"

cType :: Int -> Context -> CTerm -> Type -> Result ()
cType ii g (Inf e) ty = do
    ty' <- iType ii g e
    unless (ty == ty') (throwError "type mismatch")
cType ii g (Lam e) (Fun ty ty') =
    cType (ii + 1) ((Local ii, HasType ty) : g)
    (cSubst 0 (Free (Local ii)) e) ty'
cType ii g _ _ =
    throwError "type mismatch"

type Result a = Either String a

iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst ii r (Ann e ty)  = Ann (cSubst ii r e) ty
iSubst ii r (Bound j)   = if ii == j then r else Bound j
iSubst ii r (Free y)    = Free y
iSubst ii r (e1 :@: e2) = iSubst ii r e1 :@: cSubst ii r e2

cSubst :: Int -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e) = Inf (iSubst ii r e)
cSubst ii r (Lam e) = Lam (cSubst (ii + 1) r e)

-- quote0 :: Value -> CTerm
-- quote0 = quote 0

-- quote :: Int -> Value -> CTerm
-- quote ii (VLam f)     = Lam (quote (ii + 1) (f (vfree (Quote ii))))
-- quote ii (VNeutral n) = Inf (neutralQuote ii n)

-- neutralQuote :: Int -> Neutral -> ITerm
-- neutralQuote ii (NFree x)  = boundfree ii x
-- neutralQuote ii (NApp n v) = neutralQuote ii n :@: quote ii v

-- boundfree :: Int -> Name -> ITerm
-- boundfree ii (Quote k) = Bound (ii - k - 1)
-- boundfree ii x         = Free x

id'     = Lam (Inf (Bound 0))
const'  = Lam (Lam (Inf (Bound 1)))
tfree a = TFree (Global a)
free x  = Inf (Free (Global x))

term1    = Free $ Global "y"
term2    = Ann const' (Fun  (Fun (tfree "b") (tfree "b"))
                           (Fun  (tfree "a")
                                 (Fun (tfree "b") (tfree "b"))))
          :@: id' :@: free "y"

env1 = [(Global "y", HasType (tfree "a")),(Global "a", HasKind Star)]
env2 = [(Global "b", HasKind Star)] ++ env1

-- test_eval1 = quote0 (iEval term1 ([],[]))
-- test_eval2 = quote0 (iEval term2 ([],[]))

test_type1 = iType0 env1 term1
test_type2 = iType0 env2 term2
