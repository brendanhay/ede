{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Checker.Unify
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Unify where

import           Control.Applicative
import           Control.Monad
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Map
import           Data.String
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Checker.Monad

unify :: Type -> Type -> Check ()
unify t1 t2 = get >>= either throw put . unifyTypes t1 t2

-- If either t1 or t2 are type variables, they are definitely unbound
--unifyTypes TInt TInt = Right
unifyTypes :: Type -> Type -> Env Int -> Either String (Env Int)
unifyTypes x y e = go (chase x e) (chase y e) e
  where
    go (TApp t1a t1r) (TApp t2a t2r) = either Left (unifyTypes t1r t2r) . unifyTypes t1a t2a
    go (TVar v1) t2 = unifyFree v1 t2
    go t1 (TVar v2) = unifyFree v2 t1
    go t1 t2 = const . Left $ unwords ["constant mismatch:",show t1,"and",show t2]

-- Unify a free variable v1 with t2
unifyFree :: Int -> Type -> Env Int -> Either String (Env Int)
unifyFree v1 (TVar v2) tve =
    if v1 == v2 then Right tve
       else Right (Map.insert v1 (TVar v2) tve) -- record new constraint
unifyFree v1 t2 tve = if occurs v1 t2 tve
                      then Left $ unwords ["occurs check:",show (TVar v1),
                                           "in",show $ substitute t2 tve]
                      else Right (Map.insert v1 t2 tve)

-- The occurs check: if v appears free in t
occurs :: Int -> Type -> Env Int -> Bool
--occurs v TInt _ = False
occurs v (TApp t1 t2) tve = occurs v t1 tve || occurs v t2 tve
occurs v (TVar v2)    tve = maybe (v == v2) (\t -> occurs v t tve) (Map.lookup v2 tve)

-- Typee variables are logic variables: hypothetical reasoning
substitute :: Type -> Env Int -> Type
substitute (TApp t1 t2) tve = TApp (substitute t1 tve) (substitute t2 tve)
substitute (TVar v) tve | Just t <- Map.lookup v tve = substitute t tve
substitute t _ = t

-- `shallow' substitution; check if tv is bound to anything `substantial'
chase :: Type -> Env Int -> Type
chase (TVar v) tve | Just t <- Map.lookup v tve = chase t tve
chase t _ = t
