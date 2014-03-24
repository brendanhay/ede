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

import           Bound.Scope                     (instantiate1)
import           Control.Applicative
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Map
import           Data.Hashable
import           Data.String
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Checker.Unify

-- Resolve all type aariables, as far as possible
recon :: (Eq a, Hashable a, IsString a) => Exp a -> Env a -> Either String Type
recon e env = evalCheck $ substitute <$> reconType e env <*> get

-- Type reconstruction
reconType :: (Eq a, Hashable a, IsString a) => Exp a -> Env a -> Check Type
reconType (EVar v) env =
    maybe (throw "Unbound variable")
          return
          (Map.lookup v env)
reconType (ELit l) _ = return $ reconLit l
reconType (ELam b) env = do
    tv <- freshTVar
    te <- reconType (instantiate1 (EVar "x") b) (Map.insert "x" tv env)
    return $ TApp tv te
reconType (EApp x y) env = do
    tx <- reconType x env
    ty <- reconType y env
    t  <- freshTVar
    unify tx (TApp ty t)
    return t

reconLit :: Lit -> Type
reconLit (LBool _) = TBool
reconLit (LNum  _) = TNum
reconLit (LText _) = TText

