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

import           Control.Monad
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Map
import           Data.Maybe
import           Text.EDE.Internal.Checker.Class
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Types

infixr 4 -->
(-->) :: Type -> Type -> Type
(-->) a b = TApp (TApp tarrow a) b

preludeClasses :: ClassEnv
preludeClasses = fromJust $
    (  addClass "Eq" []
   <:> addClass "Monoid" []
   <:> addInst [] (IsIn "Monoid" ttext)
    ) initialEnv

preludeFuns :: HashMap Id Scheme
preludeFuns = Map.fromList
    [ ("==",       Forall [Star] $ [IsIn "Eq" (TGen 0)] :=> (TGen 0 --> TGen 0 --> tbool))
    , ("mappend",  Forall [Star] $ [IsIn "Monoid" (TGen 0)] :=> (TGen 0 --> TGen 0 --> TGen 0))
    , ("subtract", Forall [Star] $ [IsIn "Num" (TGen 0)] :=> (TGen 0 --> TGen 0 --> TGen 0))
    , ("text",     Forall [Star] $ [] :=> ttext)
    ]

tiExp :: Exp -> Check (Qual Type)
tiExp as (EVar i) = do
    ms <- find i
    instantiate ms
tiExp as (ELit l) = do
    tiLit l
tiExp as (EApp e f) = do
    ps :=> te <- tiExp e
    qs :=> tf <- tiExp f
    t         <- local Star
    unify (tf --> t) te
    return $ (ps ++ qs) :=> t
-- tiExp ce as (ELet bg e) = do
--     (ps, as')  <- tiBindGroup ce as bg
--     (qs, t)    <- tiExp ce (as' ++ as) e
--     return (ps ++ qs, t)

tiLit :: Lit -> Check (Qual Type)
tiLit (LNum  _) = do
    v <- local Star
    return $ [IsIn "Num" v] :=> v
tiLit (LChar _) = return $ [] :=> tchar
tiLit (LText _) = return $ [] :=> ttext
tiLit (LBool _) = return $ [] :=> tbool

-- introduce :: Monad m => Id -> Scheme -> m 
-- introduce i sc 
