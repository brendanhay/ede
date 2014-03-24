{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Reduce
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Reduce where

import           Bound
import           Bound.Scope
import           Bound.Var
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text           as Text
import           Text.EDE.Internal.AST

-- | Compute the normal form of an expression
nf :: Exp a -> Exp a
nf e@(ELit l) = l `deepseq` e
nf e@EVar{}   = e
nf (ELam b)   = ELam . toScope . nf $ fromScope b
nf (EApp f a) =
    case whnf f of
        ELam b -> nf (instantiate1 a b)
        f'     -> EApp (nf f') (nf a)
nf (ELet bs b) = nf (inst b)
  where
    inst = instantiate (es !!)
    es   = map inst bs

whnf :: Exp a -> Exp a
whnf e@ELit{}    = e
whnf e@EVar{}    = e
whnf e@ELam{}    = e
whnf (EApp f a)  =
    case whnf f of
        ELam b -> whnf (instantiate1 a b)
        f'     -> EApp f' a
whnf (ELet bs b) = whnf (inst b)
  where
    inst = instantiate (es !!)
    es   = map inst bs
