{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.EDE.Internal.AST
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- AST smart constructors.
module Text.EDE.Internal.AST where

import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Aeson.Types (Value (..))
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Maybe as Maybe
import Text.EDE.Internal.Types

newtype Fix f = Fix (f (Fix f))

cofreeFix :: Functor f => a -> Fix f -> Cofree f a
cofreeFix x = go
  where
    go (Fix f) = x :< fmap go f
{-# INLINEABLE cofreeFix #-}

var :: Id -> Var
var = Var . (:| [])
{-# INLINEABLE var #-}

eapp :: a -> [Exp a] -> Exp a
eapp x [] = cofreeFix x blank
eapp _ [e] = e
eapp _ (e : es) = Foldable.foldl' (\x y -> Comonad.extract x :< EApp x y) e es
{-# INLINEABLE eapp #-}

efun :: Id -> Exp a -> Exp a
efun i e = let x = Comonad.extract e in x :< EApp (x :< EFun i) e
{-# INLINEABLE efun #-}

efilter :: Exp a -> (Id, [Exp a]) -> Exp a
efilter e (i, ps) = let x = Comonad.extract e in eapp x ((x :< EFun i) : e : ps)
{-# INLINEABLE efilter #-}

elet :: Maybe (Id, Exp a) -> Exp a -> Exp a
elet m e = maybe e (\(i, b) -> Comonad.extract b :< ELet i b e) m
{-# INLINEABLE elet #-}

ecase ::
  Exp a ->
  [Alt (Exp a)] ->
  Maybe (Exp a) ->
  Exp a
ecase p ws f = Comonad.extract p :< ECase p (ws ++ maybe [] ((: []) . wild) f)
{-# INLINEABLE ecase #-}

eif ::
  (Exp a, Exp a) ->
  [(Exp a, Exp a)] ->
  Maybe (Exp a) ->
  Exp a
eif t ts f =
  Foldable.foldr'
    c
    (Maybe.fromMaybe (Comonad.extract (fst t) `cofreeFix` blank) f)
    (t : ts)
  where
    c (p, w) e = Comonad.extract p :< ECase p [true w, false e]
{-# INLINEABLE eif #-}

eempty :: Exp a -> Exp a -> Maybe (Exp a) -> Exp a
eempty v e = maybe e (eif (efun "!" (efun "empty" v), e) [] . Just)
{-# INLINEABLE eempty #-}

true, false, wild :: Exp a -> Alt (Exp a)
true = (PLit (Bool True),)
false = (PLit (Bool False),)
wild = (PWild,)
{-# INLINEABLE true #-}
{-# INLINEABLE false #-}
{-# INLINEABLE wild #-}

blank :: Fix ExpF
blank = Fix (ELit (String mempty))
{-# INLINEABLE blank #-}
