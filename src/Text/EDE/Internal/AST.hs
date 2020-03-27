{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE.Internal.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AST smart constructors.
module Text.EDE.Internal.AST where

import Control.Comonad
import Control.Comonad.Cofree
import Data.Aeson.Types
import Data.Foldable
import Data.List.NonEmpty      (NonEmpty(..))
import Data.Maybe
import Text.EDE.Internal.Types

newtype Mu f = Mu (f (Mu f))

cofree :: Functor f => a -> Mu f -> Cofree f a
cofree x = go
  where
    go (Mu f) = x :< fmap go f

var :: Id -> Var
var = Var . (:| [])

eapp :: a -> [Exp a] -> Exp a
eapp x []     = cofree x blank
eapp _ [e]    = e
eapp _ (e:es) = foldl' (\x y -> extract x :< EApp x y) e es

efun :: Id -> Exp a -> Exp a
efun i e = let x = extract e in x :< EApp (x :< EFun i) e

efilter :: Exp a -> (Id, [Exp a]) -> Exp a
efilter e (i, ps) = let x = extract e in eapp x ((x :< EFun i) : e : ps)

elet :: Maybe (Id, Exp a) -> Exp a -> Exp a
elet m e = maybe e (\(i, b) -> extract b :< ELet i b e) m

ecase :: Exp a
      -> [Alt (Exp a)]
      -> Maybe (Exp a)
      -> Exp a
ecase p ws f = extract p :< ECase p (ws ++ maybe [] ((:[]) . wild) f)

eif :: (Exp a, Exp a)
    -> [(Exp a, Exp a)]
    -> Maybe (Exp a)
    -> Exp a
eif t ts f = foldr' c (fromMaybe (extract (fst t) `cofree` blank) f) (t:ts)
  where
    c (p, w) e = extract p :< ECase p [true w, false e]

eempty :: Exp a -> Exp a -> Maybe (Exp a) -> Exp a
eempty v e = maybe e (eif (efun "!" (efun "empty" v), e) [] . Just)

true, false, wild :: Exp a -> Alt (Exp a)
true  = (PLit (Bool True),)
false = (PLit (Bool False),)
wild  = (PWild,)

blank :: Mu ExpF
blank = Mu (ELit (String mempty))
