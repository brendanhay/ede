{-# LANGUAGE TupleSections #-}

-- Module      : Text.EDE.Internal.AST
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Abstract syntax smart constructors.
module Text.EDE.Internal.AST where

import Data.Foldable           (foldl', foldr')
import Data.Maybe
import Data.Monoid
import Data.Text               (Text)
import Text.EDE.Internal.Types

evar :: Meta -> Text -> Exp
evar m = EVar m . Id

epartial :: Meta -> Text -> (Exp -> Exp)
epartial m t = EApp m (evar m t)

eapp :: Exp -> [Exp] -> Exp
eapp e [] = e
eapp e xs = foldl' (\x -> EApp (meta x) x) e xs

ecase :: Exp -> [Alt] -> Maybe Exp -> Exp
ecase p ws f = ECase (meta p) p (ws ++ maybe [] ((:[]) . wild) f)

eif :: [(Exp, Exp)] -> Maybe Exp -> Exp
eif ts f = foldr' c (fromMaybe bld f) ts
  where
    c (p, w) e = ECase (meta p) p [true w, false e]

wild, true, false :: Exp -> Alt
wild  = alt PWild
true  = alt (PLit (LBool True))
false = alt (PLit (LBool False))

alt :: Pat -> Exp -> Alt
alt = (,)

bld :: Exp
bld = EBld (mkMeta "bld.meta") mempty
