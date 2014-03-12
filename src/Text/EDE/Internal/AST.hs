{-# LANGUAGE DataKinds #-}

-- Module      : Text.EDE.Internal.AST
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Abstract syntax smart constructors
module Text.EDE.Internal.AST
    ( module Text.EDE.Internal.AST
    , module Text.EDE.Internal.Types
    ) where

import Bound
import Data.List               (elemIndex)
import Data.Text               (Text)
import Text.EDE.Internal.Types

varp :: a -> Pattern a
varp a = Pattern (const PVar) [a]

wildp :: Pattern a
wildp = Pattern (const PWild) []

asp :: a -> Pattern a -> Pattern a
asp a (Pattern p as) = Pattern (\bs -> PAs (p (a:bs))) (a:as)

conp :: String -> [Pattern a] -> Pattern a
conp g ps = Pattern (PCon g . go ps) (ps >>= _bindings)
  where
    go (Pattern p as:ps) bs = p bs : go ps (bs ++ as)
    go [] _ = []

-- | view patterns can view variables that are bound earlier than them in the pattern
viewp :: Eq a => Exp a -> Pattern a -> Pattern a
viewp t (Pattern p as) = Pattern (\bs -> PView (abstract (`elemIndex` bs) t) (p bs)) as

elam :: Eq a => Pattern a -> Exp a -> Exp a
elam (Pattern p as) t = ELam (length as) (p []) (abstract (`elemIndex` as) t)

-- | Let expression smart constructor.
elet :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
elet [] b = b
elet bs b = ELet (length bs) (map (abstr . snd) bs) (abstr b)
  where
    abstr = abstract (`elemIndex` map fst bs)

eif :: Eq a => Exp a -> Exp a -> Exp a -> Exp a
eif p x y = ECase p [true x, false y]

true :: Eq a => Exp a -> Alt Exp a
true = alt (Pattern (const (PLit $ LBool True)) [])

false :: Eq a => Exp a -> Alt Exp a
false = alt (Pattern (const (PLit $ LBool False)) [])

alt :: Eq a => Pattern a -> Exp a -> Alt Exp a
alt (Pattern p as) = Alt (length as) (p []) . abstract (`elemIndex` as)
