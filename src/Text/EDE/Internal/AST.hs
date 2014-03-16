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
import Text.EDE.Internal.Types

elam :: Eq a => a -> Exp a -> Exp a
elam v = ELam . abstract1 v

eapp :: [Exp a] -> Exp a
eapp = foldl1 EApp

elet :: [(Id, Exp Id)] -> Exp Id -> Exp Id
elet [] b = b
elet bs b = ELet (map (f . snd) bs) (f b)
  where
    f = abstract (`elemIndex` map fst bs)

eif :: Eq a => Exp a -> Exp a -> Exp a -> Exp a
eif p x y = ECase p [true x, false y]

true :: Eq a => Exp a -> Alt Exp a
true = alt (plit $ LBool True)

false :: Eq a => Exp a -> Alt Exp a
false = alt (plit $ LBool False)

alt :: (Monad f, Eq a) => Binder a -> f a -> Alt f a
alt (Binder vs p) = Alt p . abstract (`lookup` zip vs (paths p))

pwild :: Binder a
pwild = Binder [] PWild

pvar :: a -> Binder a
pvar v = Binder [v] PVar

plit :: Lit -> Binder a
plit = Binder [] . PLit

pas :: a -> Binder a -> Binder a
pas v (Binder vs p) = Binder (v:vs) (PAs p)
