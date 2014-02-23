-- Module      : Text.EDE.Internal.Checker.Context
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Context where

import Data.Monoid
import Text.EDE.Internal.Types

data ContextKind = Complete | Incomplete

-- | Context elements, indexed by their kind: Complete or Incomplete.
--   Only Incomplete contexts can have unsolved existentials.
data ContextElem :: ContextKind -> * where
    CForall       :: TVar -> ContextElem a             -- ^ alpha
    CVar          :: Var  -> Polytype -> ContextElem a -- ^ x : A
    CExists       :: TVar -> ContextElem Incomplete    -- ^ alpha^
    CExistsSolved :: TVar -> Monotype -> ContextElem a -- ^ alpha^ = tau
    CMarker       :: TVar -> ContextElem a             -- ^ |> alpha^

deriving instance Eq   (ContextElem a)
deriving instance Show (ContextElem a)

newtype GContext a = Context [ContextElem a]

type CompleteContext = GContext Complete
type Context         = GContext Incomplete

-- | Snoc
(>:) :: GContext a -> ContextElem a -> GContext a
Context gamma >: x = Context $ x : gamma

-- | Context & list of elems append
(>++) :: GContext a -> [ContextElem a] -> GContext a
gamma >++ elems = gamma <> context elems

context :: [ContextElem a] -> GContext a
context = Context . reverse

dropMarker :: ContextElem a -> GContext a -> GContext a
dropMarker m (Context gamma) = Context $ tail $ dropWhile (/= m) gamma

breakMarker :: ContextElem a -> GContext a -> (GContext a, GContext a)
breakMarker m (Context xs) = let (r, _:l) = break (== m) xs in (Context l, Context r)

instance Monoid (GContext a) where
    mempty = Context []
    mappend (Context gamma) (Context delta) = Context (delta ++ gamma)
