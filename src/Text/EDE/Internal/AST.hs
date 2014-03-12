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

data P a = P
    { pattern  :: [a] -> Pat Exp a
    , bindings :: [a]
    }

varp :: a -> P a
varp a = P (const PVar) [a]

wildp :: P a
wildp = P (const PWild) []

asp :: a -> P a -> P a
asp a (P p as) = P (\bs -> PAs (p (a:bs))) (a:as)

-- |
-- >>> lam (conp "Hello" [varp "x", wildp]) (V "y")
-- Lam 1 (ConP "Hello" [VarP,WildP]) (Scope (V (F (V "y"))))
conp :: String -> [P a] -> P a
conp g ps = P (PCon g . go ps) (ps >>= bindings)
  where
    go (P p as:ps) bs = p bs : go ps (bs ++ as)
    go [] _ = []

-- | view patterns can view variables that are bound earlier than them in the pattern
viewp :: Eq a => Exp a -> P a -> P a
viewp t (P p as) = P (\bs -> PView (abstract (`elemIndex` bs) t) (p bs)) as

elam :: Eq a => P a -> Exp a -> Exp a
elam (P p as) t = ELam (length as) (p []) (abstract (`elemIndex` as) t)

-- | Let expression smart constructor.
elet :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
elet [] b = b
elet bs b = ELet (length bs) (map (abstr . snd) bs) (abstr b)
  where
    abstr = abstract (`elemIndex` map fst bs)

-- ecase p [] = p -- FIXME: error
-- ecase p as = ECase p 
--   where
--     abstr = abstract (`elemIndex` map fst as)

alt :: Eq a => P a -> Exp a -> Alt Exp a
alt (P p as) t = Alt (length as) (p []) (abstract (`elemIndex` as) t)
