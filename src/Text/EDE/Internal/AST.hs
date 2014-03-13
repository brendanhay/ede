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

import Data.Text               (Text)
import Text.EDE.Internal.Types

-- varp :: a -> Pattern a
-- varp a = Pattern (const PVar) [a]

-- wildp :: Pattern a
-- wildp = Pattern (const PWild) []

-- asp :: a -> Pattern a -> Pattern a
-- asp a (Pattern p as) = Pattern (\bs -> PAs (p (a:bs))) (a:as)

-- conp :: String -> [Pattern a] -> Pattern a
-- conp g ps = Pattern (PCon g . go ps) (ps >>= _bindings)
--   where
--     go (Pattern p as:ps) bs = p bs : go ps (bs ++ as)
--     go [] _ = []

-- -- | view patterns can view variables that are bound earlier than them in the pattern
-- viewp :: Eq a => Exp a -> Pattern a -> Pattern a
-- viewp t (Pattern p as) = Pattern (\bs -> PView (abstract (`elemIndex` bs) t) (p bs)) as

-- elam :: Eq a => Pattern a -> Exp a -> Exp a
-- elam (Pattern p as) t = ELam (length as) (p []) (abstract (`elemIndex` as) t)

-- -- | Let expression smart constructor.
-- elet :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
-- elet [] b = b
-- elet bs b = ELet (length bs) (map (abstr . snd) bs) (abstr b)
--   where
--     abstr = abstract (`elemIndex` map fst bs)

-- eif :: Eq a => Exp a -> Exp a -> Exp a -> Exp a
-- eif p x y = ECase p [true x, false y]

-- true :: Eq a => Exp a -> Alt Exp a
-- true = alt (Pattern (const (PLit $ LBool True)) [])

-- false :: Eq a => Exp a -> Alt Exp a
-- false = alt (Pattern (const (PLit $ LBool False)) [])

-- alt :: Eq a => Pattern a -> Exp a -> Alt Exp a
-- alt (Pattern p as) = Alt (length as) (p []) . abstract (`elemIndex` as)

-- efree :: a -> Id -> Exp a
-- efree a = EVar a . VFree

-- ebound :: a -> Id -> Exp a
-- ebound a = EVar a . VBound

evar :: a -> Id -> Exp a
evar a = EVar a . Var

eabs :: a -> Id -> Exp a -> Exp a
eabs a = EAbs a . Var

elet :: a -> Id -> Exp a -> Exp a -> Exp a
elet a v = ELet a (Var v)

eapp :: a -> [Exp a] -> Exp a
eapp a = foldl1 (EApp a)

einteger :: a -> Integer -> Exp a
einteger a = ELit a . LNum

etext :: a -> Text -> Exp a
etext a = ELit a . LText

ebool :: a -> Bool -> Exp a
ebool a = ELit a . LBool

tvar :: Id -> Type a
tvar = TVar . TypeVar

infixr 4 -->
(-->) :: Type a -> Type a -> Type a
(-->) = TFun

exists :: Id -> Type a
exists = TExists . TypeVar

tnum :: Type a
tnum = TCon TNum

tbool :: Type a
tbool = TCon TBool

tforall :: Id -> Polytype -> Polytype
tforall = TForall . TypeVar

tforalls :: [TVar] -> Polytype -> Polytype
tforalls = flip (foldr TForall)

infixr 3 ==>
(==>) :: Id -> Polytype -> Elem
v ==> a = CVar (Var v) a
