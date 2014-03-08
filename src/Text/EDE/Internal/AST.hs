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

efree :: a -> Id -> Exp a
efree a = EVar a . VFree

ebound :: a -> Id -> Exp a
ebound a = EVar a . VBound

eabs :: a -> Id -> Exp a -> Exp a
eabs a = EAbs a . Bind

elet :: a -> Id -> Exp a -> Exp a -> Exp a
elet a v = ELet a (Bind v)

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
v ==> a = CVar (VBound v) a
