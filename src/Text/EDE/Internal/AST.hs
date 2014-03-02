-- | Abstract syntax smart constructors
module Text.EDE.Internal.AST where

import Data.Text               (Text)
import Text.EDE.Internal.Types

var :: a -> String -> Exp a
var a = EVar a . Var

eabs :: a -> String -> Exp a -> Exp a
eabs a = EAbs a . Var

eapp :: a -> [Exp a] -> Exp a
eapp a = foldl1 (EApp a)

eint :: a -> Integer -> Exp a
eint a = ELit a . LNum

etext :: a -> Text -> Exp a
etext a = ELit a . LText

ebool :: a -> Bool -> Exp a
ebool a = ELit a . LBool

tvar :: String -> Type a
tvar = TVar . TypeVar

infixr 4 -->
(-->) :: Type a -> Type a -> Type a
(-->) = TFun

exists :: String -> Type a
exists = TExists . TypeVar

tforall :: String -> Polytype -> Polytype
tforall = TForall . TypeVar

tforalls :: [TVar] -> Polytype -> Polytype
tforalls = flip (foldr TForall)

infixr 3 ==>
(==>) :: String -> Polytype -> Elem
v ==> a = CVar (Var v) a
