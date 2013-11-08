{-# LANGUAGE GADTs #-}

module EDE.Internal.TypeCheck where

import Control.Monad
import Data.Monoid
import EDE.Internal.Types

typeCheck :: Type a => UExp -> Either String (TExp a)
typeCheck = f <=< check
  where
    f :: Type a => AExp -> Either String (TExp a)
    f = g typeof

    g :: TType a -> AExp -> Either String (TExp a)
    g t (e ::: t') = do
        Eq <- equal t t'
        return e

check :: UExp -> Either String AExp

check (UText t) = return $ TText t ::: TTText
check (UBool b) = return $ TBool b ::: TTBool
check (UInt  i) = return $ TInt  i ::: TTInt
check (UDbl  d) = return $ TDbl  d ::: TTDbl
check (UVar  v) = return $ TVar  v ::: TTFrag
check (UFrag b) = return $ TFrag b ::: TTFrag

check (UApp a b) = do
    a' ::: at <- check a
    b' ::: bt <- check b
    Eq <- equal at TTFrag
    Eq <- equal bt TTFrag
    return $ TApp a' b' ::: TTFrag

check (UNeg e) = do
    e' ::: TTBool <- check e
    return $ TNeg e' ::: TTBool

check (UBin op x y) = do
    x' ::: TTBool <- check x
    y' ::: TTBool <- check y
    return $ TBin op x' y' ::: TTBool

check (URel op x y) = do
    x' ::: xt <- check x
    y' ::: yt <- check y
    Eq  <- equal xt yt
    Ord <- order xt
    return $ TRel op x' y' ::: TTBool

check (UCond p l r) = do
    p' ::: pt     <- check p
    l' ::: TTFrag <- check l
    r' ::: TTFrag <- check r
    Eq <- equal pt TTBool
    return $ TCond p' l' r' ::: TTFrag

check (ULoop b i l r) = do
    l' ::: TTFrag <- check l
    r' ::: TTFrag <- check r
    return $ TLoop b i l' r' ::: TTFrag

data Equal a b where
    Eq :: Equal a a

equal :: TType a -> TType b -> Either String (Equal a b)
equal TTText TTText = Right Eq
equal TTBool TTBool = Right Eq
equal TTInt  TTInt  = Right Eq
equal TTDbl  TTDbl  = Right Eq
equal TTFrag TTFrag = Right Eq
equal a b = Left $
    concat ["type equality check ", show a, " ~ ", show b, " failed."]

data Order a where
    Ord :: Ord a => Order a

order :: TType a -> Either String (Order a)
order TTText = Right Ord
order TTBool = Right Ord
order TTInt  = Right Ord
order TTDbl  = Right Ord
order t = Left $
    concat ["constraint check Ord a => a ~ ", show t, "failed."]
