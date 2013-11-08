{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module EDE.Internal.TypeChecker where

import Control.Monad
import Data.Text.Format
import Data.Text.Format.Params (Params)
import EDE.Internal.Types

-- FIXME:
-- use metadata extraction function from types to annotate
-- equality check on line 22

typeCheck :: Type a => UExp -> Either TypeError (TExp a)
typeCheck = f <=< check
  where
    f :: Type a => AExp -> Either TypeError (TExp a)
    f = g typeof

    g :: TType a -> AExp -> Either TypeError (TExp a)
    g t (e ::: t') = do
        Eq <- equal Unknown t t'
        return e

check :: UExp -> Either TypeError AExp

check (UText m t) = return $ TText m t ::: TTText
check (UBool m b) = return $ TBool m b ::: TTBool
check (UInt  m i) = return $ TInt  m i ::: TTInt
check (UDbl  m d) = return $ TDbl  m d ::: TTDbl
check (UVar  m v) = return $ TVar  m v ::: TTFrag
check (UFrag m b) = return $ TFrag m b ::: TTFrag

check (UApp m a b) = do
    a' ::: at <- check a
    b' ::: bt <- check b
    Eq <- equal m at TTFrag
    Eq <- equal m bt TTFrag
    return $ TApp m a' b' ::: TTFrag

check (UNeg m e) = do
    e' ::: TTBool <- check e
    return $ TNeg m e' ::: TTBool

check (UBin m op x y) = do
    x' ::: TTBool <- check x
    y' ::: TTBool <- check y
    return $ TBin m op x' y' ::: TTBool

check (URel m op x y) = do
    x' ::: xt <- check x
    y' ::: yt <- check y
    Eq  <- equal m xt yt
    Ord <- order m xt
    return $ TRel m op x' y' ::: TTBool

check (UCond m p l r) = do
    p' ::: pt     <- check p
    l' ::: TTFrag <- check l
    r' ::: TTFrag <- check r
    Eq <- equal m pt TTBool
    return $ TCond m p' l' r' ::: TTFrag

check (ULoop m b i l r) = do
    l' ::: TTFrag <- check l
    r' ::: TTFrag <- check r
    return $ TLoop m b i l' r' ::: TTFrag

data Equal a b where
    Eq :: Equal a a

equal :: Meta -> TType a -> TType b -> Either TypeError (Equal a b)
equal _ TTText TTText = Right Eq
equal _ TTBool TTBool = Right Eq
equal _ TTInt  TTInt  = Right Eq
equal _ TTDbl  TTDbl  = Right Eq
equal _ TTFrag TTFrag = Right Eq
equal m a b = throw m "type equality check of {} ~ {} failed." [show a, show b]

data Order a where
    Ord :: Ord a => Order a

order :: Meta -> TType a -> Either TypeError (Order a)
order _ TTText = Right Ord
order _ TTBool = Right Ord
order _ TTInt  = Right Ord
order _ TTDbl  = Right Ord
order m t = throw m "constraint check of Ord a => a ~ {} failed." [show t]

throw :: Params ps => Meta -> Format -> ps -> Either TypeError a
throw m f = Left . TypeError m . format f
