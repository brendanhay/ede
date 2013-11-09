{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.EDE.Internal.TypeChecker where

import Control.Monad
import Data.Text.Format
import Data.Text.Format.Params (Params)
import Text.EDE.Internal.Types

-- FIXME:
-- use metadata extraction function from types to annotate
-- equality check on line 22

typeCheck :: Type a => UExp -> Result (TExp a)
typeCheck = g typeof <=< check
  where
    g t (e ::: t') = do
        Eq <- equal Unknown t t'
        return e

check :: UExp -> Result AExp

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

equal :: Meta -> TType a -> TType b -> Result (Equal a b)
equal _ TTText TTText = return Eq
equal _ TTBool TTBool = return Eq
equal _ TTInt  TTInt  = return Eq
equal _ TTDbl  TTDbl  = return Eq
equal _ TTFrag TTFrag = return Eq
equal m a b = throw m "type equality check of {} ~ {} failed." [show a, show b]

data Order a where
    Ord :: Ord a => Order a

order :: Meta -> TType a -> Result (Order a)
order _ TTText = return Ord
order _ TTBool = return Ord
order _ TTInt  = return Ord
order _ TTDbl  = return Ord
order m t = throw m "constraint check of Ord a => a ~ {} failed." [show t]

throw :: Params ps => Meta -> Format -> ps -> Result a
throw m f = TypeError m . format f
