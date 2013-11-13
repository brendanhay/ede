{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

-- Module      : Text.EDE.Internal.Compiler
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Compiler where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (Array, Object, Value(..))
import           Data.Attoparsec.Number     (Number(..))
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Buildable        as Build
import           Data.Text.Format           (Format, format)
import           Data.Text.Format.Params    (Params)
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Vector                as Vector
import           Text.EDE.Internal.Types

data TType a where
    TText :: TType Text
    TBool :: TType Bool
    TInt  :: TType Integer
    TDbl  :: TType Double
    TBld  :: TType Builder
    TMap  :: TType Object
    TList :: TType Array

deriving instance Show (TType a)

class Type a where
    typeof :: TType a

instance Type Text    where typeof = TText
instance Type Bool    where typeof = TBool
instance Type Integer where typeof = TInt
instance Type Double  where typeof = TDbl
instance Type Builder where typeof = TBld
instance Type Object  where typeof = TMap
instance Type Array   where typeof = TList

data TExp = forall a. a ::: TType a

type Env = ReaderT Object Result

data Equal a b where
    Eq :: Equal a a

data Order a where
    Ord :: Ord a => Order a

-- FIXME:
-- Prevent rebinding/shadowing of variables

render :: Object -> UExp -> Result Builder
render o e = flip runReaderT o $ do
     r <- eval e
     b <- build (Meta "render" 0 0) r
     cast TBld b

cast :: TType a -> TExp -> Env a
cast t (v ::: t') = do
    Eq <- equal (Meta "src" 0 0) t t'
    return v

eval :: UExp -> Env TExp
eval UNil = return $ mempty ::: TBld

eval (UText _ t) = return $ t ::: TText
eval (UBool _ b) = return $ b ::: TBool
eval (UInt  _ n) = return $ n ::: TInt
eval (UDbl  _ d) = return $ d ::: TDbl
eval (UBld  _ b) = return $ b ::: TBld

eval (UVar m i) = resolve m i

eval (UApp _ UNil e) = eval e
eval (UApp _ v@(UVar m (Id i)) e) = eval v >>= f
  where
    f (o ::: TMap) = bind (const o) e
    f (_ ::: t) =
        throw m "variable {} of type {} does not supported nested accessors."
            [Text.unpack i, show t]
eval (UApp m a b) = do
    a' ::: TBld <- f a
    b' ::: TBld <- f b
    return $ (a' <> b') ::: TBld
  where
    f x = eval x >>= build m

eval (UNeg _ e) = do
    e' ::: TBool <- predicate e
    return $ not e' ::: TBool

eval (UBin _ op a b) = do
    a' ::: TBool <- predicate a
    b' ::: TBool <- predicate b
    return $ f op a' b' ::: TBool
  where
    f And = (&&)
    f Or  = (||)

eval (URel m op a b) = do
    a' ::: at <- eval a
    b' ::: bt <- eval b
    Eq  <- equal m at bt
    Ord <- order m at
    return $ f op a' b' ::: TBool
  where
    f Equal        = (==)
    f NotEqual     = (/=)
    f Greater      = (>)
    f GreaterEqual = (>=)
    f Less         = (<)
    f LessEqual    = (<=)

eval (UCond _ p a b) = do
    p' ::: TBool <- predicate p
    eval $ if p' then a else b

eval e = throw (metadata e) "unable to evaluate unsupported expression {}"
    [show e]

equal :: Meta -> TType a -> TType b -> Env (Equal a b)
equal _ TText TText = return Eq
equal _ TBool TBool = return Eq
equal _ TInt  TInt  = return Eq
equal _ TDbl  TDbl  = return Eq
equal _ TBld  TBld  = return Eq
equal _ TMap  TMap  = return Eq
equal _ TList TList = return Eq
equal m a b = throw m "type equality check of {} ~ {} failed." [show a, show b]

order :: Meta -> TType a -> Env (Order a)
order _ TText = return Ord
order _ TBool = return Ord
order _ TInt  = return Ord
order _ TDbl  = return Ord
order m t = throw m "constraint check of Ord a => a ~ {} failed." [show t]

bind :: (Object -> Object) -> UExp -> Env TExp
bind f = withReaderT f . eval

predicate :: UExp -> Env TExp
predicate = mapReaderT (return . (::: TBool) . f) . eval
  where
    f (Success _) = True
    f _           = False

resolve :: Meta -> Id -> Env TExp
resolve m (Id i) = do
    mv <- Map.lookup i <$> ask
    maybe (throw m "binding {} doesn't exist." [i]) (return . f) mv
  where
    f (String t)     = t     ::: TText
    f (Bool   b)     = b     ::: TBool
    f (Number (I n)) = n     ::: TInt
    f (Number (D d)) = d     ::: TDbl
    f (Object o)     = o     ::: TMap
    f (Array  a)     = a     ::: TList
    f Null           = False ::: TBool

build :: Meta -> TExp -> Env TExp
build _ (t ::: TText) = return $ Build.build t ::: TBld
build _ (b ::: TBool) = return $ Build.build b ::: TBld
build _ (n ::: TInt)  = return $ Build.build n ::: TBld
build _ (d ::: TDbl)  = return $ Build.build d ::: TBld
build _ (b ::: TBld)  = return $ b ::: TBld
build m (_ ::: t)     = throw m "unable to render variable of type {}" [show t]

throw :: Params ps => Meta -> Format -> ps -> Env a
throw m f = lift . Error m . (:[]) . LText.unpack . format f
