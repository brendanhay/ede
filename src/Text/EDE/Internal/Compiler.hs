{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}

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
import           Control.Arrow              (first)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 hiding (Result, Success)
import           Data.Attoparsec.Number     (Number(..))
import           Data.Foldable              (Foldable, foldlM)
import qualified Data.HashMap.Strict        as Map
import           Data.HashMap.Strict        (HashMap)
import           Data.List                  (sortBy)
import           Data.Monoid
import           Data.Ord
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Buildable        as Build
import           Data.Text.Format           (Format)
import           Data.Text.Format.Params    (Params)
import           Data.Text.Lazy.Builder     (Builder)
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import           Text.EDE.Internal.Types

data Equal a b where
    Eq :: Equal a a

data Order a where
    Ord :: Ord a => Order a

data Col where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Col

data TExp = forall a. a ::: TType a

type Env = ReaderT (Object, HashMap Text Filter) Result

render :: UExp -> Object -> HashMap Text Filter -> Result Builder
render e o fs = flip runReaderT (o, fs) $ do
    v ::: vt <- eval e >>= build (mkMeta "render")
    Eq       <- equal (mkMeta "cast") TBld vt
    return v

eval :: UExp -> Env TExp
eval UNil        = return $ () ::: TNil
eval (UText _ t) = return $ t  ::: TText
eval (UBool _ b) = return $ b  ::: TBool
eval (UInt  _ n) = return $ n  ::: TInt
eval (UDbl  _ d) = return $ d  ::: TDbl
eval (UBld  _ b) = return $ b  ::: TBld

eval (UVar m i) = binding m i

eval (UFil m f) = (::: TFil) <$> filter' m f

eval (UApp m (UFil fm f) e) = do
    e' ::: et   <- eval e
    Fn xt yt f' <- filter' fm f
    Eq          <- equal m et xt
    return $ f' e' ::: yt

eval (UApp _ e UNil) = eval e
eval (UApp _ UNil e) = eval e

eval (UApp _ v@(UVar m (Id i)) e) = eval v >>= f
  where
    f (o ::: TMap) = bind (const o) e
    f (_ ::: t) =
        throw m "variable {} :: {} does not supported nested accessors."
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
    Eq        <- equal m at bt
    Ord       <- order m at
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

eval (ULoop _ (Id i) v a b) = eval v >>= f >>= loop i a b
  where
    f (x ::: TList) = return $ Col (Vector.length x) (vec x)
    f (x ::: TMap)  = return $ Col (Map.size x) (hmap x)
    f (_ ::: t)     = throw (_meta v) "invalid loop target {}" [show t]

    vec :: Vector Value -> Vector (Maybe Text, Value)
    vec = Vector.map (Nothing,)

    hmap :: HashMap Text Value -> [(Maybe Text, Value)]
    hmap  = map (first Just) . sortBy (comparing fst) . Map.toList

loop :: Text -> UExp -> UExp -> Col -> Env TExp
loop _ _ b (Col 0 _)  = eval b
loop k a _ (Col l xs) = fmap ((::: TBld) . snd) $ foldlM iter (1, mempty) xs
  where
    iter (n, bld) x = do
        shadowed (_meta a) k
        a' ::: at <- bind (Map.insert k $ context n x) a
        Eq        <- equal (_meta a) at TBld
        return (n + 1, bld <> a')

    context n (mk, v) = object $
        [ "value" .= v
        , "length"     .= l
        , "index"      .= n
        , "index0"     .= (n - 1)
        , "remainder"  .= (l - n)
        , "remainder0" .= (l - n - 1)
        , "first"      .= (n == 1)
        , "last"       .= (n == l)
        , "odd"        .= (n `mod` 2 == 1)
        , "even"       .= (n `mod` 2 == 0)
        ] ++ maybe [] (\x -> ["key" .= x]) mk

equal :: Meta -> TType a -> TType b -> Env (Equal a b)
equal _ TNil  TNil  = return Eq
equal _ TText TText = return Eq
equal _ TBool TBool = return Eq
equal _ TInt  TInt  = return Eq
equal _ TDbl  TDbl  = return Eq
equal _ TBld  TBld  = return Eq
equal _ TMap  TMap  = return Eq
equal _ TList TList = return Eq
equal m a b = throw m "type equality check of {} ~ {} failed." [show a, show b]

order :: Meta -> TType a -> Env (Order a)
order _ TNil  = return Ord
order _ TText = return Ord
order _ TBool = return Ord
order _ TInt  = return Ord
order _ TDbl  = return Ord
order m t = throw m "constraint check of Ord a => a ~ {} failed." [show t]

bind :: (Object -> Object) -> UExp -> Env TExp
bind f = withReaderT (first f) . eval

predicate :: UExp -> Env TExp
predicate = mapReaderT (return . (::: TBool) . f) . eval
  where
    f (Success (_ ::: TNil))  = False
    f (Success (p ::: TBool)) = p
    f (Success _)             = True
    f _                       = False

shadowed :: Meta -> Text -> Env ()
shadowed m k = ask >>= maybe (return ()) f . Map.lookup k . fst
  where
    f x = throw m "binding {} shadows existing variable {}."
        [Text.unpack k, show x]

binding :: Meta -> Id -> Env TExp
binding m (Id i) = do
    mv <- Map.lookup i . fst <$> ask
    maybe (throw m "binding {} doesn't exist." [i]) (return . f) mv
  where
    f Null           = () ::: TNil
    f (String t)     = t  ::: TText
    f (Bool   b)     = b  ::: TBool
    f (Number (I n)) = n  ::: TInt
    f (Number (D d)) = d  ::: TDbl
    f (Object o)     = o  ::: TMap
    f (Array  a)     = a  ::: TList

filter' :: Meta -> Id -> Env Filter
filter' m (Id k) = do
    fs <- snd <$> ask
    maybe (throw m "filter {} doesn't exist." [k]) return $ Map.lookup k fs

build :: Meta -> TExp -> Env TExp
build _ (_ ::: TNil)  = return $ mempty ::: TBld
build _ (t ::: TText) = return $ Build.build t ::: TBld
build _ (b ::: TBool) = return $ Build.build b ::: TBld
build _ (n ::: TInt)  = return $ Build.build n ::: TBld
build _ (d ::: TDbl)  = return $ Build.build d ::: TBld
build _ (b ::: TBld)  = return $ b ::: TBld
build m (_ ::: t)     = throw m "unable to render variable of type {}" [show t]

throw :: Params ps => Meta -> Format -> ps -> Env a
throw m f = lift . throwError m f
