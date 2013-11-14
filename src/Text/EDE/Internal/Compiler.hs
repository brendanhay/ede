{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
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
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Buildable        as Build
import           Data.Text.Format           (Format)
import           Data.Text.Format.Params    (Params)
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Vector                as Vector
import           Text.EDE.Internal.Types

-- FIXME:
-- Prevent rebinding/shadowing of variables
type Env = ReaderT Object Result

data TType a where
    TText :: TType Text
    TBool :: TType Bool
    TInt  :: TType Integer
    TDbl  :: TType Double
    TBld  :: TType Builder
    TMap  :: TType Object
    TList :: TType Array

deriving instance Show (TType a)

data TExp = forall a. a ::: TType a

data Equal a b where
    Eq :: Equal a a

data Order a where
    Ord :: Ord a => Order a

data Col where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Col

render :: UExp -> Object -> Result Builder
render e o = flip runReaderT o $ do
    v ::: vt <- eval e >>= build (mkMeta "render")
    Eq       <- equal (mkMeta "cast") TBld vt
    return v

eval :: UExp -> Env TExp
eval UNil = return $ mempty ::: TBld

eval (UText _ t) = return $ t ::: TText
eval (UBool _ b) = return $ b ::: TBool
eval (UInt  _ n) = return $ n ::: TInt
eval (UDbl  _ d) = return $ d ::: TDbl
eval (UBld  _ b) = return $ b ::: TBld

eval (UVar m i) = resolve m i

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
    f :: TExp -> Env Col
    f (x ::: TList) = return $ Col (Vector.length x) (valued x)
    f (x ::: TMap)  = return $ Col (Map.size x) (keyed x)
    f (_  ::: t)    = throw (_meta v) "invalid loop target {}" [show t]

    -- vector: {{ var }}.loop, {{ var }}.value
    valued = Vector.map (Nothing,)

    -- hashmap: {{ var }}.loop, {{ var }}.key, {{ var }}.value
    keyed  = map (first Just) . Map.toList

loop :: Text -> UExp -> UExp -> Col -> Env TExp
loop _ _ b (Col 0 _)  = eval b
loop k a _ (Col l xs) = fmap ((::: TBld) . snd) $ foldlM iter (1, mempty) xs
  where
    iter (n, bld) x = do
        shadow (_meta a) k
        a' ::: at <- bind (Map.insert k $ context n x) a
        Eq        <- equal (_meta a) at TBld
        return (n + 1, bld <> a')

    context n (mk, v) = object $
        [ "value" .= v
        , "loop"  .= object
             [ "length"     .= l                -- length of the loop
             , "index"      .= n                -- index of the iteration
             , "index0"     .= (n - 1)          -- zero based index of the iteration
             , "remainder"  .= (l - n)          -- remaining number of iterations
             , "remainder0" .= (l - n - 1)      -- zero based remaining number of iterations
             , "first"      .= (n == 1)         -- is this the first iteration?
             , "last"       .= (n == l)         -- is this the last iteration?
             , "odd"        .= (n `mod` 2 == 1) -- is this an odd iteration?
             , "even"       .= (n `mod` 2 == 0) -- is this an even iteration?
             ]
         ] ++ maybe [] (\x -> ["key" .= x]) mk

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
    f (Success (p ::: TBool)) = p
    f (Success _)             = True
    f _                       = False

shadow :: Meta -> Text -> Env ()
shadow m k = ask >>= maybe (return ()) f . Map.lookup k
  where
    f x = throw m "binding {} shadows existing variable {}."
        [Text.unpack k, show x]

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
throw m f = lift . throwError m f
