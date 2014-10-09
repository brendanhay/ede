{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

-- Module      : Text.EDE.Internal.Quoter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Quoter where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson              hiding (Result, Success, Error)
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder
import           Text.EDE.Internal.Types

tfun :: String
tfun = "TFun"

qapp :: Quoted -> Quoted -> Result Quoted
qapp (QLam f) v = f v
qapp (QLit v) _ =
    throwError Quoter "unable to apply literal :: {}" [typeof v]

-- qeval :: (Show a, Quote a) => Id -> Quoted -> a -> Result Quoted
-- qeval k q x = qapp q (quote x) --
--              >>= \case
    -- QLam _ -> throwError Quoter "unable to evaluate unapplied function {} :: {}" [show k, show x]
    -- QLit v -> return v

bpoly :: Quote a => (Value -> Value -> a) -> Quoted
bpoly = quote

unum :: (Scientific -> Scientific) -> Quoted
unum = quote

bnum :: Quote a => (Scientific -> Scientific -> a) -> Quoted
bnum = quote

useq :: Quote a => (Text -> a) -> (Object -> a) -> (Array -> a) -> Quoted
useq f g h = QLam $ \x ->
   case x of
       QLit (String t) -> return . quote $ f t
       QLit (Object o) -> return . quote $ g o
       QLit (Array  v) -> return . quote $ h v
       QLit y          -> err (typeof y)
       _               -> err tfun
  where
    err t = throwError Quoter "expected a {}, {}, or {}, but got {}"
        [show TText, show TMap, show TList, t]

class Quote a where
    quote :: a -> Quoted

instance Quote Quoted where
    quote = id

instance Quote TExp where
    quote = QLit . \case
        _  ::: TNil  -> Null
        b  ::: TBool -> Bool b
        n  ::: TNum  -> Number n
        o  ::: TMap  -> Object o
        a  ::: TList -> Array a
        t  ::: TText -> String (LText.toStrict t)
        b  ::: TBld  -> String (LText.toStrict (toLazyText b)) -- FIXME: Grauugh!!

instance Quote Text where
    quote = QLit . String

instance Quote LText.Text where
    quote = quote . LText.toStrict

instance Quote Bool where
    quote = QLit . Bool

instance Quote Int where
    quote = QLit . Number . fromIntegral

instance Quote Scientific where
    quote = QLit . Number

class Unquote a where
    unquote :: Quoted -> Result a

instance Unquote Value where
    unquote = \case
        QLit v -> return v
        _      -> throwError Quoter "unable to unquote {} -> literal" [tfun]

instance Unquote Text where
    unquote = unquote >=> \case
        String t -> return t
        v        -> unexpected (typeof v) TText

instance Unquote LText.Text where
    unquote = fmap LText.fromStrict . unquote

instance Unquote Bool where
    unquote = unquote >=> \case
        Bool b -> return b
        v      -> unexpected (typeof v) TBool

instance Unquote Scientific where
    unquote = \case
        QLit (Number n) -> return n
        QLit v          -> unexpected (typeof v)  TText
        _               -> unexpected tfun TText

instance (Unquote a, Quote b) => Quote (a -> b) where
    quote f = QLam (fmap (quote . f) . unquote)

-- instance Quote a => Quote (Value -> Value -> a) where
--     quote f = QLam $ \x ->
--         Success . QLam $ \y ->
--             case (x, y) of
--                 (_,      QLam g) -> join (qapp <$> pure (quote f) <*> g x)
--                 (QLit a, QLit b) -> Success (quote (f a b))
--                 _                -> error "anus!"

instance (Unquote a, Unquote b, Quote c) => Quote (a -> b -> c) where
    quote f = QLam $ \x ->
        return . QLam $ \y ->
            case y of
                QLam g -> join (qapp <$> pure (quote f) <*> g x)
                _      -> quote <$> (f <$> unquote x <*> unquote y)

unexpected :: String -> Type a -> Result b
unexpected x y = throwError Quoter "unable to unquote {} -> {}" [x, show y]
