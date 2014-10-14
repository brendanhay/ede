{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

-- Module      : Text.EDE.Internal.HOAS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.HOAS where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson              hiding (Result, Success, Error)
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder
import           Text.EDE.Internal.Types

data Binding
    = BVal !Value
    | BLam (Binding -> Result Binding)

instance Show Binding where
    show (BVal v) = show v
    show _        = "<function>"

instance Eq Binding where
    BVal a == BVal b = a == b
    _      == _      = False

typeOf :: Value -> String
typeOf = \case
    Null     -> "Null"
    Bool   _ -> "Bool"
    Number _ -> "Number"
    Object _ -> "Object"
    Array  _ -> "Array"
    String _ -> "String"

typeFun :: String
typeFun = "Function"

qapp :: Binding -> Binding -> Result Binding
qapp a b = case (a, b) of
    (BLam f, x) -> f x
    (BVal x, _) -> throwError "unable to apply literal {} -> {}\n{}"
        [typeOf x, typeFun, show x]

qpoly2 :: Quote a => (Value -> Value -> a) -> Binding
qpoly2 = quote

qnum1 :: (Scientific -> Scientific) -> Binding
qnum1 = quote

qnum2 :: Quote a => (Scientific -> Scientific -> a) -> Binding
qnum2 = quote

qcol1 :: Quote a => (Text -> a) -> (Object -> a) -> (Array -> a) -> Binding
qcol1 f g h = BLam $ \case
    BVal (String t) -> pure . quote $ f t
    BVal (Object o) -> pure . quote $ g o
    BVal (Array  v) -> pure . quote $ h v
    BVal y          -> err (typeOf y)
    _               -> err typeFun
  where
    err = throwError "expected a String, Object, or Array, but got {}" . (:[])

class Quote a where
    quote :: a -> Binding

instance Quote Binding where
    quote = id

instance Quote Lit where
    quote = \case
        LBool b -> BVal (Bool b)
        LNum  n -> BVal (Number n)
        LText t -> BVal (String t)

instance Quote Value where
    quote = BVal

instance Quote Text where
    quote = BVal . String

instance Quote LText.Text where
    quote = quote . LText.toStrict

instance Quote Builder where
    quote = quote . toLazyText

instance Quote Bool where
    quote = BVal . Bool

instance Quote Int where
    quote = BVal . Number . fromIntegral

instance Quote Scientific where
    quote = BVal . Number

class Unquote a where
    unquote :: Binding -> Result a

instance Unquote Value where
    unquote = \case
        BVal v -> pure v
        _      -> unexpected typeFun "Literal"

instance Unquote Text where
    unquote = unquote >=> \case
        String t -> pure t
        v        -> unexpected (typeOf v) "String"

instance Unquote LText.Text where
    unquote = fmap LText.fromStrict . unquote

instance Unquote Bool where
    unquote = unquote >=> \case
        Bool b -> pure b
        v      -> unexpected (typeOf v) "Bool"

instance Unquote Scientific where
    unquote = \case
        BVal (Number n) -> pure n
        BVal v          -> unexpected (typeOf v) "String"
        _               -> unexpected typeFun "String"

instance (Unquote a, Quote b) => Quote (a -> b) where
    quote f = BLam (fmap (quote . f) . unquote)

unexpected :: String -> String -> Result b
unexpected x y = throwError "unable to coerce {} -> {}" [x, y]
