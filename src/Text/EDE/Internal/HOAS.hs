{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
import           Data.Aeson                   hiding (Result(..))
import           Data.Bifunctor
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (sortBy)
import           Data.Ord                     (comparing)
import           Data.Scientific
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import qualified Data.Vector                  as Vector
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), (<+>))
import           Text.Trifecta.Delta

default (Double, Integer)

-- | A HOAS representation of (possibly partially applied) values
-- in the environment.
data Term
    = TVal !Value
    | TLam !Id (Int -> Term -> Result Term)

instance Show Term where
    show (TVal v) = show v
    show _        = "<function>"

instance Eq Term where
    TVal a == TVal b = a == b
    _      == _      = False

-- | Retrieve a consistent type from a 'Value' to use in error messages.
typeOf :: Value -> String
typeOf = \case
    Null     -> "Null"
    Bool   _ -> "Bool"
    Number _ -> "Number"
    Object _ -> "Object"
    Array  _ -> "Array"
    String _ -> "String"

-- | The default type for partially applied 'Term's in error messages.
typeFun :: String
typeFun = "Function"

-- | Attempt to apply two 'Term'ings.
qapply :: Delta -> Term -> Term -> Result Term
qapply d a b = case (a, b) of
    (TLam k f, x) ->
        case f 0 x of
            Failure e -> Failure (pretty d <+> pretty (show k) <+> ": " <+> e)
            Success y -> return y
    (TVal x, _) -> throwError "unable to apply literal {} -> {}\n{}"
        [typeOf x, typeFun, show x]

class Unquote a where
    unquote :: Id -> Int -> Term -> Result a

instance Unquote Value where
    unquote k n = \case
        TVal v -> pure v
        _      -> unexpected k n typeFun "Literal"

instance Unquote Text where
    unquote k n = unquote k n >=> \case
        String t -> pure t
        v        -> unexpected k n (typeOf v) "String"

instance Unquote LText.Text where
    unquote k n = fmap LText.fromStrict . unquote k n

instance Unquote Bool where
    unquote k n = unquote k n >=> \case
        Bool b -> pure b
        v      -> unexpected k n (typeOf v) "Bool"

instance Unquote Int where
    unquote k n = unquote k n >=>
        maybe (unexpected k n "Number" "Int") pure
            . toBoundedInteger

instance Unquote Integer where
    unquote k n = unquote k n >=>
        either (const (unexpected k n "Number" "Integral")) pure
            . floatingOrInteger

instance Unquote Double where
    unquote k n = fmap toRealFloat . unquote k n

instance Unquote Scientific where
    unquote k n = unquote k n >=> \case
        Number d -> pure d
        v        -> unexpected k n (typeOf v) "Number"

instance Unquote Object where
    unquote k n = \case
        TVal (Object o) -> pure o
        TVal v          -> unexpected k n (typeOf v) "Object"
        _               -> unexpected k n typeFun "Object"

instance Unquote Array where
    unquote k n = \case
        TVal (Array a) -> pure a
        TVal v         -> unexpected k n (typeOf v) "Array"
        _              -> unexpected k n typeFun "Array"

instance Unquote Collection where
    unquote k n q =
            text    <$> unquote k n q
        <|> hashMap <$> unquote k n q
        <|> vector  <$> unquote k n q
      where
        text t = Col (Text.length t)
            . map (\c -> (Nothing, String (Text.singleton c)))
            $ Text.unpack t

        hashMap m = Col (Map.size m)
            . map (first Just)
            . sortBy (comparing fst)
            $ Map.toList m

        vector v = Col (Vector.length v) (Vector.map (Nothing,) v)

unexpected :: Id -> Int -> String -> String -> Result b
unexpected k n x y = throwError "unable to coerce {}:{}:{} -> {}" [show k, show n, x, y]

qprim :: (ToJSON a, Quote a) => a -> Term
qprim = quote "Value"

class Quote a where
    quote :: Id -> a -> Term

    default quote :: ToJSON a => Id -> a -> Term
    quote = const (TVal . toJSON)

instance (Unquote a, Quote b) => Quote (a -> b) where
    quote k f = TLam k (\n x -> (quote k . f <$> unquote k (succ n) x))

instance Quote Term where
    quote = const id

instance Quote Value
instance Quote Text
instance Quote [Text]
instance Quote LText.Text
instance Quote Bool
instance Quote Int
instance Quote Integer
instance Quote Double
instance Quote Scientific
instance Quote Object
instance Quote Array

instance Quote Builder where
    quote k = quote k . toLazyText
