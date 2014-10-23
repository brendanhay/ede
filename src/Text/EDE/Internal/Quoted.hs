{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Text.EDE.Internal.Quoted
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Quoted where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                   as A
import           Data.Aeson                   hiding (Result(..))
import           Data.Bifunctor
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (sortBy)
import           Data.Monoid
import           Data.Ord                     (comparing)
import           Data.Scientific
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import           Data.Text.Manipulate         (toOrdinal)
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
    show (TVal   v) = show v
    show (TLam k _) = Text.unpack ("<function:" <> k <> ">")

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

-- | Fully apply two 'Term's.
qapply :: Delta -> Term -> Term -> Result Term
qapply d a b = case (a, b) of
    (TLam k f, x) ->
        case f 0 x of
            Failure e -> Failure (pretty d <+> pretty (show k) <+> ": " <+> e)
            Success y -> return y
    (TVal x, _) -> throwError "unable to apply literal {} -> {}\n{}"
        [typeOf x, typeFun, show x]

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

class Unquote a where
    unquote :: Id -> Int -> Term -> Result a

    default unquote :: FromJSON a => Id -> Int -> Term -> Result a
    unquote k n = \case
        TLam{} -> unexpected k n typeFun "Literal"
        TVal v ->
            case fromJSON v of
                A.Success x -> pure x
                A.Error   e ->
                    throwError "type error in {} argument to {}: {}"
                        [toOrdinal n, k, Text.pack e]

instance Unquote Value
instance Unquote Text
instance Unquote [Text]
instance Unquote LText.Text
instance Unquote Bool
instance Unquote Double
instance Unquote Scientific
instance Unquote Object
instance Unquote Array

instance Unquote Int where
    unquote k n = unquote k n >=>
        maybe (unexpected k n "Double" "Int") pure
            . toBoundedInteger

instance Unquote Integer where
    unquote k n = unquote k n >=>
        either (const (unexpected k n "Double" "Integral")) pure
            . floatingOrInteger

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
unexpected k n x y = throwError "unable to coerce {}:{}:{} -> {}"
    [show k, show n, x, y]
