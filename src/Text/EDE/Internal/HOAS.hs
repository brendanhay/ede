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
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Monad
import           Data.Aeson              hiding (Result(..))
import           Data.Bifunctor
import qualified Data.HashMap.Strict     as Map
import           Data.List               (sortBy)
import           Data.Ord                (comparing)
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder
import qualified Data.Vector             as Vector
import           Text.EDE.Internal.Types
import           Text.Trifecta.Delta

default (Double, Integer)

-- | A HOAS representation of (possibly partially applied) values
-- in the environment.
data Binding a
    = BVal !Value
    | BLam (a -> Result a)

instance Show (Binding a) where
    show (BVal v) = show v
    show _        = "<function>"

instance Eq (Binding a) where
    BVal a == BVal b = a == b
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

-- | The default type for partially applied 'Binding's in error messages.
typeFun :: String
typeFun = "Function"

-- -- | Attempt to apply two 'Binding's.
-- qapply :: Binding -> Binding -> Result Binding
-- qapply a b = case (a, b) of
--     (BLam f, x) -> f x
--     (BVal x, _) -> throwError "unable to apply literal {} -> {}\n{}"
--         [typeOf x, typeFun, show x]

-- -- | Quote a binary function which takes the most general binding value.
-- qpoly2 :: Quote a => (Value -> Value -> a) -> Binding
-- qpoly2 = quote

-- -- | Quote an unary numeric function.
-- qnum1 :: (Scientific -> Scientific) -> Binding
-- qnum1 = quote

-- -- | Quote a binary numeric function.
-- qnum2 :: Quote a => (Scientific -> Scientific -> a) -> Binding
-- qnum2 = quote

-- -- | Quote a comprehensive set of unary functions to create a binding
-- -- that supports all collection types.
-- qcol1 :: (Quote a, Quote b, Quote c)
--       => (Text   -> a)
--       -> (Object -> b)
--       -> (Array  -> c)
--       -> Binding
-- qcol1 f g h = BLam $ \case
--     BVal (String t) -> pure . quote $ f t
--     BVal (Object o) -> pure . quote $ g o
--     BVal (Array  v) -> pure . quote $ h v
--     BVal y          -> err (typeOf y)
--     _               -> err typeFun
--   where
--     err = throwError "expected a String, Object, or Array, but got {}" . (:[])

class Quote a where
    quote :: a -> Mu Binding

instance Quote (Mu Binding) where
    quote = id

instance Quote Value where
    quote = Mu . BVal

-- instance Quote Text where
--     quote = BVal . String

-- instance Quote [Text] where
--     quote = BVal . toJSON

-- instance Quote LText.Text where
--     quote = quote . LText.toStrict

-- instance Quote Builder where
--     quote = quote . toLazyText

-- instance Quote Bool where
--     quote = BVal . Bool

-- instance Quote Int where
--     quote = BVal . Number . fromIntegral

-- instance Quote Integer where
--     quote = BVal . Number . fromInteger

-- instance Quote Double where
--     quote = BVal . Number . fromFloatDigits

-- instance Quote Scientific where
--     quote = BVal . Number

-- instance Quote Object where
--     quote = BVal . Object

-- instance Quote Array where
--     quote = BVal . Array

-- class Unquote a where
--     unquote :: Binding -> Result a

-- instance Unquote Value where
--     unquote = \case
--         BVal v -> pure v
--         _      -> unexpected typeFun "Literal"

-- instance Unquote Text where
--     unquote = unquote >=> \case
--         String t -> pure t
--         v        -> unexpected (typeOf v) "String"

-- instance Unquote LText.Text where
--     unquote = fmap LText.fromStrict . unquote

-- instance Unquote Bool where
--     unquote = unquote >=> \case
--         Bool b -> pure b
--         v      -> unexpected (typeOf v) "Bool"

-- instance Unquote Int where
--     unquote = unquote >=>
--         maybe (unexpected "Number" "Int") pure . toBoundedInteger

-- instance Unquote Integer where
--     unquote = unquote >=>
--         either (const (unexpected "Number" "Integral")) pure . floatingOrInteger

-- instance Unquote Double where
--     unquote = fmap toRealFloat . unquote

-- instance Unquote Scientific where
--     unquote = unquote >=> \case
--         Number n -> pure n
--         v        -> unexpected (typeOf v) "Number"

-- instance Unquote Collection where
--     unquote q = text    <$> unquote q
--             <|> hashMap <$> unquote q
--             <|> vector  <$> unquote q
--       where
--         text t = Col (Text.length t)
--             . map (\c -> (Nothing, String (Text.singleton c)))
--             $ Text.unpack t

--         hashMap m = Col (Map.size m)
--             . map (first Just)
--             . sortBy (comparing fst)
--             $ Map.toList m

--         vector v = Col (Vector.length v) (Vector.map (Nothing,) v)

-- instance Unquote Object where
--     unquote = \case
--         BVal (Object o) -> pure o
--         BVal v          -> unexpected (typeOf v) "Object"
--         _               -> unexpected typeFun "Object"

-- instance Unquote Array where
--     unquote = \case
--         BVal (Array a) -> pure a
--         BVal v         -> unexpected (typeOf v) "Array"
--         _              -> unexpected typeFun "Array"

-- instance (Unquote a, Quote b) => Quote (a -> b) where
--     quote f = BLam (fmap (quote . f) . unquote)

-- unexpected :: String -> String -> Result b
-- unexpected x y = throwError "unable to coerce {} -> {}" [x, y]
