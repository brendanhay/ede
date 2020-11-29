{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.EDE.Internal.Quoting
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
module Text.EDE.Internal.Quoting where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Array, Object, Value (..))
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Data.Text.Manipulate (toOrdinal)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Vector as Vector
import Text.EDE.Internal.Types
import Text.Trifecta.Delta (Delta)
import qualified Text.Trifecta.Delta as Trifecta.Delta
import qualified Text.Trifecta.Rendering as Trifecta.Rendering

default (AnsiDoc, Double, Integer)

-- | A HOAS representation of (possibly partially applied) values
-- in the environment.
data Term
  = TVal !Value
  | TLam (Term -> Result Term)

instance AnsiPretty Term where
  apretty = \case
    TLam _ -> "Function"
    TVal v -> bold (pp v)

-- | Fully apply two 'Term's.
qapply :: Delta -> Term -> Term -> Result Term
qapply d a b = case (a, b) of
  (TLam f, x) ->
    case f x of
      Failure e -> Failure (Trifecta.Delta.prettyDelta d <+> red "error:" <+> e)
      Success y -> pure y
  (TVal x, _) ->
    Failure $
      "unable to apply literal"
        <+> apretty a
        <+> "->"
        <+> apretty b
        </> pp x
{-# INLINEABLE qapply #-}

-- | Quote a primitive 'Value' from the top-level.
qprim :: (ToJSON a, Quote a) => a -> Term
qprim = quote "Value" 0
{-# INLINEABLE qprim #-}

class Unquote a where
  unquote :: Id -> Int -> Term -> Result a
  default unquote :: FromJSON a => Id -> Int -> Term -> Result a
  unquote k n = \case
    f@TLam {} -> typeErr k n (apretty f) "Value"
    TVal v ->
      case Aeson.fromJSON v of
        Aeson.Success x -> pure x
        Aeson.Error e -> argumentErr k n e
  {-# INLINEABLE unquote #-}

instance Unquote Value

instance Unquote Text

instance Unquote [Text]

instance Unquote Text.Lazy.Text

instance Unquote Bool

instance Unquote Double

instance Unquote Scientific

instance Unquote Object

instance Unquote Array

instance Unquote Int where
  unquote k n =
    unquote k n
      >=> maybe (typeErr k n "Double" "Int") pure
        . Scientific.toBoundedInteger
  {-# INLINEABLE unquote #-}

instance Unquote Integer where
  unquote k n =
    unquote k n
      >=> either (const (typeErr k n "Double" "Integral")) pure
        . Scientific.floatingOrInteger
  {-# INLINEABLE unquote #-}

instance Unquote Collection where
  unquote k n q =
    text <$> unquote k n q
      <|> hashMap <$> unquote k n q
      <|> vector <$> unquote k n q
    where
      text t =
        Col (Text.length t)
          . map (\c -> (Nothing, String (Text.singleton c)))
          $ Text.unpack t

      hashMap m =
        Col (HashMap.size m)
          . map (Bifunctor.first Just)
          . sortBy (comparing fst)
          $ HashMap.toList m

      vector v = Col (Vector.length v) (Vector.map (Nothing,) v)
  {-# INLINEABLE unquote #-}

class Quote a where
  quote :: Id -> Int -> a -> Term
  default quote :: ToJSON a => Id -> Int -> a -> Term
  quote _ _ = TVal . Aeson.toJSON
  {-# INLINEABLE quote #-}

instance (Unquote a, Quote b) => Quote (a -> b) where
  quote k n f = TLam $ \x -> quote k n' . f <$> unquote k n' x
    where
      n' = succ n
  {-# INLINEABLE quote #-}

instance Quote Term where
  quote _ _ = id
  {-# INLINEABLE quote #-}

instance Quote Value

instance Quote [Value]

instance Quote Text

instance Quote [Text]

instance Quote Text.Lazy.Text

instance Quote Bool

instance Quote Int

instance Quote Integer

instance Quote Double

instance Quote Scientific

instance Quote Object

instance Quote Array

instance Quote Builder where
  quote k n = quote k n . Text.Builder.toLazyText
  {-# INLINEABLE quote #-}

typeErr :: Id -> Int -> AnsiDoc -> AnsiDoc -> Result a
typeErr k n x y = Failure $ "type" <+> pp k <+> pretty n <+> x <+> "::" <+> y

argumentErr :: Pretty a => Id -> Int -> a -> Result b
argumentErr k n e = Failure $ if self then app else arg
  where
    app =
      "unable to apply"
        <+> bold (pp k)
        <+> "to left hand side:"
        </> PP.indent 4 (pretty e </> Trifecta.Rendering.prettyRendering mark)

    arg =
      "invalid"
        <+> bold (pp $ toOrdinal (n - 1))
        <+> "argument to"
        <+> bold (pp k)
        <> ":"
        </> PP.indent 4 (pretty e </> Trifecta.Rendering.prettyRendering mark)

    mark =
      Trifecta.Rendering.renderingCaret (Trifecta.Delta.Columns col col) $
        "... | " <> Text.Encoding.encodeUtf8 k <> line

    col
      | self = 1
      | otherwise = fromIntegral (Text.length k + 4 + (n * 2))

    line
      | self = "\n"
      | otherwise =
        "(" <> ByteString.Char8.intercalate ", " (replicate (n - 1) "...") <> "\n"

    self = n <= 1
