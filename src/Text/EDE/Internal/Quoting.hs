{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Text.EDE.Internal.Quoting
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Quoting where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                   as A
import           Data.Aeson                   hiding (Result(..))
import           Data.Bifunctor
import qualified Data.ByteString.Char8        as BS
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (sortBy)
import           Data.Monoid
import           Data.Ord                     (comparing)
import           Data.Scientific
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import           Data.Text.Manipulate         (toOrdinal)
import qualified Data.Vector                  as Vector
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty(..), (<+>), (</>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta.Delta
import           Text.Trifecta.Rendering

default (Doc, Double, Integer)

-- | A HOAS representation of (possibly partially applied) values
-- in the environment.
data Term
    = TVal !Value
    | TLam (Term -> Result Term)

instance Pretty Term where
    pretty = \case
        TLam _ -> "Function"
        TVal v -> PP.bold (pp v)

-- | Fully apply two 'Term's.
qapply :: Delta -> Term -> Term -> Result Term
qapply d a b = case (a, b) of
    (TLam f, x) ->
        case f x of
            Failure e -> Failure (pretty d <+> PP.red "error:" <+> e)
            Success y -> return y
    (TVal x, _) -> Failure $
        "unable to apply literal"
            <+> pretty a
            <+> "->"
            <+> pretty b
            </> pp x

-- | Quote a primitive 'Value' from the top-level.
qprim :: (ToJSON a, Quote a) => a -> Term
qprim = quote "Value" 0

class Unquote a where
    unquote :: Id -> Int -> Term -> Result a

    default unquote :: FromJSON a => Id -> Int -> Term -> Result a
    unquote k n = \case
        f@TLam{} -> typeErr k n (pretty f) "Value"
        TVal v   ->
            case fromJSON v of
                A.Success x -> pure x
                A.Error   e -> argumentErr k n e

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
        maybe (typeErr k n "Double" "Int") pure
            . toBoundedInteger

instance Unquote Integer where
    unquote k n = unquote k n >=>
        either (const (typeErr k n "Double" "Integral")) pure
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

class Quote a where
    quote :: Id -> Int -> a -> Term

    default quote :: ToJSON a => Id -> Int -> a -> Term
    quote _ _ = TVal . toJSON

instance (Unquote a, Quote b) => Quote (a -> b) where
    quote k n f = TLam $ \x -> quote k n' . f <$> unquote k n' x
      where
        n' = succ n

instance Quote Term where
    quote _ _ = id

instance Quote Value
instance Quote [Value]
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
    quote k n = quote k n . toLazyText

typeErr :: Id -> Int -> Doc -> Doc -> Result a
typeErr k n x y = Failure $ "type" <+> pp k <+> pretty n <+> x <+> "::" <+> y

argumentErr :: Pretty a => Id -> Int -> a -> Result b
argumentErr k n e = Failure $ if self then app else arg
  where
    app = "unable to apply"
        <+> PP.bold (pp k)
        <+> "to left hand side:"
        </> PP.indent 4 (pretty e </> pretty mark)

    arg = "invalid"
        <+> PP.bold (pp $ toOrdinal (n - 1))
        <+> "argument to"
        <+> PP.bold (pp k)
        <>  ":"
        </> PP.indent 4 (pretty e </> pretty mark)

    mark = renderingCaret (Columns col col) $
        "... | " <> Text.encodeUtf8 k <> line

    col  | self      = 1
         | otherwise = fromIntegral (Text.length k + 4 + (n * 2))

    line | self      = "\n"
         | otherwise =
             "(" <> BS.intercalate ", " (replicate (n - 1) "...") <> "\n"

    self = n <= 1
