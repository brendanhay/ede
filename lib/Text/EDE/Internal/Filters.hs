{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Text.EDE.Internal.Filters
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
module Text.EDE.Internal.Filters where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Value (..))
import qualified Data.Char as Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Text.Manipulate as Text.Manipulate
import qualified Data.Text.Unsafe as Text.Unsafe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prettyprinter ((<+>))
import Text.EDE.Internal.Compat
import Text.EDE.Internal.Quoting
import Text.EDE.Internal.Types

default (Integer)

stdlib :: HashMap Text Term
stdlib =
  HashMap.fromList
    -- boolean
    [ "!" @: not,
      "&&" @: (&&),
      "||" @: (||),
      -- equality
      "==" `qpoly2` (==),
      "!=" `qpoly2` (/=),
      -- relational
      ">" `qnum2` (>),
      ">=" `qnum2` (>=),
      "<=" `qnum2` (<=),
      "<" `qnum2` (<),
      -- numeric
      "+" `qnum2` (+),
      "-" `qnum2` (-),
      "*" `qnum2` (*),
      "abs" `qnum1` abs,
      "signum" `qnum1` signum,
      "negate" `qnum1` negate,
      -- fractional
      "truncate" `qnum1` (fromIntegral . truncate),
      "round" `qnum1` (fromIntegral . round),
      "ceiling" `qnum1` (fromIntegral . ceiling),
      "floor" `qnum1` (fromIntegral . floor),
      -- text
      "lowerHead" @: Text.Manipulate.lowerHead,
      "upperHead" @: Text.Manipulate.upperHead,
      "toTitle" @: Text.Manipulate.toTitle,
      "toCamel" @: Text.Manipulate.toCamel,
      "toPascal" @: Text.Manipulate.toPascal,
      "toSnake" @: Text.Manipulate.toSnake,
      "toSpinal" @: Text.Manipulate.toSpinal,
      "toTrain" @: Text.Manipulate.toTrain,
      "toUpper" @: Text.toUpper,
      "toLower" @: Text.toLower,
      "toOrdinal" @: (Text.Manipulate.toOrdinal :: Integer -> Text),
      "dropLower" @: Text.dropWhile (not . Char.isUpper),
      "dropUpper" @: Text.dropWhile (not . Char.isLower),
      "takeWord" @: Text.Manipulate.takeWord,
      "dropWord" @: Text.Manipulate.dropWord,
      "splitWords" @: Text.Manipulate.splitWords,
      "strip" @: Text.strip,
      "stripPrefix" @: (\x p -> Maybe.fromMaybe x (p `Text.stripPrefix` x)),
      "stripSuffix" @: (\x s -> Maybe.fromMaybe x (s `Text.stripSuffix` x)),
      "stripStart" @: Text.stripStart,
      "stripEnd" @: Text.stripEnd,
      "replace" @: flip Text.replace,
      "remove" @: (\x r -> Text.replace r "" x),
      "toEllipsis" @: flip Text.Manipulate.toEllipsis,
      "toEllipsisWith" @: (\x n e -> Text.Manipulate.toEllipsisWith n e x),
      "indentLines" @: flip Text.Manipulate.indentLines,
      "prependLines" @: flip Text.Manipulate.prependLines,
      "justifyLeft" @: (\x n -> Text.justifyLeft n ' ' x),
      "justifyRight" @: (\x n -> Text.justifyRight n ' ' x),
      "center" @: (\x n -> Text.center n ' ' x),
      -- sequences
      qcol1 "length" Text.length HashMap.size Vector.length,
      qcol1 "empty" Text.null HashMap.null Vector.null,
      qcol1 "reverse" Text.reverse id Vector.reverse,
      -- lists
      qlist1 "head" headT headV,
      qlist1 "last" lastT lastV,
      qlist1 "tail" tailT tailV,
      qlist1 "init" initT initV,
      "at" @: (\x i -> x Vector.! i :: Value),
      -- object
      "keys" @: (HashMap.keys :: HashMap Text Value -> [Text]),
      "elems" @: (HashMap.elems :: HashMap Text Value -> [Value]),
      -- , "map"        @: undefined
      -- , "filter"     @: undefined
      -- , "zip"        @: undefined
      -- , "join"       @: undefined

      -- polymorphic
      "show" @: (Text.Lazy.Encoding.decodeUtf8 . Aeson.encode :: Value -> Text.Lazy.Text),
      "singleton" @: (pure :: Value -> Vector Value)
      -- FIXME: existence checks currently hardcoded into the evaluator:
      -- "default"
      -- "defined"
    ]

(@:) :: Quote a => Id -> a -> (Id, Term)
k @: q = (k, quote k 0 q)

-- | Quote a binary function which takes the most general binding value.
qpoly2 :: Quote a => Id -> (Value -> Value -> a) -> (Id, Term)
qpoly2 k = (k,) . quote k 0

-- | Quote an unary numeric function.
qnum1 :: Id -> (Scientific -> Scientific) -> (Id, Term)
qnum1 k = (k,) . quote k 0

-- | Quote a binary numeric function.
qnum2 :: Quote a => Id -> (Scientific -> Scientific -> a) -> (Id, Term)
qnum2 k = (k,) . quote k 0

-- | Quote a comprehensive set of unary functions to create a binding
-- that supports list collection types.
qlist1 ::
  (Quote a, Quote b) =>
  Id ->
  (Text -> a) ->
  (Vector Value -> b) ->
  (Id, Term)
qlist1 k f g = (k,) . TLam $ \case
  TVal (String t) -> pure . quote k 0 $ f t
  TVal (Array v) -> pure . quote k 0 $ g v
  x ->
    Failure $
      "when expecting a String or Array, encountered" <+> apretty x

-- | Quote a comprehensive set of unary functions to create a binding
-- that supports all collection types.
qcol1 ::
  (Quote a, Quote b, Quote c) =>
  Id ->
  (Text -> a) ->
  (HashMap Text Value -> b) ->
  (Vector Value -> c) ->
  (Id, Term)
qcol1 k f g h = (k,) . TLam $ \case
  TVal (String t) -> pure . quote k 0 $ f t
  TVal (Object o) -> pure . quote k 0 $ g (toHashMapText o)
  TVal (Array v) -> pure . quote k 0 $ h v
  x ->
    Failure $
      "when expecting a String, Object, or Array, encountered" <+> apretty x

headT, lastT, tailT, initT :: Text -> Value
headT = text (Text.singleton . Text.Unsafe.unsafeHead)
lastT = text (Text.singleton . Text.last)
tailT = text Text.Unsafe.unsafeTail
initT = text Text.init

headV, lastV, tailV, initV :: Vector Value -> Value
headV = vec Vector.unsafeHead
lastV = vec Vector.unsafeLast
tailV = vec (Array . Vector.unsafeTail)
initV = vec (Array . Vector.unsafeInit)

text :: (Text -> Text) -> Text -> Value
text f = String . safe mempty Text.null f

vec :: (Vector Value -> Value) -> Vector Value -> Value
vec = safe (Array Vector.empty) Vector.null

safe :: b -> (a -> Bool) -> (a -> b) -> a -> b
safe v f g x
  | f x = v
  | otherwise = g x
