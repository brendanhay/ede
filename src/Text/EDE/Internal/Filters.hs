{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Text.EDE.Filters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Filters where

import           Control.Applicative
import           Data.Aeson              (Value(..), Array, Object, encode)
import qualified Data.Char               as Char
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific         (Scientific)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText
import           Data.Text.Manipulate
import qualified Data.Vector             as Vector
import           Text.EDE.Internal.HOAS
import           Text.EDE.Internal.Types

default (Integer)

defaultFilters :: HashMap Text Term
defaultFilters = Map.fromList
    -- boolean
    [ "!"              @: not
    , "&&"             @: (&&)
    , "||"             @: (||)

    -- equality
    , "=="            `qpoly2` (==)
    , "!="            `qpoly2` (/=)

    -- relational
    , ">"             `qnum2` (>)
    , ">="            `qnum2` (>=)
    , "<="            `qnum2` (<=)
    , "<"             `qnum2` (<)

    -- numeric
    , "+"             `qnum2` (+)
    , "-"             `qnum2` (-)
    , "*"             `qnum2` (*)
    , "abs"           `qnum1` abs
    , "signum"        `qnum1` signum
    , "negate"        `qnum1` negate

    -- fractional
    , "truncate"      `qnum1` (fromIntegral . truncate)
    , "round"         `qnum1` (fromIntegral . round)
    , "ceiling"       `qnum1` (fromIntegral . ceiling)
    , "floor"         `qnum1` (fromIntegral . floor)

    -- text
    , "lowerHead"      @: lowerHead
    , "upperHead"      @: upperHead
    , "toTitle"        @: toTitle
    , "toCamel"        @: toCamel
    , "toPascal"       @: toPascal
    , "toSnake"        @: toSnake
    , "toSpinal"       @: toSpinal
    , "toTrain"        @: toTrain
    , "toUpper"        @: Text.toUpper
    , "toLower"        @: Text.toLower
    , "toOrdinal"      @: (toOrdinal :: Integer -> Text)

    , "dropLower"      @: (Text.dropWhile (not . Char.isUpper))
    , "dropUpper"      @: (Text.dropWhile (not . Char.isLower))
    , "takeWord"       @: takeWord
    , "dropWord"       @: dropWord
    , "splitWords"     @: splitWords
    , "strip"          @: Text.strip
    , "stripPrefix"    @: (\p t -> fromMaybe t (p `Text.stripPrefix` t))
    , "stripSuffix"    @: (\s t -> fromMaybe t (s `Text.stripSuffix` t))
    , "stripStart"     @: Text.stripStart
    , "stripEnd"       @: Text.stripEnd
    , "replace"        @: Text.replace
    , "remove"         @: (\t -> Text.replace t "")

    , "toEllipsis"     @: toEllipsis
    , "toEllipsisWith" @: toEllipsisWith

    , "indentLines"    @: indentLines
    , "prependLines"   @: prependLines
    , "justifyLeft"    @: (\n -> Text.justifyLeft  n ' ')
    , "justifyRight"   @: (\n -> Text.justifyRight n ' ')
    , "center"         @: (\n -> Text.center       n ' ')

    -- sequences
    , qcol1 "length"   Text.length Map.size Vector.length
    , qcol1 "empty"    Text.null   Map.null Vector.null

    -- , "sort"       @: qcol1 (Text.pack . sort . Text.unpack) id (Vector.fromList . sort . Vector.toList)
    , qcol1 "reverse"  Text.reverse id Vector.reverse

    -- , "head"       @: undefined
    -- , "tail"       @: undefined
    -- , "init"       @: undefined
    -- , "last"       @: undefined

    -- , "map"        @: undefined
    -- , "filter"     @: undefined
    -- , "zip"        @: undefined
    -- , "join"       @: undefined

    -- polymorphic
    , "show"           @: (LText.decodeUtf8 . encode :: Value -> LText.Text)
    -- , "default"      @: undefined
    -- , "defined"      @: undefined
    ]

(@:) :: Quote a => Id -> a -> (Id, Term)
k @: q = (k, quote k q)

-- | Quote a binary function which takes the most general binding value.
qpoly2 :: Quote a => Id -> (Value -> Value -> a) -> (Id, Term)
qpoly2 k = (k,) . quote k

-- | Quote an unary numeric function.
qnum1 :: Id -> (Scientific -> Scientific) -> (Id, Term)
qnum1 k = (k,) . quote k

-- | Quote a binary numeric function.
qnum2 :: Quote a => Id -> (Scientific -> Scientific -> a) -> (Id, Term)
qnum2 k = (k,) . quote k

-- | Quote a comprehensive set of unary functions to create a binding
-- that supports all collection types.
qcol1 :: (Quote a, Quote b, Quote c)
      => Id
      -> (Text   -> a)
      -> (Object -> b)
      -> (Array  -> c)
      -> (Id, Term)
qcol1 k f g h = (k,) . TLam k $ \n x ->
    case x of
        TVal (String t) -> pure . quote k $ f t
        TVal (Object o) -> pure . quote k $ g o
        TVal (Array  v) -> pure . quote k $ h v
        TVal y          -> err (typeOf y)
        _               -> err typeFun
  where
    err = throwError "expected a String, Object, or Array, but got {}" . (:[])
