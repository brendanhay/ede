{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

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

import           Data.Aeson              (Value, encode)
import qualified Data.Char               as Char
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.List               (sort)
import           Data.Maybe
import           Data.Scientific         (Scientific)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText
import           Data.Text.Manipulate
import qualified Data.Vector             as Vector
import           Text.EDE.Internal.HOAS

default (Integer)

defaultFilters :: HashMap Text Binding
defaultFilters = Map.fromList
    -- boolean
    [ "!"            @: not
    , "&&"           @: (&&)
    , "||"           @: (||)

    -- equality
    , "=="           @: qpoly2 (==)
    , "!="           @: qpoly2 (/=)

    -- relational
    , ">"            @: qnum2 (>)
    , ">="           @: qnum2 (>=)
    , "<="           @: qnum2 (<=)
    , "<"            @: qnum2 (<)

    -- numeric
    , "+"            @: qnum2 (+)
    , "-"            @: qnum2 (-)
    , "*"            @: qnum2 (*)
    , "abs"          @: qnum1 abs
    , "signum"       @: qnum1 signum
    , "negate"       @: qnum1 negate

    -- fractional
    , "truncate"     @: qnum1 (fromIntegral . truncate)
    , "round"        @: qnum1 (fromIntegral . round)
    , "ceiling"      @: qnum1 (fromIntegral . ceiling)
    , "floor"        @: qnum1 (fromIntegral . floor)

    -- text
    , "lowerHead"    @: lowerHead
    , "upperHead"    @: upperHead
    , "toTitle"      @: toTitle
    , "toCamel"      @: toCamel
    , "toPascal"     @: toPascal
    , "toSnake"      @: toSnake
    , "toSpinal"     @: toSpinal
    , "toTrain"      @: toTrain
    , "toUpper"      @: Text.toUpper
    , "toLower"      @: Text.toLower
    , "toOrdinal"    @: (toOrdinal :: Integer -> Text)

    , "dropLower"    @: (Text.dropWhile (not . Char.isUpper))
    , "dropUpper"    @: (Text.dropWhile (not . Char.isLower))
    , "takeWord"     @: takeWord
    , "dropWord"     @: dropWord
    , "splitWords"   @: splitWords
    , "strip"        @: Text.strip
    , "stripPrefix"  @: (\p t -> fromMaybe t (p `Text.stripPrefix` t))
    , "stripSuffix"  @: (\s t -> fromMaybe t (s `Text.stripSuffix` t))
    , "stripStart"   @: Text.stripStart
    , "stripEnd"     @: Text.stripEnd
    , "replace"      @: Text.replace
    , "remove"       @: (\t -> Text.replace t "")

-- truncate
-- Return a truncated copy of the string. The length is specified with the first parameter which defaults to 255. If the second parameter is true the filter will cut the text at length. Otherwise it will discard the last word. If the text was in fact truncated it will append an ellipsis sign ("..."). If you want a different ellipsis sign than "..." you can specify it using the third parameter.

    , "indentLines"  @: indentLines
    , "prependLines" @: prependLines
    , "justifyLeft"  @: (\n -> Text.justifyLeft  n ' ')
    , "justifyRight" @: (\n -> Text.justifyRight n ' ')
    , "center"       @: (\n -> Text.center       n ' ')

    -- sequences
    , "length"       @: qcol1 Text.length Map.size Vector.length
    , "empty"        @: qcol1 Text.null   Map.null Vector.null

    -- , "sort"         @: qcol1 (Text.pack . sort . Text.unpack) id (Vector.fromList . sort . Vector.toList)
    , "reverse"      @: qcol1 Text.reverse id Vector.reverse

    -- , "head"         @: undefined
    -- , "tail"         @: undefined
    -- , "init"         @: undefined
    -- , "last"         @: undefined

    -- , "map"          @: undefined
    -- , "filter"       @: undefined
    -- , "zip"          @: undefined
    -- , "join"         @: undefined

    -- polymorphic
    , "show"         @: (LText.decodeUtf8 . encode :: Value -> LText.Text)
    -- , "default"      @: undefined
    -- , "defined"      @: undefined
    ]

(@:) :: Quote a => Text -> a -> (Text, Binding)
k @: q = (k, quote q)
