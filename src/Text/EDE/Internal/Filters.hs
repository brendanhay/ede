{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Text.EDE.Internal.Filters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Filters where

import           Data.Aeson                   (Array, Object, Value (..),
                                               encode)
import qualified Data.Char                    as Char
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.Maybe
import           Data.Scientific              (Scientific)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import           Data.Text.Manipulate
import qualified Data.Text.Unsafe             as Text
import qualified Data.Vector                  as Vector
import           Text.EDE.Internal.Quoting
import           Text.EDE.Internal.Types
import           Data.Text.Prettyprint.Doc    ((<+>))

default (Integer)

stdlib :: HashMap Text Term
stdlib = Map.fromList
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

    , "dropLower"      @: Text.dropWhile (not . Char.isUpper)
    , "dropUpper"      @: Text.dropWhile (not . Char.isLower)
    , "takeWord"       @: takeWord
    , "dropWord"       @: dropWord
    , "splitWords"     @: splitWords
    , "strip"          @: Text.strip
    , "stripPrefix"    @: (\x p -> fromMaybe x (p `Text.stripPrefix` x))
    , "stripSuffix"    @: (\x s -> fromMaybe x (s `Text.stripSuffix` x))
    , "stripStart"     @: Text.stripStart
    , "stripEnd"       @: Text.stripEnd
    , "replace"        @: flip Text.replace
    , "remove"         @: (\x r -> Text.replace r "" x)

    , "toEllipsis"     @: flip toEllipsis
    , "toEllipsisWith" @: (\x n e -> toEllipsisWith n e x)

    , "indentLines"    @: flip indentLines
    , "prependLines"   @: flip prependLines
    , "justifyLeft"    @: (\x n -> Text.justifyLeft  n ' ' x)
    , "justifyRight"   @: (\x n -> Text.justifyRight n ' ' x)
    , "center"         @: (\x n -> Text.center       n ' ' x)

    -- sequences
    , qcol1 "length"   Text.length Map.size Vector.length
    , qcol1 "empty"    Text.null   Map.null Vector.null
    , qcol1 "reverse"  Text.reverse id Vector.reverse

    -- lists
    , qlist1 "head"    headT headV
    , qlist1 "last"    lastT lastV
    , qlist1 "tail"    lastT tailV
    , qlist1 "init"    initT initV

    -- object
    , "keys"           @: (Map.keys  :: Object -> [Text])
    , "elems"          @: (Map.elems :: Object -> [Value])

    -- , "map"        @: undefined
    -- , "filter"     @: undefined
    -- , "zip"        @: undefined
    -- , "join"       @: undefined

    -- polymorphic
    , "show"           @: (LText.decodeUtf8 . encode :: Value -> LText.Text)

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
qlist1 :: (Quote a, Quote b)
       => Id
       -> (Text   -> a)
       -> (Array  -> b)
       -> (Id, Term)
qlist1 k f g = (k,) . TLam $ \case
    TVal (String t) -> pure . quote k 0 $ f t
    TVal (Array  v) -> pure . quote k 0 $ g v
    x               -> Failure $
        "when expecting a String or Array, encountered" <+> apretty x

-- | Quote a comprehensive set of unary functions to create a binding
-- that supports all collection types.
qcol1 :: (Quote a, Quote b, Quote c)
      => Id
      -> (Text   -> a)
      -> (Object -> b)
      -> (Array  -> c)
      -> (Id, Term)
qcol1 k f g h = (k,) . TLam $ \case
    TVal (String t) -> pure . quote k 0 $ f t
    TVal (Object o) -> pure . quote k 0 $ g o
    TVal (Array  v) -> pure . quote k 0 $ h v
    x               -> Failure $
        "when expecting a String, Object, or Array, encountered" <+> apretty x

headT, lastT, tailT, initT :: Text -> Value
headT = text (Text.singleton . Text.unsafeHead)
lastT = text (Text.singleton . Text.last)
tailT = text Text.unsafeTail
initT = text Text.init

headV, lastV, tailV, initV :: Array -> Value
headV = vec Vector.unsafeHead
lastV = vec Vector.unsafeLast
tailV = vec (Array . Vector.unsafeTail)
initV = vec (Array . Vector.unsafeInit)

text :: (Text -> Text) -> Text -> Value
text f = String . safe mempty Text.null f

vec :: (Array -> Value) -> Array -> Value
vec = safe (Array Vector.empty) Vector.null

safe :: b -> (a -> Bool) -> (a -> b) -> a -> b
safe v f g x
    | f x       = v
    | otherwise = g x
