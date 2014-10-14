{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances       #-}
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

module Text.EDE.Filters
    (
    -- * Defaults
      defaultFilters

    -- * Constructing filters
    , Binding  (..)

    -- ** Classes
    , Quote   (..)
    , Unquote (..)

    -- ** Restricted quoters
    , qapp
    , bpoly
    , unum
    , bnum
    , useq

    -- ** Errors
    , unexpected
    , typeOf
    ) where

import           Data.Aeson              (Value, encode)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Vector             as Vector
import           Text.EDE.Internal.HOAS
import           Text.EDE.Text

default (Integer)

defaultFilters :: HashMap Text Binding
defaultFilters = Map.unions
    [ boolean
    , equality
    , relational
    , numeric
    , fractional
    , textual
    , collection
    , polymorphic
    ]

boolean :: HashMap Text Binding
boolean = Map.fromList
    [ "!"  @: quote not
    , "&&" @: quote (&&)
    , "||" @: quote (||)
    ]

equality :: HashMap Text Binding
equality = Map.fromList
    [ "==" @: bpoly (==)
    , "!=" @: bpoly (/=)
    ]

relational :: HashMap Text Binding
relational = Map.fromList
    [ ">"  @: bnum (>)
    , ">=" @: bnum (>=)
    , "<=" @: bnum (<=)
    , "<"  @: bnum (<)
    ]

numeric :: HashMap Text Binding
numeric = Map.fromList
    [ "+"      @: bnum (+)
    , "-"      @: bnum (-)
    , "*"      @: bnum (*)
    , "abs"    @: unum abs
    , "signum" @: unum signum
    , "negate" @: unum negate
    ]

fractional :: HashMap Text Binding
fractional = Map.fromList
    [ "truncate" @: unum (fromIntegral . truncate)
    , "round"    @: unum (fromIntegral . round)
    , "ceiling"  @: unum (fromIntegral . ceiling)
    , "floor"    @: unum (fromIntegral . floor)
    ]

textual :: HashMap Text Binding
textual = Map.fromList
    [ "lower"      @: quote Text.toLower
    , "upper"      @: quote Text.toUpper
    , "lowerFirst" @: quote lowerFirst
    , "upperFirst" @: quote upperFirst
    , "titleize"   @: quote titleize
    , "pascalize"  @: quote pascalize
    , "camelize"   @: quote camelize
    , "underscore" @: quote underscore
    , "hyphenate"  @: quote hyphenate
    ]

collection :: HashMap Text Binding
collection = Map.fromList
    [ "length" @: useq Text.length Map.size Vector.length
    , "empty"  @: useq Text.null Map.null Vector.null
    -- , ("join",  quote hyphenate
    ]

polymorphic :: HashMap Text Binding
polymorphic = Map.fromList
    [ "show" @: quote value
    ]

(@:) :: Quote a => Text -> a -> (Text, Binding)
k @: q = (k, quote q)

value :: Value -> LText.Text
value = LText.decodeUtf8 . encode
