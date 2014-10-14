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
    , Binding (..)

    -- ** Classes
    , Quote   (..)
    , Unquote (..)

    -- ** Restricted quoters
    , qapply
    , qpoly2
    , qnum1
    , qnum2
    , qcol1

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
    [ "==" @: qpoly2 (==)
    , "!=" @: qpoly2 (/=)
    ]

relational :: HashMap Text Binding
relational = Map.fromList
    [ ">"  @: qnum2 (>)
    , ">=" @: qnum2 (>=)
    , "<=" @: qnum2 (<=)
    , "<"  @: qnum2 (<)
    ]

numeric :: HashMap Text Binding
numeric = Map.fromList
    [ "+"      @: qnum2 (+)
    , "-"      @: qnum2 (-)
    , "*"      @: qnum2 (*)
    , "abs"    @: qnum1 abs
    , "signum" @: qnum1 signum
    , "negate" @: qnum1 negate
    ]

fractional :: HashMap Text Binding
fractional = Map.fromList
    [ "truncate" @: qnum1 (fromIntegral . truncate)
    , "round"    @: qnum1 (fromIntegral . round)
    , "ceiling"  @: qnum1 (fromIntegral . ceiling)
    , "floor"    @: qnum1 (fromIntegral . floor)
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
    [ "length" @: qcol1 Text.length Map.size Vector.length
    , "empty"  @: qcol1 Text.null   Map.null Vector.null
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