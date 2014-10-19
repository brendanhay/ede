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

-- | A default set of prelude-like filters and the means to construct your own.
module Text.EDE.Filters
    (
    -- * Prelude
      defaultFilters

    -- ** Boolean
    -- $boolean

    -- ** Equality
    -- $equality

    -- ** Relational
    -- $relational

    -- ** Numeric
    -- $numeric

    -- ** Fractional
    -- $fractional

    -- ** Textual
    -- $textual

    -- ** Collection
    -- $collection

    -- ** Polymorphic
    -- $polymorphic

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

-- $boolean
--
-- * @!@  @:: Bool -> Bool@ (/See:/ 'not')
--
-- * '&&' @:: Bool -> Bool -> Bool@
--
-- * '||' @:: Bool -> Bool -> Bool@

boolean :: HashMap Text Binding
boolean = Map.fromList
    [ "!"  @: quote not
    , "&&" @: quote (&&)
    , "||" @: quote (||)
    ]

-- $equality
--
-- * '==' @:: a -> a -> Bool@
--
-- * @!=@ @:: a -> a -> Bool@ (/See/: '/=')

equality :: HashMap Text Binding
equality = Map.fromList
    [ "==" @: qpoly2 (==)
    , "!=" @: qpoly2 (/=)
    ]

-- $relational
--
-- * '>'  @:: a -> a -> Bool@
--
-- * '>=' @:: a -> a -> Bool@
--
-- * '<=' @:: a -> a -> Bool@
--
-- * '<=' @:: a -> a -> Bool@

relational :: HashMap Text Binding
relational = Map.fromList
    [ ">"  @: qnum2 (>)
    , ">=" @: qnum2 (>=)
    , "<=" @: qnum2 (<=)
    , "<"  @: qnum2 (<)
    ]

-- $numeric
--
-- * '+'      @:: Number -> Number -> Number@
--
-- * '-'      @:: Number -> Number -> Number@
--
-- * '*'      @:: Number -> Number -> Number@
--
-- * 'abs'    @:: Number -> Number@
--
-- * 'signum' @:: Number -> Number@
--
-- * 'negate' @:: Number -> Number@

numeric :: HashMap Text Binding
numeric = Map.fromList
    [ "+"      @: qnum2 (+)
    , "-"      @: qnum2 (-)
    , "*"      @: qnum2 (*)
    , "abs"    @: qnum1 abs
    , "signum" @: qnum1 signum
    , "negate" @: qnum1 negate
    ]

-- $fractional
--
-- * 'truncate' @:: Number -> Number@
--
-- * 'round'    @:: Number -> Number@
--
-- * 'ceiling'  @:: Number -> Number@
--
-- * 'floor'    @:: Number -> Number@

fractional :: HashMap Text Binding
fractional = Map.fromList
    [ "truncate" @: qnum1 (fromIntegral . truncate)
    , "round"    @: qnum1 (fromIntegral . round)
    , "ceiling"  @: qnum1 (fromIntegral . ceiling)
    , "floor"    @: qnum1 (fromIntegral . floor)
    ]

-- $textual
--
-- * @takeWord@  @:: Text -> Text@
--
-- * @dropWord@  @:: Text -> Text@
--
-- * @lowerHead@ @:: Text -> Text@
--
-- * @upperHead@ @:: Text -> Text@
--
-- * @toTitle@   @:: Text -> Text@
--
-- * @toCamel@   @:: Text -> Text@
--
-- * @toPascal@  @:: Text -> Text@
--
-- * @toSnake@   @:: Text -> Text@
--
-- * @toSpinal@  @:: Text -> Text@
--
-- * @toTrain@   @:: Text -> Text@
--
-- * @toLower@   @:: Text -> Text@
--
-- * @toUpper@   @:: Text -> Text@
--
-- * @toOrdinal@ @:: Number -> Text@
--
-- /See:/ "Data.Text.Manipulate"

textual :: HashMap Text Binding
textual = Map.fromList
    [ "takeWord"  @: quote takeWord
    , "dropWord"  @: quote dropWord
    , "lowerHead" @: quote lowerHead
    , "upperHead" @: quote upperHead
    , "toTitle"   @: quote toTitle
    , "toCamel"   @: quote toCamel
    , "toPascal"  @: quote toPascal
    , "toSnake"   @: quote toSnake
    , "toSpinal"  @: quote toSpinal
    , "toTrain"   @: quote toTrain
    , "toUpper"   @: quote Text.toUpper
    , "toLower"   @: quote Text.toLower
    , "toOrdinal" @: (toOrdinal . truncate :: Scientific -> Text)
    ]

-- $collection
--
-- * @length@ @:: Collection -> Number@ (/See/: 'Data.Text.length', 'Data.Vector.length', 'Data.HashMap.Strict.size')
--
-- * @empty@  @:: Collection -> Bool@ (/See/: 'Data.Text.null', 'Data.Vector.null', 'Data.HashMap.Strict.null')

collection :: HashMap Text Binding
collection = Map.fromList
    [ "length" @: qcol1 Text.length Map.size Vector.length
    , "empty"  @: qcol1 Text.null   Map.null Vector.null
    -- , ("join",  quote
    ]

-- $polymorphic
--
-- * 'show' @:: a -> Text@

polymorphic :: HashMap Text Binding
polymorphic = Map.fromList
    [ "show" @: quote value
    ]

(@:) :: Quote a => Text -> a -> (Text, Binding)
k @: q = (k, quote q)

value :: Value -> LText.Text
value = LText.decodeUtf8 . encode
