-- Module      : Text.EDE.Filters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The means to construct your own filters.
module Text.EDE.Filters
    (
    -- * Prelude
    -- $prelude

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
      Term    (..)

    -- ** Classes
    , Quote   (..)
    , Unquote (..)

    -- ** Restricted quoters
    , (@:)
    , qapply
    , qpoly2
    , qnum1
    , qnum2
    , qcol1

    -- ** Errors
    , unexpected
    , typeOf
    ) where

import Text.EDE.Internal.HOAS
import Text.EDE.Internal.Filters

-- $prelude
--
-- The default filters available to a template are documented by the subsequent categories.
--
-- These filters cannot be overriden and attempting to supply your own filters to
-- 'Text.EDE.renderWith' will cause the similarly named filters to disappear when
-- they are merged with the prelude during evaluation. (/See:/ 'Data.HashMap.Strict.union')

-- $boolean
--
-- * @!@  @:: Bool -> Bool@ (/See:/ 'not')
--
-- * '&&' @:: Bool -> Bool -> Bool@
--
-- * '||' @:: Bool -> Bool -> Bool@

-- $equality
--
-- * '==' @:: a -> a -> Bool@
--
-- * @!=@ @:: a -> a -> Bool@ (/See/: '/=')

-- $relational
--
-- * '>'  @:: a -> a -> Bool@
--
-- * '>=' @:: a -> a -> Bool@
--
-- * '<=' @:: a -> a -> Bool@
--
-- * '<=' @:: a -> a -> Bool@

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

-- $fractional
--
-- * 'truncate' @:: Number -> Number@
--
-- * 'round'    @:: Number -> Number@
--
-- * 'ceiling'  @:: Number -> Number@
--
-- * 'floor'    @:: Number -> Number@

-- $textual
--
-- * 'Data.Text.Manipulate.takeWord'  @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.dropWord'  @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.lowerHead' @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.upperHead' @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toTitle'   @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toCamel'   @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toPascal'  @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toSnake'   @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toSpinal'  @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toTrain'   @:: Text -> Text@
--
-- * 'Data.Text.toLower'              @:: Text -> Text@
--
-- * 'Data.Text.toUpper'              @:: Text -> Text@
--
-- * 'Data.Text.Manipulate.toOrdinal' @:: Number -> Text@

-- $collection
--
-- * @length@ @:: Collection -> Number@ (/See/: Text.'Data.Text.length', Vector.'Data.Vector.length', HashMap.'Data.HashMap.Strict.size')
--
-- * @empty@  @:: Collection -> Bool@ (/See/: Text:'Data.Text.null', Vector.'Data.Vector.null', HashMap.'Data.HashMap.Strict.null')

-- $polymorphic
--
-- * 'show' @:: a -> Text@
