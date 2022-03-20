-- |
-- Module      : Text.EDE.Filters
-- Copyright   : (c) 2013-2022 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The means to construct your own filters.
module Text.EDE.Filters
  ( -- * Prelude
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
    Term (..),

    -- ** Classes
    Quote (..),
    Unquote (..),

    -- ** Restricted quoters
    (@:),
    qapply,
    qpoly2,
    qnum1,
    qnum2,
    qcol1,

    -- ** Errors
    typeErr,
    argumentErr,
  )
where

import Text.EDE.Internal.Filters
import Text.EDE.Internal.Quoting

-- $prelude
--
-- The default filters available to a template are documented by the subsequent categories.
--
-- These filters cannot be overriden and attempting to supply your own filters to
-- 'Text.EDE.renderWith' will cause the similarly named filters to disappear when
-- they are merged with the prelude during evaluation. (/See:/ 'Data.HashMap.Strict.union')

-- $boolean
--
-- [@! :: Bool -> Bool@]
-- /See:/ 'not'
--
-- [@'&&' :: Bool -> Bool -> Bool@]
--
-- [@'||' :: Bool -> Bool -> Bool@]

-- $equality
--
-- [@'==' :: a -> a -> Bool@]
--
-- [@!= :: a -> a -> Bool@]
-- /See/: '/='

-- $relational
--
-- [@'>' :: a -> a -> Bool@]
--
-- [@'>=' :: a -> a -> Bool@]
--
-- [@'<=' :: a -> a -> Bool@]
--
-- [@'<' :: a -> a -> Bool@]

-- $numeric
--
-- [@'+' :: Scientific -> Scientific -> Scientific@]
--
-- [@'-' :: Scientific -> Scientific -> Scientific@]
--
-- [@'*' :: Scientific -> Scientific -> Scientific@]
--
-- [@'abs' :: Scientific -> Scientific@]
--
-- [@'negate' :: Scientific -> Scientific@]
--
-- [@'signum' :: Scientific -> Scientific@]

-- $fractional
--
-- [@'ceiling' :: Scientific -> Scientific@]
--
-- [@'floor' :: Scientific -> Scientific@]
--
-- [@'round' :: Scientific -> Scientific@]
--
-- [@'truncate' :: Scientific -> Scientific@]

-- $textual
--
-- [@dropLower :: Text -> Text@]
-- Drop preceding lowercase characters.
--
-- [@'dropUpper' :: Text -> Text@]
-- Drop preceding uppercase characters.
--
-- [@'Data.Text.Manipulate.indentLines' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.prependLines' :: Text -> Text@]
--
-- [@'Data.Text.justifyLeft' :: Text -> Text@]
-- Using whitespace as fill character.
--
-- [@'Data.Text.justifyRight' :: Text -> Text@]
-- Using whitespace as fill character.
--
-- [@'Data.Text.center' :: Text -> Text@]
-- Using whitespace as fill character.
--
-- [@'Data.Text.replace' :: Text -> Text@]
--
-- [@remove@ @:: Text -> Text@]
-- Shortcut for: @replace(pattern, "")@
--
-- [@'Data.Text.Manipulate.splitWords' :: Text -> Text@]
--
-- [@'Data.Text.strip' :: Text -> Text@]
--
-- [@'Data.Text.stripPrefix' :: Text -> Text@]
--
-- [@'Data.Text.stripSuffix' :: Text -> Text@]
--
-- [@'Data.Text.stripStart' :: Text -> Text@]
--
-- [@'Data.Text.stripEnd' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.takeWord' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.dropWord' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.lowerHead' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.upperHead' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toCamel' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toEllipsis' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toEllipsisWith' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toPascal' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toSnake' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toSpinal' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toTitle' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toTrain' :: Text -> Text@]
--
-- [@'Data.Text.toLower' :: Text -> Text@]
--
-- [@'Data.Text.toUpper' :: Text -> Text@]
--
-- [@'Data.Text.Manipulate.toOrdinal' :: Scientific -> Text@]

-- $collection
--
-- [@length :: Collection -> Scientific@]
-- /See/: Text.'Data.Text.length', Vector.'Data.Vector.length', HashMap.'Data.HashMap.Strict.size'
--
-- [@empty :: Collection -> Bool@]
-- /See/: Text.'Data.Text.null', Vector.'Data.Vector.null', HashMap.'Data.HashMap.Strict.null'
--
-- [@reverse :: Collection -> Scientific@]
-- /See/: Text.'Data.Text.reverse', Vector.'Data.Vector.reverse'
--
-- [@head :: Collection -> Scientific@]
-- /See/: Text.'Data.Text.head', Vector.'Data.Vector.head'
--
-- [@last :: Collection -> Scientific@]
-- /See/: Text.'Data.Text.last', Vector.'Data.Vector.last'
--
-- [@tail :: Collection -> Scientific@]
-- /See/: Text.'Data.Text.tail', Vector.'Data.Vector.tail'
--
-- [@init@ @:: Collection -> Scientific@]
-- /See/: Text.'Data.Text.init', Vector.'Data.Vector.init'
--
-- [@'Data.HashMap.Strict.keys' :: Collection -> Scientific@]
--
-- [@'Data.HashMap.Strict.elems' :: Collection -> Scientific@]

-- $polymorphic
--
-- [@'show' :: a -> Text@]
