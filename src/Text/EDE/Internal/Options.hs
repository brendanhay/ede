-- Module      : Text.EDE.Internal.Options
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Options where

import Data.Monoid

data Options = Options
    { delimiter  = '{'
    , comment    = '#'
    , block      = '%'
    , substitute = '{'
    }

make it possible to read in comments, then 'recomment' them with the target
languages commenting syntax.

don't bother evaluating things to 'Text', just to 'Lit'.
