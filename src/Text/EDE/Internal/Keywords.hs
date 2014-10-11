-- Module      : Text.EDE.Internal.Keywords
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Keywords where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.Monoid
import           Prelude      hiding (all)

all :: HashSet String
all = start <> end <> other

start :: HashSet String
start = Set.fromList
    [ "if"
    , "elif"
    , "else"
    , "case"
    , "when"
    , "for"
    , "include"
    , "let"
    ]

end :: HashSet String
end = Set.fromList
    [ "endif"
    , "endcase"
    , "endfor"
    , "endlet"
    ]

other :: HashSet String
other = Set.fromList
    [ "in"
    , "with"
    , "_"
    , "."
    , "true"
    , "false"
    ]
