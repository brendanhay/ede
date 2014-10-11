{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Style
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Style where

import           Control.Lens
import qualified Data.HashSet               as Set
import           Text.EDE.Internal.Keywords
import qualified Text.EDE.Internal.Keywords as Keywords
import           Text.Trifecta
import           Text.Parser.Token.Style

operator :: TokenParsing m => IdentifierStyle m
operator = haskellOps & styleLetter .~ oneOf "-+!&|=><"

keyword :: TokenParsing m => IdentifierStyle m
keyword = haskellIdents & styleReserved .~ Keywords.all & styleName .~ "keyword"

variable :: TokenParsing m => IdentifierStyle m
variable = keyword & styleName .~ "variable"
