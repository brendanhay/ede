{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Syntax
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Syntax where

import           Control.Lens
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as Set
import           Data.Monoid
import           Text.Parser.Token.Style
import           Text.Trifecta

operator :: TokenParsing m => IdentifierStyle m
operator = haskellOps & styleLetter .~ oneOf "-+!&|=><"

keyword :: TokenParsing m => IdentifierStyle m
keyword = haskellIdents & styleReserved .~ reserved & styleName .~ "keyword"

variable :: TokenParsing m => IdentifierStyle m
variable = keyword & styleName .~ "variable"

reserved :: HashSet String
reserved = Set.fromList
    [ "if"
    , "elif"
    , "else"
    , "case"
    , "when"
    , "for"
    , "include"
    , "let"
    , "endif"
    , "endcase"
    , "endfor"
    , "endlet"
    , "in"
    , "with"
    , "_"
    , "."
    , "true"
    , "false"
    ]
