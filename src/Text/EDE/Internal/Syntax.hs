{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Text.Parser.Token.Style
import           Text.Trifecta

data Options = Options
    { _delimRender         :: (String, String)
    , _delimComment        :: (String, String)
    , _delimBlock          :: (String, String)
    -- , _lstripBlocks        :: !Bool
    -- , _trimBlocks          :: !Bool
    -- , _keepTrailingNewline :: !Bool
    } deriving (Eq, Show)

makeLenses ''Options

jinjaSyntax :: Options
jinjaSyntax = Options
    { _delimRender  = ("{{", "}}")
    , _delimComment = ("{#", "#}")
    , _delimBlock   = ("{%", "%}")
    }

operatorStyle :: TokenParsing m => IdentifierStyle m
operatorStyle = haskellOps & styleLetter .~ oneOf "-+!&|=><"

variableStyle :: TokenParsing m => IdentifierStyle m
variableStyle = keywordStyle & styleName .~ "variable"

keywordStyle :: TokenParsing m => IdentifierStyle m
keywordStyle = haskellIdents
     & styleReserved .~ keywordSet
     & styleName     .~ "keyword"

keywordSet :: HashSet String
keywordSet = Set.fromList
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
