{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.EDE.Internal.Syntax
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
module Text.EDE.Internal.Syntax where

import Control.Lens ((.~))
import Data.Function ((&))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Text.EDE.Internal.Types
import Text.Parser.Token.Style (CommentStyle)
import qualified Text.Parser.Token.Style as Token
import Text.Trifecta (IdentifierStyle, TokenParsing)
import qualified Text.Trifecta as Trifecta

-- | The default ED-E syntax.
--
-- Delimiters:
--
-- * Pragma: @{! ... !}@
--
-- * Inline: @{{ ... }}@
--
-- * Comments: @{# ... #}@
--
-- * Blocks: @{% ... %}@
defaultSyntax :: Syntax
defaultSyntax =
  Syntax
    { _delimPragma = ("{!", "!}"),
      _delimInline = ("{{", "}}"),
      _delimComment = ("{#", "#}"),
      _delimBlock = ("{%", "%}")
    }

-- | An alternate syntax (based on Play/Scala templates) designed to
-- be used when the default is potentially ambiguous due to another encountered
-- smarty based syntax.
--
-- Delimiters:
--
-- * Inline: @\<\@ ... \@>@
--
-- * Comments: @\@* ... *\@@
--
-- * Blocks: @\@( ... )\@@
alternateSyntax :: Syntax
alternateSyntax =
  Syntax
    { _delimPragma = ("@!", "!@"),
      _delimInline = ("<@", "@>"),
      _delimComment = ("@*", "*@"),
      _delimBlock = ("@(", ")@")
    }

commentStyle :: String -> String -> CommentStyle
commentStyle s e =
  Token.emptyCommentStyle & Token.commentStart .~ s & Token.commentEnd .~ e

operatorStyle :: TokenParsing m => IdentifierStyle m
operatorStyle =
  Token.haskellOps & Trifecta.styleLetter .~ Trifecta.oneOf "-+!&|=><"

variableStyle :: TokenParsing m => IdentifierStyle m
variableStyle =
  keywordStyle & Trifecta.styleName .~ "variable"

keywordStyle :: TokenParsing m => IdentifierStyle m
keywordStyle =
  Token.haskellIdents
    & Trifecta.styleReserved .~ keywordSet
    & Trifecta.styleName .~ "keyword"

keywordSet :: HashSet String
keywordSet =
  HashSet.fromList
    [ "if",
      "elif",
      "else",
      "case",
      "when",
      "for",
      "include",
      "let",
      "endif",
      "endcase",
      "endfor",
      "endlet",
      "in",
      "with",
      "_",
      ".",
      "true",
      "false"
    ]

pragmaStyle :: TokenParsing m => IdentifierStyle m
pragmaStyle =
  Token.haskellIdents
    & Trifecta.styleReserved .~ pragmaSet
    & Trifecta.styleName .~ "pragma field"

pragmaSet :: HashSet String
pragmaSet =
  HashSet.fromList
    [ "pragma",
      "inline",
      "comment",
      "block"
    ]
