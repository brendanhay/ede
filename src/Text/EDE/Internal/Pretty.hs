{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Text.EDE.Internal.Pretty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Pretty where

import qualified Data.Text.Lazy               as LText
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.Leijen.Text

prettyString :: Pretty a => a -> String
prettyString = show . renderOneLine . pretty

instance Show Meta where
    show = prettyString

instance Pretty Meta where
    pretty Meta{..} =
          fromString metaName
       <> char '('
       <> pretty metaLine
       <> char ','
       <> pretty metaCol
       <> char ')'
