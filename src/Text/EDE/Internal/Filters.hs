{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Filters
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Filters
    ( defaults
    ) where

import           Data.Char               (toLower, toUpper)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Text.EDE.Internal.Types

-- FIXME: should be able to chain filters indefinitely using | operator
-- making a 'show' filter feasible.
-- Expression tree should utilise UApp for this.

defaults :: HashMap Text Filter
defaults = Map.fromList
    [ ("show",  (Text.pack . show) :|: TText) -- ^ Only show's text, how to make this single filter polymorphic?
    , ("lower", Text.map toLower   :|: TText)
    , ("upper", Text.map toUpper   :|: TText)
    ]
