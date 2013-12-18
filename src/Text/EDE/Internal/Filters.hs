{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Data.Char
import qualified Data.HashMap.Strict     as Map
import           Data.HashMap.Strict     (HashMap)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Vector             as Vector
import           Text.EDE.Internal.Types

-- FIXME: Create polymorphic filters
defaults :: HashMap Text Fun
defaults = Map.fromList
    [ ("lower",      Fun TText TText $ Text.map toLower)
    , ("upper",      Fun TText TText $ Text.map toUpper)
    , ("lowerFirst", Fun TText TText $ lowerFirst)
    , ("upperFirst", Fun TText TText $ upperFirst)
    , ("listLength", Fun TList TInt  $ fromIntegral . Vector.length)
    , ("mapLength",  Fun TMap TInt   $ fromIntegral . Map.size)
    ]
  where
    lowerFirst t
        | Text.null t = t
        | isUpper h   = toLower h `Text.cons` Text.tail t
        | otherwise   = t
      where
        h = Text.head t

    upperFirst t
        | Text.null t = t
        | isLower h   = toUpper h `Text.cons` Text.tail t
        | otherwise   = t
      where
        h = Text.head t
