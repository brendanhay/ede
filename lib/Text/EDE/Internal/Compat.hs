{-# LANGUAGE CPP #-}

-- |
-- Module      : Text.EDE.Internal.Eval
-- Copyright   : (c) 2022 Brendan Hay <brendan.g.hay@gmail.com>
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
module Text.EDE.Internal.Compat
  ( toHashMapText,
    fromHashMapText,
  )
where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (toHashMapText, fromHashMapText)
#else
import Data.HashMap.Strict (HashMap)
import Data.Aeson.Types (Value)
import Data.Text (Text)

toHashMapText :: HashMap Text Value -> HashMap Text Value
toHashMapText = id

fromHashMapText :: HashMap Text Value -> HashMap Text Value
fromHashMapText = id
#endif
