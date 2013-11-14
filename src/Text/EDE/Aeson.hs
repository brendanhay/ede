{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Text.EDE.Aeson
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Text.EDE.Aeson
    (
    -- * Parsing and rendering
      Template
    , parse
    , render

    -- * JSON Construction
    , toObject
    , (.=)

    -- * Data.Text.Lazy.Builder
    , toLazyText
    ) where

import           Data.Aeson              hiding (Result)
import           Data.Aeson.Types        (Pair)
import           Data.Text.Lazy.Builder  (Builder)
import           Text.EDE                hiding (render)
import qualified Text.EDE                as EDE

render :: ToJSON a => a -> Template -> Either String Builder
render x t = extract x >>= \o -> EDE.render o t

toObject :: [Pair] -> Object
toObject = (\(Object o) -> o) . object

extract :: ToJSON a => a -> Either String Object
extract x = eitherResult $
    case toJSON x of
        (Object o) -> return o
        v          -> return $ toObject ["item" .= v]

