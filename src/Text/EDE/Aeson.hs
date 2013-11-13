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
    -- * Single Pass
      compile
    , eitherCompile

    -- * Separate Passes
    , Template
    , parse
    , render

    -- * Results
    , Result (..)
    , Meta   (..)
    , eitherResult
    , result

    -- * JSON Construction
    , toObject
    , (.=)

    -- * Data.Text.Lazy.Builder
    , toLazyText
    ) where

import           Control.Monad
import           Data.Aeson              hiding (Result)
import           Data.Aeson.Types        (Pair)
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Builder  (Builder)
import           Text.EDE                hiding (compile, eitherCompile, render)
import qualified Text.EDE                as EDE
import           Text.EDE.Internal.Types

compile :: ToJSON a => Text -> a -> Result Builder
compile s = EDE.compile s <=< extract

eitherCompile :: ToJSON a => Text -> a -> Either String Builder
eitherCompile s = eitherResult . compile s

render :: ToJSON a => Template -> a -> Result Builder
render t = EDE.render t <=< extract

toObject :: [Pair] -> Object
toObject = (\(Object o) -> o) . object

extract :: ToJSON a => a -> Result Object
extract x =
    case toJSON x of
        (Object o) -> return o
        e          -> throwError (mkMeta "toObject")
            "invalid JSON top-level object {}" [show e]
