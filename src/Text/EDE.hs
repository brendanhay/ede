{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Text.EDE
    (
    -- * Rendering Functions
      render
    , renderFile
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson                    (Object)
import qualified Data.Text.Lazy                as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO             as LText
import           Text.EDE.Internal.Compiler
import           Text.EDE.Internal.Environment
import           Text.EDE.Internal.Parser
import           Text.EDE.Internal.TypeChecker
import           Text.EDE.Internal.Types

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

render :: LText.Text -> Object -> Either String Builder
render tmpl obj = evaluate obj
      $ lift (runParser "render" tmpl)
    >>= typeCheck
    >>= compile

renderFile :: MonadIO m => FilePath -> Object -> m (Either String Builder)
renderFile path obj = do
    tmpl <- liftIO $ LText.readFile path
    return $ render tmpl obj
