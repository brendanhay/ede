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
    -- * Rendering
      render
    , renderFile
    ) where

import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Class     (lift)
import           Data.Aeson                    (Object)
import           Data.Text.Lazy                (Text)
import           Data.Text.Lazy.Builder        (Builder)
import qualified Data.Text.Lazy.IO             as LText
import           Text.EDE.Internal.Compiler    (compile)
import           Text.EDE.Internal.Environment (evaluate)
import           Text.EDE.Internal.Parser      (runParser)
import           Text.EDE.Internal.TypeChecker (typeCheck)

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

render :: Text -> Object -> Either String Builder
render tmpl obj = evaluate obj
      $ lift (runParser "render" tmpl)
    >>= typeCheck
    >>= compile

renderFile :: MonadIO m => FilePath -> Object -> m (Either String Builder)
renderFile path obj = do
    tmpl <- liftIO $ LText.readFile path
    return $ render tmpl obj
