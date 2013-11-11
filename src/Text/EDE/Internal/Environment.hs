{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Environment
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Environment
    (
    -- * Evaluation Environment
      Env
    , evaluate

    -- * Variables
    , bound
    , require

    -- * Errors
    , typeError
    , compileError
    ) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (Object, Value(..))
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Text.Format           (Format)
import qualified Data.Text.Format           as Format
import           Data.Text.Format.Params    (Params)
import qualified Data.Text.Lazy             as LText
import           Text.EDE.Internal.Types

type Env = ReaderT Object Result

evaluate :: Object -> Env a -> Either String a
evaluate obj = f  . flip runReaderT obj
  where
     f (Success x) = Right x
     f e           = Left $ show e

bound :: Ident -> Env Bool
bound (Ident k) = isJust . Map.lookup k <$> ask

require :: Meta -> Ident -> Env Value
require m (Ident k) = do
    mv <- Map.lookup k <$> ask
    maybe (compileError m "binding '{}' doesn't exist." [k]) return mv

typeError :: Params ps => Meta -> Format -> ps -> Env a
typeError m f = lift . TypeError m . LText.unpack . Format.format f

compileError :: Params ps => Meta -> Format -> ps -> Env a
compileError m f = lift . CompileError m . LText.unpack . Format.format f
