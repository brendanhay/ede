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
    -- * Exported Types
      Builder
    , Meta   (..)
    , Result (..)

    -- * Rendering Functions
    , rend
    , render
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                    (Object, Value(..), (.=), object)
import qualified Data.HashMap.Strict           as Map
import           Data.Text                     (Text)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO             as LText
import           Text.EDE.Internal.Compiler    hiding (render)
import           Text.EDE.Internal.Parser
import           Text.EDE.Internal.TypeChecker
import           Text.EDE.Internal.Types

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

-- export a render which takes text

-- export a renderFile which reads a file
-- export separate render / compile steps for pre-compiled templates

rend :: IO ()
rend = do
    f <- LText.readFile "test.ede"
    case render "test.ede" f o of
        Success b -> LText.putStr $ toLazyText b
        err       -> print err
  where
    Object o = object
        [ "ident" .= ("ident_value!" :: Text)
        , "list1" .= (["hi", "ho", "off", "we", "go"] :: [Text])
        , "list2" .= ([] :: [Text])
        , "hash1" .= Map.fromList [("key" :: Text, "value" :: Text), ("1", "2")]
        ]

render :: FilePath -> LText -> Object -> Result Builder
render n tmpl obj = flip runReaderT obj $ do
    u <- lift $ runParser n tmpl
    t <- typeCheck u
    compile t
