{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Arrow
import qualified Data.Aeson              as Aeson
import           Data.List               (isSuffixOf)
import           Data.Maybe
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO       as LText
import           System.Directory
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.EDE

main :: IO ()
main = defaultMain . testGroup "ED-E" $ unsafePerformIO tests

resources :: FilePath
resources = "test/resources/"

tests :: IO [TestTree]
tests = files >>= mapM (fmap test . load)
  where
    files = map (resources ++) . filter (isSuffixOf ".ede")
        <$> getDirectoryContents resources

    load f = (,)
        <$> LText.readFile f
        <*> pure (takeWhile (/= '.') f)

    test (txt, name) =
        let (js, t) = split txt
            obj     = input js
        in  goldenVsStringDiff name diff (name ++ ".golden") $
                either error output $ parse t >>= render obj

    diff r n = ["diff", "-u", r, n]

    split = second (LText.drop 4) . LText.breakOn "---"

    input = fromMaybe (error "Failed parsing JSON")
        . Aeson.decode
        . LText.encodeUtf8

    output = return
        . LText.encodeUtf8
        . toLazyText
