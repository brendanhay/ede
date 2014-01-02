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
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
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
tests = files >>= mapM test
  where
    files :: IO [FilePath]
    files = map (resources ++) . filter (isSuffixOf ".ede")
        <$> getDirectoryContents resources

    test :: FilePath -> IO TestTree
    test f = do
        (txt, name) <- (,)
            <$> LText.readFile f
            <*> pure (takeWhile (/= '.') f)

        let (js, src) = split txt

        return . goldenVsStringDiff name diff (name ++ ".expected") $ do
            t <- parseWith (includeFile resources) (Text.pack name) src
            either error output
                . eitherResult
                $ t >>= (`render` input js)

    diff r n = ["diff", "-u", r, n]

    split = second (LText.drop 4) . LText.breakOn "---"

    input = fromMaybe (error "Failed parsing JSON")
        . Aeson.decode
        . LText.encodeUtf8

    output = return . LText.encodeUtf8
