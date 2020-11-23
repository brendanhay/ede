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

import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as LText
import Paths_ede
import System.Directory
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.Golden
import Text.EDE

main :: IO ()
main = defaultMain . testGroup "ED-E" $ unsafePerformIO tests

resources :: FilePath
resources = unsafePerformIO getDataDir

include :: Resolver IO
include = includeFile resources

tests :: IO [TestTree]
tests = files >>= mapM test
  where
    files :: IO [FilePath]
    files =
      map (resources ++) . filter (isSuffixOf ".ede")
        <$> getDirectoryContents resources

    test :: FilePath -> IO TestTree
    test f = do
      (bs, n) <-
        (,)
          <$> BS.readFile f
          <*> pure (takeWhile (/= '.') f)

      let (js, src) = split bs
          name = Text.pack (n ++ ".ede")

      return . goldenVsStringDiff n diff (n ++ ".golden") $ do
        r <- parseWith defaultSyntax include name src
        result
          (error . show)
          (return . LText.encodeUtf8)
          (r >>= (`render` js))

    diff r n = ["diff", "-u", r, n]

    split = bimap input (BS.drop 4) . BS.breakSubstring "---"

    input =
      fromMaybe (error "Failed parsing JSON")
        . Aeson.decode'
        . LBS.fromStrict
