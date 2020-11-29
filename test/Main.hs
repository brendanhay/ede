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

import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Paths_ede as Paths
import qualified System.Directory as Directory
import qualified System.IO.Unsafe as IO.Unsafe
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Tasty.Golden
import qualified Text.EDE as EDE

-- FIXME: migrate to tasty's resource bracketing.
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup "ED-E" $
      IO.Unsafe.unsafePerformIO tests

resources :: FilePath
resources = IO.Unsafe.unsafePerformIO Paths.getDataDir

include :: EDE.Resolver IO
include = EDE.includeFile resources

tests :: IO [Tasty.TestTree]
tests = files >>= mapM test
  where
    files :: IO [FilePath]
    files =
      map (resources ++) . filter (List.isSuffixOf ".ede")
        <$> Directory.getDirectoryContents resources

    test :: FilePath -> IO Tasty.TestTree
    test f = do
      (bs, n) <-
        (,)
          <$> ByteString.readFile f
          <*> pure (List.takeWhile (/= '.') f)

      let (js, src) = split bs
          name = Text.pack (n ++ ".ede")

      pure . Tasty.Golden.goldenVsStringDiff n diff (n ++ ".golden") $ do
        r <- EDE.parseWith EDE.defaultSyntax include name src

        EDE.result
          (error . show)
          (pure . Text.Lazy.Encoding.encodeUtf8)
          (r >>= (`EDE.render` js))

    diff r n = ["diff", "-u", r, n]

    split =
      Bifunctor.bimap input (ByteString.drop 4)
        . ByteString.breakSubstring "---"

    input =
      Maybe.fromMaybe (error "Failed parsing JSON")
        . Aeson.decode'
        . ByteString.Lazy.fromStrict
