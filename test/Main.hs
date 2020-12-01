{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
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
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Paths_ede as Paths
import qualified System.Directory as Directory
import System.FilePath ((-<.>), (</>))
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Tasty.Golden
import qualified Text.EDE as EDE

main :: IO ()
main = do
  resources <-
    Paths.getDataDir <&> (</> "test/resources")

  templates <-
    map (FilePath.combine resources) . filter (List.isSuffixOf ".ede")
      <$> Directory.listDirectory resources

  Tasty.defaultMain $
    Tasty.testGroup "ED-E" $
      map (test resources . FilePath.normalise) templates

test :: FilePath -> FilePath -> Tasty.TestTree
test dir path =
  Tasty.Golden.goldenVsStringDiff path diff (path -<.> ".golden") $ do
    (context, template) <-
      split <$> ByteString.readFile path

    result <-
      EDE.parseWith
        EDE.defaultSyntax
        (EDE.includeFile dir)
        (Text.pack path)
        template

    EDE.result
      (error . show)
      (pure . Text.Lazy.Encoding.encodeUtf8)
      (result >>= flip EDE.render context)
  where
    diff ref new =
      ["diff", "-u", ref, new]

    split =
      Bifunctor.bimap input (ByteString.drop 4)
        . ByteString.breakSubstring "---"

    input =
      Maybe.fromMaybe (error "Failed parsing JSON")
        . Aeson.decode'
        . ByteString.Lazy.fromStrict
