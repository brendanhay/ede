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

import           Data.Aeson.Types        hiding (Success)
import           Data.Text               (pack)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO       as LText
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.EDE

resources :: FilePath
resources = "test/resources/"

main :: IO ()
main = defaultMain $ testGroup "ED-E"
    [ test "variable"         ["var" .= pack "World"]
    , test "newline"          ["var" .= pack "more"]
    , test "cond-bool"        []
    , test "cond-variable"    ["true_var" .= True, "false_var" .= False]
    , test "cond-alternate"   []
    , test "cond-bin-bool"    []
    , test "cond-rel-integer" []
    ]

test :: String -> [Pair] -> TestTree
test name ps = goldenVsStringDiff name diff (path ++ ".golden") $ do
    f <- LText.readFile (path ++ ".ede")
    case render name f o of
        Success b -> return . LText.encodeUtf8 $ toLazyText b
        err       -> error $ show err
  where
    Object o = object ps
    path     = resources ++ name

diff :: String -> String -> [String]
diff r n = ["diff", "-u", r, n]
