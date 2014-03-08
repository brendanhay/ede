{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Data.Monoid
import Data.Text                (Text, pack)
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck
import Text.EDE.Internal.AST
import Text.EDE.Internal.Lexer
import Text.EDE.Internal.Parser

main :: IO ()
main = defaultMain $ testGroup "Parse"
    [ literals
    ]

literals :: TestTree
literals = testGroup "Literal"
    [ parse "Integer" $
        \x -> (ident (show x), einteger m x)

    , parse "Text" $
        \x -> (ident ('"' : x ++ "\""), etext m (pack x))

    , parse "Bool" $
        \x -> (ident (show x), ebool m x)
    ]

ident :: String -> Text
ident x = "{{" <> pack x <> "}}"

m :: Meta
m = Meta "m" 0 0

parse :: Testable IO (a -> Either String String)
      => String
      -> (a -> (Text, Exp Meta))
      -> TestTree
parse src f = testProperty src $ \x -> prop (f x)
  where
    prop (txt, e) = do
        ts <- runLexer src txt
        a  <- either (Left . show) Right (runParser show src pDoc ts)

        let b = const (meta a) `fmap` e
         in if b /= a
                then Left  $ "Actual: " ++ show a ++ "\nExpected: " ++ show b
                else Right $ "Correctly parsed: " ++ show a

