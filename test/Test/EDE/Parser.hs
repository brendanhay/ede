{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- Module      : Test.EDE.Parser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.EDE.Parser (tests) where

import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Parser

tests :: TestTree
tests = testGroup "Parsing"
    [ operators
    , variables
    , literals
    ]

operators :: TestTree
operators = testGroup "Operators" $ map f
    ["-", "+", "!", "&&", "||", "==", "!=", ">", ">=", "<=", "<"]
  where
    f g = parseCase ("x " <> g <> " y") $ eapp m
        [ ebound m g
        , efree m "x"
        , efree m "y"
        ]

variables :: TestTree
variables = testGroup "Variables" $ map (\x -> parseCase x (efree m x))
    [ "var'"
    , "_alpha123"
    , "test_Name'"
    , "_123"
    ]

literals :: TestTree
literals = testGroup "Literals"
    [ parseProp "String"  $ \x -> (ident (quote x), etext m x)
    , parseProp "Integer" $ \x -> (ident (pack x), einteger m x)
    , parseProp "Bool"    $ \x -> (ident (pack x), ebool m x)
    ]

parseCase :: Text -> Exp Meta -> TestTree
parseCase txt ex =
    let src = Text.unpack txt
     in testCase src $
            either assertFailure
                   (\act -> NoMeta ex @=? NoMeta act)
                   (parse src $ ident txt)

parseProp :: Testable IO (a -> Either String [Char])
          => [Char]
          -> (a -> (Text, Exp Meta))
          -> TestTree
parseProp src f = testProperty src $ \x -> prop (f x)
  where
    prop (txt, ex) = do
        act <- parse src txt
        if NoMeta ex /= NoMeta act
            then Left  $ "Actual: " ++ show act ++ "\nExpected: " ++ show ex
            else Right $ "Correctly parsed: " ++ show act

parse :: String -> Text -> Either String (Exp Meta)
parse src txt = do
    ts <- runLexer src txt
    either (Left . show) Right (runParser show src pDoc ts)

pack :: Show a => a -> Text
pack = Text.pack . show

ident :: Text -> Text
ident x = "{{ " <> x <> " }}"

quote :: Text -> Text
quote x = "\"" <> x <> "\""

m :: Meta
m = Meta "m" 0 0

newtype NoMeta = NoMeta (Exp Meta)

instance Show NoMeta where
    show (NoMeta x) = show x

instance Eq NoMeta where
    NoMeta a == NoMeta b = f a == f b
      where
        f = fmap (const True)

instance Monad m => Serial m Text where
    series = cons1 Text.pack
