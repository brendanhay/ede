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
    [ literals
    , variables
    , operators
    , comments
    , sections
    ]

literals :: TestTree
literals = testGroup "Literals"
    [ parseProp "Integer" $ \x -> (ident (pack x), einteger m x)
    , parseProp "Bool"    $ \x -> (ident (pack x), ebool m x)
    , testGroup "Text"    $ map (\x -> parseCase (ident $ quote x) (etext m x))
        [ "c"
        , "some text"
        , "break\n"
        , "esc\'ap\\\"ed\\\""
        ]
    ]

variables :: TestTree
variables = testGroup "Variables" $ map f
    [ "var'"
    , "_alpha123"
    , "test_Name'"
    , "_123"
    ]
  where
    f x = parseCase (ident x) (efree m x)

operators :: TestTree
operators = testGroup "Operators" $ map f
    ["-", "+", "!", "&&", "||", "==", "!=", ">", ">=", "<=", "<"]
  where
    f g = parseCase (ident $ "x " <> g <> " y") $ eapp m
        [ ebound m g
        , efree m "x"
        , efree m "y"
        ]

comments :: TestTree
comments = testGroup "Comments" $ map f
    [ "some random shit"
    , "more \n random \t shit {{ with }} junk\r\n "
    ]
  where
    f x = parseCase (comment x <> " ") (etext m " ")

sections :: TestTree
sections = testGroup "Sections"
   [
--        [ parseCase (sectionBlock "raw" "{{ var }} text {% if false %}\n{% endif %}")
--                    (etext m "{{ var }} text {% if false %}\n{% endif %}")
-- --       , sectionLine "raw" "{{ var }} text {% if false %}\n{% endif %}"
--        ]
   ]

parseCase :: Text -> Exp Meta -> TestTree
parseCase txt ex =
    let src = init . tail $ show txt
     in testCase src $
            either assertFailure
                   (\act -> NoMeta ex @=? NoMeta act)
                   (parse src txt)

parseProp :: Testable IO (a -> Either String String)
          => String
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

sectionBlock, sectionLine :: Text -> Text -> Text
sectionBlock k x = sectionLine k ("\n" <> x <> "\n")
sectionLine  k x = "{% " <> k <> " %}" <> x <> "{% end" <> k <> " %}"

comment :: Text -> Text
comment x = "{- " <> x <> " -}"

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
