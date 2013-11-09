{-# LANGUAGE OverloadedStrings #-}

module Text.EDE where

import           Data.Aeson                    (Object, Value(..), (.=), object)
import qualified Data.HashMap.Strict           as Map
import           Data.Text                     (Text)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO             as LText
import           Text.EDE.Internal.Compiler
import           Text.EDE.Internal.Parser
import           Text.EDE.Internal.TypeChecker
import           Text.EDE.Internal.Types

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

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

render :: FilePath -> LText -> Object -> Result Frag
render n tmpl obj = do
    u <- runParser n tmpl
    t <- typeCheck u
    compile t obj
