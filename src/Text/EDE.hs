{-# LANGUAGE OverloadedStrings #-}

module Text.EDE where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict      as Map
import           Data.Text                (Text)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO        as LText
import           Text.EDE.Internal.Compiler
import           Text.EDE.Internal.Parser
import           Text.EDE.Internal.TypeChecker
import           Text.EDE.Internal.Types
import           Text.Parsec              (ParseError)

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

rend :: IO ()
rend = do
    u <- load "test.ede"

    let (Right u') = u
        t          = typeCheck u'
        (Right t') = t
        b          = compile o t'

    print b

    let (Right b') = b

    putStrLn "Template:"
    readFile "test.ede" >>= putStrLn

    putStrLn "Builder:"
    LText.putStr $ toLazyText b'
  where
    Object o = object
        [ "ident" .= ("ident_value!" :: Text)
        , "list1" .= (["hi", "ho", "off", "we", "go"] :: [Text])
        , "list2" .= ([] :: [Text])
        , "hash1" .= Map.fromList [("key" :: Text, "value" :: Text), ("1", "2")]
        ]

load :: FilePath -> IO (Either ParseError UExp)
load path = runParser path <$> LText.readFile path
