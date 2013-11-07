{-# LANGUAGE OverloadedStrings #-}

module Tmpl where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict       as Map
import           Data.Text                 (Text)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO         as LText
import           Text.Parsec               hiding (parse)
import           Tmpl.Internal.Interpreter
import           Tmpl.Internal.Parser
import           Tmpl.Internal.Types

--rend :: IO (Either String [Expr])

rend = do
    l <- load "test.tmpl"

    print l

    let (Right es)   = l
        (Object obj) = o
        x            = evaluate obj es

    print x

    let Right b = x

    putStrLn "Template:"
    readFile "test.tmpl" >>= putStrLn

    putStrLn "Expressions:"
    print es

    putStrLn "Builder:"
    LText.putStr $ toLazyText b
  where
    o = object
        [ "ident" .= ("ident_value!" :: Text)
        , "list1" .= (["hi", "ho", "off", "we", "go"] :: [Text])
        , "list2" .= ([] :: [Text])
        , "hash1" .= Map.fromList [("key" :: Text, "value" :: Text), ("1", "2")]
        ]

load :: FilePath -> IO (Either ParseError [Expr])
load path = parse <$> LText.readFile path
