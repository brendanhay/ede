{-# LANGUAGE OverloadedStrings #-}

module EDE where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict      as Map
import           Data.Monoid
import           Data.Text                (Text)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO        as LText
import           EDE.Internal.Interpreter
import           EDE.Internal.Parser
import           EDE.Internal.TypeChecker
import           EDE.Internal.Types
import           Text.Parsec              hiding (parse)

--rend :: IO (Either String [Expr])

rend = do
    u <- load "test.tmpl"
    print u

    let (Right us) = u
        c          = typeCheck us

    print c

    let (Right cs)   = c

    print $ evaluate o cs

  --   let (Right es)   = l
  --       (Object obj) = o
  --       x            = evaluate obj es

  --   print x

  --   let Right b = x

  --   putStrLn "Template:"
  --   readFile "test.tmpl" >>= putStrLn

  --   putStrLn "Expressions:"
  --   print es

  --   putStrLn "Builder:"
  --   LText.putStr $ toLazyText b
  where
    Object o = object
        [ "ident" .= ("ident_value!" :: Text)
        , "list1" .= (["hi", "ho", "off", "we", "go"] :: [Text])
        , "list2" .= ([] :: [Text])
        , "hash1" .= Map.fromList [("key" :: Text, "value" :: Text), ("1", "2")]
        ]

load :: FilePath -> IO (Either ParseError UExp)
load path = parse <$> LText.readFile path
