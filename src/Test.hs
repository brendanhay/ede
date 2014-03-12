{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Control.Applicative
import           Data.Monoid
import qualified Data.Text                 as Text
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Parser
import           Text.EDE.Internal.Types

f :: Show e => Either e a -> Either String a
f x = either (Left . show) Right x

g :: Show a => Either String a -> IO ()
g = either putStrLn print

run p t = f . runParser p =<< runLexer "lex" t

-- h :: Pretty a => Either String a -> IO ()
-- h = either putStrLn ppLn

