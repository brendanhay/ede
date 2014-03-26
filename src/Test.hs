{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text                   as Text
import           Data.Text (Text)
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Evaluator
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Parser

f :: Show e => Either e a -> Either String a
f x = either (Left . show) Right x

g :: Show a => Either String a -> IO ()
g = either putStrLn print

h :: Either String a -> a
h = either error id

run p = f . runParser "parse" p <=< runLexer "lex"

eid = elam ("x" :: Text) (EVar "x")

esucc = elam ("n" :: Text) $ eapp [EVar "+", ELit (LNum 1), EVar "n"]
epred = elam ("n" :: Text) $ eapp [EVar "-", ELit (LNum 1), EVar "n"]

-- h :: Pretty a => Either String a -> IO ()
-- h = either putStrLn ppLn

