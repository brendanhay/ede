{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Text.EDE where

import           Control.Monad
import           Data.Aeson                  ((.=))
import           Data.Aeson.Types            (Object)
import           Data.Foldable               (foldrM)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as LText
import           Data.Text.Lazy.Builder      (toLazyText)
import qualified Data.Text.Lazy.IO           as LText
import           System.Directory
import           System.FilePath
import qualified Text.EDE.Internal.Checker   as Checker
import qualified Text.EDE.Internal.Evaluator as Evaluator
import qualified Text.EDE.Internal.Lexer     as Lexer
import qualified Text.EDE.Internal.Parser    as Parser
import           Text.EDE.Internal.Types

-- FIXME: detect include/import loops

-- parseFile :: FilePath -- ^ Path to the template to load and parse.
--           -> IO (Result Template)
-- parseFile p = loadFile p >>= result failure (parseWith inc $ Text.pack p)
--   where
--     inc = includeFile $ takeDirectory p
