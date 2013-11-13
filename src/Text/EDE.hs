{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Text.EDE
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Text.EDE
    (
    -- * Single Pass
      compile
    , eitherCompile

    -- * Separate Passes
    , Template
    , parse
    , render

    -- * Results
    , Result   (..)
    , Meta     (..)
    , eitherResult
    , result

    -- * Data.Aeson
    , object'
    , (.=)

    -- * Data.Text.Lazy.Builder
    , toLazyText
    ) where

import           Control.Monad
import           Data.Aeson                 (object, (.=))
import           Data.Aeson.Types           (Object, Value(..), Pair)
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder, toLazyText)
import qualified Text.EDE.Internal.Compiler as Compiler
import qualified Text.EDE.Internal.Parser   as Parser
import           Text.EDE.Internal.Types

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

newtype Template = Template { template :: UExp }
    deriving (Eq, Ord)

compile :: Object -> LText.Text -> Result Builder
compile o = render o <=< parse

eitherCompile :: Object -> LText.Text -> Either String Builder
eitherCompile o = eitherResult . compile o

parse :: LText.Text -> Result Template
parse = fmap Template . Parser.runParser

render :: Object -> Template -> Result Builder
render o = Compiler.render o . template

eitherResult :: Result a -> Either String a
eitherResult = result f Right
  where
    f Meta{..} e = Left . unlines $
        [ "ED-E Error"
        , "Position: " ++ concat [source, ":(", show row, ",", show column, ")"]
        , "Messages:"
        ] ++ e

object' :: [Pair] -> Object
object' = (\(Object o) -> o) . object
