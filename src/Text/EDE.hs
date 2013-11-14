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
    , Result (..)
    , Meta   (..)
    , eitherResult
    , result

    -- * Data.Text.Lazy.Builder
    , toLazyText
    ) where

import           Data.Aeson.Types           (Object)
import           Data.Text.Lazy             (Text)
import           Data.Text.Lazy.Builder     (Builder, toLazyText)
import qualified Text.EDE.Internal.Compiler as Compiler
import qualified Text.EDE.Internal.Parser   as Parser
import           Text.EDE.Internal.Types

-- FIXME:
-- syntax/semantic test suite
-- criterion benchmarks

newtype Template = Template { template :: UExp }
    deriving (Eq, Ord)

compile :: Text -> Object -> Result Builder
compile s o = parse s >>= (`render` o)

eitherCompile :: Text -> Object -> Either String Builder
eitherCompile s = eitherResult . compile s

parse :: Text -> Result Template
parse = fmap Template . Parser.runParser

render :: Template -> Object -> Result Builder
render t = Compiler.render (template t)

eitherResult :: Result a -> Either String a
eitherResult = result f Right
  where
    f Meta{..} e = Left . unlines $
        [ "ED-E Error"
        , "Position: " ++ concat [_source, ":(", show _row, ",", show _column, ")"]
        , "Messages:"
        ] ++ e
