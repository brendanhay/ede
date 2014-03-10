-- Module      : Test.EDE.Lexer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.EDE.Lexer (tests) where

import Data.Foldable            (foldMap)
import Data.Text                (pack)
import Prelude                  hiding (lex)
import Test.Tasty
import Test.Tasty.HUnit
import Text.EDE.Internal.Lexer
import Text.EDE.Internal.Pretty

tests :: TestTree
tests = testGroup "Lexing" [atoms, captures]

atoms :: TestTree
atoms = testGroup "Atoms"
    [

    ]

captures :: TestTree
captures = testGroup "Captures"
    [

    ]

-- ident :: [Atom]

-- lex ts = testCase name $ Right ts @=? runLexer name (pack name)
--   where
--     m = 0

--     name = foldMap pp ts
