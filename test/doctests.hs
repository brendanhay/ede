-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
    [ "-isrc"
    , "-optP-include"
    , "-optPdist/build/autogen/cabal_macros.h"
    , "src/Text/EDE/Internal/Parser.hs"
    ]
