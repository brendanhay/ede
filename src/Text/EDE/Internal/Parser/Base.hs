-- Module      : Text.EDE.Internal.Parser.Base
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Parser.Base where

import           Data.Functor.Identity
import           Text.EDE.Internal.Lexer
import           Text.EDE.Internal.Types
import           Text.Parsec             hiding (runParser)
import qualified Text.Parsec             as P
import           Text.Parsec.Error

type Parser a = ParsecT [Token Tok] ParserState Identity a

type ParserState = String

runParser :: String      -- ^ Source name for error messages.
          -> Parser a    -- ^ Parser to run.
          -> [Token Tok] -- ^ Tokens to parse.
          -> Either ParseError a
runParser name parser = P.runParser parser name name

-- | Accept the given token.
pTok :: Tok -> Parser ()
pTok t = pTokMaybe $ \k -> if t == k then Just () else Nothing

-- | Accept a token if the function returns `Just`.
pTokMaybe :: (k -> Maybe a) -> Parser k a
pTokMaybe f = do
    state <- P.getState
    P.token (show . tokenTok)
            (sourcePos)
            (f . tokenTok)

