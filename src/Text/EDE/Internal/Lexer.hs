{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Lexer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Lexer
    ( lexString
    , module Text.EDE.Internal.Lexer.Tokens
    ) where

import Data.List                      (isPrefixOf, tails)
import Text.EDE.Internal.Lexer.Names
import Text.EDE.Internal.Lexer.Tokens
import Text.EDE.Internal.Types

lexString :: String -> String -> [Token]
lexString name = lexWord name 0 1

lexWord :: String -> Int -> Int -> String -> [Token]
lexWord name line column w = case w of
    -- Empty
    ""              -> []

    -- Whitespace
    ' '  : w'       -> lexMore 1 w'
    '\t' : w'       -> lexMore 8 w'

    -- Meta tokens
    '{'  : '-' : w' -> lexMore 2 (lexTo "-}" w')
    '-'  : '}' : w' -> lexMore 2 w'

    '\n' : w'       -> tok KNewLine : lexNextLine 1 w'

    -- Sections
    '{' : '%' : w'  -> tokA KSectionL : lexMore 2 w'
    '%' : '}' : w'  -> tokA KSectionR : lexMore 2 w'

    -- Identifiers
    '{' : '{' : w'  -> tokA KIdentL : lexMore 2 w'
    '}' : '}' : w'  -> tokA KIdentR : lexMore 2 w'

    -- Parens
    '(' : w'        -> tokA KParenL : lexMore 1 w'
    ')' : w'        -> tokA KParenR : lexMore 1 w'

    -- Punctuation
    ',' : w'        -> tokA KComma : lexMore 1 w'

    -- Literals
    c : cs
     | isStringStart c
     , (body, rest) <- span isStringBody cs
         -> tokP (KLit $ KText body) : lexMore (length body) rest

     | isNumStart c
     , (body, rest) <- span isNumBody cs
         -> tokP (KLit $ KNum (c : body)) : lexMore (length (c : body)) rest

    -- Operators
     |  isOpStart c
     ,  (body, rest) <- span isOpBody cs
         -> tokA (KOp (c : body)) : lexMore (length (c : body)) rest

    -- Keywords
     | isVarStart c
     , (body,  rest)  <- span isVarBody cs
     , (body', rest') <- case rest of
                             '#' : rest' -> (body ++ "#", rest')
                             _           -> (body, rest)
         -> let readNamedVar s
                 | Just t <- keyword s
                 = tok t           : lexMore (length s) rest'

                 | Just v <- readVar s
                 = tokP (KVar v) : lexMore (length s) rest'

                 | otherwise
                 = [tok (KJunk [c])]
            in  readNamedVar (c : body')

    -- Some unrecognised character.
    -- We still need to keep lexing as this may be in a comment.
    c : cs -> (tok $ KJunk [c]) : lexMore 1 cs
  where
    tok t = Token t (Meta name line column)
    tokA  = tok . KA
    tokP  = tok . KP

    lexTo pat rest =
        case dropWhile (not . isPrefixOf pat) (tails rest) of
            x : _ -> x
            _     -> []

    lexNextLine n = lexWord name (line + 1) (column + n)
    lexMore n     = lexWord name line (column + n)
