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

import Data.Char
import Data.List                      (isPrefixOf, tails)
import Text.EDE.Internal.Lexer.Names
import Text.EDE.Internal.Lexer.Tokens
import Text.EDE.Internal.Types

lexString :: String -> String -> [Token Tok]
lexString name = lexWord name 0 1

lexWord :: String -> Int -> Int -> String -> [Token Tok]
lexWord name line column w = case w of
    -- Empty
    "" -> []

    -- Whitespace
    ' '  : w' -> lexMore 1 w'
    '\t' : w' -> lexMore 8 w'

    c : cs
     | isStringStart c
     , (body, rest) <- span isStringBody cs
         -> tokP (KLit $ KText body) : lexMore (length body) rest

    c : cs
     | isNumStart c
     , (body, rest) <- span isNumBody cs
         -> tokP (KLit $ KNum (c : body)) : lexMore (length (c : body)) rest

    -- Meta tokens
    '{' : '-' : w' -> tokM KCommentStart : lexMore 2 (lexTo "-}" w')
    '-' : '}' : w' -> tokM KCommentEnd   : lexMore 2 w'
    '\n' : w'      -> tokM KNewLine      : lexNextLine 1 w'

    -- Sections
    '{' : '%' : w' -> tokA KSectionL : lexMore 2 w'
    '%' : '}' : w' -> tokA KSectionR : lexMore 2 w'

    -- Identifiers
    '{' : '{' : w' -> tokA KIdentL : lexMore 2 w'
    '}' : '}' : w' -> tokA KIdentR : lexMore 2 w'

    -- Operators
    c : cs
     |  isOpStart c
     ,  (body, rest) <- span isOpBody cs
         -> tokA (KOp (c : body)) : lexMore (length (c : body)) rest

    -- Parens
    '(' : w' -> tokA KParenL : lexMore 1 w'
    ')' : w' -> tokA KParenR : lexMore 1 w'

    -- Punctuation
    ',' : w' -> tokA KComma : lexMore 1 w'

    -- Keywords
    c : cs
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
    tok t = Token t (SourcePos name line column)
    tokM  = tok . KM
    tokA  = tok . KA
    tokP  = tok . KP

    lexTo pat rest =
        case dropWhile (not . isPrefixOf pat) (tails rest) of
            x : _ -> x
            _     -> []

    lexNextLine n = lexWord name (line + 1) (column + n)
    lexMore n     = lexWord name line (column + n)
