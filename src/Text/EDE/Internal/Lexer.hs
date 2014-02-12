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
    ( runLexer
    , module Text.EDE.Internal.Lexer.Tokens
    ) where

import Data.List                      (isPrefixOf, tails)
import Text.EDE.Internal.Lexer.Names
import Text.EDE.Internal.Lexer.Tokens
import Text.EDE.Internal.Types

runLexer :: String -> String -> [Token]
runLexer name = fragment 0 1
  where
    lexTo pat rest =
        case dropWhile (not . isPrefixOf pat) (tails rest) of
            x : _ -> x
            _     -> []

    fragment :: Int -> Int -> String -> [Token]
    fragment line column w =
        let tok t = Token t (Meta name line column)
            tokA  = tok . KAtom

            nextLine  = fragment (line + 1) (column + 1)
            lexMore n = fragment line (column + n)
            lexLan n  = language line (column + n)
        in case w of
            -- Empty
            ""             -> []

            -- Newlines
            '\n' : w'      -> tok KNewLine : nextLine w'

            -- Meta tokens
            '{' : '-' : w' -> lexMore 2 (lexTo "-}" w')
            '-' : '}' : w' -> lexMore 2 w'

            -- Sections
            '{' : '%' : w' -> tokA KSectionL : lexLan 2 w'

            -- Identifiers
            '{' : '{' : w' -> tokA KIdentL : lexLan 2 w'

            -- Continue
            c : cs         -> tok (KFrag c) : lexMore 1 cs

    language :: Int -> Int -> String -> [Token]
    language line column w =
        let tok t = Token t (Meta name line column)
            tokA  = tok . KAtom
            tokP  = tok . KPrim

            nextLine  = language (line + 1) (column + 1)
            lexMore n = language line (column + n)
            lexFrag n = fragment line (column + n)
         in case w of
            -- Empty
            ""              -> []

            -- Newlines
            '\n' : w'       -> tok KNewLine : nextLine w'

            -- End Section
            '%' : '}' : w'  -> tokA KSectionR : lexFrag (column + 2) w'

            -- End Ident
            '}' : '}' : w'  -> tokA KIdentR : lexFrag (column + 2) w'

            -- Whitespace
            ' '  : w'       -> lexMore 1 w'
            '\t' : w'       -> lexMore 8 w'

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
                         = [tok (KJunk c)]
                    in  readNamedVar (c : body')

            -- Some unrecognised character.
            -- We still need to keep lexing as this may be in a comment.
            c : cs -> tok (KJunk c) : lexMore 1 cs
