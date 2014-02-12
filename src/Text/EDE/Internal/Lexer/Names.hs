-- Module      : Text.EDE.Internal.Lexer.Names
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Lexer.Names where

import Data.Char
import Safe                           (initMay)
import Text.EDE.Internal.Lexer.Tokens

keyword :: String -> Maybe Tok
keyword = fmap KAtom . f
  where
    f "true"       = Just KTrue
    f "false"      = Just KFalse
    f "else"       = Just KElse
    f "if"         = Just KIf
    f "elif"       = Just KElseIf
    f "elsif"      = Just KElseIf
    f "endif"      = Just KEndIf
    f "case"       = Just KCase
    f "when"       = Just KWhen
    f "endcase"    = Just KEndCase
    f "for"        = Just KFor
    f "in"         = Just KIn
    f "endfor"     = Just KEndFor
    f "include"    = Just KInclude
    f "with"       = Just KWith
    f "assign"     = Just KAssign
    f "capture"    = Just KCapture
    f "endcapture" = Just KEndCapture
    f "raw"        = Just KRaw
    f "endraw"     = Just KEndRaw
    f _            = Nothing

readVar :: String -> Maybe String
readVar s
    | isVarName s = Just s
    | otherwise   = Nothing

isVarName :: String -> Bool
isVarName [] = False
isVarName (c : cs)
    | isVarStart c
    , all isVarStart cs = True

    | _ : _       <- cs
    , Just initCs <- initMay cs
    , isVarStart c
    , all isVarBody initCs
    , last cs == '#'          = True

    | otherwise               = False

isVarStart :: Char -> Bool
isVarStart c
    =  isLower c
    || c == '?'

isVarBody  :: Char -> Bool
isVarBody c
    =  isUpper c
    || isLower c
    || isDigit c
    || c == '_'
    || c == '\''
    || c == '$'


isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'     || c == '@'     || c == '#'
        || c == '$'     || c == '%'                     || c == '&'
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'                     || c == '/'     || c == '|'
        || c == '<'     || c == '>'

isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'     || c == '@'     || c == '#'
        || c == '$'     || c == '%'     || c == '^'     || c == '&'
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'
        || c == '<'     || c == '>'

isStringStart :: Char -> Bool
isStringStart c = c == '"'

isStringBody :: Char -> Bool
isStringBody c = c /= '"'

isNumStart :: Char -> Bool
isNumStart c = isDigit c || c == '-'

isNumBody :: Char -> Bool
isNumBody c = isDigit c || c == '.'
