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
keyword = fmap KA . f
  where
    f "true"    = Just KTrue
    f "false"   = Just KFalse
    f "else"    = Just KElse
    f "if"      = Just KIf
    f "elif"    = Just KElseIf
    f "elsif"   = Just KElseIf
    f "endif"   = Just KEndIf
    f "case"    = Just KCase
    f "when"    = Just KWhen
    f "endcase" = Just KEndCase
    f "for"     = Just KFor
    f "in"      = Just KIn
    f "endfor"  = Just KEndFor
    f "include" = Just KInclude
    f "with"    = Just KWith
    f "assign"  = Just KAssign
    f "capture" = Just KCapture
    f "cycle"   = Just KCycle
    f _         = Nothing

-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar s
    | isVarName s = Just s
    | otherwise   = Nothing

-- | String is a variable name
isVarName :: String -> Bool
isVarName [] = False
isVarName (c : cs)
    | isVarStart c
    , and (map isVarStart cs) = True

    | _ : _       <- cs
    , Just initCs <- initMay cs
    , isVarStart c
    , and (map isVarBody initCs)
    , last cs == '#'          = True

    | otherwise               = False

-- | Charater can start a variable name.
isVarStart :: Char -> Bool
isVarStart c
    =  isLower c
    || c == '?'

-- | Character can be part of a variable body.
isVarBody  :: Char -> Bool
isVarBody c
    =  isUpper c
    || isLower c
    || isDigit c
    || c == '_'
    || c == '\''
    || c == '$'

-- -- | String is the name of an operator.
-- isOpName :: String -> Bool
-- isOpName [] = False
-- isOpName (c : cs)
--     | isOpStart c
--     , and (map isOpStart cs) = True
--     | otherwise              = False

-- | Character can start an operator.
isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'     || c == '@'     || c == '#'
        || c == '$'     || c == '%'                     || c == '&'
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'                     || c == '/'     || c == '|'
        || c == '<'     || c == '>'

-- | Character can be part of an operator body.
isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'     || c == '@'     || c == '#'
        || c == '$'     || c == '%'     || c == '^'     || c == '&'
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'
        || c == '<'     || c == '>'

-- -- | String is the name of a literal.
-- isLitName :: String -> Bool
-- isLitName [] = False
-- isLitName (c : cs)
--     | isLitStart c
--     , and (map isLitBody cs) = True
--     | otherwise              = False

-- | Character can start a literal.
isStringStart :: Char -> Bool
isStringStart c = c == '"'

isStringBody :: Char -> Bool
isStringBody c = c /= '"'

isNumStart :: Char -> Bool
isNumStart c = isDigit c || c == '-'

isNumBody :: Char -> Bool
isNumBody c = isDigit c || c == '.'

-- -- | Character can be part of a literal body.
-- isLitBody :: Char -> Bool
-- isLitBody c
--     = 
--     =  isDigit c
--     || c /= 'b' || c == 'o' || c == 'x'
--     || c == 'w' || c == 'f' || c == 'i'
--     || c == '.'
--     || c == '#'
--     || c == '\''
