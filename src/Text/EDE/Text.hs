{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Text
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | A suite of helpful 'Data.Text.Text' functions which are offered
-- as part of the prelude offered by "Text.EDE.Filters".
--
-- This module is re-exported for convenience.
module Text.EDE.Text
    ( lowerFirst
    , upperFirst
    , titleize
    , pascalize
    , camelize
    , underscore
    , hyphenate
    ) where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as Text

lowerFirst :: Text -> Text
lowerFirst t
    | Text.null t = t
    | isUpper h   = toLower h `Text.cons` Text.tail t
    | otherwise   = t
  where
    h = Text.head t

upperFirst :: Text -> Text
upperFirst t
    | Text.null t = t
    | isLower h   = toUpper h `Text.cons` Text.tail t
    | otherwise   = t
  where
    h = Text.head t

titleize :: Text -> Text
titleize = Text.toTitle

pascalize :: Text -> Text
pascalize = substitute . Text.concat . map Text.toTitle . split . upperFirst

camelize :: Text -> Text
camelize = lowerFirst . pascalize

underscore :: Text -> Text
underscore = Text.intercalate (Text.singleton '_') . split

hyphenate :: Text -> Text
hyphenate = Text.intercalate (Text.singleton '-') . split

split :: Text -> [Text]
split t
    | Text.null t = []
    | otherwise   = filter (/= "") $ loop t
  where
    loop s
        | Text.null s' = [l]
        | otherwise    = l : g (loop $ Text.tail s')
      where
        g [] = []
        g x'@(x:xs)
            | Just c <- snd . f $ Text.head s' = (c `Text.cons` x) : xs
            | otherwise = x'

        (l, s') = Text.span (not . fst . f) s

    f ' '           = (True, Nothing)
    f '\n'          = (True, Nothing)
    f '-'           = (True, Nothing)
    f c | isUpper c = (True,  Just c)
        | otherwise = (False, Nothing)

substitute :: Text -> Text
substitute = Text.concatMap f
  where
    f '.' = "_"
    f '/' = "_"
    f '(' = "_"
    f ')' = ""
    f  c  = Text.singleton c
