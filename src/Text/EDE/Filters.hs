{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Filters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Filters
    (
    -- * Default filter table
      defaultFilters

    -- * Textual
    , lower
    , upper
    , lowerFirst
    , upperFirst
    , titleize
    , pascalize
    , underscore
    , hyphenate

    -- * HashMap
    , mapLength

    -- * Vector
    , listLength

    -- * Filter signatures
    , Fun  (..)
    , Type (..)
    ) where

import           Data.Char
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as LText
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector
import           Text.EDE.Internal.Types

-- FIXME: Create polymorphic filters
defaultFilters :: HashMap Text Fun
defaultFilters = Map.fromList
    [ ("lower",       Fun TText TText lower)
    , ("upper",       Fun TText TText upper)
    , ("lowerFirst",  Fun TText TText lowerFirst)
    , ("upperFirst",  Fun TText TText upperFirst)
    , ("titleize",    Fun TText TText titleize)
    , ("pascalize",   Fun TText TText pascalize)
    , ("camelize",    Fun TText TText camelize)
    , ("underscore",  Fun TText TText underscore)
    , ("hyphenate",   Fun TText TText hyphenate)
    , ("listLength",  Fun TList TNum  listLength)
    , ("mapLength",   Fun TMap  TNum  mapLength)
    ]

lower :: LText.Text -> LText.Text
lower = LText.toLower

upper :: LText.Text -> LText.Text
upper = LText.toUpper

lowerFirst :: LText.Text -> LText.Text
lowerFirst t
    | LText.null t = t
    | isUpper h   = toLower h `LText.cons` LText.tail t
    | otherwise   = t
  where
    h = LText.head t

upperFirst :: LText.Text -> LText.Text
upperFirst t
    | LText.null t = t
    | isLower h   = toUpper h `LText.cons` LText.tail t
    | otherwise   = t
  where
    h = LText.head t

titleize :: LText.Text -> LText.Text
titleize = LText.toTitle

pascalize :: LText.Text -> LText.Text
pascalize = substitute . LText.concat . map LText.toTitle . split . upperFirst

camelize :: LText.Text -> LText.Text
camelize = lowerFirst . pascalize

underscore :: LText.Text -> LText.Text
underscore = LText.intercalate (LText.singleton '_') . split

hyphenate :: LText.Text -> LText.Text
hyphenate = LText.intercalate (LText.singleton '-') . split

listLength :: Vector a -> Scientific
listLength = fromIntegral . Vector.length

mapLength :: HashMap k v -> Scientific
mapLength = fromIntegral . Map.size

split :: LText.Text -> [LText.Text]
split t
    | LText.null t = []
    | otherwise   = filter (/= "") $ loop t
  where
    loop s
        | LText.null s' = [l]
        | otherwise    = l : g (loop $ LText.tail s')
      where
        g [] = []
        g x'@(x:xs)
            | Just c <- snd . f $ LText.head s' = (c `LText.cons` x) : xs
            | otherwise = x'

        (l, s') = LText.span (not . fst . f) s

    f ' '           = (True, Nothing)
    f '\n'          = (True, Nothing)
    f '-'           = (True, Nothing)
    f c | isUpper c = (True,  Just c)
        | otherwise = (False, Nothing)

substitute :: LText.Text -> LText.Text
substitute = LText.concatMap f
  where
    f '.' = "_"
    f '/' = "_"
    f '(' = "_"
    f ')' = ""
    f  c  = LText.singleton c
