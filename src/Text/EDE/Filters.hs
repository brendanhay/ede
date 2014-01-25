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
    , Fun   (..)
    , TType (..)
    ) where

import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Scientific
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Unsafe           (unsafeHead, unsafeTail)
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
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

lower :: Text -> Text
lower = Text.toLower

upper :: Text -> Text
upper = Text.toUpper

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

listLength :: Vector a -> Scientific
listLength = fromIntegral . Vector.length

mapLength :: HashMap k v -> Scientific
mapLength = fromIntegral . Map.size

split :: Text -> [Text]
split t
    | Text.null t = []
    | otherwise   = filter (/= "") $ loop t
  where
    loop s
        | Text.null s' = [l]
        | otherwise    = l : g (loop $ unsafeTail s')
      where
        g [] = []
        g x'@(x:xs)
            | Just c <- snd . f $ unsafeHead s' = (c `Text.cons` x) : xs
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
