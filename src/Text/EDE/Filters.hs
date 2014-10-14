{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Text.EDE.Filters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Filters where
--     (
--     -- * Default filter table
--       defaultFilters

--     -- * Textual
--     , lower
--     , upper
--     , lowerFirst
--     , upperFirst
--     , titleize
--     , pascalize
--     , underscore
--     , hyphenate

--     -- * HashMap
--     , mapLength

--     -- * Vector
--     , listLength

--     -- * Filter signatures
-- --    , Fun  (..)
-- --    , Type (..)
--     ) where

import           Data.Aeson               (Value, encode)
import           Data.Char                hiding (ord)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.Text                (Text)
import qualified Data.Text.Lazy           as LText
import qualified Data.Text.Lazy.Encoding  as LText
import           Text.EDE.Internal.Quotes
import           Text.EDE.Internal.Types

default (Integer)

-- Opaque? def? insert (use Quote constraint)/member?

defaultFilters :: HashMap Text Quoted
defaultFilters = Map.fromList prelude
  where
    prelude =
        -- Bool
        [ ("!",  quote not)
        , ("&&", quote (&&))
        , ("||", quote (||))

        -- Eq
        , ("==", bpoly (==))
        , ("!=", bpoly (/=))

        -- Ord
        , (">",  bnum (>))
        , (">=", bnum (>=))
        , ("<=", bnum (<=))
        , ("<",  bnum (<))

        -- Num
        , ("+",      bnum (+))
        , ("-",      bnum (-))
        , ("*",      bnum (*))
        , ("abs",    unum abs)
        , ("signum", unum signum)
        , ("negate", unum negate)

        -- RealFrac
        , ("truncate", unum (fromIntegral . truncate))
        , ("round",    unum (fromIntegral . round))
        , ("ceiling",  unum (fromIntegral . ceiling))
        , ("floor",    unum (fromIntegral . floor))

        -- Text
        , ("lower",      quote lower)
        , ("upper",      quote upper)
        -- , ("lowerFirst", quote lowerFirst)
        -- , ("upperFirst", quote upperFirst)
        -- , ("titleize",   quote titleize)
        -- , ("pascalize",  quote pascalize)
        -- , ("camelize",   quote camelize)
        -- , ("underscore", quote underscore)
        -- , ("hyphenate",  quote hyphenate)

        -- -- Sequences
        -- , ("length", useq Text.length Map.size Vector.length)
        -- , ("empty",  useq Text.null Map.null Vector.null)

        -- -- Collections
        -- , ("hyphenate",  quote hyphenate)

        -- -- Poly
        , ("show", quote value)
        -- , ("|", apply)
        ]

    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    -- apply = QLam $ \f -> return . QLam $ \g -> trace (show (f, g)) (qapp f g)

value :: Value -> LText.Text
value = LText.decodeUtf8 . encode

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
