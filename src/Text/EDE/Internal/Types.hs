{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Types where

import           Data.List                    (nub)
import           Data.Maybe
import           Data.Scientific              (Scientific)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LText
import           Text.PrettyPrint.Leijen.Text

prettyString :: Pretty a => a -> String
prettyString = show . renderCompact . pretty

data Meta = Meta
    { metaName :: String
    , metaLine :: Int
    , metaCol  :: Int
    } deriving (Eq)

instance Show Meta where
    show = prettyString

instance Pretty Meta where
    pretty Meta{..} =
          fromString metaName
       <> char '('
       <> pretty metaLine
       <> char ','
       <> pretty metaCol
       <> char ')'

-- data Ann a = Ann
--     { annType :: Type
--     , annTail :: a
--     } deriving (Show)

data ITerm
    = Ann CTerm Type
    | Bound Int
    | Free Name
    | ITerm :@: CTerm
      deriving (Eq)

instance Pretty ITerm where
    pretty = iPrint 0 0

instance Show ITerm where
    show = show . renderOneLine . pretty

data CTerm
    = Inf ITerm
    | Lam CTerm
     deriving (Eq)

instance Pretty CTerm where
    pretty = cPrint 0 0

instance Show CTerm where
    show = show . renderOneLine . pretty

data Name
    = Global String
    | Local Int
    | Quote Int
     deriving (Show, Eq)

data Type
    = TFree Name
    | Fun   Type Type
      deriving (Eq)

instance Pretty Type where
    pretty = tPrint 0

instance Show Type where
    show = show . renderOneLine . pretty

data Value
    = VLam     (Value -> Value)
    | VNeutral Neutral

data Neutral
    = NFree Name
    | NApp  Neutral Value

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

data Kind = Star
    deriving (Show)

data Info
    = HasKind Kind
    | HasType Type
      deriving (Show)

type Context = [(Name, Info)]

tPrint :: Int -> Type -> Doc
tPrint p (TFree (Global s))  =  text $ LText.pack s
tPrint p (Fun ty ty')        =  parensIf (p > 0) (sep [tPrint 0 ty <> text " ->", nest 2 (tPrint 0 ty')])

iPrint :: Int -> Int -> ITerm -> Doc
iPrint p ii (Ann c ty)       =  parensIf (p > 1) (cPrint 2 ii c <> text " :: " <> tPrint 0 ty)
iPrint p ii (Bound k)        =  text $ LText.pack (vars !! (ii - k - 1))
iPrint p ii (Free (Global s))=  text $ LText.pack s
iPrint p ii (i :@: c)        =  parensIf (p > 2) (sep [iPrint 2 ii i, nest 2 (cPrint 3 ii c)])
iPrint p ii x                =  text $ LText.pack ("[" ++ show x ++ "]")

cPrint :: Int -> Int -> CTerm -> Doc
cPrint p ii (Inf i)    = iPrint p ii i
cPrint p ii (Lam c)    = parensIf (p > 0) (text "\\ " <> text (LText.pack $ vars !! ii) <> text " -> " <> cPrint 0 (ii + 1) c)

vars :: [String]
vars = [c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w']]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id
