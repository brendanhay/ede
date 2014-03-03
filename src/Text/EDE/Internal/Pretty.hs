{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Text.EDE.Internal.Pretty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Pretty
    ( Pretty (..)
    , ppLn
    , pp
    , parensIf

    -- * Text.PrettyPrint
    , (<+>)
    , dot
    , text
    , char
    , bool
    , integer
    , int
    ) where

import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LText
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.Leijen.Text hiding (Pretty(..), list)

ppLn :: Pretty a => a -> IO ()
ppLn = putStrLn . pp

pp :: Pretty a => a -> String
pp = show . renderOneLine . pretty 0

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

class Pretty a where
    pretty :: Int -> a -> Doc

instance Pretty a => Pretty [a] where
    pretty _ xs = lbracket <> go xs
      where
        go []     = rbracket
        go [y]    = pretty 0 y <> go []
        go (y:ys) = pretty 0 y <> comma <+> go ys

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty _ (a, b) = tupled [pretty 0 a, pretty 0 b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty _ (a, b, c) = tupled [pretty 0 a, pretty 0 b, pretty 0 c]

instance Pretty Text where
    pretty _ = text . LText.fromStrict

instance Pretty Meta where
    pretty _ Meta{..} =
          fromString metaName
       <> ":("
       <> int metaRow
       <> char ','
       <> int metaCol
       <> char ')'

instance Pretty Lit where
    pretty _ (LNum  n) = integer n
    pretty _ (LText t) = dquotes $ text (LText.fromStrict t)
    pretty _ (LBool b) = bool b

instance Pretty Bound where
    pretty d (Bound i) = pretty d i

instance Pretty Bind where
    pretty d (Bind i) = pretty d i

instance Pretty (Exp a) where
    pretty d expr = case expr of
        ELit _ l ->
            pretty d l
        EVar _ v ->
            pretty d v
        EAbs _ v e ->
            parensIf (d > absPrec) $ "\\" <> pretty (absPrec + 1) v <> dot <+> pretty absPrec e
        EApp _ e1 e2 ->
            parensIf (d > appPrec) $ pretty appPrec e1 <+> pretty (appPrec + 1) e2
      where
        absPrec, appPrec :: Int
        absPrec = 1
        appPrec = 10

instance Show e => Pretty (Either e (Exp a)) where
    pretty d (Left  e) = parensIf (d > 1) $ "Left:" <+> fromString (show e)
    pretty d (Right x) = parensIf (d > 1) $ "Right:" <+> pretty 0 x

instance Pretty TVar where
    pretty d (TypeVar v) = pretty d v

instance Pretty (Type a) where
    pretty d ty = case ty of
        TCon c ->
            fromString . drop 1 $ show c
        TVar v ->
            pretty d v
        TExists v ->
            parensIf (d > existsPrec) ("exists" <+> pretty existsPrec v)
        TForall v t ->
            parensIf (d > forallPrec) ("forall" <+> pretty (forallPrec + 1) v <> char '.' <+> pretty forallPrec t)
        TFun t1 t2 ->
            parensIf (d > funPrec) $ pretty (funPrec + 1) t1 <+> "->" <+> pretty funPrec t2
      where
        existsPrec, forallPrec, funPrec :: Int
        existsPrec = 10
        forallPrec = 1
        funPrec    = 1

instance Pretty Elem where
    pretty d ctx = case ctx of
        CVar v t ->
            parensIf (d > hastypePrec) $ pretty (hastypePrec + 1) v <+> "::" <+> pretty hastypePrec t
        CForall v ->
            pretty d v
        CExists v ->
            parensIf (d > existsPrec) $ "exists" <+> pretty existsPrec v
        CExistsSolved v t ->
            parensIf (d > existsPrec) $ "exists" <+> pretty existsPrec v <+> "=" <+> pretty existsPrec t
        CMarker v ->
            parensIf (d > appPrec) $ "marker" <+> pretty (appPrec + 1) v
      where
        existsPrec, hastypePrec, appPrec :: Int
        existsPrec  = 1
        hastypePrec = 1
        appPrec     = 10

instance Pretty Context where
    pretty d (Context xs) = pretty d $ reverse xs
