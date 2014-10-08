{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ( pshow
    --  ppLn
    -- , pp
--    , parensIf

    -- * Text.PrettyPrint
    -- , (<+>)
    -- , dot
    -- , text
    -- , char
    -- , bool
    -- , integer
    -- , int
    ) where

import           Data.Foldable                     (foldl')
import           Data.Monoid                       (mempty)
import           Data.Scientific                   (Scientific)
import           Data.String
import           Data.Text                         (Text)
import           Data.Text.Buildable
import qualified Data.Text.Lazy                    as LText
import qualified Data.Text.Lazy.Builder            as LText
import qualified Data.Text.Lazy.Builder.Scientific as Sci
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.Leijen.Text      hiding (list)

-- ppLn :: PP a => a -> IO ()
-- ppLn = putStrLn . pp

pshow :: PP a => a -> IO ()
pshow = putStrLn . show . pp 0

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

class PP a where
    pp :: Int -> a -> Doc

    default pp :: Pretty a => Int -> a -> Doc
    pp = const pretty

instance PP a => PP (Either String a) where
    pp d e = parensIf (d > 1) $
        case e of
            Left  l -> "Left:"  <+> fromString l
            Right r -> "Right:" <+> pp 0 r

-- instance PP a => PP [a] where
--     pp _ xs = lbracket <> go xs
--       where
--         go []     = rbracket
--         go [y]    = pp 0 y <> go []
--         go (y:ys) = pp 0 y <> comma <+> go ys

-- instance (PP a, PP b) => PP (a, b) where
--     pp _ (a, b) = tupled [pp 0 a, pp 0 b]

-- instance (PP a, PP b, PP c) => PP (a, b, c) where
--     pp _ (a, b, c) = tupled [pp 0 a, pp 0 b, pp 0 c]

-- instance PP Int
-- instance PP Integer
-- instance PP Bool

instance PP Text where
    pp _ = text . LText.fromStrict

instance PP Scientific where
    pp _ = pretty . LText.toLazyText . Sci.scientificBuilder

-- instance PP Meta where
--     pp _ (Meta n r c) = pretty n <> char ':' <> pretty r <> char ':' <> pretty c

instance PP Id where
    pp d (Id i) = pp d i

instance PP (Type a) where
    pp _ ty = case ty of
        TNil  -> "Nil"
        TText -> "Text"
        TBool -> "Bool"
        TNum  -> "Scientific"
        TBld  -> "Builder"
        TMap  -> "Object"
        TList -> "Array"
        TFun  -> "Fun"
        TVar  -> "Var a"

instance PP BinOp where
    pp _ And = text "&&"
    pp _ Or  = text "||"

instance PP RelOp where
    pp _ r = case r of
        Equal        -> "=="
        NotEqual     -> "/="
        Greater      -> ">"
        GreaterEqual -> ">="
        Less         -> "<"
        LessEqual    -> "<="

instance PP Op where
    pp d o = case o of
        ONeg e -> char '!' <> pp d e
        OBin b x y -> pp (d + 1) x <+> pp 0 b <+> pp (d + 1) y
        ORel r x y -> pp (d + 1) x <+> pp 0 r <+> pp (d + 1) y

instance PP Lit where
    pp _ (LBool b) = bool b
    pp d (LNum  n) = pp d n
    pp _ (LText t) = dquotes $ text (LText.fromStrict t)

instance PP Pat where
    pp _ PWild    = "_"
    pp d (PVar v) = pp d v
    pp d (PLit l) = pp d l

-- instance PP (Pat, Exp) where

instance PP [Alt] where
    pp _ = foldl' (\s (p, e) -> s <> line <> pp 0 p <+> "->" <+> pp 0 e) empty

instance PP Exp where
    pp d e' = case e' of
        ELit  _ l         -> pp d l
        EBld  _ b         -> text (LText.toLazyText b)
        EVar  _ v         -> pp d v
        EApp  _ x y       -> parensIf (d > appPrec) $ pp appPrec x <+> pp (appPrec + 1) y
        EOp   _ o         -> pp d o
        ELet  _ v e       -> "let" <+> pp 0 v <+> "=" <+> pp 0 e
        ECase _ p as      -> "case" <+> pp 0 p <+> "of" <> nest 4 (pp 0 as)
        ELoop _ v e bdy f -> "for" <+> pp 0 v <+> "in" <+> pp 0 e <$> nest 4 ("then" <+> pp 0 bdy) <$> nest 4 ("else" <+> pp 0 f)
        EIncl _ n p       -> "include" <+> pp 0 n <+> "with" <+> pp 0 p
      where
        appPrec :: Int
        appPrec = 10

-- instance PP Var where
--     pp d (VBound i) = pp d i
--     pp d (VFree  i) = pp d i

-- instance PP Bind where
--     pp d (Bind i) = pp d i




-- instance PP (Exp a) where
--     pp d expr = case expr of
--         ELit _ l ->
--             pp d l
--         EVar _ v ->
--             pp d v
--         EAbs _ v e ->
--             parensIf (d > absPrec) $ "λ" <> pp (absPrec + 1) v <> dot <+> pp absPrec e
--         EApp _ e1 e2 ->
--             parensIf (d > appPrec) $ pp appPrec e1 <+> pp (appPrec + 1) e2
--         ELet _ v rhs bdy ->
--             "let" <+> pp 0 v <+> "=" <+> pp 0 rhs <+> "in" <$> nest 4 (pp 0 bdy)
--       where
--         absPrec, appPrec :: Int
--         absPrec = 1
--         appPrec = 10

-- -- instance PP TVar where
-- --     pp d (TypeVar v) = pp d v

-- -- instance PP Elem where
-- --     pp d ctx = case ctx of
-- --         CVar v t ->
-- --             parensIf (d > hastypePrec) $ pp (hastypePrec + 1) v -- <+> "::" <+> pp hastypePrec t
-- --         CForall v ->
-- --             pp d v
-- --         CExists v ->
-- --             parensIf (d > existsPrec) $ "exists" <+> pp existsPrec v
-- --         CExistsSolved v t ->
-- --             parensIf (d > existsPrec) $ "exists" <+> pp existsPrec v <+> "=" <+> pp existsPrec t
-- --         CMarker v ->
-- --             parensIf (d > appPrec) $ "marker" <+> pp (appPrec + 1) v
-- --       where
-- --         existsPrec, hastypePrec, appPrec :: Int
-- --         existsPrec  = 1
-- --         hastypePrec = 1
-- --         appPrec     = 10

-- -- instance PP Context where
-- --     pp d (Context xs) = pp d $ reverse xs
