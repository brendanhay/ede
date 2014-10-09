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
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Monoid                       (mempty)
import           Data.Scientific                   (Scientific)
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Text.Buildable
import qualified Data.Text.Lazy                    as LText
import qualified Data.Text.Lazy.Builder            as LText
import qualified Data.Text.Lazy.Builder.Scientific as Sci
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.Leijen.Text      hiding (list)

-- ppLn :: PP a => a -> IO ()
-- ppLn = putStrLn . pp

pshow :: PP a => a -> String
pshow = show . pp 0

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
    pp d (Id _ i) = pp d i

instance PP Var where
    pp d = pp d . Text.intercalate "." . map idName . NonEmpty.toList . unVar

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

instance PP Lit where
    pp _ (LBool b) = bool b
    pp d (LNum  n) = pp d n
    pp _ (LText t) = dquotes (text t)

instance PP Pat where
    pp _ PWild    = "_"
    pp d (PVar v) = pp d v
    pp d (PLit l) = pp d l

instance (PP l, PP r) => PP (Either l r) where
    pp d (Left  l) = pp d l
    pp d (Right r) = pp d r

instance PP [Alt] where
    pp _ = foldl' (\s (p, e) -> s <> line <> pp 0 p <+> "->" <+> pp 0 e) empty

instance PP Exp where
    pp d e' = case e' of
        ELit  _ l         -> pp d l
        EBld  _ b         -> dquotes (text (LText.toLazyText b))
        EVar  _ v         -> pp d v
        EFun  _ f         -> pp d f
        EApp  _ x y       -> parensIf (d > appPrec) $ pp appPrec x <+> pp (appPrec + 1) y
        ELet  _ v e       -> "let" <+> pp 0 v <+> "=" <+> pp 0 e
        ECase _ p as      -> "case" <+> pp 0 p <+> "of" <> nest 4 (pp 0 as)
        ELoop _ v e bdy f -> "for" <+> pp 0 v <+> "in" <+> pp 0 e <$> nest 4 ("then" <+> pp 0 bdy) <$> may "else" f
        EIncl _ n p       -> "include" <+> pp 0 n <$> may "with" p
      where
        appPrec :: Int
        appPrec = 10

        may n (Just e) = nest 4 (n <+> pp 0 e)
        may _ Nothing  = empty
