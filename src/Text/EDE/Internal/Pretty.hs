{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.EDE.Internal.Pretty where

import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import           Text.EDE.Internal.Types
import qualified Text.PrettyPrint.Leijen.Text as PP
import           Text.PrettyPrint.Leijen.Text hiding (Pretty(..), list)

ppLn :: Pretty a => a -> IO ()
ppLn = putStrLn . pp

pp :: Pretty a => a -> String
pp = show . renderOneLine . pretty 0

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

-- instance Pretty Var where
--     pretty _ (Var v) = fromString v

-- instance Pretty TVar where
--     pretty _ (TypeVar v) = fromString v

instance Pretty Doc where
    pretty _ = id

instance Pretty TVarName where
    pretty _ n = char $ toEnum (n + 97)

instance Pretty Type where
    pretty d ty = case ty of
        TInt       -> "Int"
        TBool      -> "Bool"
        TVar v     -> pretty d v
        TFun t1 t2 -> parensIf (d > funPrec) $ pretty (funPrec + 1) t1 <+> "->" <+> pretty funPrec t2
      where
        funPrec = 1

instance Pretty Scheme where
    pretty d (Forall vs t) = parensIf (d > forallPrec) $ quantified <> pretty 0 t
      where
        quantified
            | [] <- vs  = empty
            | otherwise = "forall" <+> hsep (map (pretty 0) vs) <> ". "

        forallPrec = 1

instance Pretty Lit where
    pretty _ (LInt  n) = integer n
--    pretty _ (LText t) = text $ LText.fromStrict t
    pretty _ (LBool b) = bool b

instance Pretty (Exp a) where
    pretty d expr = case expr of
        ELit _ l ->
            pretty d l
        EVar _ v ->
            pretty 0 (var v)
        EAbs _ v e ->
            parensIf (d > absPrec) $ "\\" <> pretty (absPrec + 1) (var v) <+> "->" <+> pretty absPrec e
        EApp _ e1 e2 ->
            parensIf (d > appPrec) $ pretty appPrec e1 <+> pretty (appPrec + 1) e2
      where
        var :: String -> Doc
        var = fromString

        absPrec, appPrec :: Int
        absPrec = 1
        appPrec = 10

-- instance Pretty (GContext a) where
--     pretty d (Context xs) = pretty d $ reverse xs

-- instance Pretty (ContextElem a) where
--     pretty d ctx = case ctx of
--         CForall v ->
--             pretty d v
--         CVar v t ->
--             parensIf (d > hastypePrec) $ pretty (hastypePrec + 1) v <+> "::" <+> pretty hastypePrec t
--         CExists v ->
--             parensIf (d > existsPrec) $ "exists" <+> pretty existsPrec v
--         CExistsSolved v t ->
--             parensIf (d > existsPrec) $ "exists" <+> pretty existsPrec v <+> "=" <+> pretty existsPrec t
--         CMarker v ->
--             parensIf (d > appPrec) $ "marker" <+> pretty (appPrec + 1) v
--       where
--         existsPrec, hastypePrec, appPrec :: Int
--         existsPrec  = 1
--         hastypePrec = 1
--         appPrec     = 10

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id
