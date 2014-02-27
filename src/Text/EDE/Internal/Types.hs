{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as LText
import           Text.PrettyPrint.Leijen.Text hiding ((<>), list)

data Meta = Meta
    { metaName :: String
    , metaRow  :: Int
    , metaCol  :: Int
    } deriving (Eq, Show)

instance Pretty Meta where
    pretty Meta{..} =
          fromString metaName
       <> char '('
       <> pretty metaRow
       <> char ','
       <> pretty metaCol
       <> char ')'

type Id = String

data Pat
    = PWildcard
    | PVar Id
    | PLit Lit
    | PCon Id Scheme [Pat]
      deriving (Show)

instance Pretty Pat where
    pretty PWildcard     = char '_'
    pretty (PVar i)      = fromString i
    pretty (PLit l)      = pretty l
    pretty (PCon i s ps) = ann <+> prettyList ps
      where
        ann = (fromString i <+> "::") $$ nest 2 (pretty s)

data Alt = Alt Pat Exp
    deriving (Show)

instance Pretty Alt where
    pretty (Alt p e) = pretty p <+> "=" <+> pretty e

data Lit
    = LNum  Scientific
    | LChar Char
    | LText Text
    | LBool Bool
      deriving (Show)

instance Pretty Lit where
    pretty (LNum s) = text (LText.toLazyText bld)
      where
        bld | base10Exponent s == 0 = formatScientificBuilder Fixed (Just 0) s
            | otherwise             = scientificBuilder s

    pretty (LChar c) = squotes (char c)
    pretty (LText t) = dquotes (text $ LText.fromStrict t)
    pretty (LBool b) = bool b

data Var
    = Bound Id
    | Free  Id
      deriving (Show)

instance Pretty Var where
    pretty (Bound i) = fromString i
    pretty (Free  i) = fromString i

data Exp
    = EVar Var
    | ELit Lit
    | ELet Id  [Alt] Exp
    | EApp Exp Exp
      deriving (Show)

instance Pretty Exp where
    pretty (EVar v)       = pretty v
    pretty (ELit l)       = pretty l
    pretty (EApp e1 e2)   = pprApp $ EApp e1 e2
    pretty (ELet v rhs b) = sep
        [ "let {"
        , nest 2 (fromString v <+> "=" <+> pretty rhs <+> char '}')
        , "in"
        , pretty b
        ]

pprApp :: Exp -> Doc
pprApp e = go e []
  where
    go (EApp e1 e2) es = go e1 (e2:es)
    go e' es           = pprParendTerm e' <+> sep (map pprParendTerm es)

    pprParendTerm :: Exp -> Doc
    pprParendTerm e@(EVar _) = pretty e
    pprParendTerm e@(ELit _) = pretty e
    pprParendTerm e          = parens (pretty e)

ebound, efree :: Id -> Exp
ebound = EVar . Bound
efree  = EVar . Free

elit :: Lit -> Exp
elit = ELit

elet :: Id -> [Alt] -> Exp -> Exp
elet = ELet

eapp :: [Exp] -> Exp
eapp = foldl1 EApp

-- | let _lambda p = e in _lambda
eabs :: Pat -> Exp -> Exp
eabs p e = elet "_lambda" [Alt p e] (ebound "_lambda")

-- | \_for -> foldMap (\p -> e) _for
efor :: Pat -> Exp -> Exp
efor p e = eabs (PVar "_for") $ eapp [ebound "foldMap", eabs p e, ebound "_for"]
-- \meta current ->
--     let pat  = current
--     in  exp

-- | let _case as in _case p
ecase :: Exp -> [Alt] -> Exp
ecase p as = elet "_case" as (eapp [ebound "_case", p])

eif :: [(Exp, Exp)] -> Exp -> Exp
eif = flip (foldr branch)
  where
    branch (p, t) f = ecase p [Alt true t, Alt false f]

true, false :: Pat
true  = PCon "true"  (Forall [] ([] :=> tbool)) []
false = PCon "false" (Forall [] ([] :=> tbool)) []

-- scheme :: Type -> Scheme
-- scheme t = Forall [] ([] :=> t)

data Kind
    = Star
    | KFun Kind Kind
      deriving (Eq, Show)

instance Pretty Kind where
    pretty Star       = text "*"
    pretty (KFun x y) = parens $ pretty x <+> "->" <+> pretty y

data TVar = TV Id Kind
      deriving (Eq, Show)

instance Pretty TVar where
    pretty (TV v _) = fromString v

data TCon = TC Id Kind
      deriving (Eq, Show)

instance Pretty TCon where
    pretty (TC c _) = fromString c

data Type
    = TVar TVar
    | TCon TCon
    | TApp Type Type
    | TGen Int
      deriving (Eq, Show)

instance Pretty Type where
    pretty (TVar v)   = pretty v
    pretty (TCon c)   = pretty c
    pretty (TApp l r) = parens $ pretty l <+> "->" <+> pretty r
    pretty (TGen n)   = parens $ int n

tnumber  = TCon (TC "Number" Star)
tchar    = TCon (TC "Char"   Star)
tbool    = TCon (TC "Bool"   Star)
tlist    = TCon (TC "[]"     (KFun Star Star))
tarrow   = TCon (TC "(->)"   (KFun Star (KFun Star Star)))
ttuple2  = TCon (TC "(,)"    (KFun Star (KFun Star Star)))

ttext :: Type
ttext = list tchar

list :: Type -> Type
list = TApp tlist

pair :: Type -> Type -> Type
pair a b = (ttuple2 `TApp` a) `TApp` b

data Qual a = [Pred] :=> a
    deriving (Eq, Show)

instance Pretty a => Pretty (Qual a) where
    pretty ([] :=> t) = pretty t
    pretty (ps :=> t) = (prettyList ps <+> text "=>") $$ nest 2 (pretty t)

data Pred = IsIn Id Type
    deriving (Eq, Show)

instance Pretty Pred where
    pretty (IsIn i t) = fromString i <+> pretty t

    prettyList []     = mempty
    prettyList ps     = encloseSep lparen rparen ", " $ map pretty ps

data Scheme = Forall [Kind] (Qual Type)
    deriving (Eq, Show)

instance Pretty Scheme where
    pretty (Forall ks qt) = (text "forall" <+> pretty ks <+> ".") $$ nest 2 (pretty qt)

class HasKind a where
    kind :: a -> Maybe Kind

instance HasKind TVar where
    kind (TV v k) = Just k

instance HasKind TCon where
    kind (TC v k) = Just k

instance HasKind Type where
    kind (TCon tc)                  = kind tc
    kind (TVar u)                   = kind u
    kind (TApp t _)
        | Just (KFun _ k) <- kind t = Just k
    kind _                          = Nothing

class Instantiate a where
    inst :: [Type] -> a -> a

instance Instantiate Type where
    inst ts (TApp l r) = TApp (inst ts l) (inst ts r)
    inst ts (TGen n)   = ts !! n
    inst ts t          = t

instance Instantiate a => Instantiate [a] where
    inst ts = map (inst ts)

instance Instantiate a => Instantiate (Qual a) where
    inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)

prettyShow :: Pretty a => a -> String
prettyShow = show . renderOneLine . pretty

($$) :: Doc -> Doc -> Doc
($$) x y = align (x <$> y)
