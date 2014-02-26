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

-- import           Control.Applicative
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as Set
import           Data.Hashable                (Hashable)
import           Data.List                    (union)
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific              (Scientific)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LText
import           Text.PrettyPrint.Leijen.Text hiding ((<>), list)

data Meta = Meta
    { metaName :: String
    , metaRow  :: Int
    , metaCol  :: Int
    } deriving (Eq)

instance Show Meta where
    show = prettyShow

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
    = PWildcard         -- ^ _
    | PVar Id           -- ^ x
    | PLit Lit          -- ^ 123
    | PCon Assump [Pat] -- ^ ?
      deriving (Show)

data Alt = Alt Pat Exp
    deriving (Show)

data Bind = Bind Id [Alt]
    deriving (Show)

data Lit
    = LInt  Integer
    | LChar Char
    | LStr  String
    | LBool Bool
      deriving (Show)

data Exp
    = EVar Id
    | ELit Lit
    | ELet Bind Exp
    | EApp Exp  Exp
      deriving (Show)

evar :: Id -> Exp
evar = EVar

elit :: Lit -> Exp
elit = ELit

elet :: Id -> [Alt] -> Exp -> Exp
elet n as = ELet (Bind n as)

eapp :: [Exp] -> Exp
eapp = foldl1 EApp

ecase :: Exp -> [Alt] -> Exp
ecase p as = elet "_case" as $ eapp [evar "_case", p]

eif :: [(Exp, Exp)] -> Exp -> Exp
eif = flip (foldr eif)
  where
    eif (p, t) f = ecase p [Alt (PCon true []) t, Alt (PCon false []) f]

data Kind
    = Star
    | KFun Kind Kind
      deriving (Eq)

instance Show Kind where
    show = prettyShow

instance Pretty Kind where
    pretty Star       = text "*"
    pretty (KFun x y) = parens $ pretty x <+> "->" <+> pretty y

data TVar = TV Id Kind
    deriving (Eq)

instance Show TVar where
    show = prettyShow

instance Pretty TVar where
    pretty (TV v _) = fromString v

data TCon = TC Id Kind
    deriving (Eq)

instance Show TCon where
    show = prettyShow

instance Pretty TCon where
    pretty (TC c _) = fromString c

data Type
    = TVar TVar
    | TCon TCon
    | TApp Type Type
    | TGen Int
      deriving (Eq)

instance Show Type where
    show = prettyShow

instance Pretty Type where
    pretty (TVar v)   = pretty v
    pretty (TCon c)   = pretty c
    pretty (TApp l r) = parens $ pretty l <+> "->" <+> pretty r
    pretty (TGen n)   = parens $ int n

infixr 4 -->
(-->) :: Type -> Type -> Type
(-->) a b = TApp (TApp tarrow a) b

tinteger = TCon (TC "Int"  Star)
tchar    = TCon (TC "Char" Star)
tbool    = TCon (TC "Bool" Star)
tlist    = TCon (TC "[]"   (KFun Star Star))
tarrow   = TCon (TC "(->)" (KFun Star (KFun Star Star)))
ttuple2  = TCon (TC "(,)"  (KFun Star (KFun Star Star)))

tstring :: Type
tstring = list tchar

list :: Type -> Type
list t = tlist --> t

pair :: Type -> Type -> Type
pair a b = (ttuple2 --> a) --> b

data Qual a = [Pred] :=> a
    deriving (Eq)

instance Pretty a => Show (Qual a) where
    show = prettyShow

instance Pretty a => Pretty (Qual a) where
    pretty (ps :=> t) = (pretty ps <+> text "=>") $$ nest 2 (pretty t)

data Pred = IsIn Id Type
    deriving (Eq)

instance Show Pred where
    show = prettyShow

instance Pretty Pred where
    pretty (IsIn i t) = text "isIn1" <+> fromString ('c' : i) <+> pretty t

data Scheme = Forall [Kind] (Qual Type)
    deriving (Eq)

instance Show Scheme where
    show = prettyShow

instance Pretty Scheme where
    pretty (Forall ks qt) = (text "forall" <+> pretty ks <+> ".") $$ nest 2 (pretty qt)

scheme :: Type -> Scheme
scheme t = Forall [] ([] :=> t)

false = "false" :>: Forall [] ([] :=> tbool)
true  = "true"  :>: Forall [] ([] :=> tbool)

data Assump = Id :>: Scheme

instance Show Assump where
    show = prettyShow

instance Pretty Assump where
    pretty (i :>: s) = (fromString i <+> ":>:") $$ nest 2 (pretty s)

find :: Monad m => Id -> [Assump] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i' :>: sc) : as)
    | i == i'   = return sc
    | otherwise = find i as

class HasKind a where
    kind :: a -> Kind

instance HasKind TVar where
    kind (TV v k) = k

instance HasKind TCon where
    kind (TC v k) = k

instance HasKind Type where
    kind (TCon tc)  = kind tc
    kind (TVar u)   = kind u
    kind (TApp t _) =
        case kind t of
            (KFun _ k) -> k

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
