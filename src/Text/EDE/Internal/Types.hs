{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

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

import           Control.Applicative
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as Set
import           Data.Hashable                (Hashable)
import           Data.List                    (nub)
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific              (Scientific)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LText
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

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

newtype Id = Id String

enumId  :: Int -> Id
enumId = mappend "v" . show

data Lit
    = LInt  Integer
    | LChar Char
    | LStr  String
    | LBool Bool

data Expr
    = EVar Id
    | ELit Lit
    | EApp Exp Exp
    | ELet Id  Exp Exp

data Kind
    = Star
    | KFun Kind Kind
      deriving (Show, Eq)

data TVar = TV Id Kind
    deriving (Eq, Show)

data TCon = TC Id Kind
    deriving (Eq, Show)

tChar   = TCon (TC "Char" Star)
tBool   = TCon (TC "Bool" Star)
tList   = TCon (TC "[]"   (KFun Star Star))
tArrow  = TCon (TC "(->)" (KFun Star (KFun Star Star)))
tTuple2 = TCon (TC "(,)"  (KFun Star (KFun Star Star)))

data Type
    = TVar TVar
    | TCon TCon
    | TApp Type Type
    | TGen Int
      deriving (Eq, Show)

tString :: Type
tString = list tChar

infixr 4 (-->)
(-->) :: Type -> Type -> Type
(-->) a b = TApp (TApp tArrow a) b

list :: Type -> Type
list t = TApp tList t

pair :: Type -> Type -> Type
pair a b = TApp (TApp tTuple2 a) b

data Qual a = [Pred] :=> a
    deriving (Eq)

data Pred = IsIn Id Type
    deriving (Eq, Show)

type Instance = Qual Pred
type Class    = ([Id], [Instance])

data Scheme = Forall [Kind] (Qual Type)
    deriving (Eq)

quantify :: [TVar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [v | v <- tv qt, v `elem` vs]
    ks  = map kind vs'
    s   = zip vs' $ map TGen [0..]

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = Id :>: Scheme

find :: Monad m => Id -> [Assump] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i':>:sc):as) = if i==i' then return sc else find i as

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
        case (kind t) of
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

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [TVar]

instance Types Type where
    apply s (TVar u) = case lookup u s of
                         Just t  -> t
                         Nothing -> TVar u
    apply s (TApp l r) = TApp (apply s l) (apply s r)
    apply s t = t

    tv (TVar u) = [u]
    tv (TApp l r) = tv l `union` tv r
    tv t = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concat . map tv

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tv (IsIn i t) = tv t

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall ks qt) = tv qt

instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    tv (i :>: sc) = tv sc
