{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Data.IORef
import Data.List                    (nub)
import Data.Maybe
import Data.Scientific              (Scientific)
import Data.String
import Data.Text                    (Text)
import Text.PrettyPrint.Leijen.Text

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

data Ann a = Ann
    { annType :: Type
    , annTail :: a
    } deriving (Show)

type Sigma = Type
-- newtype Sigma = Sigma { sigma :: Type }

-- | No top-level ForAll
type Rho = Type
-- newtype Rho = Rho { rho :: Type }

-- | No ForAlls anywhere
type Tau = Type
--newtype Tau = Tau { tau :: Type }

-- | Can unify with any tau-type
data TMeta = TM Int TRef

type TRef = IORef (Maybe Tau)
-- 'Nothing' means the type variable is not substituted
-- 'Just ty' means it has been substituted by 'ty'

instance Show TMeta where
    show (TM idx _) = "TM " ++ show idx

instance Eq TMeta where
    (TM u1 _) == (TM u2 _) = u1 == u2

data TVar
    = TBound  { tvarName :: String }
      -- ^ A type variable bound by a ForAll
    | TSkolem { tvarName :: String, tvarUniq :: Int }
      -- ^ A skolem constant; the String is
      -- just to improve error messages
      deriving (Show)

instance Eq TVar where
    (TBound s1)    == (TBound s2)    = s1 == s2
    (TSkolem _ u1) == (TSkolem _ u2) = u1 == u2
    _              == _              = False

data TCon
    = TText
    | TBool
    | TNum
      deriving (Eq, Show)

data Type
    = TForAll [TVar] Rho  -- ^ Forall type
    | TFun    Type   Type -- ^ Function type
    | TCon    TCon        -- ^ Type constants
    | TVar    TVar        -- ^ Always bound by a ForAll
    | TMeta   TMeta
      deriving (Show)

infixr 4 -->
(-->) :: Sigma -> Sigma -> Sigma
(-->) arg res = TFun arg res

type Name = String

data Exp a
    = ELit  !a !Lit
    | EVar  !a !Name
    | ELam  !a !Name    !(Exp a)
    | ELet  !a !Name    !(Exp a) !(Exp a)
    | EApp  !a !(Exp a) !(Exp a)
    --  ECond !a [Alt a]
    --  ECase !a !(Exp a) [Alt a]
    --  ELoop !a !Name    !(Exp a) (Maybe (Alt a))
    --  EIncl !a !Name    (Maybe (Exp a))
      deriving (Show)

-- data Alt a
--     = ACond    !(Exp a) !(Exp a)
--     | ADefault !(Exp a)
--       deriving (Show)

data Lit
    = LText !Text
    | LBool !Bool
    | LNum  !Scientific
      deriving (Show)

literalType :: Lit -> Tau
literalType (LText _) = TCon TText
literalType (LBool _) = TCon TBool
literalType (LNum  _) = TCon TNum

-- type Env = [(TVar, Type)] -- TAU

-- | Replace the specified quantified type variables by
-- given meta type variables
-- No worries about capture, because the two kinds of type
-- variable are distinct
substTy :: [TVar] -> [Type] -> Type -> Type
substTy tvs tys ty = subst_ty (tvs `zip` tys) ty

subst_ty :: [(TVar, Type)] -> Type -> Type
subst_ty env (TFun arg res)   = TFun (subst_ty env arg) (subst_ty env res)
subst_ty env (TVar n)         = fromMaybe (TVar n) (lookup n env)
subst_ty env (TMeta tv)       = TMeta tv
subst_ty env (TCon tc)        = TCon tc
subst_ty env (TForAll ns rho) = TForAll ns (subst_ty env' rho)
  where
    env' = [(n,ty') | (n,ty') <- env, not (n `elem` ns)]

-- Get the MetaTvs from a type; no duplicates in result
metaTvs :: [Type] -> [TMeta]
metaTvs tys = foldr go [] tys
  where
    go (TMeta tv) acc
        | tv `elem` acc   = acc
        | otherwise       = tv : acc
    go (TVar _)       acc = acc
    go (TCon _)       acc = acc
    go (TFun arg res) acc = go arg (go res acc)
    go (TForAll _ ty) acc = go ty acc    -- ForAll binds TVars only

-- | Get all the binders used in ForAlls in the type, so that
-- when quantifying an outer for-all we can avoid these inner ones
tyVarBndrs :: Rho -> [TVar]
tyVarBndrs ty = nub (bndrs ty)
  where
    bndrs (TForAll tvs body) = tvs ++ bndrs body
    bndrs (TFun arg res)     = bndrs arg ++ bndrs res
    bndrs _                  = []

freeTyVars :: [Type] -> [TVar]
-- Get the free TVars from a type; no duplicates in result
freeTyVars tys = foldr (go []) [] tys
  where 
    go :: [TVar]       -- Ignore occurrences of bound type variables
       -> Type          -- Type to look at
       -> [TVar]       -- Accumulates result
       -> [TVar]
    go bound (TVar tv)      acc 
        | tv `elem` bound        = acc
        | tv `elem` acc          = acc
        | otherwise              = tv : acc
    go bound (TMeta _)      acc = acc
    go bound (TCon _)       acc = acc
    go bound (TFun arg res)   acc = go bound arg (go bound res acc)
    go bound (TForAll tvs ty) acc = go (tvs ++ bound) ty acc
