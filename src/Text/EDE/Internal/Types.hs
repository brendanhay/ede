{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

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

import Bound
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Foldable
import Data.Text           (Text)
import Data.Traversable
import Prelude.Extras
import Data.Scientific

data Meta = Meta !String !Int !Int
    deriving (Eq, Show)

class Metadata a where
    meta :: a -> Meta

type Id = Text

data Type
    = TVar Int
    | TApp Type Type
    | TBool
    | TNum
    | TText
      deriving (Eq, Show)

data Lit
    = LBool !Bool
    | LNum  !Scientific
    | LText !Text
      deriving (Eq, Show)

instance NFData Lit where
    rnf (LBool b) = rnf b
    rnf (LNum  n) = rnf n
    rnf (LText t) = rnf t

data Exp a
    = EVar  a
    | ELit  Lit
    | ELam  (Scope () Exp a)
    | EApp  (Exp a) (Exp a)
    | ELet  [Scope Int Exp a] (Scope Int Exp a)
    | ECase (Exp a) [Alt Exp a]
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
    pure  = EVar
    (<*>) = ap

instance Monad Exp where
    return = EVar

    EVar  a    >>= f = f a
    ELit  l    >>= _ = ELit  l
    ELam  e    >>= f = ELam  (e >>>= f)
    EApp  x y  >>= f = EApp  (x >>= f) (y >>= f)
    ELet  bs e >>= f = ELet  (map (>>>= f) bs) (e >>>= f)
    ECase e as >>= f = ECase (e >>= f) (map (>>>= f) as)

instance Eq1   Exp
instance Show1 Exp

data Pat
    = PWild
    | PVar
    | PLit Lit
--    | PAs  Pat
      deriving (Eq, Show)

data Alt f a = Alt !Int Pat (Scope Int f a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bound Alt where
    Alt n p b >>>= f = Alt n p (b >>>= f)

data Binder a = Binder Pat [a]
    deriving (Eq, Show, Functor, Foldable, Traversable)
    -- 
