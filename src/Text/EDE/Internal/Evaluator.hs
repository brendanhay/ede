{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.EDE.Internal.Evaluator
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Evaluator where

import           Bound
import           Bound.Scope
import           Bound.Var
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.List           (find)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text           as Text
import           Text.EDE.Internal.AST

data Value
    = VLam (Value -> Value)
    | VNeutral Neutral

instance Show Value where
    show (VLam     _) = "Unapplied function"
    show (VNeutral n) = show n

data Neutral
    = NVar Id
    | NLit Lit
    | NApp Neutral Value

instance Show Neutral where
    show (NVar v)   = Text.unpack ("NVar " <> v)
    show (NLit l)   = show l
    show (NApp n v) = "VApp " ++ show n ++ " " ++ show v

vlit :: Lit -> Value
vlit = VNeutral . NLit

vvar :: Id -> Value
vvar = VNeutral . NVar

env :: HashMap Id Value
env = Map.fromList
    [ ("+", binary plus)
    , ("-", binary minus)
    ]
  where
    plus x y =
        case (x, y) of
            (VNeutral (NLit (LNum a)), VNeutral (NLit (LNum b))) -> vlit $ LNum (a + b)
            _ -> error "ass"

    minus x y =
        case (x, y) of
            (VNeutral (NLit (LNum a)), VNeutral (NLit (LNum b))) -> vlit $ LNum (a - b)
            _ -> error "ass"

    binary f = VLam $ \x -> VLam $ \y -> f x y


eval :: (Show a, IsString a, Eq a, Hashable a) => Exp a -> HashMap a Value -> Value
eval (ELit l)   _ = vlit l
eval (EVar v)   g = fromMaybe (error $ "Unbound variable: " ++ show v) (Map.lookup v g)
eval (ELam b)   g = VLam $ \x -> eval (instantiate1 (EVar "x") b) (Map.insert "x" x g)
eval (EApp f a) g = vapp (eval f g) (eval a g)
  where
    vapp :: Value -> Value -> Value
    vapp (VLam     f) v = f v
    vapp (VNeutral n) v = VNeutral (NApp n v)

-- eval (ELet bs b) g = eval (inst b) γ
--   where
--     γ = g <> Map.fromList (zip (bindings bs) es)

--     inst = instantiate (es !!)
--     es   = map inst bs

eval (ECase c as) g =
    maybe (error "No matching alternates")
          (\(Alt _ _ s) -> eval (instantiate1 (EVar "x") s) gam)
          (find (\(Alt _ p _) -> match p) as)
  where
    match PWild _   = True
    match PVar  _   = True
    match PLit  x
        | x == res  = True
        | otherwise = False

    gam = Map.insert "x" res g
    res = eval c g
