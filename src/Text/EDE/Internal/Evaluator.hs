{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.Scientific
import           Data.String
import qualified Data.Text           as Text
import           Data.Text (Text)
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Checker

data Value
    = VLit Lit
    | VLam (Value -> Value)

literal :: Value -> Maybe Lit
literal (VLit l) = Just l
literal _        = Nothing

instance Show Value where
    show (VLit l)   = show l
    show (VLam _)   = "VLam <function>"

eval :: Exp Id -> Value
eval = go env . nf
  where
    go :: (a -> Value) -> Exp a -> Value
    go _ (ELit l)   = VLit l
    go g (EVar v)   = g v
    go g (ELam b)   = VLam $ \x -> go (ext g x) (unscope b)

    go g (EApp f a) = vapp (go g f) (go g a)

    ext _ x (B ()) = x
    ext g _ (F a)  = go g a

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp f        a = error $ "vapp" ++ show (f, a)

-- data Typed a
--     = FBool (Bool    -> a)
--     | FNum  (Integer -> a)
--     | FText (Text    -> a)

-- instance Show (Typed a) where
--     show (FBool _) = "bool"
--     show (FNum  _) = "integer"
--     show (FText _) = "string"

-- typed :: Typed a -> Maybe (Value -> Value) -> Value -> Either String a
-- typed t Nothing (VLam _)  = Left "Unapplied function instead of literal"
-- typed t (Just f) (VLam x) = f x
-- typed t _       (VLit l)  =
--     case (t, l) of
--         (FBool f, LBool n) -> Right (f n)
--         (FNum  f, LNum  n) -> Right (f n)
--         (FText f, LText n) -> Right (f n)
--         _ -> Left $ "Literal type mismatch: " ++ show (t, l)

class Quoted a where
    quote :: a -> Value

instance Quoted (Scientific -> Scientific -> Scientific) where
    quote f = VLam $ \x ->
        case x of
            VLit (LNum a) -> VLam $ \y ->
                case y of
                    VLit (LNum b) -> VLit $ LNum (f a b)
                    VLit l        -> error $ "Invalid type: " ++ show l
                    VLam g        -> vapp (quote f) (g x)
            _ -> error $ "Numeric mismatch: " ++ show x

env :: Id -> Value
env k = fromMaybe
    (error $ "Undefined variable: " ++ Text.unpack k)
    (Map.lookup k m)
  where
    m = Map.fromList
        [ ("+", quote ((+) :: Scientific -> Scientific -> Scientific))
        , ("-", quote ((-) :: Scientific -> Scientific -> Scientific))
        , ("/", quote ((/) :: Scientific -> Scientific -> Scientific))
        , (".", dot)
        ]

    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    dot = VLam $ \f -> VLam $ \g ->
        case (f, g) of
            (VLam x, VLam y) -> VLam (x . y)
            e                -> error $ "Argument mismatch to . " ++ show e

-- | Compute the normal form of an expression
nf :: Exp a -> Exp a
nf e@(ELit l) = l `deepseq` e
nf e@EVar{}   = e
nf (ELam b)   = ELam . toScope . nf $ fromScope b
nf (EApp f a) =
    case whnf f of
        ELam b -> nf (instantiate1 a b)
        f'     -> EApp (nf f') (nf a)
nf (ELet bs b) = nf (inst b)
  where
    inst = instantiate (es !!)
    es   = map inst bs

whnf :: Exp a -> Exp a
whnf e@ELit{}    = e
whnf e@EVar{}    = e
whnf e@ELam{}    = e
whnf (EApp f a)  =
    case whnf f of
        ELam b -> whnf (instantiate1 a b)
        f'     -> EApp f' a
whnf (ELet bs b) = whnf (inst b)
  where
    inst = instantiate (es !!)
    es   = map inst bs
