{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

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
import           Control.Arrow
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
    | VLam (Value -> Maybe Value)

literal :: Value -> Maybe Lit
literal (VLit l) = Just l
literal _        = Nothing

instance Show Value where
    show (VLit l)   = show l
    show (VLam _)   = "VLam <function>"

eval :: Exp Id -> Maybe Value
eval = go env . nf
  where
    go :: (a -> Maybe Value) -> Exp a -> Maybe Value
    go _ (ELit l)   = Just (VLit l)
    go g (EVar v)   = g v
    go g (ELam b)   = Just . VLam $ \x -> go (ext g x) (unscope b)
    go g (EApp f a) = join $ vapp <$> go g f <*> go g a

    ext :: (a -> Maybe Value) -> Value -> Var () (Exp a) -> Maybe Value
    ext _ x (B ()) = Just x
    ext g _ (F a)  = go g a

vapp :: Value -> Value -> Maybe Value
vapp (VLam f) v = f v
vapp f        a = Nothing

prelude :: Id -> Maybe Value
prelude k = Map.lookup k prelude
  where
    prelude = Map.fromList $ base ++ num ++ realFrac

    base =
        [ "." @: compose
        ]

    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    compose = VLam $ \f ->
        Just . VLam $ \g ->
            case (f, g) of
                (VLam x, VLam y) -> Just $ VLam (y >=> x)
                e                -> Nothing


    num =
        [ "+"      @: binary (+)
        , "-"      @: binary (-)
        , "*"      @: binary (*)
        , "abs"    @: unary abs
        , "signum" @: unary signum
        , "negate" @: unary negate
        ]

    realFrac =
        [ "truncate" @: unary (fromIntegral . truncate)
        , "round"    @: unary (fromIntegral . round)
        , "ceiling"  @: unary (fromIntegral . ceiling)
        , "floor"    @: unary (fromIntegral . floor)
        ]

    unary :: (Scientific -> Scientific) -> Value
    unary = quote

    binary :: (Scientific -> Scientific -> Scientific) -> Value
    binary = quote

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

class Quote a where
    quote :: a -> Value

(@:) :: Quote a => Id -> a -> (Id, Value)
k @: v = (k, quote v)

instance Quote Value where
    quote = id

instance Quote Scientific where
    quote = VLit . LNum

class Unquote a where
    unquote :: Value -> Maybe a

instance Unquote Scientific where
    unquote (VLit (LNum n)) = Just n
    unquote _               = Nothing

instance (Unquote a, Quote b) => Quote (a -> b) where
    quote f = VLam (fmap (quote . f) . unquote)

instance (Unquote a, Unquote b, Quote c) => Quote (a -> b -> c) where
    quote f = VLam $ \x ->
        Just . VLam $ \y ->
            case y of
                VLam g -> join $ vapp <$> pure (quote f) <*> g x
                _      -> quote <$> (f <$> unquote x <*> unquote y)
