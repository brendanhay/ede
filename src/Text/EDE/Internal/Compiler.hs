{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Text.EDE.Internal.Compiler
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Compiler (compile) where

import           Control.Applicative
import           Control.Monad                 (liftM2)
import           Control.Monad.Trans.Reader
import           Data.Aeson                    (Object, Value(..))
import           Data.Attoparsec.Number        (Number(..))
import           Data.Foldable                 (Foldable, foldrM)
import qualified Data.HashMap.Strict           as Map
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text.Buildable           as Build
import qualified Data.Text.Lazy                as LText
import           Data.Text.Lazy.Builder        (Builder)
import qualified Data.Vector                   as Vector
import           Text.EDE.Internal.Environment
import           Text.EDE.Internal.Types

-- FIXME:
-- Prevent rebinding/shadowing of variables

compile :: TExp Frag -> Env Builder
compile = (render =<<) . eval

eval :: TExp a -> Env a
eval (TFrag _ b) = return b
eval (TText _ t) = return t
eval (TBool _ b) = return b
eval (TInt  _ i) = return i
eval (TDbl  _ d) = return d

eval (TVar m i TTText) = require m i >>= \(String s)     -> return s
eval (TVar m i TTBool) = require m i >>= \(Bool   b)     -> return b
eval (TVar m i TTInt)  = require m i >>= \(Number (I n)) -> return n
eval (TVar m i TTDbl)  = require m i >>= \(Number (D n)) -> return n
eval (TVar m i TTMap)  = require m i >>= \(Object o)     -> return o
eval (TVar m i TTList) = require m i >>= \(Array  a)     -> return a
eval (TVar m i t) =
    compileError m "type mistmatch for binding {} :: {}" [show i, show t]

eval (TCons _ a b) = do
    a' <- eval a
    b' <- eval b
    renderM2 a' b'

eval (TNeg _ e) = not <$> eval e

eval (TBin _ And x y) = evalM2 (&&) x y
eval (TBin _ Or  x y) = evalM2 (||) x y

eval (TRel _ Equal        x y) = evalM2 (==) x y
eval (TRel _ NotEqual     x y) = evalM2 (/=) x y
eval (TRel _ Greater      x y) = evalM2 (>)  x y
eval (TRel _ GreaterEqual x y) = evalM2 (>=) x y
eval (TRel _ Less         x y) = evalM2 (<)  x y
eval (TRel _ LessEqual    x y) = evalM2 (<=) x y

eval (TCond _ p l r) = do
    p' <- eval p
    eval $ if p' then l else r

eval (TLoop _ (destruct -> (p, s)) i@(TVar _ _ TTList) l r) = eval i >>= f
  where
    f x | Vector.null x = eval r
        | Just s' <- s  = loop (indexed s') . zip indices $ Vector.toList x
        | otherwise     = loop body x

    indexed s' (v, n) = bind (Map.insert p v . Map.insert s' n) l

    indices = Number . I <$> [1..]

    body v = bind (Map.insert p v) l

eval (TLoop _ (destruct -> (p, s)) i@(TVar _ _ TTMap) l r) = eval i >>= f
  where
    f x | Map.null x   = eval r
        | Just s' <- s = loop (keyed s') $ Map.toList x
        | otherwise    = loop body $ Map.elems x

    keyed s' (k, v) = bind (Map.insert p (String k) . Map.insert s' v) l

    body v = bind (Map.insert p v) l

eval (TLoop m _ e _ _) =
    compileError m "invalid loop expression {}" [show e]

eval (TScope _ i@(TVar _ _ TTMap) e) = eval i >>= \env -> bind (const env) e
eval (TScope m _ e) =
    compileError m "invalid scope expression {}" [show e]

evalM2 :: (a -> a -> b) -> TExp a -> TExp a -> Env b
evalM2 f x y = liftM2 f (eval x) (eval y)

destruct :: Bind -> (Text, Maybe Text)
destruct (Bind _ p ms) = (ident p, ident <$> ms)

bind :: (Object -> Object) -> TExp Frag -> Env Frag
bind f = withReaderT f . eval

loop :: Foldable t => (a -> Env Frag) -> t a -> Env Frag
loop f = foldrM (\v b -> f v >>= flip renderM2 b) (FBld mempty)

renderM2 :: Frag -> Frag -> Env Frag
renderM2 a b = FBld <$> liftM2 (<>) (render a) (render b)

render :: Frag -> Env Builder
render (FBld b)   = return b
render (FVar m i) = require m i >>= build m

build :: Meta -> Value -> Env Builder
build _ (Number (I n)) = return $ Build.build n
build _ (Number (D d)) = return $ Build.build d
build _ (Bool   b)     = return $ Build.build b
build _ (String s)     = return $ Build.build s
build m (Object _)     = buildError m "object"
build m (Array  _)     = buildError m "array"
build m Null           = buildError m "null"

buildError :: Meta -> LText.Text -> Env a
buildError m = compileError m "unable to build {} value." . (:[])
