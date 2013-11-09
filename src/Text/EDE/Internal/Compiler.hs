{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.EDE.Internal.Compiler where

import           Control.Applicative
import           Control.Monad              (liftM2)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (Object, Value(..))
import           Data.Attoparsec.Number     (Number(..))
import           Data.Foldable              (Foldable, foldr')
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import qualified Data.Text                  as Text
import qualified Data.Text.Buildable        as Build
import           Data.Text.Format           (Format)
import qualified Data.Text.Format           as Format
import           Data.Text.Format.Params    (Params)
import qualified Data.Text.Lazy             as LText
import qualified Data.Vector                as Vector
import           Text.EDE.Internal.Types

-- FIXME:
-- Prevent rebinding/shadowing of variables

type Env = ReaderT Object Result

compile :: TExp a -> Object -> Result a
compile e = runReaderT (eval e)

eval :: TExp a -> Env a
eval (TFrag _ b) = return b
eval (TText _ t) = return t
eval (TBool _ b) = return b
eval (TInt  _ i) = return i
eval (TDbl  _ d) = return d
eval (TVar  m v) = require m v >>= build m

eval (TApp _ a b) = evalM2 (<>) a b

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

eval (TLoop m b i l r) = require m i >>= loop
  where
    p = ident $ bindPrim b
    s = ident <$> bindSec b

    loop (Array a)
        | Vector.null a = alternate
        | Just s' <- s  = scope (indexed s') . zip indices $ Vector.toList a
        | otherwise     = scope consequent $ Vector.toList a
    loop (Object o)
        | Map.null o    = alternate
        | Just s' <- s  = scope (keyed s') $ Map.toList o
        | otherwise     = scope consequent $ Map.elems o
    loop e = throw m
        "loop target '{}' was expected to be array or hashmap, got: {}"
        [ident i, Text.pack $ show e]

    scope f = fmap (foldr' (<>) mempty) . mapM f
    bind  f = withReaderT f $ eval l

    alternate    = eval r
    consequent v = bind (ins p v)

    indexed s' (v, n) = bind (ins p v . ins s' n)
    keyed   s' (k, v) = bind (ins p (String k) . ins s' v)

    indices = Number . I <$> [1..]

    ins = Map.insert

evalM2 :: (a -> a -> b) -> TExp a -> TExp a -> Env b
evalM2 f x y = liftM2 f (eval x) (eval y)

build :: Meta -> Value -> Env Frag
build _ (Number (I n)) = return $ Build.build n
build _ (Number (D d)) = return $ Build.build d
build _ (Bool   b)     = return $ Build.build b
build _ (String s)     = return $ Build.build s
build m (Object _)     = buildError m "object"
build m (Array  _)     = buildError m "array"
build m Null           = buildError m "null"

buildError :: Meta -> LText -> Env a
buildError m = throw m "unable to build renderable {} value." . (:[])

require :: Meta -> Ident -> Env Value
require m (Ident k) = ask >>=
    maybe (throw m "binding '{}' doesn't exist." [k]) return . Map.lookup k

throw :: Params ps => Meta -> Format -> ps -> Env a
throw m f = lift . CompileError m . LText.unpack . Format.format f
