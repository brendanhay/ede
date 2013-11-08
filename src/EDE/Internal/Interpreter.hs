{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module EDE.Internal.Interpreter where

import           Control.Applicative
import           Control.Monad              (liftM2)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Attoparsec.Number     (Number(..))
import           Data.Foldable              (Foldable, foldr', toList)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Buildable        as Build
import           Data.Text.Format           (Format)
import qualified Data.Text.Format           as Format
import           Data.Text.Format.Params    (Params)
import qualified Data.Vector                as Vector
import           EDE.Internal.Types
import           Prelude                    hiding (lookup)

-- FIXME:
-- Prevent rebinding/shadowing of variables

newtype Env a = Env { unwrap :: ErrorT EvalError (Reader Object) a }
    deriving (Functor, Applicative, Monad)

evaluate :: Object -> TExp Frag -> Either EvalError Frag
evaluate obj e = runReader (runErrorT . unwrap $ eval e) obj

bind :: (Object -> Object) -> Env a -> Env a
bind f = Env . (mapErrorT $ withReader f) . unwrap

require :: Meta -> Ident -> Env Value
require m (Ident k) = do
    mv <- Env $ Map.lookup k <$> lift ask
    maybe (throw m "binding '{}' doesn't exist." [k]) return mv

throw :: Params ps => Meta -> Format -> ps -> Env a
throw m f = Env . throwError . EvalError m . Format.format f

eval :: TExp a -> Env a
eval (TFrag _ b) = return b
eval (TText _ t) = return t
eval (TBool _ b) = return b
eval (TInt  _ i) = return i
eval (TDbl  _ d) = return d
eval (TVar  m v) = require m v >>= render m

eval (TApp _ a b) = liftM2 (<>) (eval a) (eval b)

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
        | Vector.null a = eval r
--        | Just s' <- s  = indexed s' . zip indices $ Vector.toList a
        | otherwise     = consequent a

    loop e = throw m "for loop expects an array or hashmap at '{}', got: {}"
        [show i, show e]

    consequent xs = do
        fs <- mapM (\v -> bind (ins p v) $ eval l) $ toList xs
        return $ foldr' (<>) mempty fs

    -- indexed s' = mapM_ (\(v, n) -> bind (ins p v . ins s' n) $ eval l)

    -- indices = Number . I <$> [1..]

    ins = Map.insert

evalM2 :: (a -> a -> b) -> TExp a -> TExp a -> Env b
evalM2 f x y = liftM2 f (eval x) (eval y)

render :: Meta -> Value -> Env Frag
render _ (Number (I n)) = return $ Build.build n
render _ (Number (D d)) = return $ Build.build d
render _ (Bool   b)     = return $ Build.build b
render _ (String s)     = return $ Build.build s
render m (Object _)     = renderError m "object"
render m (Array  _)     = renderError m "array"
render m Null           = renderError m "null"

renderError :: Meta -> LText -> Env a
renderError m = throw m "unable to render {} value." . (:[])

-- expression (ELoop (Bind (Ident prim) (fmap unident -> sec)) b l r) = do
--     b' <- require b
--     case b' of
--         (Array  a) -> array a
--         (Object o) -> object o
--         e -> failf "for loop expects an array at '{}' but got: {}"
--             [show prim, show e]
--   where
--     array a
--         | Vector.null a = alternate
--         | otherwise     = maybe (nominal a) (`indexed` a) sec

--     indexed s = zipWithM_ (\i n -> conseq $ ins prim i . ins s n) ns
--         . Vector.toList

--     ns = Number . I <$> [1..]

--     object o
--         | Map.null o = alternate
--         | otherwise  = maybe (nominal $ Map.elems o) (`keyed` o) sec

--     keyed s = mapM_ (\(x, y) -> conseq $ ins prim (String x) . ins s y)
--         . Map.toList

--     nominal :: Foldable t => t Value -> Env ()
--     nominal = mapM_ (\v -> conseq $ ins prim v)

--     conseq f  = bind f $ mapM_ expression l
--     alternate = mapM_ expression r

--     ins = Map.insert
