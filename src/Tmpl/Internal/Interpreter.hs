{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tmpl.Internal.Interpreter where

import           Control.Applicative
import           Control.Monad              (liftM2, zipWithM_, foldM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Error  hiding (Error)
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Attoparsec.Number     (Number(..))
import           Data.Foldable              (Foldable, foldr', mapM_, toList, foldrM)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Buildable        (Buildable)
import qualified Data.Text.Buildable        as Build
import           Data.Text.Format           (Format)
import qualified Data.Text.Format           as Format
import           Data.Text.Format.Params    (Params)
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Vector                as Vector
import           Prelude                    hiding (lookup, mapM_)
import           Tmpl.Internal.Types

-- FIXME:
-- Prevent rebinding/shadowing of variables

newtype Env a = Env { unwrap :: ErrorT String (Reader Object) a }
    deriving (Functor, Applicative, Monad)

evaluate :: Object -> TExp Frag -> Either String Frag
evaluate obj = runEnv obj . eval

runEnv :: Object -> Env Frag -> Either String Frag
runEnv obj env = runReader (runErrorT $ unwrap env) obj

bind :: (Object -> Object) -> Env a -> Env a
bind f = Env . (mapErrorT $ withReader f) . unwrap

lookup :: Ident -> Env (Maybe Value)
lookup (Ident k) = Env $ Map.lookup k <$> lift ask

require :: Ident -> Env Value
require k = lookup k !? fmt "binding '{}' doesn't exist." [k]

(!?) :: Env (Maybe a) -> String -> Env a
(!?) m e = m >>= maybe (fail e) return

failf :: (Monad m, Params a) => Format -> a -> m b
failf f = fail . fmt f

fmt :: Params ps => Format -> ps -> String
fmt f = LText.unpack . Format.format f

eval :: TExp a -> Env a
eval (TFrag b) = return b
eval (TText t) = return t
eval (TBool b) = return b
eval (TInt  i) = return i
eval (TDbl  d) = return d
eval (TVar  v) = require v >>= render

eval (TApp a b) = liftM2 (<>) (eval a) (eval b)

eval (TNeg e) = not <$> eval e

eval (TBin And x y) = evalM2 (&&) x y
eval (TBin Or  x y) = evalM2 (||) x y

eval (TRel Equal        x y) = evalM2 (==) x y
eval (TRel NotEqual     x y) = evalM2 (/=) x y
eval (TRel Greater      x y) = evalM2 (>)  x y
eval (TRel GreaterEqual x y) = evalM2 (>=) x y
eval (TRel Less         x y) = evalM2 (<)  x y
eval (TRel LessEqual    x y) = evalM2 (<=) x y

eval (TCond p l r) = do
    p' <- eval p
    eval $ if p' then l else r

eval (TLoop (Bind prim msec) i l r) = require i >>= loop
  where
    p = unident prim
    s = unident <$> msec

    loop (Array a)
        | Vector.null a = eval r
--        | Just s' <- s  = indexed s' . zip indices $ Vector.toList a
        | otherwise     = consequent a

    loop e = failf "for loop expects an array or hashmap at '{}', got: {}"
        [show i, show e]

    consequent xs = do
        fs <- mapM (\v -> bind (ins p v) $ eval l) $ toList xs
        return $ foldr' (<>) mempty fs

    -- indexed s' = mapM_ (\(v, n) -> bind (ins p v . ins s' n) $ eval l)

    -- indices = Number . I <$> [1..]

    ins = Map.insert

evalM2 :: (a -> a -> b) -> TExp a -> TExp a -> Env b
evalM2 f x y = liftM2 f (eval x) (eval y)

render :: Monad m => Value -> m Frag
render (Number (I n)) = return $ Build.build n
render (Number (D d)) = return $ Build.build d
render (Bool   b)     = return $ Build.build b
render (String s)     = return $ Build.build s
render (Object _)     = fail "unable to render object value."
render (Array  _)     = fail "unable to render array value."
render Null           = fail "unable to render null value."

    -- binder :: Text -> TExp a -> Value -> Env (TExp a)

    -- binder :: Text -> Text -> Value -> Env Text
    -- binder k e v = bind (ins k v) $ return e

    -- bindScopes :: (a -> Value -> Env a) -> [Value] -> Env a
    -- bindScopes f = foldM f ""


-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

--        | Just s' <- s  = mapM (bind (\v -> ins p v) $ eval l) a

    -- loop (Object o)
    --     | Map.null o    -> eval r
    --     | Just s <- ms  -> keyed
    --     | otherwise     -> bindScope $ Map.elems o

    -- zipWithM_

    -- bind :: (Object -> Object) -> Env a -> Env a
    -- bind f = Env . (mapErrorT . mapWriterT $ withReader f) . unwrap

    -- indexed v n = ins prim i . ins s n
    --     . Vector.toList

    -- indices = Number . I <$> [1..]

    -- -- keyed s = mapM_ (\(x, y) -> conseq $ ins prim (String x) . ins s y)
    -- --     . Map.toList

    -- bindScope :: Foldable t => t Value -> Env a
    -- bindScope = mapM_ (\v -> bind $ ins (unident k) v)

-- expression :: Exp -> Env ()

-- expression (ELit l) = build l
-- expression (EVar k) = require k >>= render
-- expression (ECond expr l r) = do
--     p <- condition expr
--     mapM_ expression $ if p then l else r

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

-- expression expr = failf "invalid expression: {}" [expr]

-- condition :: Exp -> Env Bool
-- condition (ELit LNil)      = return False
-- condition (ELit (LBool b)) = return b
-- condition (ELit _)         = return True
-- condition (EVar v)         = isJust <$> lookup v
-- condition (ENeg expr)      = not <$> condition expr
-- condition (EBin op x y)    = binary op x y
-- condition (ERel op x y)    = relational op x y
-- condition expr             = failf "invalid '{}' as condition predicate." [expr]

-- literal :: Exp -> Env Literal
-- literal (ELit l) = return l
-- literal (EVar v) = do
--     x <- require v
--     case x of
--         String s     -> return $ UText s
--         Number (I n) -> return $ UInt  n
--         Number (D d) -> return $ UDbl  d
--         Bool   b     -> return $ UBool b
--         Null         -> return $ UNil
--         e            -> failf "invalid used of '{}' as literal value." [show e]
-- literal expr = failf "invalid use of '{}' as literal value." [show expr]

-- binary :: BinOp -> Exp -> Exp -> Env Bool
-- binary op x y = do
--     a <- condition x
--     b <- condition y
--     return $ case op of
--         And -> a && b
--         Or  -> a || b

-- relational :: RelOp -> Exp -> Exp -> Env Bool
-- relational op l r = cmp <$> literal l <*> literal r >>= fmap (result op)
--   where
--     cmp (LChar x) (LChar y) = f x y
--     cmp (LText x) (LText y) = f x y
--     cmp (LBool x) (LBool y) = f x y
--     cmp (LInt  x) (LInt  y) = f x y
--     cmp (LDoub x) (LDoub y) = f x y

--     cmp LNil LNil = return EQ
--     cmp _    LNil = return GT
--     cmp LNil _    = return LT

--     cmp x y = failf "invalid comparison between {} and {} using {}"
--         [show x, show y, show op]

--     f x y = return $ x `compare` y

--     result Equal EQ        = True
--     result GreaterEqual EQ = True
--     result LessEqual EQ    = True
--     result Greater GT      = True
--     result GreaterEqual GT = True
--     result Less LT         = True
--     result LessEqual LT    = True
--     result NotEqual GT     = True
--     result NotEqual LT     = True
--     result _  _            = False
