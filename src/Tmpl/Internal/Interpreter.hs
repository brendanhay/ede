{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Tmpl.Internal.Interpreter where

import           Control.Applicative
import           Control.Monad              (zipWithM_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Error  hiding (Error)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Aeson
import           Data.Attoparsec.Number     (Number(..))
import           Data.Foldable              (Foldable, mapM_)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Text                  (Text)
import           Data.Text.Buildable        (Buildable)
import qualified Data.Text.Buildable        as Buildable
import           Data.Text.Format           (Format)
import qualified Data.Text.Format           as Format
import           Data.Text.Format.Params    (Params)
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Vector                as Vector
import           Prelude                    hiding (lookup, mapM_)
import           Tmpl.Internal.Types

-- FIXME:
-- Support binary conditional expressions
-- Prevent rebinding/shadowing of variables

type ReaderEnv = Reader Object
type WriterEnv = WriterT Builder ReaderEnv
type ErrorEnv  = ErrorT String WriterEnv

newtype Env a = Env { unwrap :: ErrorEnv a }
    deriving (Functor, Applicative, Monad)

evaluate :: Object -> [Expr] -> Either String Builder
evaluate obj = (\(r, b) -> fmap (const b) r) . runEnv obj . mapM_ expression

runEnv :: Object -> Env a -> (Either String a, Builder)
runEnv obj env = runReader (runWriterT . runErrorT $ unwrap env) obj

bind :: (Object -> Object) -> Env a -> Env a
bind f = Env . (mapErrorT . mapWriterT $ withReader f) . unwrap

lookup :: Ident -> Env (Maybe Value)
lookup (Ident k) = Env $ Map.lookup k <$> lift (lift ask)

require :: Ident -> Env Value
require k = lookup k !? fmt "binding '{}' doesn't exist." [k]

(!?) :: Env (Maybe a) -> String -> Env a
(!?) m e = m >>= maybe (fail e) return

-- failf :: (Monad m, Params ps) => Format -> ps -> m ()
failf f = fail . fmt f

fmt :: Params ps => Format -> ps -> String
fmt f = LText.unpack . Format.format f

build :: Buildable a => a -> Env ()
build = Env . lift . tell . Buildable.build

expression :: Expr -> Env ()
expression (ELit l) = build l
expression (EVar k) = require k >>= render
expression (ECond expr l r) = do
    p <- condition expr
    mapM_ expression $ if p then l else r

expression (ELoop (Bind (Ident prim) (fmap unident -> sec)) b l r) = do
    b' <- require b
    case b' of
        (Array  a) -> array a
        (Object o) -> object o
        e -> failf "for loop expects an array at '{}' but got: {}"
            [show prim, show e]
  where
    array a
        | Vector.null a = alternate
        | otherwise     = maybe (nominal a) (`indexed` a) sec

    indexed s = zipWithM_ (\i n -> conseq $ ins prim i . ins s n) ns
        . Vector.toList

    ns = Number . I <$> [1..]

    object o
        | Map.null o = alternate
        | otherwise  = maybe (nominal $ Map.elems o) (`keyed` o) sec

    keyed s = mapM_ (\(x, y) -> conseq $ ins prim (String x) . ins s y)
        . Map.toList

    nominal :: Foldable t => t Value -> Env ()
    nominal = mapM_ (\v -> conseq $ ins prim v)

    conseq f  = bind f $ mapM_ expression l
    alternate = mapM_ expression r

    ins = Map.insert

expression expr = failf "invalid expression: {}" [expr]

condition :: Expr -> Env Bool
condition (ELit LNil)      = return False
condition (ELit (LBool b)) = return b
condition (ELit _)         = return True
condition (EVar v)         = isJust <$> lookup v
condition (ENeg expr)      = not <$> condition expr
condition (EBin op x y)    = binary op x y
condition (ERel op x y)    = relational op x y
condition expr             = failf "invalid '{}' as condition predicate." [expr]

literal :: Expr -> Env Literal
literal (ELit l) = return l
literal (EVar v) = do
    x <- require v
    case x of
        String s     -> return $ LText s
        Number (I n) -> return $ LInt  n
        Number (D d) -> return $ LDoub d
        Bool   b     -> return $ LBool b
        Null         -> return $ LNil
        e            -> failf "invalid used of '{}' as literal value." [show e]
literal expr = failf "invalid use of '{}' as literal value." [show expr]

binary :: BinOp -> Expr -> Expr -> Env Bool
binary op x y = do
    a <- condition x
    b <- condition y
    return $ case op of
        And -> a && b
        Or  -> a || b

relational :: RelOp -> Expr -> Expr -> Env Bool
relational op l r = cmp <$> literal l <*> literal r >>= fmap (result op)
  where
    cmp (LChar x) (LChar y) = f x y
    cmp (LText x) (LText y) = f x y
    cmp (LBool x) (LBool y) = f x y
    cmp (LInt  x) (LInt  y) = f x y
    cmp (LDoub x) (LDoub y) = f x y

    cmp LNil LNil = return EQ
    cmp _    LNil = return GT
    cmp LNil _    = return LT

    cmp x y = failf "invalid comparison between {} and {} using {}"
        [show x, show y, show op]

    f x y = return $ x `compare` y

    result Equal EQ        = True
    result GreaterEqual EQ = True
    result LessEqual EQ    = True
    result Greater GT      = True
    result GreaterEqual GT = True
    result Less LT         = True
    result LessEqual LT    = True
    result NotEqual GT     = True
    result NotEqual LT     = True
    result _  _            = False

render :: Value -> Env ()
render (String s)     = build s
render (Number (I n)) = build n
render (Number (D d)) = build d
render (Bool   b)     = build b
render (Object _)     = fail "unable to render object value."
render (Array  _)     = fail "unable to render array value."
render Null           = fail "unable to render null value."
