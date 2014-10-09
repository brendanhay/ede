{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

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

import           Control.Applicative
import           Control.Arrow                     (first)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Aeson                        hiding (Result, Success, Error)
import           Data.Foldable                     (Foldable, foldlM)
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as Map
import           Data.List                         (sortBy)
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Scientific                   (base10Exponent)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Buildable               as Build
import           Data.Text.Format                  (Format)
import           Data.Text.Format.Params           (Params)
import qualified Data.Text.Lazy                    as LText
import           Data.Text.Lazy.Builder            (Builder)
import           Data.Text.Lazy.Builder.Scientific
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Debug.Trace
import           Text.EDE.Internal.Quoter
import           Text.EDE.Internal.Types

data Env = Env
    { _filters   :: HashMap Text Quoted
    , _templates :: HashMap Text Exp
    , _variables :: Object
    }

type Context = StateT Env Result

render :: HashMap Text Quoted
       -> HashMap Text Exp
       -> Exp
       -> Object
       -> Result Builder
render fs ts e o = flip evalStateT (Env fs ts o) $ do
    v ::: vt <- eval e >>= build (Meta "render" 0 0)
    Eq       <- ceq (meta e) TBld vt
    return v

eval :: Exp -> Context TExp

eval (ELit _ l) = return $
    case l of
        LBool b -> b ::: TBool
        LNum  n -> n ::: TNum
        LText t -> t ::: TText

eval (EBld _ b) = return $ b ::: TBld

eval (EVar _ v) = cast <$> variable v

eval (EFun m i) = throw m "unable to evaluate unapplied function {}" [i]

-- eval (EApp m (EFun _ (Id _ "show")) e) = do
--     e' ::: et <- eval e
--     Show       <- cshow m et
--     return (LText.pack (show e') ::: TText)

-- This would be an operator
eval (EApp _ (EFun m k) x) = do
    y <- eval x
    f <- function
    q <- lift (qapp f (quote y))

    return (q ::: TFun)
--    trace (show (k, f, quote y, q, x)) undefined
--    build m (cast v)
  where
    function = do
        q <- Map.lookup (idName k) <$> gets _filters
        maybe (throw m "filter {} doesn't exist." [k])
              return
              q

eval (EApp m a b) = do
    a' ::: at <- eval a

    case at

    b' ::: TBld <- f b
    return ((a' <> b') ::: TBld)
  where
    go (q ::: TFun) = eval b >>= qapp q 
    go x            = build m x

eval (EApp m a b) = do
    a' ::: TBld <- f a
    b' ::: TBld <- f b
    return ((a' <> b') ::: TBld)
  where
    f x = eval x >>= build m

-- eval (EApp _  x (EFun m L k)) = do
--     y <- eval x
--     g <- function
--     v <- lift (qeval k g y)
--     build m (cast v)
--   where
--     function = do
--         q <- Map.lookup (idName k) <$> gets _filters
--         maybe (throw m "filter {} doesn't exist." [k])
--               return
--               q

-- eval (EApp _ e UNil) = eval e
-- eval (EApp _ UNil e) = eval e

-- eval (EApp _ v@(EVar m (Id i)) e) = eval v >>= f
--   where
--     f (o ::: TMap) = bind (const o) e
--     f (_ ::: t)    =
--         throw m "variable {} :: {} doesn't supported nested accessors."
--             [Text.unpack i, show t]

-- eval (ENeg _ e) = do
--     e' ::: TBool <- predicate e
--     return $ not e' ::: TBool

-- eval (EBin _ op a b) = do
--     a' ::: TBool <- predicate a
--     b' ::: TBool <- predicate b
--     return $ f op a' b' ::: TBool
--   where
--     f And = (&&)
--     f Or  = (||)

-- eval (ERel m op a b) = do
--     a' ::: at <- eval a
--     b' ::: bt <- eval b
--     Eq        <- ceq m at bt
--     Ord       <- cord m at
--     return $ f op a' b' ::: TBool
--   where
--     f Eq        = (==)
--     f NotEq     = (/=)
--     f Greater      = (>)
--     f GreaterEq = (>=)
--     f Less         = (<)
--     f LessEq    = (<=)

-- eval (ECond _ p a b) = do
--     p' ::: TBool <- predicate p
--     eval $ if p' then a else b

eval (ELet _ (Id _ k) e) = do
    v <- either reify variable e
    modify $ \s -> s { _variables = Map.insert k v (_variables s) }
    return (mempty ::: TBld)
  where
    reify (LBool b) = return (Bool   b)
    reify (LNum  n) = return (Number n)
    reify (LText t) = return (String (LText.toStrict t))

-- FIXME: We have to recompute c everytime due to the predicate ..
eval (ECase m p ws) = do
    r <- cond ws
    eval (fromMaybe (EBld m mempty) r)
  where
    cond []          = return Nothing
    cond ((a, e):as) =
        case a of
            PWild  -> return (Just e)
            PVar v -> eval (EVar m v) >>= match e as
            PLit l -> eval (ELit m l) >>= match e as

    match e as (y ::: TBool) = do
        x <- predicate p
        if x == y
            then return (Just e)
            else cond as

    match e as (y ::: yt) = do
        x ::: xt <- eval p
        Eq       <- ceq m xt yt
        if x == y
           then return (Just e)
           else cond as

eval (ELoop m (Id _ k) v bdy a) = eval (EVar m v) >>= f >>= loop k bdy a
  where
    f (x ::: TList) = return $ Col (Vector.length x) (vec x)
    f (x ::: TMap)  = return $ Col (Map.size x)      (hmap x)
    f (_ ::: t)     = throw m "invalid loop target {}" [show t]

    vec :: Vector Value -> Vector (Maybe Text, Value)
    vec = Vector.map (Nothing,)

    hmap :: HashMap Text Value -> [(Maybe Text, Value)]
    hmap = map (first Just) . sortBy (comparing fst) . Map.toList

eval (EIncl m k mu) = do
    te <- template m k
    s  <- maybe (return global) local' mu
    bind s (eval te)
  where
    global o = fromPairs ["scope" .= o]

    local' u = do
        e ::: et <- eval u
        JS       <- cjs m et
        return . const $ fromPairs ["scope" .= e]

loop :: Text -> Exp -> Maybe Exp -> Col' -> Context TExp
loop _ a b (Col 0 _)  = eval (fromMaybe (EBld (meta a) mempty) b)
loop k a _ (Col l xs) = fmap ((::: TBld) . snd) $ foldlM iter (1, mempty) xs
  where
    iter (n, bld) x = do
        shadowed
        a' ::: at <- bind (Map.insert k $ context n x) (eval a)
        Eq        <- ceq (meta a) at TBld
        return (n + 1, bld <> a')

    context n (mk, v) = object $
        [ "value"      .= v
        , "length"     .= l
        , "index"      .= n
        , "index0"     .= (n - 1)
        , "remainder"  .= (l - n)
        , "remainder0" .= (l - n - 1)
        , "first"      .= (n == 1)
        , "last"       .= (n == l)
        , "odd"        .= (n `mod` 2 == 1)
        , "even"       .= (n `mod` 2 == 0)
        ] ++ maybe [] (\x -> ["key" .= x]) mk

    shadowed =
        let f x = [Text.unpack k, show x]
            g   = throw  (meta a) "binding {} shadows existing variable {}." . f
        in gets _variables >>= maybe (return ()) g . Map.lookup k

bind :: (Object -> Object) -> Context a -> Context a
bind f = withStateT (\x -> x { _variables = f $ _variables x })

-- | A variable can be tested for truthiness, but a non-whnf expr cannot.
predicate :: Exp -> Context Bool
predicate x = do
    r <- evalStateT (eval x) <$> get
    case x of
        EVar{} -> f r
        _      -> g r
  where
    f :: Result TExp -> Context Bool
    f (Success (_ ::: TNil))  = lift (Success False)
    f (Success (p ::: TBool)) = lift (Success p)
    f (Success _)             = lift (Success True)
    f _                       = lift (Success False)

    g :: Result TExp -> Context Bool
    g (Error   e)          = lift (Error e)
    g (Success (p ::: pt)) = do
        Eq <- ceq (meta x) pt TBool
        lift (Success p)

variable :: Var -> Context Value
variable (Var is) = gets _variables >>= go (NonEmpty.toList is) [] . Object
  where
    go []     _ v = return v
    go (k:ks) r v = do
        m <- nest v
        maybe (throw (meta k) "binding {} doesn't exist." [fmt (k:r)])
              (go ks (k:r))
              (Map.lookup (idName k) m)
      where
        nest :: Value -> Context Object
        nest (Object o)        = return o
        nest (cast -> _ ::: t) =
            throw (meta k) "variable {} :: {} doesn't supported nested accessors."
                [fmt (k:r), show t]

        fmt = Text.unpack . Text.intercalate "." . map idName

template :: Meta -> Text -> Context Exp
template m k = do
    ts <- gets _templates
    maybe (throw m "template {} is not in scope: {}"
              [k, Text.intercalate "," $ Map.keys ts])
          return
          (Map.lookup k ts)

build :: Meta -> TExp -> Context TExp
build _ (_ ::: TNil)  = return $ mempty ::: TBld
build _ (t ::: TText) = return $ Build.build t ::: TBld
build _ (b ::: TBool) = return $ Build.build b ::: TBld
build _ (n ::: TNum)  = return $ bld ::: TBld
  where
    bld | base10Exponent n == 0 = formatScientificBuilder Fixed (Just 0) n
        | otherwise             = scientificBuilder n

build _ (b ::: TBld)  = return $ b ::: TBld
build m (_ ::: t)     = throw m "unable to render variable of type {}" [show t]

throw :: Params ps => Meta -> Format -> ps -> Context a
throw m f = lift . throwError (Evaluator m) f

cast :: Value -> TExp
cast v = case v of
    Null     -> () ::: TNil
    Bool   b -> b  ::: TBool
    Number n -> n  ::: TNum
    Object o -> o  ::: TMap
    Array  a -> a  ::: TList
    String t -> LText.fromStrict t ::: TText

data Col' where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Col'

data Eq' a b where
    Eq :: Eq' a a

ceq :: Meta -> Type a -> Type b -> Context (Eq' a b)
ceq _ TNil  TNil  = return Eq
ceq _ TText TText = return Eq
ceq _ TBool TBool = return Eq
ceq _ TNum  TNum  = return Eq
ceq _ TBld  TBld  = return Eq
ceq _ TMap  TMap  = return Eq
ceq _ TList TList = return Eq
ceq m a b = throw m "type equality check of {} ~ {} failed." [show a, show b]

data Ord' a where
    Ord :: Ord a => Ord' a

cord :: Meta -> Type a -> Context (Ord' a)
cord _ TNil  = return Ord
cord _ TText = return Ord
cord _ TBool = return Ord
cord _ TNum  = return Ord
cord m t = throw m "constraint check of Ord a => a ~ {} failed." [show t]

data Show' a where
    Show :: Show a => Show' a

cshow :: Meta -> Type a -> Context (Show' a)
cshow _ TNil  = return Show
cshow _ TText = return Show
cshow _ TBool = return Show
cshow _ TNum  = return Show
cshow _ TBld  = return Show
cshow _ TMap  = return Show
cshow _ TList = return Show
cshow m t = throw m "constraint check of Show a => a ~ {} failed." [show t]

data JS' a where
    JS :: ToJSON a => JS' a

cjs :: Meta -> Type a -> Context (JS' a)
cjs _ TNil  = return JS
cjs _ TText = return JS
cjs _ TBool = return JS
cjs _ TNum  = return JS
cjs _ TMap  = return JS
cjs _ TList = return JS
cjs m t = throw m "constraint check of ToJSON a => a ~ {} failed." [show t]
