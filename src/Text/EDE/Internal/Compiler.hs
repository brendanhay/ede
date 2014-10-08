{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

-- Module      : Text.EDE.Internal.Compiler
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Compiler where

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
import           Text.EDE.Internal.Types

data TExp = forall a. Eq a => a ::: Type a

instance Show TExp where
    show (_ ::: t) = show t

data Env = Env
    { _filters   :: HashMap Text Fun
    , _templates :: HashMap Text Exp
    , _variables :: Object
    }

type Context = StateT Env Result

render :: HashMap Text Fun
       -> HashMap Text Exp
       -> Exp
       -> Object
       -> Result Builder
render fs ts e o = flip evalStateT (Env fs ts o) $ do
    v ::: vt <- eval e >>= build (mkMeta "render")
    Eq       <- equal (meta e) TBld vt
    return v

eval :: Exp -> Context TExp

eval (ELit _ l) = return $
    case l of
        LBool b -> b ::: TBool
        LNum  n -> n ::: TNum
        LText t -> t ::: TText

eval (EBld _ b) = return $ b ::: TBld

eval (EVar _ v) = cast <$> variable v

eval (EFun _ i) = (::: TFun) <$> function i

eval (EApp m (EFun _ (Id _ "show")) e) = do
    e' ::: et <- eval e
    Shw       <- shw m et
    return (LText.pack (show e') ::: TText)

eval (EApp m (EFun _ f) e) = do
    e' ::: et    <- eval e
    Fun xt yt f' <- function f
    Eq           <- equal m et xt
    return (f' e' ::: yt)

-- eval (EApp _ e UNil) = eval e
-- eval (EApp _ UNil e) = eval e

-- eval (EApp _ v@(EVar m (Id i)) e) = eval v >>= f
--   where
--     f (o ::: TMap) = bind (const o) e
--     f (_ ::: t)    =
--         throw m "variable {} :: {} doesn't supported nested accessors."
--             [Text.unpack i, show t]

eval (EApp m a b) = do
    a' ::: TBld <- f a
    b' ::: TBld <- f b
    return ((a' <> b') ::: TBld)
  where
    f x = eval x >>= build m

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
--     Eq        <- equal m at bt
--     Ord       <- order m at
--     return $ f op a' b' ::: TBool
--   where
--     f Equal        = (==)
--     f NotEqual     = (/=)
--     f Greater      = (>)
--     f GreaterEqual = (>=)
--     f Less         = (<)
--     f LessEqual    = (<=)

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
        Eq       <- equal m xt yt
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
        JS       <- js m et
        return . const $ fromPairs ["scope" .= e]

loop :: Text -> Exp -> Maybe Exp -> Col -> Context TExp
loop _ a b (Col 0 _)  = eval (fromMaybe (EBld (meta a) mempty) b)
loop k a _ (Col l xs) = fmap ((::: TBld) . snd) $ foldlM iter (1, mempty) xs
  where
    iter (n, bld) x = do
        shadowed
        a' ::: at <- bind (Map.insert k $ context n x) (eval a)
        Eq        <- equal (meta a) at TBld
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

data Col where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Col

data Equal a b where
    Eq :: Equal a a

equal :: Meta -> Type a -> Type b -> Context (Equal a b)
equal _ TNil  TNil  = return Eq
equal _ TText TText = return Eq
equal _ TBool TBool = return Eq
equal _ TNum  TNum  = return Eq
equal _ TBld  TBld  = return Eq
equal _ TMap  TMap  = return Eq
equal _ TList TList = return Eq
equal m a b = throw m "type equality check of {} ~ {} failed." [show a, show b]

data Order a where
    Ord :: Ord a => Order a

order :: Meta -> Type a -> Context (Order a)
order _ TNil  = return Ord
order _ TText = return Ord
order _ TBool = return Ord
order _ TNum  = return Ord
order m t = throw m "constraint check of Ord a => a ~ {} failed." [show t]

data Shw a where
    Shw :: Show a => Shw a

shw :: Meta -> Type a -> Context (Shw a)
shw _ TNil  = return Shw
shw _ TText = return Shw
shw _ TBool = return Shw
shw _ TNum  = return Shw
shw _ TBld  = return Shw
shw _ TMap  = return Shw
shw _ TList = return Shw
shw m t = throw m "constraint check of Show a => a ~ {} failed." [show t]

data JS a where
    JS :: ToJSON a => JS a

js :: Meta -> Type a -> Context (JS a)
js _ TNil  = return JS
js _ TText = return JS
js _ TBool = return JS
js _ TNum  = return JS
js _ TMap  = return JS
js _ TList = return JS
js m t = throw m "constraint check of ToJSON a => a ~ {} failed." [show t]

bind :: (Object -> Object) -> Context a -> Context a
bind f = withStateT (\x -> x { _variables = f $ _variables x })

-- | A variable can be tested for truthiness, but a non-whnf expr cannot.
predicate :: Exp -> Context Bool
predicate e = do
    r <- evalStateT (eval e) <$> get
    case e of
        EVar{} -> f r
        _      -> g r
  where
    f :: Result TExp -> Context Bool
    f (Success (_ ::: TNil))  = lift (Success False)
    f (Success (p ::: TBool)) = lift (Success p)
    f (Success _)             = lift (Success True)
    f _                       = lift (Success False)

    g :: Result TExp -> Context Bool
    g (Error m es)         = lift (Error m es)
    g (Success (p ::: pt)) = do
        Eq <- equal (meta e) pt TBool
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

function :: Id -> Context Fun
function (Id m k) = do
    mf <- Map.lookup k <$> gets _filters
    maybe (throw m "filter {} doesn't exist." [k]) return mf

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
throw m f = lift . throwError m f

cast :: Value -> TExp
cast v = case v of
    Null     -> () ::: TNil
    Bool   b -> b  ::: TBool
    Number n -> n  ::: TNum
    Object o -> o  ::: TMap
    Array  a -> a  ::: TList
    String t -> LText.fromStrict t ::: TText
