{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
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
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson                        hiding (Result(..))
import           Data.Bifunctor                    (first)
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
import           Data.Text.Lazy.Builder            (Builder)
import           Data.Text.Lazy.Builder.Scientific
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Text.EDE.Internal.Quotes
import           Text.EDE.Internal.Types
import           Text.PrettyPrint.ANSI.Leijen      (Pretty(..))
import           Text.Trifecta.Delta

-- FIXME: look at adding a whnf step to reduce everything before storing as a template
-- (optimisation passes, case reducation etc.)
-- maybe Exp -> Quoted (aka Val) is the first step, without substituting from
-- env/filters

data Env = Env
    { _templates :: HashMap Text Exp
    , _quoted    :: HashMap Text Quoted
    , _values    :: HashMap Text Value
    }

type Context = ReaderT Env Result

render :: HashMap Text Exp
       -> HashMap Text Quoted
       -> Exp
       -> HashMap Text Value
       -> Result Builder
render ts fs e o = runReaderT (eval e >>= nf) (Env ts fs o)
  where
    nf (QLit v) = build d v
    nf _        = lift (Failure err)

    err = "unable to evaluate partially applied template to normal form."

    d = delta e

eval :: Exp -> Context Quoted
eval (ELit _ l) = return (quote l)
eval (EVar _ v) = quote <$> variable v
eval (EFun d i) = do
    q <- Map.lookup i <$> asks _quoted
    maybe (throwError' d "binding {} doesn't exist." [i])
          return
          q

eval (EApp d a b) = do
    x <- eval a
    y <- eval b
    qappend d x y

eval (ELet _ k rhs bdy) = do
    q <- eval rhs
    v <- lift (unquote q)
    bind (Map.insert k v) (eval bdy)

-- FIXME: We have to recompute c everytime due to the predicate ..
eval (ECase d p ws) = go ws
  where
    go []          = return (quote (LText mempty))
    go ((a, e):as) =
        case a of
            PWild  -> eval e
            PVar v -> eval (EVar d v) >>= cond e as
            PLit l -> eval (ELit d l) >>= cond e as

    cond e as y@(QLit Bool{}) = do
        x <- predicate p
        if x == y then eval e else go as
    cond e as y@QLit{} = do
        x <- eval p
        if x == y then eval e else go as
    cond _ as _  = go as

eval (ELoop d k v bdy ma) = variable v >>= go >>= loop k bdy ma
  where
    go (Object o) = return (Col (Map.size o) (hmap o))
    go (Array  a) = return (Col (Vector.length a) (vec a))
    go x          =
        throwError' d "invalid loop target {}, expected {} or {}"
            [typeof x, typeof (Object mempty), typeof (Array mempty)]

    hmap :: HashMap Text Value -> [(Maybe Text, Value)]
    hmap = map (first Just) . sortBy (comparing fst) . Map.toList

    vec :: Vector Value -> Vector (Maybe Text, Value)
    vec = Vector.map (Nothing,)

eval (EIncl d k mu) = do
    ts <- asks _templates
    t  <- maybe
        (throwError' d "template {} is not in scope: {}"
            [k, Text.intercalate "," $ Map.keys ts])
        return
        (Map.lookup k ts)
    s  <- maybe (return global) local' mu
    bind s (eval t)
  where
    -- Use the global environment.
    global o = fromPairs ["scope" .= o]

    -- Ignore the global environment (const)
    -- and set to the specified expression.
    local' u = do
        x <- case u of
            ELit _ l -> return (enc l)
            EVar _ v -> variable v
            _        -> throwError' d "unexpected template scope {}" [show (delta u)]
        return . const $ fromPairs ["scope" .= x]

    enc (LBool b) = toJSON b
    enc (LNum  n) = toJSON n
    enc (LText t) = toJSON t

bind :: (Object -> Object) -> Context a -> Context a
bind f = withReaderT (\x -> x { _values = f (_values x) })

variable :: Var -> Context Value
variable (Var is) = asks _values >>= go (NonEmpty.toList is) [] . Object
  where
    go []     _ v = return v
    go (k:ks) r v = do
        m <- nest v
        maybe (throwError' undefined "binding {} doesn't exist." [fmt (k:r)])
              (go ks (k:r))
              (Map.lookup k m)
      where
        nest :: Value -> Context Object
        nest (Object o) = return o
        nest x          =
            throwError' undefined "variable {} :: {} doesn't supported nested accessors."
                [fmt (k:r), typeof x]

        fmt = Text.unpack . Text.intercalate "."

-- | A variable can be tested for truthiness, but a non-whnf expr cannot.
predicate :: Exp -> Context Quoted
predicate x = do
    r <- runReaderT (eval x) <$> ask
    lift $ case r of
        Success q
            | QLit(Bool{}) <- q -> Success q
        Success _               -> Success (quote True)
        Failure _
            | EVar{} <- x       -> Success (quote False)
        Failure e               -> Failure e

data Collection where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Collection

loop :: Text -> Exp -> Maybe Exp -> Collection -> Context Quoted
loop _ a b (Col 0 _)  = eval (fromMaybe (ELit (delta a) (LText mempty)) b)
loop k a _ (Col l xs) = snd <$> foldlM iter (1, quote (String mempty)) xs
  where
    iter (n, p) x = do
        shadowed n
        q <- bind (Map.insert k $ context n x) (eval a)
        r <- qappend (delta a) q p
        return (n + 1, r)

    shadowed n = do
        m <- asks _values
        maybe (return ())
              (\x -> throwError' (delta a) "binding {} shadows existing variable {} :: {}, {}"
                  [Text.unpack k, show x, typeof x, show n])
              (Map.lookup k m)

    context n (mk, v) = object
        [ "key"        .= mk -- FIXME: ensure null keys don't exist
        , "value"      .= v
        , "length"     .= l
        , "index"      .= n
        , "index0"     .= (n - 1)
        , "remainder"  .= (l - n)
        , "remainder0" .= (l - n - 1)
        , "first"      .= (n == 1)
        , "last"       .= (n == l)
        , "odd"        .= (n `mod` 2 == 1)
        , "even"       .= (n `mod` 2 == 0)
        ]

qappend :: Delta -> Quoted -> Quoted -> Context Quoted
qappend d x y =
    case (x, y) of
        (QLit l, QLit r) -> quote <$> liftM2 (<>) (build d l) (build d r)
        _                -> lift (qapp x y)

build :: Delta -> Value -> Context Builder
build _ Null         = return mempty
build _ (String t)   = return (Build.build t)
build _ (Bool True)  = return "true"
build _ (Bool False) = return "false"
build _ (Number n)
    | base10Exponent n == 0 = return (formatScientificBuilder Fixed (Just 0) n)
    | otherwise             = return (scientificBuilder n)
build d x =
    throwError' d "unable to render literal {}\n{}" [typeof x, show x]

throwError' :: Params ps => Delta -> Format -> ps -> Context a
throwError' _ f = lift . throwError f
