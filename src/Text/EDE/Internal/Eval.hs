{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE.Internal.Eval
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Eval where

import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson                        hiding (Result (..))
import           Data.Foldable                     (foldlM)
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as Map
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Scientific                   (isFloating)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import qualified Data.Text                         as Text
import qualified Data.Text.Buildable               as Build
import           Data.Text.Lazy.Builder            (Builder)
import           Data.Text.Lazy.Builder.Scientific
import           Data.Text.Manipulate              (toOrdinal)
import           Text.EDE.Internal.Filters         (stdlib)
import           Text.EDE.Internal.Quoting
import           Text.EDE.Internal.Types
import           Text.Trifecta.Delta
import           Data.Text.Prettyprint.Doc         ((<+>))
import qualified Data.Text.Prettyprint.Doc         as PP

data Env = Env
    { _templates :: HashMap Id (Exp Delta)
    , _quoted    :: HashMap Id Term
    , _values    :: HashMap Id Value
    }

type Context = ReaderT Env Result

render :: HashMap Id (Exp Delta)
       -> HashMap Id Term
       -> Exp Delta
       -> HashMap Id Value
       -> Result Builder
render ts fs e o = runReaderT (eval e >>= nf) (Env ts (stdlib <> fs) o)
  where
    nf (TVal v) = build (delta e) v
    nf _        = lift $ Failure
        "unable to evaluate partially applied template to normal form."

eval :: Exp Delta -> Context Term
eval (_ :< ELit l) = return (qprim l)
eval (d :< EVar v) = quote (Text.pack (show v)) 0 <$> variable d v
eval (d :< EFun i) = do
    q <- Map.lookup i <$> asks _quoted
    maybe (throwError d $ "filter" <+> bold (pp i) <+> "doesn't exist.")
          return
          q

eval (_ :< EApp (_ :< EFun "defined") e) = predicate e

eval (d :< EApp a b) = do
    x <- eval a
    y <- eval b
    binding d x y

eval (_ :< ELet k rhs bdy) = do
    q <- eval rhs
    v <- lift (unquote k 0 q)
    bind (Map.insert k v) (eval bdy)

-- FIXME: We have to recompute c everytime due to the predicate ..
eval (d :< ECase p ws) = go ws
  where
    go []          = return (qprim (String mempty))
    go ((a, e):as) =
        case a of
            PWild  -> eval e
            PVar v -> eval (d :< EVar v) >>= cond e as
            PLit l -> eval (d :< ELit l) >>= cond e as

    cond e as y@(TVal Bool{}) = do
        x <- predicate p
        if x `eq` y
            then eval e
            else go as

    cond e as y@TVal{} = do
        x <- eval p
        if x `eq` y
            then eval e
            else go as

    cond _ as _  = go as

    eq (TVal a) (TVal b) = a == b
    eq _        _        = False

eval (_ :< ELoop i v bdy) = eval v >>= lift . unquote i 0 >>= loop
  where
    d = delta bdy

    loop :: Collection -> Context Term
    loop (Col l xs) = snd <$> foldlM iter (1, qprim (String mempty)) xs
      where
        iter (n, p) x = do
            shadowed n
            q <- bind (Map.insert i (context n x)) (eval bdy)
            r <- binding d p q
            return (n + 1, r)

        shadowed n = do
            m <- asks _values
            maybe (return ())
                  (shadowedErr n)
                  (Map.lookup i m)

        shadowedErr n x = throwError d $
                "variable"
            <+> bold (pp i)
            <+> "shadows"
            <+> pp x
            <+> "in"
            <+> pp (toOrdinal n)
            <+> "loop iteration."

        context n (k, x) = object $
            [ "value"      .= x
            , "length"     .= l
            , "index"      .= n
            , "index0"     .= (n - 1)
            , "remainder"  .= (l - n)
            , "remainder0" .= (l - n - 1)
            , "first"      .= (n == 1)
            , "last"       .= (n == l)
            , "odd"        .= (n `mod` 2 == 1)
            , "even"       .= (n `mod` 2 == 0)
            ] ++ key k

        key (Just k) = ["key" .= k]
        key Nothing  = []

eval (d :< EIncl i) = do
    ts <- asks _templates
    case Map.lookup i ts of
        Just e  -> eval e
        Nothing -> throwError d $
                "template"
            <+> bold (pp i)
            <+> "is not in scope:"
            <+> PP.brackets (pp (Text.intercalate "," $ Map.keys ts))

bind :: (Object -> Object) -> Context a -> Context a
bind f = withReaderT (\x -> x { _values = f (_values x) })

variable :: Delta -> Var -> Context Value
variable d (Var is) = asks _values >>= go (NonEmpty.toList is) [] . Object
  where
    go []     _ v = return v
    go (k:ks) r v = do
        m <- nest v
        maybe (throwError d $ "variable" <+> apretty cur <+> "doesn't exist.")
              (go ks (k:r))
              (Map.lookup k m)
      where
        cur = Var (k:|r)

        nest :: Value -> Context Object
        nest (Object o) = return o
        nest x          = throwError d $ "variable"
            <+> apretty cur
            <+> "::"
            <+> pp x
            <+> "doesn't supported nested accessors."

-- | A variable can be tested for truthiness, but a non-whnf expr cannot.
predicate :: Exp Delta -> Context Term
predicate x = do
    r <- runReaderT (eval x) <$> ask
    lift $ case r of
        Success q
            | TVal Bool{} <- q -> Success q
        Success q
            | TVal Null   <- q -> Success (qprim False)
        Success _              -> Success (qprim True)
        Failure _
            | _ :< EVar{} <- x -> Success (qprim False)
        Failure e              -> Failure e

binding :: Delta -> Term -> Term -> Context Term
binding d x y =
    case (x, y) of
        (TVal l, TVal r) -> quote "<>" 0 <$> liftM2 (<>) (build d l) (build d r)
        _                -> lift (qapply d x y)

build :: Delta -> Value -> Context Builder
build _ Null         = return mempty
build _ (String t)   = return (Build.build t)
build _ (Bool True)  = return "true"
build _ (Bool False) = return "false"
build _ (Number n)
    | isFloating n = return (formatScientificBuilder Fixed Nothing  n)
    | otherwise    = return (formatScientificBuilder Fixed (Just 0) n)
build d x =
    throwError d ("unable to render literal" <+> pp x)

-- FIXME: Add delta information to the thrown error document.
throwError :: Delta -> AnsiDoc -> Context a
throwError d doc = lift . Failure $ prettyDelta d <+> red "error:" <+> doc
