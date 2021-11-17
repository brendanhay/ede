{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.EDE.Internal.Eval
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
module Text.EDE.Internal.Eval where

import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Control.Monad as Monad
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (lift)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Object, Value (..))
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import qualified Data.Foldable as Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Scientific (isFloating)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Data.Text.Lazy.Builder.Scientific (FPFormat (Fixed), formatScientificBuilder)
import Data.Text.Manipulate (toOrdinal)
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
#else
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
#endif
import Text.EDE.Internal.Filters (stdlib)
import Text.EDE.Internal.Quoting
import Text.EDE.Internal.Types
import Text.Trifecta.Delta (Delta)
import qualified Text.Trifecta.Delta as Trifecta.Delta

data Env = Env
  { _templates :: HashMap Id (Exp Delta),
    _quoted :: HashMap Id Term,
#if MIN_VERSION_aeson(2,0,0)
    _values :: KeyMap Value
#else
    _values :: HashMap Id Value
#endif
  }

type Context = ReaderT Env Result

render ::
  HashMap Id (Exp Delta) ->
  HashMap Id Term ->
  Exp Delta ->
  HashMap Id Value ->
  Result Builder
render ts fs e o =
#if MIN_VERSION_aeson(2,0,0)
  Reader.runReaderT (eval e >>= nf) (Env ts (stdlib <> fs) $ toKeyMap o)
#else
  Reader.runReaderT (eval e >>= nf) (Env ts (stdlib <> fs) o)
#endif
  where
    nf (TVal v) = build (Trifecta.Delta.delta e) v
    nf _ =
      lift $
        Failure
          "unable to evaluate partially applied template to normal form."

eval :: Exp Delta -> Context Term
eval (_ :< ELit l) = pure (qprim l)
eval (d :< EVar v) = quote (Text.pack (show v)) 0 <$> variable d v
eval (d :< EFun i) = do
  q <- HashMap.lookup i <$> Reader.asks _quoted
  maybe
    (throwError d $ "filter" <+> bold (pp i) <+> "doesn't exist.")
    pure
    q
eval (_ :< EApp (_ :< EFun "defined") e) = predicate e
eval (d :< EApp a b) = do
  x <- eval a
  y <- eval b
  binding d x y
eval (_ :< ELet k rhs bdy) = do
  q <- eval rhs
  v <- lift (unquote k 0 q)
#if MIN_VERSION_aeson(2,0,0)
  bind (KeyMap.insert (toKey k) v) (eval bdy)
#else
  bind (HashMap.insert k v) (eval bdy)
#endif

-- FIXME: We have to recompute c everytime due to the predicate
eval (d :< ECase p ws) = go ws
  where
    go [] = pure (qprim (String mempty))
    go ((a, e) : as) =
      case a of
        PWild -> eval e
        PVar v -> eval (d :< EVar v) >>= cond e as
        PLit l -> eval (d :< ELit l) >>= cond e as

    cond e as y@(TVal Bool {}) = do
      x <- predicate p
      if x `eq` y
        then eval e
        else go as
    cond e as y@TVal {} = do
      x <- eval p
      if x `eq` y
        then eval e
        else go as
    cond _ as _ = go as

    eq (TVal a) (TVal b) = a == b
    eq _ _ = False
eval (_ :< ELoop i v bdy) = eval v >>= lift . unquote i 0 >>= loop
  where
    d = Trifecta.Delta.delta bdy

    loop :: Collection -> Context Term
    loop (Col l xs) = snd <$> Foldable.foldlM iter (1, qprim (String mempty)) xs
      where
        iter (n, p) x = do
          shadowed n
#if MIN_VERSION_aeson(2,0,0)
          q <- bind (KeyMap.insert (toKey i) (context n x)) (eval bdy)
#else
          q <- bind (HashMap.insert i (context n x)) (eval bdy)
#endif
          r <- binding d p q
          pure (n + 1, r)

        shadowed n = do
          m <- Reader.asks _values
          maybe
            (pure ())
            (shadowedErr n)
#if MIN_VERSION_aeson(2,0,0)
            (KeyMap.lookup (toKey i) m)
#else
            (HashMap.lookup i m)
#endif

        shadowedErr n x =
          throwError d $
            "variable"
              <+> bold (pp i)
              <+> "shadows"
              <+> pp x
              <+> "in"
              <+> pp (toOrdinal n)
              <+> "loop iteration."

        context n (k, x) =
          Aeson.object $
            [ "value" .= x,
              "length" .= l,
              "index" .= n,
              "index0" .= (n - 1),
              "remainder" .= (l - n),
              "remainder0" .= (l - n - 1),
              "first" .= (n == 1),
              "last" .= (n == l),
              "odd" .= (n `mod` 2 == 1),
              "even" .= (n `mod` 2 == 0)
            ]
              ++ key k

        key (Just k) = ["key" .= k]
        key Nothing = []
eval (d :< EIncl i) = do
  ts <- Reader.asks _templates
  case HashMap.lookup i ts of
    Just e -> eval e
    Nothing ->
      throwError d $
        "template"
          <+> bold (pp i)
          <+> "is not in scope:"
          <+> PP.brackets (pp (Text.intercalate "," $ HashMap.keys ts))
{-# INLINEABLE eval #-}

bind :: (Object -> Object) -> Context a -> Context a
bind f = Reader.withReaderT (\x -> x {_values = f (_values x)})
{-# INLINEABLE bind #-}

variable :: Delta -> Var -> Context Value
variable d (Var is) =
  Reader.asks _values >>= go (NonEmpty.toList is) [] . Object
  where
    go [] _ v = pure v
    go (k : ks) r v = do
      m <- nest v
      maybe
        (throwError d $ "variable" <+> apretty cur <+> "doesn't exist.")
        (go ks (k : r))
#if MIN_VERSION_aeson(2,0,0)
        (KeyMap.lookup (toKey k) m)
#else
        (HashMap.lookup k m)
#endif
      where
        cur = Var (k :| r)

        nest :: Value -> Context Object
        nest (Object o) = pure o
        nest x =
          throwError d $
            "variable"
              <+> apretty cur
              <+> "::"
              <+> pp x
              <+> "doesn't supported nested accessors."
{-# INLINEABLE variable #-}

-- | A variable can be tested for truthiness, but a non-whnf expr cannot.
predicate :: Exp Delta -> Context Term
predicate x =
  Reader.runReaderT (eval x) <$> Reader.ask
    >>= lift . \case
      Success q
        | TVal Bool {} <- q -> Success q
      Success q
        | TVal Null <- q -> Success (qprim False)
      Success _ -> Success (qprim True)
      Failure _
        | _ :< EVar {} <- x -> Success (qprim False)
      Failure e -> Failure e
{-# INLINEABLE predicate #-}

binding :: Delta -> Term -> Term -> Context Term
binding d x y =
  case (x, y) of
    (TVal l, TVal r) -> quote "<>" 0 <$> Monad.liftM2 (<>) (build d l) (build d r)
    _ -> lift (qapply d x y)
{-# INLINEABLE binding #-}

build :: Delta -> Value -> Context Builder
build _ Null = pure mempty
build _ (String t) = pure (Text.Builder.fromText t)
build _ (Bool True) = pure "true"
build _ (Bool False) = pure "false"
build _ (Number n)
  | isFloating n = pure (formatScientificBuilder Fixed Nothing n)
  | otherwise = pure (formatScientificBuilder Fixed (Just 0) n)
build d x =
  throwError d ("unable to render literal" <+> pp x)
{-# INLINEABLE build #-}

-- FIXME: Add delta information to the thrown error document.
throwError :: Delta -> AnsiDoc -> Context a
throwError d doc =
  lift . Failure $ Trifecta.Delta.prettyDelta d <+> red "error:" <+> doc
