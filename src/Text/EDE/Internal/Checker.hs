{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

-- Module      : Text.EDE.Internal.Checker
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Bidirectional typechecking for higher-rank polymorphism.
--   See: http://www.mpi-sws.org/~neelk/bidir.pdf
module Text.EDE.Internal.Checker where

import           Control.Applicative
import           Control.Monad
import qualified Data.HashSet                      as Set
import           Data.Maybe
import           Text.EDE.Internal.AST
import           Text.EDE.Internal.Checker.Context
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Checker.Subst
import           Text.EDE.Internal.Pretty

-- | Type checking:
--   check Γ e A = Δ <=> Γ |- e <= A -| Δ
check :: Context -> Exp a -> Polytype -> Check Context
check gamma expr typ =
  traceNS "check" (gamma, expr, typ) $
  checkwftype gamma typ $ case (expr, typ) of
    -- ForallI
    (e, TForall alpha a) -> do
      -- Do alpha conversion to avoid clashes
      alpha' <- freshTVar
      dropMarker (CForall alpha') <$>
        check (gamma >: CForall alpha') e (subst (TVar alpha') alpha a)
    -- ->I
    (EAbs a' x e, TFun a b) -> do
      x' <- freshVar
      dropMarker (CVar x' a) <$>
        check (gamma >: CVar x' a) (subst (EVar a' x') x e) b
    -- Sub
    (e, b) -> do
      (a, theta) <- synth gamma e
      subtype theta (apply theta a) (apply theta b)

-- | Type synthesising:
--   synth Γ e = (A, Δ) <=> Γ |- e => A -| Δ
synth :: Context -> Exp a -> Check (Polytype, Context)
synth gamma expr = traceNS "synth" (gamma, expr) $ checkwf gamma $
  case expr of
    ELit _ l ->
        return . (, gamma) $ case l of
            LNum  _ -> TCon TNum
            LText _ -> TCon TText
            LBool _ -> TCon TBool
    -- Var
    EVar _ x -> return
      ( fromMaybe (error $ "synth: not in scope " ++ pp (expr, gamma))
                  (findVarType gamma x)
      , gamma
      )
    -- ->I=> Full Damas-Milner type inference
    EAbs a' x e -> do
      x'    <- freshVar
      alpha <- freshTVar
      beta  <- freshTVar
      (delta, delta')  <- breakMarker (CMarker alpha) <$>
        check (gamma >++ [ CMarker alpha
                         , CExists alpha
                         , CExists beta
                         , CVar x' (TExists alpha)
                         ])
                  (subst (EVar a' x') x e)
                  (TExists beta)
      let tau = apply delta' (TFun (TExists alpha) (TExists beta))
      let evars = unsolved delta'
      uvars <- replicateM (length evars) freshTVar
      return ( tforalls uvars $ substs (zip (map TVar uvars) evars) tau
             , delta)
    -- ->E
    EApp _ e1 e2 -> do
      (a, theta) <- synth gamma e1
      applySynth theta (apply theta a) e2

-- | Type application synthesising
--   applySynth Γ A e = (C, Δ) <=> Γ |- A . e =>> C -| Δ
applySynth :: Context -> Polytype -> Exp a -> Check (Polytype, Context)
applySynth gamma typ e = traceNS "applySynth" (gamma, typ, e) $
  checkwftype gamma typ $
  case typ of
    -- ForallApp
    TForall alpha a -> do
      -- Do alpha conversion to avoid clashes
      alpha' <- freshTVar
      applySynth (gamma >: CExists alpha')
                     (subst (TExists alpha') alpha a)
                     e
    -- alpha^App
    TExists alpha -> do
      alpha1 <- freshTVar
      alpha2 <- freshTVar
      delta <- check (insertAt gamma (CExists alpha) $ context
                            [ CExists alpha2
                            , CExists alpha1
                            , CExistsSolved alpha $ TFun (TExists alpha1)
                                                         (TExists alpha2)
                            ])
                         e
                         (TExists alpha1)
      return (TExists alpha2, delta)
    -- ->App
    TFun a c -> do
      delta <- check gamma e a
      return (c, delta)

    _ -> error $ "applySynth: don't know what to do with: "
              ++ pp (gamma, typ, e)

-- | Algorithmic subtyping:
--   subtype Γ A B = Δ <=> Γ |- A <: B -| Δ
subtype :: Context -> Polytype -> Polytype -> Check Context
subtype gamma typ1 typ2 =
  traceNS "subtype" (gamma, typ1, typ2) $
  checkwftype gamma typ1 $ checkwftype gamma typ2 $
    case (typ1, typ2) of
    (TCon alpha, TCon alpha') | alpha == alpha' -> return gamma
    -- <:Var
    (TVar alpha, TVar alpha') | alpha == alpha' -> return gamma
    -- <:Exvar
    (TExists alpha, TExists alpha')
      | alpha == alpha' && alpha `elem` existentials gamma -> return gamma
    -- <:->
    (TFun a1 a2, TFun b1 b2) -> do
      theta <- subtype gamma b1 a1
      subtype theta (apply theta a2) (apply theta b2)
    -- <:forallL
    (TForall alpha a, b) -> do
      -- Do alpha conversion to avoid clashes
      alpha' <- freshTVar
      dropMarker (CMarker alpha') <$>
        subtype (gamma >++ [CMarker alpha', CExists alpha'])
                (subst (TExists alpha') alpha a)
                b
    -- <:forallR
    (a, TForall alpha b) -> do
      -- Do alpha conversion to avoid clashes
      alpha' <- freshTVar
      dropMarker (CForall alpha') <$>
        subtype (gamma >: CForall alpha') a (subst (TVar alpha') alpha b)
    -- <:InstantiateL
    (TExists alpha, a) | alpha `elem` existentials gamma
                      && not (alpha `Set.member` freeTVars a) ->
      instantiateL gamma alpha a
    -- <:InstantiateR
    (a, TExists alpha) | alpha `elem` existentials gamma
                      && not (alpha `Set.member` freeTVars a) ->
      instantiateR gamma a alpha
    _ -> error $ "subtype, don't know what to do with:\n"
                           ++ pp (gamma, typ1, typ2) ++ "\n" ++ show (gamma, typ1, typ2)

-- | Algorithmic instantiation (left):
--   instantiateL Γ α A = Δ <=> Γ |- α^ :=< A -| Δ
instantiateL :: Context -> TVar -> Polytype -> Check Context
instantiateL gamma alpha a =
  traceNS "instantiateL" (gamma, alpha, a) $
  checkwftype gamma a $ checkwftype gamma (TExists alpha) $
  case solve gamma alpha =<< monotype a of
    -- InstLSolve
    Just gamma' -> return gamma'
    Nothing -> case a of
      -- InstLReach
      TExists beta | ordered gamma alpha beta ->
        return $ fromJust $ solve gamma beta (TExists alpha)
      -- InstLArr
      TFun a1 a2   -> do
        alpha1 <- freshTVar
        alpha2 <- freshTVar
        theta <- instantiateR (insertAt gamma (CExists alpha) $ context
                                [ CExists alpha2
                                , CExists alpha1
                                , CExistsSolved alpha $ TFun (TExists alpha1)
                                                             (TExists alpha2)
                                ])
                              a1 alpha1
        instantiateL theta alpha2 (apply theta a2)
      -- InstLAIIR
      TForall beta b -> do
        -- Do alpha conversion to avoid clashes
        beta' <- freshTVar
        dropMarker (CForall beta') <$>
          instantiateL (gamma >++ [CForall beta'])
                       alpha
                       (subst (TVar beta') beta b)
      _ -> error $ "The impossible happened! instantiateL: "
                ++ pp (gamma, alpha, a)

-- | Algorithmic instantiation (right):
--   instantiateR Γ A α = Δ <=> Γ |- A =:< α -| Δ
instantiateR :: Context -> Polytype -> TVar -> Check Context
instantiateR gamma a alpha =
  traceNS "instantiateR" (gamma, a, alpha) $
  checkwftype gamma a $ checkwftype gamma (TExists alpha) $
  case solve gamma alpha =<< monotype a of
    Just gamma' -> return gamma'
    Nothing -> case a of
      -- InstRReach
      TExists beta | ordered gamma alpha beta -> return $
        fromJust $ solve gamma beta (TExists alpha)
      -- InstRArr
      TFun a1 a2   -> do
        alpha1 <- freshTVar
        alpha2 <- freshTVar
        theta <- instantiateL (insertAt gamma (CExists alpha) $ context
                                 [ CExists alpha2
                                 , CExists alpha1
                                 , CExistsSolved alpha $ TFun (TExists alpha1)
                                                              (TExists alpha2)
                                 ])
                              alpha1
                              a1
        instantiateR theta (apply theta a2) alpha2
      -- InstRAIIL
      TForall beta b -> do
        -- Do alpha conversion to avoid clashes
        beta' <- freshTVar
        dropMarker (CMarker beta') <$>
          instantiateR (gamma >++ [CMarker beta', CExists beta'])
                       (subst (TExists beta') beta b)
                       alpha
      _ -> error $ "The impossible happened! instantiateR: "
                ++ pp (gamma, a, alpha)

-- -- Examples
-- eid :: Exp a -- (λx. x) : ∀ t. t → t
-- eid = eabs "x" (var "x") -: tforall "t" (tvar "t" --> tvar "t")
-- -- Impredicative, so doesn't check
-- ididunit :: Exp a -- (λid. id id ()) ((λx. x) : ∀ t. t → t)
-- ididunit = eabs "id" (((var "id" -: tforall "t" (tvar "t" --> tvar "t"))  $$ var "id") $$ eunit) $$ eid
-- idunit :: Exp a -- (λid. id ()) ((λx. x) : ∀ t. t → t)
-- idunit = eabs "id" (var "id" $$ eunit) $$ eid
-- idid :: Exp a -- id id
-- idid = (eid $$ eid) -: tforall "t" (tvar "t" --> tvar "t")

-- idtrue :: Exp a
-- idtrue = eabs "x" (eabs "y" (eapp [var "+", var "x", var "y"]))

-- mappnd :: Exp a
-- mappnd = eapp [eabs "x" . eabs "y" $ var "z"]

initial :: Context
initial = Context
    [ "mappend" ==> tforall "a" (tvar "a" --> tvar "a" --> tvar "a")
    , "+"       ==> TCon TNum --> TCon TNum --> TCon TNum
    ]

-- | Strictness annotations ensure the context
--   is evaluated, and thereby well-formed.
infer :: Bool -> Exp a -> Either String (Polytype, Context)
infer t x =
    case evalCheck t (synth initial x) of
        Left  e            -> Left e
        Right (!a, !gamma) -> Right (apply gamma a, gamma)

-- realpha :: Polytype -> Polytype
-- realpha t = evalState (f t) (mempty, ['a'..'z'])
--   where
--     f (TCon c) = return $ TCon c

--     f (TVar (TypeVar [v])) = do
--         c  <- next v
--         return $ TVar (TypeVar [c])
--     f (TExists (TypeVar [v])) = do
--         c  <- next v
--         return $ TExists (TypeVar [c])
--     f (TForall (TypeVar [v]) x) = do
--         c  <- next v
--         x' <- f x
--         return $ TForall (TypeVar [c]) x'
--     f (TFun x y) = do
--         x' <- f x
--         y' <- f y
--         return $ TFun x' y'

--     next x = do
--         (m, c:cs) <- get
--         maybe (put (Map.insert x c m, cs) >> return c)
--               return
--               (Map.lookup x m)
