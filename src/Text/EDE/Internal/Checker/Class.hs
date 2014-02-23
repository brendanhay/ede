-- Module      : Text.EDE.Internal.Checker.Class
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Class where

data ClassEnv = ClassEnv
    { classes  :: Id -> Maybe Class
    , defaults :: [Type]
    }

super :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (is, its) -> is

instances :: ClassEnv -> Id -> [Inst]
instances ce i = case classes ce i of Just (is, its) -> its

defined :: Maybe a -> Bool
defined (Just x) = True
defined Nothing = False

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce{classes = \j -> if i==j then Just c
                                           else classes ce j}

initialEnv :: ClassEnv
initialEnv = ClassEnv { classes = \i -> fail "class not defined",
                         defaults = [tInteger, tDouble] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'

addClass :: Id -> [Id] -> EnvTransformer
addClass i is ce
 | defined (classes ce i) = fail "class already defined"
 | any (not . defined . classes ce) is = fail "superclass not defined"
 | otherwise = return (modify ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses ::   EnvTransformer
addCoreClasses =   addClass "Eq" []
                <:> addClass "Ord" ["Eq"]
                <:> addClass "Show" []
                <:> addClass "Read" []
                <:> addClass "Bounded" []
                <:> addClass "Enum" []
                <:> addClass "Functor" []
                <:> addClass "Monad" []

addNumClasses ::   EnvTransformer
addNumClasses =   addClass "Num" ["Eq", "Show"]
                <:> addClass "Real" ["Num", "Ord"]
                <:> addClass "Fractional" ["Num"]
                <:> addClass "Integral" ["Real", "Enum"]
                <:> addClass "RealFrac" ["Real", "Fractional"]
                <:> addClass "Floating" ["Fractional"]
                <:> addClass "RealFloat" ["RealFrac", "Floating"]

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
 | not (defined (classes ce i)) = fail "no class for instance"
 | any (overlap p) qs = fail "overlapping instance"
 | otherwise = return (modify ce i c)
   where its = insts ce i
         qs = [ q | (_ :=> q) <- its ]
         c = (super ce i, (ps:=>p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined (mguPred p q)

exampleInsts ::  EnvTransformer
exampleInsts =   addPreludeClasses
             <:> addInst [] (IsIn "Ord" tUnit)
             <:> addInst [] (IsIn "Ord" tChar)
             <:> addInst [] (IsIn "Ord" tInt)
             <:> addInst [IsIn "Ord" (TVar (TVar "a" Star)),
                          IsIn "Ord" (TVar (TVar "b" Star))]
                         (IsIn "Ord" (pair (TVar (TVar "a" Star))
                                           (TVar (TVar "b" Star))))


-- Entailment

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
 = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [ tryInst it | it <- insts ce i ]
  where
    tryInst (ps :=> h) = do
        u <- matchPred h p
        Just (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                   Nothing -> False
                   Just qs -> all (entail ce ps) qs

-- Context Reduction

inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
  where
    hnf (TVar v)   = True
    hnf (TCon tc)  = False
    hnf (TApp t _) = hnf t

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do
    pss <- mapM (toHnf ce) ps
    return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
    loop rs [] = rs
    loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                   | otherwise = loop (p:rs) ps

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do
    qs <- toHnfs ce ps
    return (simplify ce qs)

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
