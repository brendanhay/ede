-- Module      : Text.EDE.Internal.Checker.Ambiguity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Ambiguity where

import Text.EDE.Internal.Checker.Class
import Text.EDE.Internal.Types

-- type Ambiguity = (TVar, [Pred])

-- ambiguities :: ClassEnv -> [TVar] -> [Pred] -> [Ambiguity]
-- ambiguities ce vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]

-- numClasses :: [Id]
-- numClasses = ["Num", "Integral", "Floating", "Fractional",
--                "Real", "RealFloat", "RealFrac"]

-- stdClasses :: [Id]
-- stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
--                "Functor", "Monad", "MonadPlus"] ++ numClasses

-- candidates :: ClassEnv -> Ambiguity -> [Type]
-- candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
--                                    ts = [ t | IsIn i t <- qs ],
--                                all ((TVar v)==) ts,
--                                any (`elem` numClasses) is,
--                                all (`elem` stdClasses) is,
--                                t' <- defaults ce,
--                                all (entail ce []) [ IsIn i t' | i <- is ] ]

-- withDefaults :: Monad m
--              => ([Ambiguity] -> [Type] -> a)
--              -> ClassEnv
--              -> [TVar]
--              -> [Pred]
--              -> m a
-- withDefaults f ce vs ps
--     | any null tss = fail "cannot resolve ambiguity"
--     | otherwise = return (f vps (map head tss))
--       where vps = ambiguities ce vs ps
--             tss = map (candidates ce) vps

-- defaultedPreds :: Monad m => ClassEnv -> [TVar] -> [Pred] -> m [Pred]
-- defaultedPreds = withDefaults (\vps ts -> concat (map snd vps))

-- defaultSubst :: Monad m => ClassEnv -> [TVar] -> [Pred] -> m Subst
-- defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)
