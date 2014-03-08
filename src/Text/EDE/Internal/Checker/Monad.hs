{-# LANGUAGE BangPatterns #-}

-- Module      : Text.EDE.Internal.Checker.Monad
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Monad where

import           Control.Monad
import qualified Data.Text                as Text
import           Debug.Trace
import           Text.EDE.Internal.Pretty
import           Text.EDE.Internal.Types

data CheckState = CheckState
    { varNames  :: [Var]
    , tvarNames :: [TVar]
    , indent    :: !Int
    , tracing   :: !Bool
    }

newtype Check a = Check { unCheck :: CheckState -> Either String (CheckState, a) }

instance Functor Check where
    fmap = liftM

instance Monad Check where
    return !x = Check $ \s -> Right (s, x)

    (>>=) !m !k = Check $ \s ->
        case unCheck m s of
            Left  e       -> Left e
            Right (s', x) -> unCheck (k x) s'

evalCheck :: Bool -> Check a -> Either String a
evalCheck t c = fmap snd . unCheck c $ CheckState
    { varNames  = map (VBound  . Text.pack . ('$':)) namelist
    , tvarNames = map (TypeVar . Text.pack) namelist
    , indent    = 0
    , tracing   = t
    }
  where
    namelist = [1..] >>= (`replicateM` ['a'..'z'])

throw :: String -> Check a
throw !e = Check $ const (Left e)

-- | Create a fresh variable
freshVar :: Check Var
freshVar = do
    v:vs <- gets varNames
    modify $ \s -> s {varNames = vs}
    return v

-- | Create a fresh type variable
freshTVar :: Check TVar
freshTVar = do
    v:vs <- gets tvarNames
    modify $ \s -> s {tvarNames = vs}
    return v

-- | Print some debugging info
traceNS :: (Pretty a, Pretty b) => String -> a -> Check b -> Check b
traceNS f args x = do
    p <- gets tracing
    if not p
        then x
        else do
            ilevel <- gets indent
            let ind = replicate (ilevel * 3) ' '
            trace (ind ++ f ++ pp args) $ do
                modify $ \s -> s {indent = ilevel + 1}
                res <- x
                modify $ \s -> s {indent = ilevel}
                trace (ind ++ "=" ++ pp res) $ return res

gets :: (CheckState -> a) -> Check a
gets f = Check $ \s -> Right (s, f s)

modify :: (CheckState -> CheckState) -> Check ()
modify f = Check $ \s -> Right (f s, ())