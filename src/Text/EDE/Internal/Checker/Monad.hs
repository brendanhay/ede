{-# LANGUAGE BangPatterns #-}

module Text.EDE.Internal.Checker.Monad where

import Control.Monad
import Debug.Trace

import Text.EDE.Internal.Pretty
import Text.EDE.Internal.Types

data State = State
    { varNames  :: [Var]
    , tvarNames :: [TVar]
    , indent    :: !Int
    , tracing   :: !Bool
    }

newtype Check a = Check { unCheck :: State -> Either String (State, a) }

instance Functor Check where
    fmap = liftM

instance Monad Check where
    fail   !e = Check $ const (Left e)
    return !x = Check $ \s -> Right (s, x)

    (>>=) !m !k = Check $ \s ->
        case unCheck m s of
            Left  e       -> Left e
            Right (s', x) -> unCheck (k x) s'

evalCheck :: Bool -> Check a -> Either String a
evalCheck t c = fmap snd . unCheck c $ State
    { varNames  = map (Var . ('$':)) namelist
    , tvarNames = map TypeVar namelist
    , indent    = 0
    , tracing   = t
    }
  where
    namelist = [1..] >>= (`replicateM` ['a'..'z'])

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
        then do
            res <- x
            return res
        else do
            ilevel <- gets indent
            let ind = replicate (ilevel * 3) ' '
            trace (ind ++ f ++ pp args) $ do
                modify $ \s -> s {indent = ilevel + 1}
                res <- x
                modify $ \s -> s {indent = ilevel}
                trace (ind ++ "=" ++ pp res) $ return res

gets :: (State -> a) -> Check a
gets f = Check $ \s -> Right (s, f s)

modify :: (State -> State) -> Check ()
modify f = Check $ \s -> Right (f s, ())
