{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Types where

import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Aeson.Types             hiding (Result(..))
import           Data.Foldable
import           Data.HashMap.Strict          (HashMap)
import           Data.List.NonEmpty           (NonEmpty(..))
import           Data.Maybe
import           Data.Monoid                  hiding ((<>))
import           Data.Semigroup
import           Data.Text                    (Text)
import           Data.Text.Format             (Format, format)
import           Data.Text.Format.Params      (Params)
import qualified Data.Text.Lazy               as LText
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), Doc, vsep)
import           Text.Trifecta.Delta

-- | The result of running parsing or rendering steps.
data Result a
    = Success a
    | Failure Doc
      deriving (Show, Functor, Foldable, Traversable)

makePrisms ''Result

instance Monad Result where
    return          = Success
    {-# INLINE return #-}
    Success x >>= k = k x
    Failure e >>= _ = Failure e
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure = return
    {-# INLINE pure #-}
    Success f <*> Success x  = Success (f x)
    Success _ <*> Failure e  = Failure e
    Failure e <*> Success _  = Failure e
    Failure e <*> Failure e' = Failure (vsep [e, e'])
    {-# INLINE (<*>) #-}

instance Alternative Result where
    Success x <|> Success _  = Success x
    Success x <|> Failure _  = Success x
    Failure _ <|> Success x  = Success x
    Failure e <|> Failure e' = Failure (vsep [e, e'])
    {-# INLINE (<|>) #-}
    empty = Failure mempty
    {-# INLINE empty #-}

instance Show a => Pretty (Result a) where
    pretty (Success x) = pretty (show x)
    pretty (Failure e) = pretty e

-- | Convert a 'Result' to an 'Either' with the 'Left' case holding a
-- formatted error message, and 'Right' being the successful result over
-- which 'Result' is paramterised.
eitherResult :: Result a -> Either String a
eitherResult = result (Left . show) Right

-- | Perform a case analysis on a 'Result'.
result :: (Doc -> b) -- ^ Function to apply to the 'Failure' case.
       -> (a -> b)   -- ^ Function to apply to the 'Success' case.
       -> Result a   -- ^ The 'Result' to map over.
       -> b
result _ g (Success x) = g x
result f _ (Failure e) = f e

-- | Convenience for returning a successful 'Result'.
success :: Monad m => a -> m (Result a)
success = return . Success

-- | Convenience for returning an error 'Result'.
failure :: Monad m => Doc -> m (Result a)
failure = return . Failure

throwError :: Params ps => Format -> ps -> Result a
throwError fmt = Failure . pretty . LText.unpack . format fmt

type Delim = (String, String)

data Syntax = Syntax
    { _delimPragma  :: !Delim
    , _delimInline  :: !Delim
    , _delimComment :: !Delim
    , _delimBlock   :: !Delim
    }

makeClassy ''Syntax

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Syntax -> Text -> Delta -> m (Result Template)

instance Applicative m => Semigroup (Resolver m) where
    (f <> g) o k d = liftA2 (<|>) (f o k d) (g o k d) -- Haha!
    {-# INLINE (<>) #-}

-- | A parsed and compiled template.
data Template = Template
    { _tmplName :: !Text
    , _tmplExp  :: !(Cofree Exp Delta)
    , _tmplIncl :: HashMap Text (Cofree Exp Delta)
    } deriving (Eq)

type Id = Text

newtype Var = Var (NonEmpty Id)
    deriving (Eq, Show)

data Collection where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Collection

data Pat
    = PWild
    | PVar !Var
    | PLit !Value
      deriving (Eq, Show)

type Alt a = (Pat, a)

data Exp a
    = ELit  !Value
    | EVar  !Var
    | EFun  !Id
    | EApp  !a  !a
    | ELet  !Id !a !a
    | ECase !a  [Alt a]
    | ELoop !Id !a !a
    | EIncl !Text
      deriving (Eq, Show, Functor)

type AExp = Cofree Exp

instance HasDelta (AExp Delta) where
    delta = extract

newtype Mu f = Mu (f (Mu f))

cofree :: Functor f => a -> Mu f -> Cofree f a
cofree x = go
  where
    go (Mu f) = x :< fmap go f

forget :: Functor f => Cofree f a -> Mu f
forget = Mu . fmap forget . unwrap

var :: Id -> Var
var = Var . (:| [])

eapp :: a -> [AExp a] -> AExp a
eapp x []     = cofree x blank
eapp _ [e]    = e
eapp _ (e:es) = foldl' (\x y -> extract x :< EApp x y) e es

efun :: Id -> AExp a -> AExp a
efun i e = let x = extract e in x :< EApp (x :< EFun i) e

efilter :: AExp a -> (Id, [AExp a]) -> AExp a
efilter e (i, ps) = let x = extract e in eapp x ((x :< EFun i) : e : ps)

elet :: Maybe (Id, AExp a) -> AExp a -> AExp a
elet m e = maybe e (\(i, b) -> extract b :< ELet i b e) m

ecase :: AExp a
      -> [Alt (AExp a)]
      -> Maybe (AExp a)
      -> AExp a
ecase p ws f = extract p :< ECase p (ws ++ maybe [] ((:[]) . wild) f)

eif :: (AExp a, AExp a)
    -> [(AExp a, AExp a)]
    -> Maybe (AExp a)
    -> AExp a
eif t ts f = foldr' c (fromMaybe (extract (fst t) `cofree` blank) f) (t:ts)
  where
    c (p, w) e = extract p :< ECase p [true w, false e]

eempty :: AExp a -> AExp a -> Maybe (AExp a) -> AExp a
eempty v e = maybe e (eif (efun "!" (efun "empty" v), e) [] . Just)

true, false, wild :: AExp a -> Alt (AExp a)
true  = (PLit (Bool True),)
false = (PLit (Bool False),)
wild  = (PWild,)

blank :: Mu Exp
blank = Mu (ELit (String mempty))

-- | Unwrap a 'Value' to an 'Object' safely.
--
-- See 'Aeson''s documentation for more details.
fromValue :: Value -> Maybe Object
fromValue (Object o) = Just o
fromValue _          = Nothing

-- | Create an 'Object' from a list of name/value 'Pair's.
--
-- See 'Aeson''s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs = (\(Object o) -> o) . object
