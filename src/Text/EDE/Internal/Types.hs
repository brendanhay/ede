{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Control.Lens
import           Data.Aeson                   hiding (Result(..))
import           Data.Aeson.Types             (Pair)
import           Data.Foldable
import           Data.HashMap.Strict          (HashMap)
import           Data.List.NonEmpty           (NonEmpty(..))
import           Data.Monoid                  hiding ((<>))
import           Data.Maybe
import           Data.Scientific
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
    { _delimRender  :: Delim
    , _delimComment :: Delim
    , _delimBlock   :: Delim
    }

makeLenses ''Syntax

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Syntax -> Text -> Delta -> m (Result Template)

instance Applicative m => Semigroup (Resolver m) where
    (f <> g) o k d = liftA2 (<|>) (f o k d) (g o k d) -- Haha!
    {-# INLINE (<>) #-}

-- | A parsed and compiled template.
data Template = Template
    { _tmplName :: !Text
    , _tmplExp  :: !Exp
    , _tmplIncl :: HashMap Text Exp
    } deriving (Eq)

type Id = Text

newtype Var = Var (NonEmpty Id)
    deriving (Eq, Show)

-- FIXME: implement constructors, remove hardcoded bool keywords, etc.
data Lit
    = LBool !Bool
    | LNum  !Scientific
    | LText !Text
      deriving (Eq, Show)

data Pat
    = PWild
    | PVar !Var
    | PLit !Lit
      deriving (Eq, Show)

type Alt = (Pat, Exp)

data Exp
    = ELit  !Delta !Lit
    | EVar  !Delta !Var
    | EFun  !Delta !Id
    | EApp  !Delta !Exp  !Exp
    | ELet  !Delta !Id   !Exp  !Exp
    | ECase !Delta !Exp  [Alt]
    | ELoop !Delta !Id   !Var  !Exp (Maybe Exp)
    | EIncl !Delta !Text (Maybe Exp)
      deriving (Eq, Show)

instance HasDelta Exp where
    delta = \case
        ELit  d _       -> d
        EVar  d _       -> d
        EFun  d _       -> d
        EApp  d _ _     -> d
        ELet  d _ _ _   -> d
        ECase d _ _     -> d
        ELoop d _ _ _ _ -> d
        EIncl d _ _     -> d

var :: Id -> Var
var = Var . (:| [])

eapp :: Delta -> [Exp] -> Exp
eapp d []     = ELit d (LText mempty)
eapp _ [e]    = e
eapp d (e:es) = foldl' (EApp d) e es

efun :: Delta -> Id -> Exp -> Exp
efun d = EApp d . EFun d

ecase :: Exp -> [Alt] -> Maybe Exp -> Exp
ecase p ws f = ECase (delta p) p (ws ++ maybe [] ((:[]) . wild) f)

eif :: (Exp, Exp) -> [(Exp, Exp)] -> Maybe Exp -> Exp
eif t@(x, _) ts f = foldr' c (fromMaybe (bld (delta x)) f) (t:ts)
  where
    c (p, w) e = ECase (delta p) p [true w, false e]

wild, true, false :: Exp -> Alt
wild  = (PWild,)
true  = (PLit (LBool True),)
false = (PLit (LBool False),)

bld :: Delta -> Exp
bld = (`ELit` LText mempty)

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
