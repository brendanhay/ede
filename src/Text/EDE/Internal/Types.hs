{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Text.EDE.Internal.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
import           Data.Aeson.Types             hiding (Result (..))
import           Data.HashMap.Strict          (HashMap)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Monoid                  (mempty)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta.Delta

-- | Convenience wrapper for Pretty instances.
newtype PP a = PP { unPP :: a }

pp :: Pretty (PP a) => a -> Doc
pp = pretty . PP

instance Pretty (PP Text) where
    pretty = PP.string . Text.unpack . unPP

instance Pretty (PP Value) where
    pretty (PP v) =
        case v of
            Null     -> "Null"
            Bool   _ -> "Bool"
            Number _ -> "Scientific"
            Object _ -> "Object"
            Array  _ -> "Array"
            String _ -> "String"

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
    Failure e <*> Failure e' = Failure (PP.vsep [e, e'])
    {-# INLINE (<*>) #-}

instance Alternative Result where
    Success x <|> Success _  = Success x
    Success x <|> Failure _  = Success x
    Failure _ <|> Success x  = Success x
    Failure e <|> Failure e' = Failure (PP.vsep [e, e'])
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

type Delim = (String, String)

data Syntax = Syntax
    { _delimPragma  :: !Delim
    , _delimInline  :: !Delim
    , _delimComment :: !Delim
    , _delimBlock   :: !Delim
    }

makeClassy ''Syntax

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Syntax -> Id -> Delta -> m (Result Template)

-- | A parsed and compiled template.
data Template = Template
    { _tmplName :: !Text
    , _tmplExp  :: !(Exp Delta)
    , _tmplIncl :: HashMap Id (Exp Delta)
    } deriving (Eq)

type Id = Text

newtype Var = Var (NonEmpty Id)
    deriving (Eq)

instance Pretty Var where
    pretty (Var is) = PP.hcat
        . PP.punctuate "."
        . map (PP.bold . pp)
        . reverse
        $ NonEmpty.toList is

instance Show Var where
    show = show . pretty

data Collection where
    Col :: Foldable f => Int -> f (Maybe Text, Value) -> Collection

data Pat
    = PWild
    | PVar !Var
    | PLit !Value
      deriving (Eq, Show)

type Alt a = (Pat, a)

data ExpF a
    = ELit  !Value
    | EVar  !Var
    | EFun  !Id
    | EApp  !a  !a
    | ELet  !Id !a !a
    | ECase !a  [Alt a]
    | ELoop !Id !a !a
    | EIncl !Text
      deriving (Eq, Show, Functor)

type Exp = Cofree ExpF

instance HasDelta (Exp Delta) where
    delta = extract

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
