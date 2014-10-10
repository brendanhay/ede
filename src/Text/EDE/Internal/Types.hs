{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
import           Control.Monad
import           Data.Aeson              hiding (Result, Success, Error)
import           Data.Aeson.Types        (Pair)
import           Data.HashMap.Strict     (HashMap)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Monoid             hiding ((<>))
import           Data.Scientific
import           Data.Semigroup
import           Data.Text               (Text)
import           Data.Text.Buildable
import           Data.Text.Format        (Format, format)
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as LText
import           Data.Text.Lazy.Builder
import           Text.Parsec             (ParseError, SourcePos)
import qualified Text.Parsec             as Parsec

-- | A function to resolve the target of an @include@ expression.
type Resolver m = Text -> Meta -> m (Result Template)

instance Monad m => Semigroup (Resolver m) where
    (<>) f g = \x y -> liftM2 mplus (f x y) (g x y)
    {-# INLINE (<>) #-}

-- | A parsed and compiled template.
data Template = Template
    { tmplName :: !Text
    , tmplExpr :: Exp
    , tmplIncl :: HashMap Text Exp
    } deriving (Eq)

-- | Meta information describing the source position of an expression or error.
data Meta = Meta !String !Int !Int
    deriving (Eq, Show)

instance Buildable Meta where
    build (Meta n l c) =
        mconcat ["`", build n, "` (line ", build l, ", column ", build c, ")"]
    {-# INLINE build #-}

class Metadata a where
    meta :: a -> Meta

instance Metadata Meta where
    meta = id

instance Metadata ParseError where
    meta = meta . Parsec.errorPos

instance Metadata SourcePos where
    meta p = Meta
        (Parsec.sourceName p)
        (Parsec.sourceLine p)
        (Parsec.sourceColumn p)

data Error
    = Lexer     !Meta [String]
    | Parser    !Meta [String]
    | Evaluator !Meta [String]
    | Resolver  !Meta [String]
    | Quoter    [String]
      deriving (Eq)

instance Show Error where
    show = \case
        Lexer     m e -> pos "lexing"     m e
        Parser    m e -> pos "parsing"    m e
        Evaluator m e -> pos "evaluation" m e
        Resolver  m e -> pos "io"  m e
        Quoter      e -> "ED-E quotation error:\n" ++ msg e
      where
        pos k m e = LText.unpack $
            format "ED-E {} error in {}:\n{}"
                [k, build m, build (msg e)]

        msg = init . unlines . map (mappend " - ")

-- | The result of running parsing or rendering steps.
data Result a
    = Success !a
    | Error   !Error
      deriving (Eq, Show)

instance Functor Result where
    fmap f (Success x) = Success (f x)
    fmap _ (Error   e) = Error e
    {-# INLINE fmap #-}

instance Monad Result where
    return          = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error   e >>= _ = Error e
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Alternative Result where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) a@(Success _) _ = a
    (<|>) _ b             = b
    {-# INLINE (<|>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Monoid a => Monoid (Result a) where
    mempty  = Success mempty
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Convert a 'Result' to an 'Either' with the 'Left' case holding a formatted
-- error message, and 'Right' being the successful result over which 'Result' is paramterised.
eitherResult :: Result a -> Either String a
eitherResult = result (Left . show) Right

-- | Perform a case analysis on a 'Result'.
result :: (Error -> b) -- ^ Function to apply to the 'Error' case.
       -> (a -> b)     -- ^ Function to apply to the 'Success' case.
       -> Result a     -- ^ The 'Result' to map over.
       -> b
result _ g (Success x) = g x
result f _ (Error   e) = f e

-- | Convenience for returning a successful 'Result'.
success :: Monad m => a -> m (Result a)
success = return . Success

-- | Convenience for returning an error 'Result'.
failure :: Monad m => Error -> m (Result a)
failure = return . Error

data Quoted
    = QLit !Value
    | QLam (Quoted -> Result Quoted)

instance Show Quoted where
    show (QLit v) = show v
    show _        = "<function>"

instance Eq Quoted where
    QLit a == QLit b = a == b
    _      == _      = False

data Type a where
    TNil  :: Type ()
    TText :: Type LText.Text
    TBool :: Type Bool
    TNum  :: Type Scientific
    TBld  :: Type Builder
    TMap  :: Type Object
    TList :: Type Array
    TFun  :: Type Quoted

deriving instance Show (Type a)

typeof :: Value -> String
typeof = \case
    Null     -> show TNil
    Bool   _ -> show TBool
    Number _ -> show TNum
    Object _ -> show TMap
    Array  _ -> show TList
    String _ -> show TText

data TExp = forall a. Eq a => a ::: Type a

instance Show TExp where
    show (_ ::: t) = show t

data Id = Id
    { idMeta :: !Meta
    , idName :: !Text
    } deriving (Eq, Show)

instance Metadata Id where
    meta = idMeta

instance Buildable Id where
    build i = mconcat
        ["`", build (idName i), "` (line ", build l, ", column ", build c, ")"]
      where
        Meta _ l c = idMeta i
    {-# INLINE build #-}

newtype Var = Var { unVar :: NonEmpty Id }
    deriving (Eq, Show, Semigroup)

instance Metadata Var where
    meta = meta . NonEmpty.head . unVar

data Lit
    = LBool !Bool
    | LNum  !Scientific
    | LText LText.Text
      deriving (Eq, Show)

data Pat
    = PWild
    | PVar Var
    | PLit Lit
      deriving (Eq, Show)

type Alt = (Pat, Exp)

data Exp
    = ELit  !Meta !Lit
    | EBld  !Meta !Builder
    | EVar  !Meta !Var
    | EFun  !Meta !Id
    | EApp  !Meta !Exp  !Exp
    | ELet  !Meta !Id   !Exp  !Exp
    | ECase !Meta !Exp  [Alt]
    | ELoop !Meta !Id   !Var  !Exp (Maybe Exp)
    | EIncl !Meta !Text (Maybe Exp)
      deriving (Eq, Show)

instance Metadata Exp where
    meta = \case
        ELit  m _       -> m
        EBld  m _       -> m
        EVar  m _       -> m
        EFun  m _       -> m
        EApp  m _ _     -> m
        ELet  m _ _ _   -> m
        ECase m _ _     -> m
        ELoop m _ _ _ _ -> m
        EIncl m _ _     -> m

throwError :: Params ps => ([String] -> Error) -> Format -> ps -> Result a
throwError f fmt = Error . f . (:[]) . LText.unpack . format fmt

-- | Create an 'Object' from a list of name/value 'Pair's.
-- See 'Aeson''s documentation for more details.
fromPairs :: [Pair] -> Object
fromPairs = (\(Object o) -> o) . object
