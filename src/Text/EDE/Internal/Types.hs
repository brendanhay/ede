{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Text.EDE.Internal.Types where

import           Data.Monoid
import           Data.Text              (Text)
import           Data.Text.Buildable
import qualified Data.Text.Lazy         as LText
import           Data.Text.Lazy.Builder
import qualified Text.Parsec            as Parsec

-- FIXME:
-- type expression metadata extraction function
-- unsound monoid instance for untyped expressions
--   should probably propagate left expression's metadata
-- correctly handle comments

type LText = LText.Text
type Frag  = Builder

data Meta
    = Meta !String !Int !Int
    | Unknown
      deriving (Eq)

instance Show Meta where
    show (Meta s r c) = concat [s, ":(", show r, ",", show c, ")"]
    show Unknown      = "unknown"

data Result a
    = Success      a
    | ParseError   !Parsec.ParseError
    | TypeError    !Meta !String
    | CompileError !Meta !String

instance Show a => Show (Result a) where
    show = const "result"

instance Functor Result where
    fmap f (Success a) = Success $ f a

    fmap _ (ParseError     e) = ParseError     e
    fmap _ (TypeError    m e) = TypeError    m e
    fmap _ (CompileError m e) = CompileError m e

instance Monad Result where
    return          = Success
    Success a >>= k = k a

    ParseError     e >>= _ = ParseError     e
    TypeError    m e >>= _ = TypeError    m e
    CompileError m e >>= _ = CompileError m e

newtype Ident = Ident { ident :: Text }
    deriving (Show)

instance Buildable Ident where
    build = build . ident

data Bind = Bind
    { bindMeta :: !Meta
    , bindPrim :: !Ident
    , bindSec  :: Maybe Ident
    } deriving (Show)

data AExp = forall a. TExp a ::: TType a

deriving instance Show AExp

data TType a where
    TTText :: TType LText
    TTBool :: TType Bool
    TTInt  :: TType Integer
    TTDbl  :: TType Double
    TTFrag :: TType Frag

deriving instance Show (TType a)

class Type a where
    typeof :: TType a

instance Type LText   where typeof = TTText
instance Type Bool    where typeof = TTBool
instance Type Integer where typeof = TTInt
instance Type Double  where typeof = TTDbl
instance Type Frag    where typeof = TTFrag

data TExp a where
    TText :: Meta          -> LText     -> TExp LText
    TBool :: Meta          -> Bool      -> TExp Bool
    TInt  :: Meta          -> Integer   -> TExp Integer
    TDbl  :: Meta          -> Double    -> TExp Double
    TVar  :: Meta          -> Ident     -> TExp Frag
    TFrag :: Meta          -> Frag      -> TExp Frag
    TApp  :: Meta          -> TExp Frag -> TExp Frag -> TExp Frag
    TNeg  :: Meta          -> TExp Bool -> TExp Bool
    TBin  :: Meta          -> BinOp     -> TExp Bool -> TExp Bool -> TExp Bool
    TRel  :: Ord a => Meta -> RelOp     -> TExp a    -> TExp a    -> TExp Bool
    TCond :: Meta          -> TExp Bool -> TExp Frag -> TExp Frag -> TExp Frag
    TLoop :: Meta          -> Bind      -> Ident     -> TExp Frag -> TExp Frag -> TExp Frag

deriving instance Show (TExp a)

data UExp
    = UText !Meta !LText
    | UBool !Meta !Bool
    | UInt  !Meta !Integer
    | UDbl  !Meta !Double
    | UVar  !Meta !Ident
    | UFrag !Meta !Frag
    | UApp  !Meta !UExp  !UExp
    | UNeg  !Meta !UExp
    | UBin  !Meta !BinOp !UExp  !UExp
    | URel  !Meta !RelOp !UExp  !UExp
    | UCond !Meta !UExp  !UExp  !UExp
    | ULoop !Meta !Bind  !Ident !UExp !UExp
      deriving (Show)

instance Monoid UExp where
    mempty  = UFrag Unknown mempty
    mappend = UApp  Unknown

data BinOp
    = And
    | Or
      deriving (Show)

data RelOp
    = Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
      deriving (Show)
