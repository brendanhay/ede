{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module EDE.Internal.Types where

import           Data.Aeson
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Text.Buildable
import qualified Data.Text.Lazy         as LText
import           Data.Text.Lazy.Builder

type LText = LText.Text

newtype Ident = Ident { unident :: Text }
    deriving (Show)

type Frag = Builder

data Meta = Meta !Int !Int !String
    deriving (Show)

data Bind = Bind !Ident (Maybe Ident)
    deriving (Show)

data AExp = forall a. TExp a ::: TType a

deriving instance Show AExp

data TType a where
    TTText :: TType Text
    TTBool :: TType Bool
    TTInt  :: TType Integer
    TTDbl  :: TType Double
    TTFrag :: TType Frag

deriving instance Show (TType a)

class Type a where
    typeof :: TType a

instance Type Text    where typeof = TTText
instance Type Bool    where typeof = TTBool
instance Type Integer where typeof = TTInt
instance Type Double  where typeof = TTDbl
instance Type Frag    where typeof = TTFrag

data TExp a where
    TText :: Meta -> Text           -> TExp Text
    TBool :: Meta -> Bool           -> TExp Bool
    TInt  :: Meta -> Integer        -> TExp Integer
    TDbl  :: Meta -> Double         -> TExp Double
    TVar  :: Meta -> Ident          -> TExp Frag
    TFrag :: Meta -> Frag           -> TExp Frag
    TApp  :: Meta -> TExp Frag      -> TExp Frag -> TExp Frag
    TNeg  :: Meta -> TExp Bool      -> TExp Bool
    TBin  :: Meta -> BinOp          -> TExp Bool -> TExp Bool -> TExp Bool
    TRel  :: Meta -> Ord a => RelOp -> TExp a    -> TExp a    -> TExp Bool
    TCond :: Meta -> TExp Bool      -> TExp Frag -> TExp Frag -> TExp Frag
    TLoop :: Meta -> Bind           -> Ident     -> TExp Frag -> TExp Frag -> TExp Frag

deriving instance Show (TExp a)

data UExp
    = UText !Meta !Text
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
    mempty      = UFrag mempty
    mappend a b = UApp a b

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

instance Buildable Ident where
    build (Ident k) = build k

-- instance Buildable Exp where
--     build = build . show

-- instance Buildable Literal where
--     build (LChar c) = build c
--     build (LText t) = build t
--     build (LBool b) = build b
--     build (LInt  i) = build i
--     build (LDoub d) = build d
--     build LNil      = mempty
