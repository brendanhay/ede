{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tmpl.Internal.Types where

import           Data.Aeson
import           Data.Monoid
import           Data.Text           (Text)
import           Data.Text.Buildable
import qualified Data.Text.Lazy      as LText

type LText = LText.Text

newtype Ident = Ident { unident :: Text }
    deriving (Show)

data Meta = Meta !Int !Int !String
    deriving (Show)

data Bind = Bind !Ident (Maybe Ident)
    deriving (Show)

data AExpr = forall a. TExpr a ::: TType a

deriving instance Show AExpr

data TType a where
    TTText :: TType Text
    TTBool :: TType Bool
    TTInt  :: TType Integer
    TTDbl  :: TType Double
    TTVar  :: TType Value
    TTList :: TType [a]

deriving instance Show (TType a)

data TExpr a where
    TText :: Text           -> TExpr Text
    TBool :: Bool           -> TExpr Bool
    TInt  :: Integer        -> TExpr Integer
    TDbl  :: Double         -> TExpr Double
    TVar  :: Ident          -> TExpr Value
    TNeg  :: TExpr Bool     -> TExpr Bool
    TBin  :: BinOp          -> TExpr Bool    -> TExpr Bool -> TExpr Bool
    TRel  :: Ord a => RelOp -> TExpr a       -> TExpr a    -> TExpr Bool
    TCond :: TExpr Bool     -> TExpr a       -> TExpr a    -> TExpr a
    TLoop :: Bind           -> Ident         -> TExpr a    -> TExpr a -> TExpr [a]
    TCons :: TExpr a        -> TExpr a       -> TExpr a

deriving instance Show (TExpr a)

data UExpr
    = UText !Text
    | UBool !Bool
    | UInt  !Integer
    | UDbl  !Double
    | UVar  !Ident
    | UNeg  !UExpr
    | UBin  !BinOp !UExpr  !UExpr
    | URel  !RelOp !UExpr  !UExpr
    | UCond !UExpr !UExpr  !UExpr
    | ULoop !Bind  !Ident  !UExpr !UExpr
    | UCons !UExpr !UExpr
      deriving (Show)

instance Monoid UExpr where
    mempty      = UText mempty
    mappend a b = UCons a b

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

-- instance Buildable Expr where
--     build = build . show

-- instance Buildable Literal where
--     build (LChar c) = build c
--     build (LText t) = build t
--     build (LBool b) = build b
--     build (LInt  i) = build i
--     build (LDoub d) = build d
--     build LNil      = mempty
