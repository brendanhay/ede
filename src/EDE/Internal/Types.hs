{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module EDE.Internal.Types where

import           Control.Monad.Trans.Error
import           Data.Monoid
import           Data.Text                 (Text)
import           Data.Text.Buildable
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Builder

type LText = LText.Text
type Frag  = Builder

data Meta = Meta
    { metaSrc :: !String
    , metaRow :: !Int
    , metaCol :: !Int
    } deriving (Eq, Show)

data TypeError = TypeError !Meta !String
    deriving (Eq, Show)

data EvalError = EvalError !Meta !String
    deriving (Eq, Show)

instance Error EvalError where
    strMsg s = EvalError (Meta s 0 0) s

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
    TText :: Meta          -> Text      -> TExp Text
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

-- FIXME: Hrmm.
instance Monoid UExp where
    mempty      = UFrag (Meta "mempty"  0 0) mempty
    mappend a b = UApp  (Meta "mappend" 0 0) a b

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
