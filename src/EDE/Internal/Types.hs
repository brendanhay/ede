{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module EDE.Internal.Types where

import           Control.Monad.Trans.Error
import           Data.Monoid
import           Data.Text                 (Text)
import           Data.Text.Buildable
import           Data.Text.Format          (format)
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Builder

-- FIXME:
-- type expression metadata extraction function
-- unsound monoid instance for untyped expressions
--   should probably propagate left expression's metadata

type LText = LText.Text
type Frag  = Builder

data Meta
    = Meta !String !Int !Int
    | Unknown
      deriving (Eq)

instance Show Meta where
    show (Meta s r c) = concat [s, ":(", show r, ",", show c, ")"]
    show Unknown      = "unknown"

data TypeError = TypeError Meta LText
    deriving (Eq)

instance Show TypeError where
    show (TypeError m msg) = formatError "TypeError" m msg

data EvalError = EvalError Meta LText
    deriving (Eq)

instance Show EvalError where
    show (EvalError m msg) = formatError "EvalError" m msg

formatError :: LText -> Meta -> LText -> String
formatError e m msg = LText.unpack $
    format "{}\nPosition: {}\nMessage: {}" [e, LText.pack $ show m, msg]

instance Error EvalError where
    strMsg = EvalError Unknown . LText.pack

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
