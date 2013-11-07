module Tmpl.Internal.Types where

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

data Expr
    = ELit  !Literal
    | EVar  !Ident
    | ENeg  !Expr
    | EBin  !BinOp (Expr Bool)  (Expr Bool)
    | ERel  !RelOp !Expr  !Expr
    | ECond !Expr  [Expr] [Expr]
    | ELoop !Bind  !Ident [Expr] [Expr]
      deriving (Show)

data Literal
    = LChar !Char
    | LText !Text
    | LBool !Bool
    | LInt  !Integer
    | LDoub !Double
    | LNil
      deriving (Show)

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

instance Buildable Expr where
    build = build . show

instance Buildable Literal where
    build (LChar c) = build c
    build (LText t) = build t
    build (LBool b) = build b
    build (LInt  i) = build i
    build (LDoub d) = build d
    build LNil      = mempty
