module Text.EDE.Internal.Lexer.Tokens where

import Data.Text (Text)

data Token
    = Atom   !Int !Int !Atom !Text
    | Lexeme !Int !Int !Lexeme
      deriving (Eq, Show)

data Atom
    = AWhiteSpace
    | ANum
    | AIdent
    | AText
    | AOp
    | AFrag
      deriving (Eq, Show)

data Lexeme
    = LExpL
    | LExpR
    | LParenL
    | LParenR
    | LComma

    | LTrue
    | LFalse
    | LElse
    | LIf
    | LElseIf
    | LEndIf
    | LCase
    | LWhen
    | LEndCase
    | LFor
    | LIn
    | LEndFor
    | LInclude
    | LWith
    | LAssign
    | LCapture
    | LEndCapture
    | LRaw
    | LEndRaw

    | LNewLine
    | LEOF
      deriving (Eq, Show)

eof :: Token -> Bool
eof (Lexeme _ _ LEOF) = True
eof _                 = False
