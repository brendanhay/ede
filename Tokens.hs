import Data.Text (Text)

data TokFamily
    = Symbol
    | Keyword
    | Constructor
    | Index
      deriving (Eq, Show)

data Tok
    = KJunk Text
    | KM !TokMeta
    | KA !TokAtom
    | KN !TokNamed
      deriving (Eq, Show)

data TokMeta
    = KNewLine
    | KCommentLineStart
    | KCommentBlockStart
    | KCommentBlockEnd
    | KCommentUnterminated
    | KOffsideClosingBrace
      deriving (Eq, Show)

data TokAtom
    = KOp    !Text
    | KOpVar !Text
    | KIf
    | KElIf
    | KElse
    | KCase
    | KWhen
    | KFor
    | KIn
    | KWith
    | KInclude
    | KAssign
    | KCapture
    | KCycle
    | KComma
      deriving (Eq, Show)

data TokNamed
    = KCon !Text
    | KVar !Text
    | KLit !Text
      deriving (Eq, Show)
