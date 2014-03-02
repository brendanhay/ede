{
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE RecordWildCards          #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -w                    #-}

-- Module      : Text.EDE.Internal.Lexer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Lexer (tokenise) where

import           Control.Monad
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Unsafe               as Text
import           Text.EDE.Internal.Lexer.Tokens
}

$whitespace  = [\ \t\b\xa0]
$newline     = [\n]

$digit       = 0-9
$octit       = 0-7
$hexit       = [$digit A-F a-f]

$letter      = [a-zA-Z_]
$ident       = [$digit $letter ']

$fragment    = [^\{]

$dquoted     = \0-\255 # [\"\n]
@descape     = \\ ([ntvbrfaeE\\\?\"] | $octit{1,3} | x$hexit+ | X$hexit+)
@string      = $dquoted | @descape

@sign        = [\-\+]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@number      = @decimal
             | @decimal \. @decimal @exponent?
             | @decimal @exponent
             | 0[oO] @octal
             | 0[xX] @hexadecimal

@expr_start  = "{{" | "{%"
@expr_end    = "}}" | "%}"

@comm_start  = "{-"
@comm_end    = "-}"

@unary       = "-"  | "+"  | "!"
@logical     = "&&" | "||"
@equality    = "==" | "!="
@relational  =  ">" | ">=" | "<=" | "<"

tokens :-

$newline               { lexeme LNewLine }

<0> @expr_start        { lexeme LExpL `andBegin` (fromIntegral expr) }
<0> @comm_start        { begin comm }
<0> $whitespace+       { atom AWhiteSpace }
<0> $fragment+         { atom AFrag }

<expr> @expr_end       { lexeme LExpR `andBegin` (fromIntegral 0) }
<expr> $white+         { skip }
<expr> @sign? @number  { atom ANum }
<expr> $letter $ident+ { atom AIdent }
<expr> \" @string* \"  { atom AText }

<expr> @unary          { atom AOp }
<expr> @logical        { atom AOp }
<expr> @equality       { atom AOp }
<expr> @relational     { atom AOp }

<expr> "true"          { lexeme LTrue }
<expr> "false"         { lexeme LFalse }
<expr> "else"          { lexeme LElse }
<expr> "if"            { lexeme LIf }
<expr> "elif"          { lexeme LElseIf }
<expr> "elsif"         { lexeme LElseIf }
<expr> "endif"         { lexeme LEndIf }
<expr> "case"          { lexeme LCase }
<expr> "when"          { lexeme LWhen }
<expr> "endcase"       { lexeme LEndCase }
<expr> "for"           { lexeme LFor }
<expr> "in"            { lexeme LIn }
<expr> "endfor"        { lexeme LEndFor }
<expr> "include"       { lexeme LInclude }
<expr> "with"          { lexeme LWith }
<expr> "assign"        { lexeme LAssign }
<expr> "capture"       { lexeme LCapture }
<expr> "endcapture"    { lexeme LEndCapture }
<expr> "raw"           { lexeme LRaw }
<expr> "endraw"        { lexeme LEndRaw }

<expr> \(              { lexeme LParenL  }
<expr> \)              { lexeme LParenR }
<expr> \,              { lexeme LComma }
<expr> \,              { lexeme LComma }

<comm> @comm_end       { begin 0 }
<comm> .               { skip }

{
atom :: Atom -> AlexInput -> Int -> Alex Token
atom a (AlexInput _ _ s) n = return $ Atom 0 0 a (Text.take n s)

lexeme :: Lexeme -> AlexInput -> Int -> Alex Token
lexeme l (AlexInput _ _ _) _ = return $ Lexeme 0 0 l

data AlexPosn = AlexPn !Int !Int !Int
    deriving (Eq,Show)

start :: AlexPosn
start = AlexPn 0 1 1

move :: AlexPosn -> Char -> AlexPosn
move (AlexPn a l c) '\t' = AlexPn (a + 1)  l      (((c + 7) `div` 8) * 8 + 1)
move (AlexPn a l c) '\n' = AlexPn (a + 1) (l + 1) 1
move (AlexPn a l c) _    = AlexPn (a + 1)  l      (c + 1)

data State = State
    { stPos   :: AlexPosn
    , stText  :: Text
    , stLast  :: Char
    , stCode  :: Int
    }

data AlexInput = AlexInput
    { inpPos  :: AlexPosn
    , inpLast :: {-# UNPACK #-} !Char
    , inpText :: {-# UNPACK #-} !Text
    } deriving (Show)

newtype Alex a = Alex { unAlex :: State -> Either String (State, a) }

instance Monad Alex where
    fail   !e = Alex $ const (Left e)
    return !x = Alex $ \s -> Right (s, x)

    (>>=) !m !k = Alex $ \s ->
        case unAlex m s of
            Left  e       -> Left e
            Right (s', x) -> unAlex (k x) s'

runAlex :: Text -> Alex a -> Either String a
runAlex inp (Alex f) = snd `fmap` f (State start inp '\n' 0)

getInput :: Alex AlexInput
getInput = Alex $ \s@State{..} ->
    Right (s, AlexInput stPos stLast stText)

setInput :: AlexInput -> Alex ()
setInput (AlexInput pos c inp) = Alex $ \s ->
    Right (s { stPos = pos, stLast = c, stText = inp }, ())

getCode :: Alex Int
getCode = Alex $ \s -> Right (s, stCode s)

setCode :: Int -> Alex ()
setCode c = Alex $ \s -> Right (s { stCode = c }, ())

scan :: Alex Token
scan = do
    inp@(AlexInput _ _ str) <- getInput
    c <- getCode
    case alexScan inp c of
        AlexEOF ->
            return (Lexeme 0 0 LEOF)
        AlexError (AlexInput (AlexPn _ line column) _ _) ->
            fail $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
        AlexSkip inp' len -> do
            setInput inp'
            scan
        AlexToken inp'@(AlexInput _ _ str') len action -> do
            setInput inp'
            action inp len
          where
            len = Text.length str - Text.length str'

-- | Ignore this token and scan another.
skip :: AlexInput -> Int -> Alex Token
skip _ _ = scan

-- | Ignore this token, but set the start code to a new value.
begin :: Int -> AlexInput -> Int -> Alex Token
begin c _ _ = setCode c >> scan

-- | Perform an action for this token, and set the start code to a new value.
andBegin :: (AlexInput -> Int -> Alex a) -> Int -> AlexInput -> Int -> Alex a
(a `andBegin` c) inp len = setCode c >> a inp len

alexGetByte :: Num a => AlexInput -> Maybe (a, AlexInput)
alexGetByte (AlexInput p _ t)
    | Text.null t = Nothing
    | otherwise   = Just $! (c2w c, AlexInput (move p c) c cs)
  where
    (c, cs) = (Text.unsafeHead t, Text.unsafeTail t)

    -- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
    -- silently truncates to 8 bits Chars > '\255'. It is provided as
    -- convenience for ByteString construction.
    c2w = fromIntegral . ord

tokenise :: Text -> Either String [Token]
tokenise txt = runAlex txt loop
  where
    loop = do
        t <- scan
        if eof t
            then return []
            else do
                ts <- loop
                return $ t : ts
}
