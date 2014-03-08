{
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
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

module Text.EDE.Internal.Lexer
     ( runLexer
     , module Text.EDE.Internal.Lexer.Tokens
     ) where

import           Control.Monad
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Unsafe               as Text
import           Text.EDE.Internal.Lexer.Tokens
import           Text.EDE.Internal.Types
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

@identl      = "{{"
@identr      = "}}"

@sectionl    = "{%"
@sectionr    = "%}"
@sectionws   = $white+

@commentl    = "{-"
@commentr    = "-}"

@unary       = "-"  | "+"  | "!"
@logical     = "&&" | "||"
@equality    = "==" | "!="
@relational  =  ">" | ">=" | "<=" | "<"

tokens :-

$newline               { atom KNewLine }

<frag> @identl         { atom KIdentL `andBegin` expr }
<frag> @sectionl       { atom KSectionL `andBegin` expr }
<frag> @commentl       { begin comm }

<frag> $whitespace+    { capture KWhiteSpace }
<frag> $fragment+      { capture KFrag }

<expr> @identr         { atom KIdentR `andBegin` frag }
<expr> @sectionr       { atom KSectionR `andBegin` frag }
<expr> @sectionws      { skip }

<expr> @unary          { capture KOp }
<expr> @logical        { capture KOp }
<expr> @equality       { capture KOp }
<expr> @relational     { capture KOp }

<expr> "true"          { atom KTrue }
<expr> "false"         { atom KFalse }
<expr> "else"          { atom KElse }
<expr> "if"            { atom KIf }
<expr> "elif"          { atom KElseIf }
<expr> "elsif"         { atom KElseIf }
<expr> "endif"         { atom KEndIf }
<expr> "case"          { atom KCase }
<expr> "when"          { atom KWhen }
<expr> "endcase"       { atom KEndCase }
<expr> "for"           { atom KFor }
<expr> "in"            { atom KIn }
<expr> "endfor"        { atom KEndFor }
<expr> "include"       { atom KInclude }
<expr> "with"          { atom KWith }
<expr> "assign"        { atom KAssign }
<expr> "capture"       { atom KCapture }
<expr> "endcapture"    { atom KEndCapture }
<expr> "raw"           { atom KRaw }
<expr> "endraw"        { atom KEndRaw }

<expr> @sign? @number  { capture KNum }
<expr> $letter $ident* { capture KIdent }
<expr> \" @string* \"  { capture KText }

<expr> \(              { atom KParenL }
<expr> \)              { atom KParenR }
<expr> \[              { atom KBracketL }
<expr> \]              { atom KBracketR }
<expr> \.              { atom KDot }
<expr> \,              { atom KComma }
<expr> \_              { atom KUnderscore }

<comm> @commentr       { begin frag }
<comm> .               { skip }

{
capture :: Capture -> AlexInput -> Int -> Alex Token
capture k inp len = return $ TC (inpMeta inp) k (Text.take len $ inpText inp)

atom :: Atom -> AlexInput -> Int -> Alex Token
atom k inp _ = return $ TA (inpMeta inp) k

start :: String -> Meta
start src = Meta src 1 1

move :: Meta -> Char -> Meta
move (Meta src l c) '\t' = Meta src  l      (((c + 7) `div` 8) * 8 + 1)
move (Meta src l c) '\n' = Meta src (l + 1) 1
move (Meta src l c) _    = Meta src  l      (c + 1)

data AlexInput = AlexInput
    { inpMeta :: Meta
    , inpLast :: {-# UNPACK #-} !Char
    , inpText :: {-# UNPACK #-} !Text
    } deriving (Show)

data AlexState = AlexState
    { stateInput :: AlexInput
    , stateCode  :: Int
    }

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
    return !x = Alex $ \s -> Right (s, x)

    (>>=) !m !k = Alex $ \s ->
        case unAlex m s of
            Left  e       -> Left e
            Right (s', x) -> unAlex (k x) s'

runAlex :: String -> Text -> Alex a -> Either String a
runAlex src txt (Alex f) = snd `fmap` f state
  where
    state = AlexState
        { stateInput = input
        , stateCode  = frag
        }

    input = AlexInput
        { inpMeta = start src
        , inpLast = '\n'
        , inpText = txt
        }

throw :: String -> Alex a
throw !e = Alex $ const (Left e)

getInput :: Alex AlexInput
getInput = Alex $ \s -> Right (s, stateInput s)

setInput :: AlexInput -> Alex ()
setInput inp = Alex $ \s -> Right (s { stateInput = inp }, ())

getCode :: Alex Int
getCode = Alex $ \s -> Right (s, stateCode s)

setCode :: Int -> Alex ()
setCode c = Alex $ \s -> Right (s { stateCode = c }, ())

scan :: Alex Token
scan = do
    inp <- getInput
    c   <- getCode
    case alexScan inp c of
        AlexEOF ->
            return $ TA (inpMeta inp) KEOF
        AlexError inp' ->
            throw $ "lexical error at: " ++ show (inpMeta inp')
        AlexSkip inp' len -> do
            setInput inp'
            scan
        AlexToken inp' len action -> do
            setInput inp'
            action inp len
          -- where
          --   len = Text.length (inpText inp) - Text.length (inpText inp')

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

runLexer :: String -> Text -> Either String [Token]
runLexer src txt = runAlex src txt loop
  where
    loop = do
        t <- scan
        if tokenEOF t
            then return []
            else do
                ts <- loop
                return $ t : ts
}
