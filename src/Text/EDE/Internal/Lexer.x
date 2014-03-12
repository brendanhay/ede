{
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE RecordWildCards          #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

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

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Unsafe               as Text
import           Prelude                        hiding (exp)
import           Text.EDE.Internal.Lexer.Tokens
import           Text.EDE.Internal.Types
}

$whitespace  = [\ \t\b\xa0]
$newline     = [\n]

$sign        = [\-\+]
$digit       = 0-9
$octit       = 0-7

@hexit       = [$digit A-F a-f]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = @hexit+
@exponent    = [eE] [\-\+]? @decimal

@number      = @decimal
             | @decimal \. @decimal @exponent?
             | @decimal @exponent
             | 0[oO] @octal
             | 0[xX] @hexadecimal

$letter      = [a-zA-Z_]
$fragment    = [^\{]

@escape      = \\ ([ntvbrfaeE\\\?\"] | $octit{1,3} | x@hexit+ | X@hexit+)
@ident       = [$digit $letter ']
@string      = [. $newline] # [\"\\] | @escape

tokens :-

<0> "{{"              { atom KIdentL `andBegin` exp }
<0> "{%"              { atom KSectionL `andBegin` exp }
<0> "{-"              { begin com }

<0> $newline          { atom KNewLine }
<0> $whitespace+      { capture KWhiteSpace }
<0> .                 { captureFrag }

<exp> "}}"            { atom KIdentR `andBegin` 0 }
<exp> "%}"            { atom KSectionR `andBegin` 0 }
<exp> $white+         { skip }

<exp> "="             { atom KEquals }

<exp> "True"          { atom KTrue }
<exp> "true"          { atom KTrue }
<exp> "False"         { atom KFalse }
<exp> "false"         { atom KFalse }

<exp> "else"          { atom KElse }
<exp> "if"            { atom KIf }
<exp> "elif"          { atom KElseIf }
<exp> "elsif"         { atom KElseIf }
<exp> "endif"         { atom KEndIf }
<exp> "case"          { atom KCase }
<exp> "when"          { atom KWhen }
<exp> "endcase"       { atom KEndCase }
<exp> "for"           { atom KFor }
<exp> "in"            { atom KIn }
<exp> "endfor"        { atom KEndFor }
<exp> "include"       { atom KInclude }
<exp> "with"          { atom KWith }
<exp> "assign"        { atom KAssign }
<exp> "capture"       { atom KCapture }
<exp> "endcapture"    { atom KEndCapture }

<exp> $sign? @number  { capture KNum }
<exp> $letter @ident* { capture KIdent }

<exp> \" @string* \"  { scoped KText (Text.tail . Text.init) }

<exp> "-"             { capture KOp }
<exp> "+"             { capture KOp }
<exp> "!"             { capture KOp }
<exp> "&&"            { capture KOp }
<exp> "||"            { capture KOp }
<exp> "=="            { capture KOp }
<exp> "!="            { capture KOp }
<exp> ">"             { capture KOp }
<exp> ">="            { capture KOp }
<exp> "<="            { capture KOp }
<exp> "<"             { capture KOp }

<exp> \(              { atom KParenL }
<exp> \)              { atom KParenR }
<exp> \[              { atom KBracketL }
<exp> \]              { atom KBracketR }
<exp> \.              { atom KDot }
<exp> \,              { atom KComma }
<exp> \_              { atom KUnderscore }

<com> "-}"            { begin 0 }
<com> [. $newline]    { skip }

{
scoped :: Capture -> (Text -> Text) -> AlexInput -> Int -> Alex Token
scoped k f inp len = return $ TC (inpMeta inp) k (f . Text.take len $ inpText inp)

capture :: Capture -> AlexInput -> Int -> Alex Token
capture k inp len = return $ TC (inpMeta inp) k (Text.take len $ inpText inp)

atom :: Atom -> AlexInput -> Int -> Alex Token
atom k inp _ = return $ TA (inpMeta inp) k

captureFrag AlexInput{..} _ = do
    setInput $ AlexInput
        { inpMeta = Text.foldl' move inpMeta res
        , inpText = Text.drop (Text.length res) inpText
        }
    return $ TC inpMeta KFrag res
  where
    res = go inpText

    go txt =
        let (x, y) = Text.break (== '{') txt
            r      = fst <$> Text.uncons (Text.drop 1 y)
         in case (Text.null y, r) of
                (_,     Just '%') -> x
                (_,     Just '-') -> x
                (_,     Just '{') -> x
                (True,  _)        -> x
                (False, _)        -> x <> Text.take 2 y <> go (Text.drop 2 y)

start :: String -> Meta
start src = Meta src 1 1

move :: Meta -> Char -> Meta
move (Meta src l c) '\t' = Meta src  l      (((c + 7) `div` 8) * 8 + 1)
move (Meta src l c) '\n' = Meta src (l + 1) 1
move (Meta src l c) _    = Meta src  l      (c + 1)

data AlexInput = AlexInput
    { inpMeta :: Meta
    , inpText :: {-# UNPACK #-} !Text
    } deriving (Show)

data AlexState = AlexState
    { stateInput :: AlexInput
    , stateCode  :: {-# UNPACK #-} !Int
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
        , stateCode  = 0
        }

    input = AlexInput
        { inpMeta = start src
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
            throw $ "lexical error at: " ++ show inp'
        AlexSkip inp' len -> do
            setInput inp'
            scan
        AlexToken inp' len action -> do
            setInput inp'
            action inp len

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
alexGetByte (AlexInput p t)
    | Text.null t = Nothing
    | otherwise   = Just $! (c2w c, AlexInput (move p c) cs)
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
