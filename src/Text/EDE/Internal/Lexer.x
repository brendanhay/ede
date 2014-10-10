{
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ViewPatterns             #-}

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

import Debug.Trace
import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as LText
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

@escape      = \\ ([ntvbrfaeE\\\?\"] | $octit{1,3} | x@hexit+ | X@hexit+)
@ident       = [$digit $letter ']
@string      = [. $newline] # [\"\\] | @escape

ede :-

-- start block
<0> {
    "{#"              / { jinja } { coml }
    "{{"              / { jinja } { varl }
    "{%+"             / { jinja } { blockl }
    $whitespace* "{%" / { jinja } { blockl }

    "@*"              / { play } { coml }
    "@{"              / { play } { varl }
    "@(+"             / { play } { blockl }
    $whitespace* "@(" / { play } { blockl }
}

-- fragments
<0> $newline     { atom KNewLine }
<0> $whitespace+ { capture KWhiteSpace }
<0> .            { fragment }

<com> {
    "#}" $whitespace+ $newline / { jinja } { comr }
    "#}"                       / { jinja } { comr }

    "*@" $whitespace+ $newline / { play } { comr }
    "*@"                       / { play } { comr }
}

<com> [. $newline] { skip }

-- end block
<exp> {
    "}}"                       / { jinja } { varr }
    "+%}"                      / { jinja } { blockr }
    "%}" $whitespace* $newline / { jinja } { blockr }
    "%}"                       / { jinja } { blockr }

    "}@"                       / { play } { varr }
    "+)@"                      / { play } { blockr }
    ")@" $whitespace* $newline / { play } { blockr }
    ")@"                       / { play } { blockr }
}

-- expression
<exp> $newline        { atom KNewLine }
<exp> $whitespace+    { skip }

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
<exp> "let"           { atom KLet }
<exp> "endlet"        { atom KEndLet }

<exp> $sign? @number  { capture KNum }
<exp> $letter @ident* { capture KVar }

<exp> \" @string* \"  { scoped KText (LText.tail . LText.init) }

<exp> "-"             { capture KOp }
<exp> "+"             { capture KOp }
<exp> "!"             { capture KOp }
<exp> "&&"            { capture KOp }
<exp> "||"            { capture KOp }
<exp> "|"             { capture KOp }
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
<exp> \_              { atom KUnderscore }

{
jinja, play :: a -> AlexInput -> Int -> AlexInput -> Bool
jinja _ _ _ inp = inpSyntax inp == Jinja
play  _ _ _ inp = inpSyntax inp == Play

coml, comr, blockl, blockr, varl, varr :: AlexInput -> Int -> Alex Token
coml   = begin com
comr   = begin 0
blockl = atom KBlockL `andBegin` 0
blockr = atom KBlockR `andBegin` 0
varl   = atom KVarL   `andBegin` 0
varr   = atom KVarR   `andBegin` 0

scoped :: Capture -> (Text -> Text) -> AlexInput -> Int -> Alex Token
scoped k f inp len = return $
    TC (inpMeta inp) k (f . LText.take (fromIntegral len) $ inpText inp)

capture :: Capture -> AlexInput -> Int -> Alex Token
capture k inp len = return $
    TC (inpMeta inp) k (LText.take (fromIntegral len) $ inpText inp)

atom :: Atom -> AlexInput -> Int -> Alex Token
atom k inp _ = return $ TA (inpMeta inp) k

fragment :: AlexInput -> a -> Alex Token
fragment (AlexInput s m t) _ = do
    setInput $ AlexInput
        { inpSyntax = s
        , inpMeta   = LText.foldl' move m res
        , inpText   = LText.drop (LText.length res) t
        }
    return (TC m KFrag res)
  where
    res = go t

    go txt = case z of
        Just c | match s c    -> x
        _      | LText.null y -> x
        _                     -> x <> LText.take 2 y <> go (LText.drop 2 y)
      where
        z = fst <$> LText.uncons (LText.drop 1 y)

        (x, y) = LText.break (break s) txt

    break Jinja = (== '{')
    break Play  = (== '@')

    match Jinja c = c == '{' || c == '#' || c == '%' || c == '+'
    match Play  c = c == '{' || c == '*' || c == '(' || c == '+'

start :: String -> Meta
start src = Meta src 1 1

move :: Meta -> Char -> Meta
move (Meta src l c) '\t' = Meta src  l      (((c + 7) `div` 8) * 8 + 1)
move (Meta src l c) '\n' = Meta src (l + 1) 1
move (Meta src l c)  _   = Meta src  l      (c + 1)

data AlexInput = AlexInput
    { inpSyntax :: !Syntax
    , inpMeta   :: Meta
    , inpText   :: Text
    } deriving (Show)

data AlexState = AlexState
    { stateInput :: AlexInput
    , stateCode  :: !Int
    }

newtype Alex a = Alex { unAlex :: AlexState -> Either Error (AlexState, a) }

instance Monad Alex where
    return !x = Alex $ \s -> Right (s, x)
    {-# INLINE return #-}

    (>>=) !m !k = Alex $ \s ->
        case unAlex m s of
            Left  e       -> Left e
            Right (s', x) -> unAlex (k x) s'
    {-# INLINE (>>=) #-}

runAlex :: String -> Syntax -> Text -> Alex a -> Either Error a
runAlex src s txt (Alex f) = snd `fmap` f state
  where
    state = AlexState
        { stateInput = input
        , stateCode  = 0
        }

    input = AlexInput
        { inpSyntax = s
        , inpMeta   = start src
        , inpText   = txt
        }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = const '\n'

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
            return (TA (inpMeta inp) KEOF)
        AlexError inp' ->
            Alex (const (Left (err inp inp')))
        AlexSkip inp' len -> do
            setInput inp'
            scan
        AlexToken inp' len action -> do
            setInput inp'
            action inp len
  where
    err p c = Lexer (inpMeta c)
        ["lexical scanner error in ..`" ++ LText.unpack (snip p) ++ "`.."]

    snip (inpText -> t)
        | LText.null t        = "<eof>"
        | LText.length t >= 6 = LText.take 6 t
        | LText.length t >= 3 = LText.take 3 t
        | otherwise           = LText.take 1 t

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
alexGetByte (AlexInput s m t)
    | LText.null t = Nothing
    | otherwise    = Just $! (c2w c, AlexInput s (move m c) cs)
  where
    (c, cs) = (LText.head t, LText.tail t)

    -- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
    -- silently truncates to 8 bits Chars > '\255'. It is provided as
    -- convenience for ByteString construction.
    c2w = fromIntegral . ord

runLexer :: String -> Syntax -> Text -> Result [Token]
runLexer src s txt = case runAlex src s txt loop of
    Left  e -> Error   e
    Right x -> Success x
  where
    loop = do
        t <- scan
        if tokenEOF t
            then return []
            else do
                ts <- loop
                return $ t : ts
}
