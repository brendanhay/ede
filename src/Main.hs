{-# LANGUAGE RecordWildCards #-}

-- Module      : Text.EDE
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Conduit
import qualified Data.Conduit.Attoparsec    as Conduit
import qualified Data.Conduit.Binary        as Conduit
import           Data.Monoid
import qualified Data.Text.Lazy.IO          as Text
import           Data.Version               (showVersion)
import           Options.Applicative
import qualified System.Directory           as Dir
import qualified System.IO                  as IO
import qualified Text.EDE                   as EDE

data Options = Options
    { template  :: FilePath
    , bindings  :: Maybe Object
    , alternate :: !Bool
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> (strOption
         ( short   't'
        <> long    "template"
        <> metavar "FILE"
        <> help    "ED-E template to render."
         )
        <|> argument str (metavar "FILE"))

    <*> optional (option (eitherReader reader)
         ( short   'd'
        <> long    "data"
        <> metavar "JSON"
        <> help    "Bindings to make available in the environment, as JSON. \
                   \If not given, standard input is read."
         ))

    <*> switch
         ( short 'a'
        <> long  "alternate-syntax"
        <> help  "Use alternate template syntax."
         )

optionInfo :: ParserInfo Options
optionInfo = info (helper <*> optionParser)
    ( fullDesc
   <> header   ("ed-e v" ++ showVersion EDE.version)
   <> progDesc "ED-E Template Engine CLI."
    )

main :: IO ()
main = execParser optionInfo >>= render
  where
    render Options{..} = do
        t <- parse template alternate
        b <- maybe stdin return bindings
        EDE.result errRender Text.putStrLn (EDE.render t b)

    parse f alt = do
        p <- Dir.getPermissions f
        unless (Dir.readable p) (errUnreadable f)

        let syntax | alt       = EDE.alternateSyntax
                   | otherwise = EDE.defaultSyntax

        EDE.parseFileWith syntax f >>=
            EDE.result (error . show) return

    stdin = do
        IO.hSetBinaryMode IO.stdin True
        v <- Conduit.sourceHandle IO.stdin $$ Conduit.sinkParser json'
        either (error . show) return (fromValue v)

    errRender = error . mappend "Error rendering template:\n" . show

    errUnreadable = error . (<> " is not readable")

reader :: String -> Either String Object
reader = join . fmap fromValue . eitherDecode . LBS.pack

fromValue :: Value -> Either String Object
fromValue = maybe (Left "Bindings must be given as a JSON object") Right
    . EDE.fromValue
