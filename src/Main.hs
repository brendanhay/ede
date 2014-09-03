{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad              (unless)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Conduit
import qualified Data.Conduit.Attoparsec    as C
import qualified Data.Conduit.Binary        as C
import qualified Data.Text.Lazy.IO          as T
import           Data.Version               (showVersion)
import           Options.Applicative
import           Paths_ede                  (version)
import           System.Directory           (getPermissions, readable)
import           System.IO                  (hSetBinaryMode, stdin)
import qualified Text.EDE                   as EDE


data Opts = Opts
    { template :: FilePath
    , bindings :: Maybe Object
    } deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
    <$> ( strOption
           ( short   't'
          <> long    "template"
          <> metavar "FILE"
          <> help    "ED-E template to render."
           )
       <|> argument str (metavar "FILE")
        )
    <*> optional
        ( option (eitherReader readJSON)
          ( short   'd'
         <> long    "data"
         <> metavar "JSON"
         <> help    ( "Bindings to make available in the environment, as JSON. "
                   ++ "If not given, standard input is read.")
          )
        )
  where
    readJSON = either Left jObject . eitherDecode . B.pack

optInfo :: ParserInfo Opts
optInfo = info (helper <*> parseOpts)
    ( fullDesc
   <> header   ("ed-e v" ++ showVersion version)
   <> progDesc "ED-E Template Engine CLI."
    )

main :: IO ()
main = execParser optInfo >>= \ Opts{..} -> do
    t <- readTemplate template
    b <- maybe fromStdin return bindings
    EDE.result errRender T.putStrLn $ EDE.render t b
  where
    readTemplate t = do
        p <- getPermissions t
        unless (readable p) $ errUnreadable t
        EDE.parseFile t >>= EDE.result (error . show) return

    fromStdin = do
        hSetBinaryMode stdin True
        j <- C.sourceHandle stdin $$ C.sinkParser json'
        either error pure $ jObject j

    errRender m  es = error . unlines $ "Error rendering template:" : show m : es
    errUnreadable t = error $ t ++ " is not readable"


jObject :: Value -> Either String Object
jObject (Object o) = Right o
jObject _          = Left  "Bindings must be given as a JSON object"
