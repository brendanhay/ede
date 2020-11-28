{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<*>))
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<$>))
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text (pack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy.IO as Text (putStr)
import Options.Applicative (mappend)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty
import qualified Text.EDE as EDE

main :: IO ()
main = do
  opts <- getOpts
  ctx <- case (jsonFile opts, jsonObject opts) of
    (Just fp, Just obj1) -> do
      eobj2 <- JSON.eitherDecode <$> LBS.readFile fp
      case eobj2 of
        Left e -> fail e
        Right obj2 -> return $ Map.union obj1 obj2
    (Just fp, Nothing) -> do
      eobj <- JSON.eitherDecode <$> LBS.readFile fp
      case eobj of
        Left e -> fail e
        Right obj -> return obj
    (Nothing, Just obj) -> return obj
    (Nothing, Nothing) -> BS.getContents >>= either fail return . P.parseOnly stdinParser
  tplResult <- EDE.parseFile (templateFile opts)
  case tplResult of
    EDE.Failure err -> print err
    EDE.Success tpl ->
      case EDE.render tpl ctx of
        EDE.Failure err -> print err
        EDE.Success output -> Text.putStr output

stdinParser :: P.Parser JSON.Object
stdinParser = JSON.json >>= requireJsonObj

data Opts = Opts
  { templateFile :: FilePath,
    jsonFile :: Maybe FilePath,
    jsonObject :: Maybe JSON.Object
  }

optsParser :: Opt.Parser Opts
optsParser =
  Opts
    <$> templateFileParser
    <*> Opt.optional jsonFileParser
    <*> Opt.optional jsonParser

templateFileParser :: Opt.Parser FilePath
templateFileParser =
  Opt.strOption $
    Opt.long "template-file"
      `mappend` Opt.metavar "PATH"
      `mappend` Opt.help "Path of a file containing an EDE template"

jsonParser :: Opt.Parser JSON.Object
jsonParser =
  Opt.option readJsonObj $
    Opt.long "context-json"
      `mappend` Opt.metavar "OBJECT"
      `mappend` Opt.help "Template context as a JSON object"

jsonFileParser :: Opt.Parser FilePath
jsonFileParser =
  Opt.strOption $
    Opt.long "context-file"
      `mappend` Opt.metavar "PATH"
      `mappend` Opt.help "Path of a file containing a JSON object which is used as the template context"

readJsonVal :: Opt.ReadM JSON.Value
readJsonVal = Opt.str >>= decodeJsonStr

decodeJsonStr :: Monad m => String -> m JSON.Value
decodeJsonStr =
  either fail return . JSON.eitherDecode . LBS.fromStrict
    . Text.encodeUtf8
    . Text.pack

readJsonObj :: Opt.ReadM JSON.Object
readJsonObj = readJsonVal >>= requireJsonObj

requireJsonObj :: Monad m => JSON.Value -> m JSON.Object
requireJsonObj (JSON.Object obj) = return obj
requireJsonObj _ = fail "JSON value must be an object"

optsParserInfo :: Opt.ParserInfo Opts
optsParserInfo =
  Opt.info (Opt.helper <*> optsParser) $
    Opt.header "EDE template processor"
      `mappend` Opt.progDescDoc
        ( Just $
            vcat
              [ empty,
                "The --context-file and --context-json options are processed as follows:",
                indent 2 "1.)"
                  <+> ( align
                          ( "Both are provided"
                              <$$> "Both objects are merged into one with the keys of --context-json taking precedence over those in the file provided by --context-file."
                          )
                      ),
                indent 2 "2.)"
                  <+> ( align
                          ( "None of them is provided"
                              <$$> "The JSON object is read from STDIN."
                          )
                      ),
                indent 2 "3.)"
                  <+> ( align
                          ( "One of them is provided"
                              <$$> "The obvious thing happens."
                          )
                      )
              ]
        )

getOpts :: IO Opts
getOpts = Opt.execParser optsParserInfo
