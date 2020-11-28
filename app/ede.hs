{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<*>))
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Functor ((<$>))
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Options.Applicative as Options
import qualified Options.Applicative.Help.Pretty as Pretty
import qualified Text.EDE as EDE

data Options = Options
  { templateFile :: FilePath,
    jsonFile :: Maybe FilePath,
    jsonObject :: Maybe Aeson.Object
  }

optionsParser :: Options.Parser Options
optionsParser =
  Options
    <$> Options.strOption
      ( Options.long "template-file"
          <> Options.metavar "PATH"
          <> Options.help
            "Path of a file containing an EDE template"
      )
    <*> Options.optinal
      ( Options.option
          readObject
          ( Options.long "context-json"
              <> Options.metavar "OBJECT"
              <> Options.help
                "Template context as a JSON object"
          )
      )
    <*> Options.optinal
      ( Options.strOption
          ( Options.long "context-file"
              <> Options.metavar "PATH"
              <> Options.help
                "Path of a file containing a JSON object which is used \
                \as the template context"
          )
      )

parserInfo :: Options.ParserInfo Options
parserInfo =
  Options.info
    (Options.helper <*> optionsParser)
    (Options.header "EDE template processor" <> Options.progDescDoc (Just usage))
  where
    usage =
      Pretty.vcat
        [ Pretty.empty,
          "The --context-file and --context-json options are processed as follows:",
          Pretty.indent 2 "1.)"
            <+> ( Pretty.align
                    ( "Both are provided"
                        Pretty.<$$> "Both objects are merged into one with the keys of \
                                    \--context-json taking precedence over those in the \
                                    \file provided by --context-file."
                    )
                ),
          Pretty.indent 2 "2.)"
            <+> ( Pretty.align
                    ( "None of them are provided"
                        Pretty.<$$> "The JSON object is read from STDIN."
                    )
                ),
          Pretty.indent 2 "3.)"
            <+> ( Pretty.align
                    ( "One of them is provided"
                        Pretty.<$$> "The JSON object is read from the supplied option."
                    )
                )
        ]

main :: IO ()
main = do
  options <- Options.execParser parserInfo

  ctx <-
    case (jsonFile options, jsonObject options) of
      (Just path, Just obj1) -> do
        eobj2 <- Aeson.eitherDecode <$> LBS.readFile path

        case eobj2 of
          Left err -> fail err
          Right obj2 -> pure (Map.union obj1 obj2)
      --
      (Just path, Nothing) -> do
        eobj <- Aeson.eitherDecode <$> LBS.readFile path
        case eobj of
          Left err -> fail err
          Right obj -> pure obj
      --
      (Nothing, Just obj) ->
        pure obj
      --
      (Nothing, Nothing) ->
        BS.getContents
          >>= either fail pure . Parsec.parseOnly stdinParser

  EDE.parseFile (templateFile options) >>= \case
    EDE.Failure err -> print err >> Exit.exitFailure
    EDE.Success tpl ->
      case EDE.render tpl ctx of
        EDE.Failure err -> print err >> Exit.exitFailure
        EDE.Success output -> Text.Lazy.IO.putStr output

stdinParser :: Parsec.Parser Aeson.Object
stdinParser = Aeson.json >>= requireObject

readValue :: Options.ReadM Aeson.Value
readValue = Options.str >>= decodeJsonStr

decodeJsonStr :: Monad m => String -> m Aeson.Value
decodeJsonStr =
  either fail pure . Aeson.eitherDecode . LBS.fromStrict
    . Text.encodeUtf8
    . Text.pack

readObject :: Options.ReadM Aeson.Object
readObject = readValue >>= requireObject

requireObject :: Monad m => Aeson.Value -> m Aeson.Object
requireObject = \case
  Aeson.Object obj -> pure obj
  _ -> fail "JSON value must be an object"
