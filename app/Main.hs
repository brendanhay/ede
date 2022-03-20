{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Options.Applicative as Options
import qualified Options.Applicative.Help.Pretty as Pretty
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
import qualified System.Exit as Exit
import qualified Text.EDE as EDE
import qualified Text.EDE.Internal.Compat as Compat

data Options = Options
  { templateFile :: FilePath,
    jsonFile :: Maybe FilePath,
    jsonObject :: Maybe (HashMap Text Aeson.Value)
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
    <*> Options.optional
      ( Options.strOption
          ( Options.long "context-file"
              <> Options.metavar "PATH"
              <> Options.help
                "Path of a file containing a JSON object which is used \
                \as the template context"
          )
      )
    <*> Options.optional
      ( Options.option
          readObject
          ( Options.long "context-json"
              <> Options.metavar "OBJECT"
              <> Options.help
                "Template context as a JSON object"
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
            Pretty.<+> ( Pretty.align
                           ( "Both are provided"
                               Pretty.<$$> "Both objects are merged into one with the keys of \
                                           \--context-json taking precedence over those in the \
                                           \file provided by --context-file."
                           )
                       ),
          Pretty.indent 2 "2.)"
            Pretty.<+> ( Pretty.align
                           ( "None of them are provided"
                               Pretty.<$$> "The JSON object is read from STDIN."
                           )
                       ),
          Pretty.indent 2 "3.)"
            Pretty.<+> ( Pretty.align
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
        eobj2 <- Aeson.eitherDecode <$> ByteString.Lazy.readFile path

        case eobj2 of
          Left err -> fail err
          Right obj2 -> pure (obj1 <> obj2)
      --
      (Just path, Nothing) -> do
        eobj <- Aeson.eitherDecode <$> ByteString.Lazy.readFile path
        case eobj of
          Left err -> fail err
          Right obj -> pure obj
      --
      (Nothing, Just obj) ->
        pure obj
      --
      (Nothing, Nothing) ->
        ByteString.getContents
          >>= either fail pure . Parsec.parseOnly stdinParser

  EDE.parseFile (templateFile options) >>= \case
    EDE.Failure err -> PP.putDoc (err <> PP.hardline) >> Exit.exitFailure
    EDE.Success tpl ->
      case EDE.render tpl ctx of
        EDE.Failure err -> PP.putDoc (err <> PP.hardline) >> Exit.exitFailure
        EDE.Success output -> Text.Lazy.IO.putStr output

stdinParser :: Parsec.Parser (HashMap Text Aeson.Value)
stdinParser = Aeson.json >>= requireObject

readValue :: Options.ReadM Aeson.Value
readValue = Options.str >>= decodeJsonStr

decodeJsonStr :: Fail.MonadFail m => String -> m Aeson.Value
decodeJsonStr =
  either fail pure . Aeson.eitherDecode . ByteString.Lazy.fromStrict
    . Text.Encoding.encodeUtf8
    . Text.pack

readObject :: Options.ReadM (HashMap Text Aeson.Value)
readObject = readValue >>= requireObject

requireObject :: Fail.MonadFail m => Aeson.Value -> m (HashMap Text Aeson.Value)
requireObject = \case
  Aeson.Object obj -> pure (Compat.toHashMapText obj)
  _ -> fail "JSON value must be an object"
