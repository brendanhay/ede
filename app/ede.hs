import           Control.Applicative  ((<*>))
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor         ((<$>))
import qualified Data.Text            as Text (pack)
import qualified Data.Text.Encoding   as Text (encodeUtf8)
import qualified Data.Text.Lazy.IO    as Text (putStr)
import           Options.Applicative  (mappend)
import qualified Options.Applicative  as Opt
import qualified Text.EDE             as EDE

main :: IO ()
main = do
  opts <- getOpts
  tplResult <- EDE.parseFile (file opts)
  case tplResult of
    EDE.Failure err -> print err
    EDE.Success tpl ->
      case EDE.render tpl (json opts) of
        EDE.Failure err    -> print err
        EDE.Success output -> Text.putStr output

data Opts = Opts { file :: FilePath, json :: JSON.Object }

optsParser :: Opt.Parser Opts
optsParser = Opts <$> fileParser <*> jsonParser

fileParser :: Opt.Parser FilePath
fileParser = Opt.strOption $
  Opt.long "template-file" `mappend`
  Opt.help "Path of a file containing an EDE template"

jsonParser :: Opt.Parser JSON.Object
jsonParser = Opt.option readJsonObj $
  Opt.long "context-json" `mappend`
  Opt.help "Template context as a JSON object"

readJsonVal :: Opt.ReadM JSON.Value
readJsonVal = Opt.str >>= decodeJsonStr

decodeJsonStr :: Monad m => String -> m JSON.Value
decodeJsonStr =
  either fail return . JSON.eitherDecode . LBS.fromStrict .
  Text.encodeUtf8 . Text.pack

readJsonObj :: Opt.ReadM JSON.Object
readJsonObj = readJsonVal >>= requireJsonObj

requireJsonObj :: Monad m => JSON.Value -> m JSON.Object
requireJsonObj (JSON.Object obj) = return obj
requireJsonObj _                 = fail "JSON value must be an object"

optsParserInfo :: Opt.ParserInfo Opts
optsParserInfo = Opt.info (Opt.helper <*> optsParser) $
  Opt.header "EDE template processor"

getOpts :: IO Opts
getOpts = Opt.execParser optsParserInfo
