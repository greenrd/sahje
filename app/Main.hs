module Main where

import qualified Args
import Model
import UI

import ClassyPrelude hiding ((<>))
import Data.Aeson (eitherDecode, FromJSON)
import Data.JsonSchema.Draft4 ( fetchHTTPAndValidate
                              , HTTPValidationFailure(..)
                              , SchemaWithURI(..)
                              )
import Options.Applicative

parseJsonFile :: FromJSON a => FilePath -> IO a
parseJsonFile =
  (=<<) . flip either return . bail <*> fmap eitherDecode . readFile
  where
    bail :: FilePath -> String -> IO a
    bail fp = fail . ((fp ++ " does not contain well-formed JSON: ") ++)

main :: IO ()
main = do
  args <- execParser . info (helper <*> Args.argParser) $
    fullDesc <> progDesc "Edit FILE.json in a structured way" <>
    header "sahje - Schema Aware Haskell JSON Editor"
  json <- parseJsonFile $ Args.fileToEdit args
  theSchema <- parseJsonFile $ Args.schema args
  let schemaWithURI = SchemaWithURI theSchema . return . pack $ "file:///" ++ Args.schema args
  validationResult <- fetchHTTPAndValidate schemaWithURI json
  case validationResult of
    Left (HVRequest failure) -> fail $ "HTTP request failed: " ++ show failure
    Left (HVSchema si) -> fail $ Args.schema args ++ " is not a valid draft4 JSON Schema: " ++ show si
    Left (HVData iv) -> fail $ Args.fileToEdit args ++ " failed JSON schema validation: " ++ show iv
    Right () ->
      let initialState = SahjeState { schema = schemaWithURI
                                    , filename = Args.fileToEdit args
                                    , model = toModel json
                                    }
      in brickMain initialState
