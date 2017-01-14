module Main where

import Args
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
  args <- execParser . info (helper <*> argParser) $
    fullDesc <> progDesc "Edit FILE.json in a structured way" <>
    header "sahje - Schema Aware Haskell JSON Editor"
  json <- parseJsonFile $ fileToEdit args
  theSchema <- parseJsonFile $ schema args
  let schemaWithURI = SchemaWithURI theSchema . return . pack $ "file:///" ++ schema args
  validationResult <- fetchHTTPAndValidate schemaWithURI json
  case validationResult of
    Left (HVRequest failure) -> fail $ "HTTP request failed: " ++ show failure
    Left (HVSchema si) -> fail $ schema args ++ " is not a valid draft4 JSON Schema: " ++ show si
    Left (HVData iv) -> fail $ fileToEdit args ++ " failed JSON schema validation: " ++ show iv
    Right () -> brickMain schemaWithURI (fileToEdit args) json
