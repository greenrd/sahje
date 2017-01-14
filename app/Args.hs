{-# LANGUAGE NoImplicitPrelude #-}
module Args (argParser, SahjeArgs(..)) where

import Options.Applicative ((<>), argument, help, long, metavar, Parser, short, str, strOption)

import ClassyPrelude hiding ((<>))

data SahjeArgs = SahjeArgs { schema     :: FilePath
                           , fileToEdit :: FilePath
                           }

schemaOption =
  strOption
   ( long "schema"
  <> short 's'
  <> metavar "SCHEMA"
  <> help "Use SCHEMA as the JSON schema file" )

fileArg =
  argument str (metavar "FILE.json")

argParser :: Parser SahjeArgs
argParser = SahjeArgs <$> schemaOption <*> fileArg
