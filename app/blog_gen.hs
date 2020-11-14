module Main where

import Options.Applicative

import Network.Wai.Handler.Warp

import Process
import Server

main :: IO ()
main = do
  cmd <- execParser argumentsParserInfo
  case cmd of
    Convert config ->
      processDir (cfgReplace config) (cfgInputDir config) (cfgOutputDir config)
    Serve port inputDir -> do
      putStrLn ("Serving blog over port " <> show port)
      run port (app inputDir)

data Command
  = Convert Config
  | Serve Port FilePath

data Config
  = Config
  { cfgInputDir :: FilePath
  , cfgOutputDir :: FilePath
  , cfgReplace :: Bool
  }

argumentsParserInfo :: ParserInfo Command
argumentsParserInfo =
  info
    (argumentsParser <**> helper)
    ( fullDesc
      <> header "hsblog-gen - a simple blog generator program"
      <> footer "For more information, visit https://github.com/soupi/learn-haskell-blog-generator"
    )

argumentsParser :: Parser Command
argumentsParser =
  subparser
    ( command "convert" convertArgumentsParserInfo
      <> command "serve" serveArgumentsParserInfo
    )

serveArgumentsParserInfo :: ParserInfo Command
serveArgumentsParserInfo =
  info
    (serveArgumentsParser <**> helper)
    ( progDesc "Serve blog over http"
    )

serveArgumentsParser :: Parser Command
serveArgumentsParser =
  Serve
    <$>
      ( option auto
        ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 8000
          <> help "Port. The default is 8000"
        )
      )
    <*>
      ( strOption
        ( long "input-dir"
          <> short 'i'
          <> metavar "DIRECTORY"
          <> help "Input directory path"
        )
      )

convertArgumentsParserInfo :: ParserInfo Command
convertArgumentsParserInfo =
  info
    (fmap Convert convertArgumentsParser <**> helper)
    ( progDesc "Convert blogs to html"
    )

convertArgumentsParser :: Parser Config
convertArgumentsParser =
  Config
    <$>
      ( strOption
        ( long "input-dir"
          <> short 'i'
          <> metavar "DIRECTORY"
          <> help "Input directory path"
        )
      )
    <*>
      ( strOption
        ( long "output-dir"
          <> short 'o'
          <> metavar "DIRECTORY"
          <> help "Output directory path"
        )
      )
    <*>
      ( switch
        ( long "replace"
          <> help "Delete output directory if it exists"
        )
      )
