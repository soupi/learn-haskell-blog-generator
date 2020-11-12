module Main where

import Options.Applicative

import Process

main :: IO ()
main = do
  config <- execParser argumentsParserInfo
  processDir (cfgReplace config) (cfgInputDir config) (cfgOutputDir config)


data Config
  = Config
  { cfgInputDir :: FilePath
  , cfgOutputDir :: FilePath
  , cfgReplace :: Bool
  }

argumentsParserInfo :: ParserInfo Config
argumentsParserInfo =
  info
    (argumentsParser <**> helper)
    ( fullDesc
      <> header "hsblog-gen - a simple blog generator program"
      <> footer "For more information, visit https://github.com/soupi/learn-haskell-blog-generator"
    )

argumentsParser :: Parser Config
argumentsParser =
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
