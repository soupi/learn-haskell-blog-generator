-- Main.hs
module Main where

import qualified Markup
import qualified Html
import Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- No program arguments: reading from stdin and writing to stdout
    [] -> do
      content <- getContents
      putStrLn (process "Empty title" content)

    -- With input and output file paths as program arguments
    [input, output] -> do
      content <- readFile input
      exists <- doesFileExist output
      let
        writeResult = writeFile output (process input content)
      if exists
        then whenIO confirm writeResult
        else writeResult

    -- Any other kind of program arguments
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
