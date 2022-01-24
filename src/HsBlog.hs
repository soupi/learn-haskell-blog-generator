-- src/HsBlog.hs

module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  , buildIndex
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)
import HsBlog.Directory (convertDirectory, buildIndex)
import HsBlog.Env (defaultEnv)

import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse
