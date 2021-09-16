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

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
