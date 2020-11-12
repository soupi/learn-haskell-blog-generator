module Main where

import Html
import Markup
import MarkupToHtml

main :: IO ()
main = do
  txt <- readFile "blog-post.txt"
  let
    outputHtml = textToHtmlStr txt
  writeFile "output.html" outputHtml

textToHtmlStr :: String -> String
textToHtmlStr =
  render . markupToHtml "placeholder title" . parse
