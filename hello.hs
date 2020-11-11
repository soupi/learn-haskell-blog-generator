module Main where

import Html
import Markup
import MarkupToHtml

main :: IO ()
main = putStrLn (textToHtmlStr mymorkup)

textToHtmlStr :: String -> String
textToHtmlStr =
  render . markupToHtml "placeholder title" . parse

mymorkup :: String
mymorkup =
  unlines
    [ "@ Header 1"
--  , "@@ Header 2"
--  , "@@@ Header 3"
--  , "@@@@ Header 4"
--  , "@@@@@ Header 5"
    , ""
    , "Paragraph - Keep grouping lines until there's a line with only whitespace"
    , ""
    , "- Unordered list - Keep grouping unordered list until there's a line that does not start with '- '"
    , "- Numbered list - Keep grouping numbered list lines until there's a line that does not start with '# '"
    , "- Code block - Keep grouping numbered list lines until there's a line that does not start with '> '"
    , ""
    , "Exercises - Add:"
    , "# More headers"
    , "# Code blocks"
    ]
