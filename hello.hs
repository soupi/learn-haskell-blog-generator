module Main where

import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    [ h1_ "Header"
    , p_ "Paragraph #1 which contains:"
    , ul_
      [ "<html>code</html>"
      , "unordered lists"
      ]
    , p_ "Paragraph #2 where we list our tutorial sections:"
    , ol_
      [ "Tiny Html generation library (Done for now!)"
      , "Defining our custom markup language and parsing it"
      , "Glue things together"
      , "Use libraries to add command line parsing and concurrent proccessing"
      ]
    ]


mymorkup :: String
mymorkup =
  unlines
    [ "@ Header 1"
    , "@@ Header 2"
    , "@@@ Header 3"
    , "@@@@ Header 4"
    , "@@@@@ Header 5"
    , ""
    , "Paragraph - Keep grouping lines until there's a line with only whitespace"
    , ""
    , "- Unordered list - Keep grouping unordered list until there's a line that does not start with '- '"
    , ""
    , "- Numbered list - Keep grouping numbered list lines until there's a line that does not start with '# '"
    , ""
    , "- Code block - Keep grouping numbered list lines until there's a line that does not start with '> '"
    ]
