module Main where

import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    [ h1_ "Header"
    , p_ "Paragraph #1 which may contain <html>code</html>"
    , p_ "Paragraph #2"
    ]
