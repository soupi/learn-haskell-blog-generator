# Summary

In this chapter we built a very minimal HTML EDSL.
We will later use this library to convert our custom markup formatted text to HTML.

We've also learned about:

- Defining and using functions
- Types and type signatures
- Embedded domain specific languages
- Chaining functions using the `.` operator
- Preventing incorrect use with `newtype`s
- Defining modules and the `Internal` module pattern
- Encapsulation using `newtype`s and modules

Here's our complete program up to this point:

```hs
-- hello.hs

import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
      (h1_ "Heading")
      ( append_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )
```

```hs
-- Html.hs

module Html
  ( Html
  , Title
  , Structure
  , html_
  , h1_
  , p_
  , ul_
  , ol_
  , code_
  , append_
  , render
  )
  where

import Html.Internal
```

```hs
-- Html/Internal.hs

module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar
```

> You can also [browse the code as a tree](https://github.com/soupi/learn-haskell-blog-generator/tree/2a4691de627bcb280e92f3d02a88d5404179dc86).
