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
      (h1_ "Header")
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
  , HtmlTitle
  , HtmlStructure
  , html_
  , p_
  , h1_
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

newtype HtmlStructure
  = HtmlStructure String

type HtmlTitle
  = String

-- * EDSL

html_ :: HtmlTitle -> HtmlStructure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getHtmlStructureString content)
      )
    )

p_ :: String -> HtmlStructure
p_ = HtmlStructure . el "p" . escape

h1_ :: String -> HtmlStructure
h1_ = HtmlStructure . el "h1" . escape

ul_ :: [HtmlStructure] -> HtmlStructure
ul_ =
  HtmlStructure . el "ul" . concat . map (el "li" . getHtmlStructureString)

ol_ :: [HtmlStructure] -> HtmlStructure
ol_ =
  HtmlStructure . el "ol" . concat . map (el "li" . getHtmlStructureString)

code_ :: String -> HtmlStructure
code_ = HtmlStructure . el "pre"

append_ :: HtmlStructure -> HtmlStructure -> HtmlStructure
append_ c1 c2 =
  HtmlStructure (getHtmlStructureString c1 <> getHtmlStructureString c2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getHtmlStructureString :: HtmlStructure -> String
getHtmlStructureString content =
  case content of
    HtmlStructure str -> str

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
