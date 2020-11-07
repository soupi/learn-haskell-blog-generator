module Html
  ( Html
  , HtmlTitle
  , HtmlBody
  , HtmlBodyContent
  , html_
  , p_
  , h1_
  , render
  )
where

-----------
-- Types --
-----------

newtype Html
  = Html String

type HtmlTitle
  = String

type HtmlBody
  = [HtmlBodyContent]

newtype HtmlBodyContent
  = HtmlBodyContent String

----------
-- EDSL --
----------

html_ :: HtmlTitle -> HtmlBody -> Html
html_ title content =
  Html
    ( el "html"
      ( concat
        [ el "head" (el "title" (escape title))
        , el "body" (concat (map getBodyContentString content))
        ]
      )
    )

p_ :: String -> HtmlBodyContent
p_ = HtmlBodyContent . el "p" . escape

h1_ :: String -> HtmlBodyContent
h1_ = HtmlBodyContent . el "h1" . escape

------------
-- Render --
------------

render :: Html -> String
render (Html str) = str

-----------
-- Utils --
-----------

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getBodyContentString :: HtmlBodyContent -> String
getBodyContentString (HtmlBodyContent str) = str

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
