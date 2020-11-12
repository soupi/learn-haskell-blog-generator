-- Warning: Internal module - usage at your own risk!
module Html.Internal where

-----------
-- Types --
-----------

newtype Html
  = Html String

type HtmlTitle
  = String

type HtmlBody
  = [HtmlStructure]

newtype HtmlStructure
  = HtmlStructure String

newtype HtmlContent
  = HtmlContent String

----------
-- EDSL --
----------

html_ :: HtmlTitle -> HtmlBody -> Html
html_ title content =
  Html
    ( el "html"
      ( concat
        [ el "head" (el "title" (escape title))
        , el "body" (concat (map getHtmlStructureString content))
        ]
      )
    )

p_ :: HtmlContent -> HtmlStructure
p_ = HtmlStructure . el "p" . getHtmlContent

h1_ :: HtmlContent -> HtmlStructure
h1_ = HtmlStructure . el "h1" . getHtmlContent

h2_ :: HtmlContent -> HtmlStructure
h2_ = HtmlStructure . el "h2" . getHtmlContent

ul_ :: [HtmlContent] -> HtmlStructure
ul_ = HtmlStructure . el "ul" . concat . map (el "li" . getHtmlContent)

ol_ :: [HtmlContent] -> HtmlStructure
ol_ = HtmlStructure . el "ol" . concat . map (el "li" . getHtmlContent)

code_ :: String -> HtmlStructure
code_ = HtmlStructure . el "pre"

span_ :: [HtmlStructure] -> HtmlStructure
span_ = HtmlStructure . el "span" . concat . map getHtmlStructureString

txt_ :: String -> HtmlContent
txt_ = HtmlContent . escape

link_ :: String -> HtmlContent -> HtmlContent
link_ path (HtmlContent content) =
  HtmlContent
    ("<a href=\"" <> path <> "\">" <> content <> "</a>")

img_ :: String -> HtmlContent
img_ path =
  HtmlContent $ "<img src=\"" <> path <> "\"/<img>"

empty_ :: HtmlContent
empty_ = HtmlContent ""

---

instance Semigroup HtmlContent where
  (<>) (HtmlContent c1) (HtmlContent c2) =
    HtmlContent (c1 <> c2)

instance Monoid HtmlContent where
  mempty = empty_


(<+>) :: HtmlContent -> HtmlContent -> HtmlContent
(<+>) (HtmlContent c1) (HtmlContent c2) =
  HtmlContent (c1 <> "&nbsp;" <> c2)

infixr 5 <+>

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

getHtmlStructureString :: HtmlStructure -> String
getHtmlStructureString (HtmlStructure str) = str

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

getHtmlContent :: HtmlContent -> String
getHtmlContent (HtmlContent str) = str
