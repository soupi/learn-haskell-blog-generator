-- Warning: Internal module - usage at your own risk!

{-# language OverloadedStrings #-}

module Html.Internal where

import qualified Data.Text as T

-----------
-- Types --
-----------

newtype Html
  = Html T.Text

type HtmlTitle
  = T.Text

type HtmlBody
  = [HtmlStructure]

newtype HtmlStructure
  = HtmlStructure T.Text

newtype HtmlContent
  = HtmlContent T.Text

----------
-- EDSL --
----------

html_ :: HtmlTitle -> HtmlBody -> Html
html_ title content =
  Html
    ( el "html"
      ( T.concat
        [ el "head" (el "title" (escape title))
        , el "body" (T.concat (map getHtmlStructureText content))
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
ul_ = HtmlStructure . el "ul" . T.concat . map (el "li" . getHtmlContent)

ol_ :: [HtmlContent] -> HtmlStructure
ol_ = HtmlStructure . el "ol" . T.concat . map (el "li" . getHtmlContent)

code_ :: T.Text -> HtmlStructure
code_ = HtmlStructure . el "pre"

span_ :: [HtmlStructure] -> HtmlStructure
span_ = HtmlStructure . el "span" . T.concat . map getHtmlStructureText

txt_ :: T.Text -> HtmlContent
txt_ = HtmlContent . escape

link_ :: T.Text -> HtmlContent -> HtmlContent
link_ path (HtmlContent content) =
  HtmlContent
    ("<a href=\"" <> path <> "\">" <> content <> "</a>")

img_ :: T.Text -> HtmlContent
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

render :: Html -> T.Text
render (Html str) = str

-----------
-- Utils --
-----------

el :: T.Text -> T.Text -> T.Text
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getHtmlStructureText :: HtmlStructure -> T.Text
getHtmlStructureText (HtmlStructure str) = str

escape :: T.Text -> T.Text
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> T.singleton c
  in
    T.concatMap escapeChar

getHtmlContent :: HtmlContent -> T.Text
getHtmlContent (HtmlContent str) = str
