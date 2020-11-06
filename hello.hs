main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    [ h1_ "Header"
    , p_ "Paragraph #1"
    , p_ "Paragraph #2"
    ]

newtype Html
  = Html String

type HtmlTitle
  = String

type HtmlBody
  = [HtmlBodyContent]

newtype HtmlBodyContent
  = HtmlBodyContent String

html_ :: HtmlTitle -> HtmlBody -> Html
html_ title content =
  Html
    ( el "html"
      ( concat
        [ el "head" (el "title" title)
        , el "body" (concat (map getBodyContentString content))
        ]
      )
    )

p_ :: String -> HtmlBodyContent
p_ = HtmlBodyContent . el "p"

h1_ :: String -> HtmlBodyContent
h1_ = HtmlBodyContent . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getBodyContentString :: HtmlBodyContent -> String
getBodyContentString (HtmlBodyContent str) = str

render :: Html -> String
render (Html str) = str
