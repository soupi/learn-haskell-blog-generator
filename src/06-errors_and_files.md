# Handling errors and multiple files

We have left an unimplemented function last chapter,
There are a few more things left for us to do to actually call our program a static blog generator.
We still need to process multiple files in a directory and create an index landing page with links to other pages.

## Links in HTML

Our HTML EDSL currently does not support links or other content modifiers such as bold and italics.
We should add these so we can use them when creating an index.

Up until now we've passed `String` to `Structure` creating functions such as `p_`
and `h_`. Instead, we could create and pass them a new type, `Content`, we
can be regular text, links, images, and so on.

---

**Exercise**: implement what we've just discussed. Follow the compiler errors and refactor what needs refactoring.

<details><summary>Solution</summary>

<details><summary>src/Html/Internal.hs</summary>

```hs
-- Html/Internal.hs
module HsBlog.Html.Internal where

import Numeric.Natural

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

newtype Content
  = Content String

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

-- * Structure

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h1_ :: Content -> Structure
h1_ = Structure . el "h1" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = Structure ""

-- * Content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

instance Semigroup Content where
  (<>) c1 c2 =
    Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
  mempty = Content ""

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString structure =
  case structure of
    Structure str -> str

getContentString :: Content -> String
getContentString content =
  case content of
    Content str -> str

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

</details>

<details><summary>src/Html.hs</summary>

```hs
-- Html.hs

module HsBlog.Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h_
  , h1_
  , ul_
  , ol_
  , code_
  , txt_
  , img_
  , link_
  , b_
  , i_
  , render
  )
where

import HsBlog.Html.Internal
```

</details>

<details><summary>src/Convert.hs</summary>

```hs
-- Convert.hs
module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Header n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

</details>

</details>

---

## Creating an index page

With our extended HTML EDSL, we can now create an index page with links to the other pages.

To create an index page, we need a list of files with their *target destination*,
as well as their `Markup` (so we can extract information to include in our index page,
such as the first header and paragraph). Our output should be an `Html` page.

---

We need to implement the following function:

```hs
buildIndex :: [(FilePath, Markup)] -> Html
```

<details><summary>Solution</summary>

```hs
buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Header 1 header : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ header))
                <> foldMap convertStructure (take 3 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      "Blog"
      ( Html.h1_ (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )
```


</details>

---

## Processing directories

As was hinted in the last chapter, we are going to use the
[`directory`](https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html)
package to complete our task. We need to:

- Create the output directory
- Grab all file names in a directory
- Filter them according to their extension, we want to process `txt` file and
  copy other files without modification
- We want to parse each text file, build an index of the result,
  convert the files to HTML, and write everything to the target directory

While our parsing function can't really fail, trying to read or write a file
to the file-system can fail in several ways. It would be nice if our
static blog generator was robust enough that it wouldn't fail completely if one
single file gave it some trouble. This is a good opportunity to learn about
error handling in Haskell, both in uneffectful code and for I/O code.

In the next few chapters we'll survey the landscape of error handling in Haskell
before figuring out what is the right approach for our use case.

