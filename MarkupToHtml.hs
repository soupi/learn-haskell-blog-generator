module MarkupToHtml where

import Markup
import Html

markupToHtml :: String -> Markup -> Html
markupToHtml title markup =
  html_ title (map markupPartToHtml markup)


markupPartToHtml :: MarkupPart -> HtmlBodyContent
markupPartToHtml part =
  case part of
    Header _ txt ->
      -- we'll currently ignore the header level because we only have one option
      h1_ txt

    Paragraph p ->
      p_ p

    UnorderedList list ->
      ul_ list

    OrderedList list ->
      ol_ list
