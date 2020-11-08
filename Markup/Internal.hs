module Markup.Internal where

type Markup
  = [MarkupPart]

data MarkupPart
  = Header Int String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

