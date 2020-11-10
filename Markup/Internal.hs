module Markup.Internal where

type Markup
  = [MarkupPart]

data MarkupPart
  = Header Int String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

parse :: String -> [MarkupPart]
parse =
  let
    impl current txt =
      case txt of
        -- done case
        [] ->
          maybe id (:) current []

        -- Header 1
        ('@' : ' ' : line) : rest ->
          maybe id (:) current (Header 1 (trim line) : impl Nothing rest)

        -- Paragraph
        line : rest ->
          case trim line of
            -- Empty line
            "" ->
              maybe id (:) current (impl Nothing rest)

            trimmedLine ->
              case current of
                -- Part of existing paragraph
                Just (Paragraph p) ->
                  impl (Just (Paragraph (p <> " " <> trimmedLine))) rest

                _ ->
                  maybe id (:) current (impl (Just (Paragraph trimmedLine)) rest)

  in
    impl Nothing . lines

trim :: String -> String
trim = unwords . words
