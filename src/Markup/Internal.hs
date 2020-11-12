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

        -- Unordered list
        ('-' : ' ' : line) : rest ->
          case current of
            -- Part of existing unordered list
            Just (UnorderedList list) ->
              impl (Just (UnorderedList (list <> [trim line]))) rest

            _ ->
              maybe id (:) current (impl (Just (UnorderedList [trim line])) rest)

        -- Ordered list
        ('#' : ' ' : line) : rest ->
          case current of
            -- Part of existing Ordered list
            Just (OrderedList list) ->
              impl (Just (OrderedList (list <> [trim line]))) rest

            _ ->
              maybe id (:) current (impl (Just (OrderedList [trim line])) rest)

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
