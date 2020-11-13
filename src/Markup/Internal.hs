{-# language OverloadedStrings #-}

module Markup.Internal where

import qualified Data.Text as T

type Markup
  = [MarkupPart]

data MarkupPart
  = Header Int T.Text
  | Paragraph T.Text
  | UnorderedList [T.Text]
  | OrderedList [T.Text]
  | CodeBlock [T.Text]
  deriving Show

parse :: T.Text -> [MarkupPart]
parse =
  let
    impl current txt =
      case txt of
        -- done case
        [] ->
          maybe id (:) current []

        line : rest ->
          handleLine current line rest

    handleLine current fullline rest
        -- Header 1
      | Just line <- dropPrefixIfExists "@ " fullline =
          maybe id (:) current (Header 1 (trim line) : impl Nothing rest)

        -- Unordered list
      | Just line <- dropPrefixIfExists "- " fullline =
          case current of
            -- Part of existing unordered list
            Just (UnorderedList list) ->
              impl (Just (UnorderedList (list <> [trim line]))) rest

            _ ->
              maybe id (:) current (impl (Just (UnorderedList [trim line])) rest)

        -- Ordered list
      | Just line <- dropPrefixIfExists "# " fullline =
          case current of
            -- Part of existing Ordered list
            Just (OrderedList list) ->
              impl (Just (OrderedList (list <> [trim line]))) rest

            _ ->
              maybe id (:) current (impl (Just (OrderedList [trim line])) rest)

        -- Paragraph
      | otherwise =
          case trim fullline of
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
    impl Nothing . T.lines

trim :: T.Text -> T.Text
trim = T.unwords . T.words

dropPrefixIfExists :: T.Text -> T.Text -> Maybe T.Text
dropPrefixIfExists prefix text
  | prefix `T.isPrefixOf` text =
    Just (T.drop (T.length prefix) text)

  | otherwise =
    Nothing
