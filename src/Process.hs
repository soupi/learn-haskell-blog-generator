module Process where

import System.Directory
import System.FilePath

import Html
import Markup
import MarkupToHtml

processDir :: FilePath -> FilePath -> IO ()
processDir inputDir outputDir = do
  postsMarkup <- parseDir inputDir
  let
    index =
      render (createIndex postsMarkup)
    posts =
      map
      (\(filename, markup) -> (filename, render (markupToHtml filename markup)))
      postsMarkup
    files =
      map
        (\(path, html) -> (outputDir </> path, html))
        (("index.html", index) : posts)
  createDirectory outputDir
  mapM_ (\(path, html) -> writeFile path html) files

parseDir :: FilePath -> IO [(FilePath, Markup)]
parseDir inputDir = do
  filenames <- listDirectory inputDir
  mapM
    (\filename -> do
      content <- readFile (inputDir </> filename)
      pure
        ( dropExtension (takeFileName filename) <.> "html"
        , parse content
        )
    )
    filenames

createIndex :: [(FilePath, Markup)] -> Html
createIndex =
  html_ "Home" . map toIndexEntry

toIndexEntry :: (FilePath, Markup) -> HtmlStructure
toIndexEntry (path, markup) =
  case markup of
    Header _ title : structure : _ ->
      span_
        [ h2_ (link_ path (txt_ title))
        , markupPartToHtml structure
        , p_ (link_ path (txt_ "..."))
        ]
    _ ->
      h2_ (link_ path (txt_ path))
