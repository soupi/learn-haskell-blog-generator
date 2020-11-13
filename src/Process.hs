{-# language OverloadedStrings #-}

module Process where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Directory
import System.FilePath

import Html
import Markup
import MarkupToHtml

processDir :: Bool -> FilePath -> FilePath -> IO ()
processDir deleteOutputIfExists inputDir outputDir = do
  postsMarkup <- parseDir inputDir
  let
    index =
      render (createIndex postsMarkup)
    posts =
      map
      (\(filename, markup) -> (filename, render (markupToHtml (T.pack filename) markup)))
      postsMarkup
    files =
      map
        (\(path, html) -> (outputDir </> path, html))
        (("index.html", index) : posts)

  createDir deleteOutputIfExists outputDir

  mapM_ (\(path, html) -> T.writeFile path html) files

createDir :: Bool -> FilePath -> IO ()
createDir deleteOutputIfExists outputDir = do
  createDirectoryIfMissing True outputDir
  files <- listDirectory outputDir
  if deleteOutputIfExists
    then
      mapM_ (removeFile . (outputDir </>)) files
    else
      if null files -- no files in directory
        then
          pure ()
        else do
          hPutStrLn stderr ("Error: output path '" <> outputDir <> "' already exists and contains files.")
          exitFailure

parseDir :: FilePath -> IO [(FilePath, Markup)]
parseDir inputDir = do
  filenames <- listDirectory inputDir
  mapM
    (\filename -> do
      content <- T.readFile (inputDir </> filename)
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
        [ h2_ (link_ (T.pack path) (txt_ title))
        , markupPartToHtml structure
        , p_ (link_ (T.pack path) (txt_ "..."))
        ]
    _ ->
      h2_ (link_ (T.pack path) (txt_ (T.pack path)))
