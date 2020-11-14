{-# language OverloadedStrings #-}

module Server where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL

import Network.Wai
import Network.HTTP.Types

import Control.Exception

import Process
import Html


app :: FilePath -> Application
app inputDir request respond = do
  mHtmls <-
    catch
      (Right <$> prepareHtmls inputDir "")
      (\e -> pure (Left (show (e :: IOException))))
  respond $
    case (mHtmls, pathInfo request) of
      (Left err, _) ->
        internalError err

      (Right htmls, []) ->
        respondWithPage "index.html" htmls

      (Right htmls, [file]) ->
        respondWithPage (T.unpack file) htmls

      _ ->
        notFound

respondWithPage :: FilePath -> [(FilePath, Html)] -> Response
respondWithPage path htmls =
  case lookup path htmls of
    Nothing ->
      notFound

    Just index ->
      responseLBS
        status200
        [("Content-Type", "text/html")]
        (renderToLBS index)

notFound :: Response
notFound =
  responseLBS
    status404
    [("Content-Type", "text/html")]
    ( renderToLBS $
      html_
        "404 Not Found"
        [ h1_ (txt_ "404 Not Found")
        , p_ (txt_ "Page does not exist.")
        ]
    )

internalError :: String -> Response
internalError err =
  responseLBS
    status500
    [("Content-Type", "text/html")]
    ( renderToLBS $
      html_
        "500 Internal Error"
        [ h1_ (txt_ "500 Internal Error")
        , p_ (txt_ (T.pack err))
        ]
    )

renderToLBS :: Html -> BL.ByteString
renderToLBS = BL.fromStrict . T.encodeUtf8 . render
