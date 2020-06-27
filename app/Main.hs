{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Network.HTTP.Req
import System.Environment (getEnv)

main :: IO ()
main = do
  client_id <- B.pack <$> getEnv "MAL_CLIENT_ID"
  runReq defaultHttpConfig $ do
    let headers =
          header "X-MAL-Client-ID" client_id
            <> "q" =: ("Toradora" :: Text)
        rget = req GET
    r <-
      rget
        (https "api.myanimelist.net" /: "v2" /: "anime")
        NoReqBody
        jsonResponse
        headers
    r' <-
      rget
        (https "api.jikan.moe" /: "v3" /: "user" /: "emekoi" /: "history" /: "manga")
        NoReqBody
        jsonResponse
        mempty
    liftIO $ do
      print (responseBody r :: Value)
      print (responseBody r' :: Value)
