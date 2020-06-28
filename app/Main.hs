module Main
  ( main,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import System.Environment (getEnv)

tshow :: Show a => a -> Text
tshow = T.pack . show

animeInfo :: B.ByteString -> Int -> IO Value
animeInfo client_id anime_id = do
  r <- runReq defaultHttpConfig $ do
    let headers =
          header "X-MAL-Client-ID" client_id
            <> httpVersion 2 0
        rget = req GET
    rget
      (https "api.myanimelist.net" /: "v2" /: "anime" /: tshow anime_id)
      NoReqBody
      jsonResponse
      headers
  return (responseBody r :: Value)

main :: IO ()
main = do
  client_id <- B.pack <$> getEnv "MAL_CLIENT_ID"
  ai <- animeInfo client_id 1
  L.putStrLn $ encodePretty ai
