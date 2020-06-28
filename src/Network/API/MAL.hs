{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Network.API.MAL
  ( userAuthenticate,
    userReAuthenticate,
    userInfo,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (traceShow)
import Network.API.MAL.Types
import Network.HTTP.Req
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

clientIdB :: B.ByteString
clientIdB = unsafePerformIO $ B.pack <$> getEnv "MAL_CLIENT_ID"
{-# NOINLINE clientIdB #-}

clientIdT :: Text
clientIdT = unsafePerformIO $ T.pack <$> getEnv "MAL_CLIENT_ID"
{-# NOINLINE clientIdT #-}

endpointV1 :: Url 'Https
endpointV1 = https "api.myanimelist.net" /: "v1"

endpointV2 :: Url 'Https
endpointV2 = https "api.myanimelist.net" /: "v2"

-- endpointV2' :: Url 'Https
-- endpointV2' = https "localhost"

baseHeaders :: Option 'Https
baseHeaders =
  header "X-MAL-Client-ID" clientIdB
    <> httpVersion 2 0

paging :: Int -> Int -> Option 'Https
paging limit offset =
  "limit" =: limit
    <> "offset" =: offset

fields :: [Text] -> Option 'Https
fields f = "fields" =: T.intercalate "," f

{- mkReq :: MonadIO m => HttpConfig -> Req a -> m a
mkReq = runReq -}

tshow :: Show a => a -> Text
tshow = T.pack . show

userAuthenticate :: MonadIO m => Text -> Text -> m AuthToken
userAuthenticate u p = do
  r <- runReq defaultHttpConfig $ do
    req
      POST
      (endpointV2 /: "auth" /: "token")
      (ReqBodyUrlEnc params)
      jsonResponse
      baseHeaders
  let (Success b) = fromJSON (responseBody r :: Value)
  return b
  where
    params =
      "client_id" =: clientIdT
        <> "grant_type" =: ("password" :: Text)
        <> "password" =: p
        <> "username" =: u

userReAuthenticate :: MonadIO m => AuthToken -> m AuthToken
userReAuthenticate AuthToken {..} = do
  r <- runReq defaultHttpConfig $ do
    req
      POST
      (endpointV1 /: "oauth2" /: "token")
      (ReqBodyUrlEnc params)
      jsonResponse
      baseHeaders
  let (Success b) = fromJSON (responseBody r :: Value)
  return b
  where
    params =
      "client_id" =: clientIdT
        <> "grant_type" =: ("refresh_token" :: Text)
        <> "refresh_token" =: refresh_token
userReAuthenticate _ = error "cannot re-authenticate invalid auth token"

userInfo :: MonadIO m => AuthToken -> Maybe Int -> m User
userInfo AuthToken {..} (Just uid) = do
  r <- runReq defaultHttpConfig $ do
    req
      GET
      (endpointV2 /: "users" /: tshow uid)
      NoReqBody
      jsonResponse
      (baseHeaders <> oAuth2Bearer (encodeUtf8 access_token))
  let (Success b) = fromJSON (responseBody r :: Value)
  return b
userInfo AuthToken {..} Nothing = do
  r <- runReq defaultHttpConfig $ do
    req
      GET
      (endpointV2 /: "users" /: "@me")
      NoReqBody
      jsonResponse
      (baseHeaders <> oAuth2Bearer (encodeUtf8 access_token))
  let (Success b) = fromJSON (responseBody r :: Value)
  return b
