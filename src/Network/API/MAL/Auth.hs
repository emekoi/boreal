module Network.API.MAL.Auth
  ( userAuthenticate,
    userReAuthenticate,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Network.API.MAL.Constants
import Network.API.MAL.Types
import Network.HTTP.Req

userAuthenticate :: MonadIO m => Text -> Text -> m (Result AuthToken)
userAuthenticate u p = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          POST
          (endpointV2 /: "auth" /: "token")
          (ReqBodyUrlEnc params)
          jsonResponse
          baseHeaders
      )
  where
    params =
      "client_id" =: clientIdT
        <> "grant_type" =: ("password" :: Text)
        <> "password" =: p
        <> "username" =: u

userReAuthenticate :: MonadIO m => AuthToken -> m (Result AuthToken)
userReAuthenticate AuthToken {..} = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          POST
          (endpointV1 /: "oauth2" /: "token")
          (ReqBodyUrlEnc params)
          jsonResponse
          baseHeaders
      )
  where
    params =
      "client_id" =: clientIdT
        <> "grant_type" =: ("refresh_token" :: Text)
        <> "refresh_token" =: refresh_token
userReAuthenticate _ = return $ Error "cannot re-authenticate invalid auth token"
