module Network.API.MAL.User
  ( userInfo,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.API.MAL.Constants
import Network.API.MAL.Types
import Network.HTTP.Req

tshow :: Show a => a -> Text
tshow = T.pack . show

userInfo :: MonadIO m => AuthToken -> Maybe Int -> m (Result User)
userInfo InvalidToken {..} _ = error $ T.unpack message
userInfo AuthToken {..} (Just uid) = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "users" /: tshow uid)
          NoReqBody
          jsonResponse
          (baseHeaders <> oAuth2Bearer (encodeUtf8 access_token))
      )
userInfo AuthToken {..} Nothing = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( do
          req
            GET
            (endpointV2 /: "users" /: "@me")
            NoReqBody
            jsonResponse
            (baseHeaders <> oAuth2Bearer (encodeUtf8 access_token))
      )
