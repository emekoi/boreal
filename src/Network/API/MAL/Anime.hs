module Network.API.MAL.Anime
  ( searchAnime,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Network.API.MAL.Constants
import Network.API.MAL.Types
import Network.HTTP.Req

searchAnime :: MonadIO m => Text -> m (Result [Anime])
searchAnime title = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "anime")
          NoReqBody
          jsonResponse
          (baseHeaders <> "q" =: title)
      )
