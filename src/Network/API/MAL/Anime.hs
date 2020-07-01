module Network.API.MAL.Anime
  ( searchAnime,
    animeInfo,
    getAnimeList,
    updateAnime,
  )
where

import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.MAL.Constants
import Network.API.MAL.Types
import Network.HTTP.Req

parseAnimeList :: Value -> Parser [Anime]
parseAnimeList = withObject "AnimeList" (mapM (.: "node") <=< (parseJSONList <=< (.: "data")))

searchAnime :: MonadIO m => Option 'Https -> Text -> m (Result [Anime])
searchAnime opts title = do
  parse parseAnimeList . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "anime")
          NoReqBody
          jsonResponse
          (baseHeaders <> opts <> "q" =: title)
      )

animeInfo :: MonadIO m => Option 'Https -> Int -> m (Result Anime)
animeInfo opts aid = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "anime" /: T.pack (show aid))
          NoReqBody
          jsonResponse
          (baseHeaders <> opts)
      )

getAnimeList :: MonadIO m => AuthToken -> Option 'Https -> Text -> m (Result [Anime])
getAnimeList AuthToken {..} opts user = do
  parse parseAnimeList . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "users" /: user /: "animelist")
          NoReqBody
          jsonResponse
          (baseHeaders <> opts <> params)
      )
  where
    params =
      "sort" =: ("anime_title" :: Text)
        <> oAuth2Bearer (encodeUtf8 access_token)
getAnimeList InvalidToken {..} _ _ = return . Error $ T.unpack message

updateAnime :: MonadIO m => AuthToken -> Anime -> AnimeListStatus -> m (Result AnimeListStatus)
updateAnime InvalidToken {..} _ _ = return . Error $ T.unpack message
updateAnime AuthToken {..} _ _ = do
  return $ Error "unimplemented"
