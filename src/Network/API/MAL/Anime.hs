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

searchAnime :: MonadIO m => [Option 'Https] -> Text -> m (Result [Anime])
searchAnime opts title = do
  parse parseAnimeList . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "anime")
          NoReqBody
          jsonResponse
          (baseHeaders <> mconcat opts <> "q" =: title)
      )

animeInfo :: MonadIO m => Maybe AuthToken -> [Option 'Https] -> Int -> m (Result Anime)
animeInfo at opts aid = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "anime" /: T.pack (show aid))
          NoReqBody
          jsonResponse
          (baseHeaders <> maybe mempty (oAuth2Bearer . encodeUtf8 . access_token) at <> mconcat opts)
      )

getAnimeList :: MonadIO m => AuthToken -> [Option 'Https] -> Text -> m (Result [Anime])
getAnimeList AuthToken {..} opts user = do
  parse parseAnimeList . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          GET
          (endpointV2 /: "users" /: user /: "animelist")
          NoReqBody
          jsonResponse
          (baseHeaders <> mconcat opts <> params)
      )
  where
    params =
      "sort" =: ("anime_title" :: Text)
        <> oAuth2Bearer (encodeUtf8 access_token)
getAnimeList InvalidToken {..} _ _ = return . Error $ T.unpack message

tshow :: Show a => a -> Text
tshow = T.toLower . T.pack . show

updateAnime :: MonadIO m => AuthToken -> Anime -> AnimeListStatus -> m (Result AnimeListStatus)
updateAnime InvalidToken {..} _ _ = return . Error $ T.unpack message
updateAnime AuthToken {..} a asl = do
  fromJSON . responseBody
    <$> runReq
      defaultHttpConfig
      ( req
          PUT
          (endpointV2 /: "anime" /: (T.pack . show . anime_id $ a) /: "my_list_status")
          (ReqBodyUrlEnc $ params asl)
          jsonResponse
          (baseHeaders <> oAuth2Bearer (encodeUtf8 access_token))
      )
  where
    params AnimeListStatus {..} =
      mconcat
        [ "num_watched_episodes" =: tshow num_episodes_watched,
          "status" =: tshow status,
          "score" =: tshow score,
          maybe mempty (("start_date" =:) . tshow) start_date,
          maybe mempty (("finish_date" =:) . tshow) finish_date
        ]
