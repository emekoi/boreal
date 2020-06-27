{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.API.MAL.Internals.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Function (on)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, ZonedTime)
import Data.Time.Format.ISO8601 (Format (..), FormatExtension (..), ISO8601 (..), iso8601ParseM, iso8601Show, zonedTimeFormat)

data Broadcast
  = Broadcast -- the broadcasting schedule of the anime
      { day_of_the_week :: DayOfWeek, -- the day of the week that the media is released
        start_time :: Text -- time of the broadcast
      }
  deriving (Show)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Broadcast

data Date
  = ZonedTime ZonedTime -- an iso8601 formatted time string
  | Day Day
  deriving (Show)

instance FromJSON Date where
  parseJSON = withText "Date" $ \v ->
    Day <$> iso8601ParseM (T.unpack v) <|> ZonedTime <$> iso8601ParseM (T.unpack v)

instance ToJSON Date where
  toJSON (ZonedTime day) = String . T.pack . iso8601Show $ day
  toJSON (Day day) = String . T.pack . iso8601Show $ day

data Picture
  = Picture -- a set of pictures
      { large :: Text, -- an absolute URL to the the high(er) resolution picture
        medium :: Text -- an absolute URL to the the medium resolution picture
      }
  deriving (Show)

deriveJSON defaultOptions ''Picture

data SortingMethod
  = AnimeTitle -- the method of sorting entries in response lists
  | ListScore
  | ListUpdatedAt
  | AnimeStartDate
  deriving (Show)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''SortingMethod

data Season
  = Winter -- a season in a year
  | Spring
  | Summer
  | Fall
  deriving (Show)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Season

data AnimeSeason
  = AnimeSeason -- a specific anime season
      { season :: Season,
        year :: Int
      }
  deriving (Show)

deriveJSON defaultOptions ''AnimeSeason

data AnimeStatus
  = Watching
  | Completed
  | OnHold
  | Dropped
  | PlanToWatch
  deriving (Show)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''AnimeStatus

data AlternativeTitles
  = AlternativeTitles -- a set of titles and synonyms of the anime
      { en :: Text, -- english title of the media
        ja :: Text, -- the original (native) name of the media
        synonyms :: [Text] -- a lost of synonyms of the media
      }
  deriving (Show)

deriveJSON defaultOptions ''AlternativeTitles

data AnimeListStatus
  = AnimeListStatus -- a library entry
      { comments :: Text,
        is_rewatching :: Bool,
        num_episodes_watched :: Int,
        num_times_rewatched :: Int,
        priority :: Int,
        rewatch_value :: Int,
        score :: Double,
        status :: AnimeStatus,
        tags :: [Text],
        updated_at :: Date
      }
  deriving (Show)

deriveJSON defaultOptions ''AnimeListStatus

data Anime
  = Anime -- an anime in the MAL database
      { alternative_titles :: AlternativeTitles,
        average_episode_duration :: Int, -- the average duration (in seconds) of the episodes
        broadcast :: Broadcast,
        created_at :: Date,
        end_date :: Date, -- the date at which the anime ended
        anime_id :: Int, -- the identifier of this media on MAL
        main_picture :: Picture, -- the poster artwork of the anime
        mean :: Double, -- the mean score of this media on MAL
        media_type :: Text, -- the type of this media (e.g. tv)
        nsfw :: Text, -- the NSFW state for this media (e.g. white)
        num_episodes :: Int, -- the number of episodes in this anime
        num_favorites :: Int, -- the number of users that added this media to their favorites
        num_list_users :: Int, -- the number of uses that added this media to their lists
        num_scoring_users :: Int, -- (?) The number of users that voted for the scores
        popularity :: Int, -- the popularity rankings of this anime
        rank :: Int, -- the rankings of this anime
        start_date :: Date, -- the date at which the anime started
        start_season :: Season, -- the season at which the anime started broadcasting
        status :: Text, -- an enumeration representing the broadcasting status of the anime (e.g. finished_airing)
        synopsis :: Text, -- the synopsis of the anime
        title :: Text, -- the canonical (?) title of the anime
        updated_at :: Date, -- the last time that the information is updated on MAL
        my_list_status :: AnimeListStatus,
        background :: Text, -- background story of the anime
        related_anime :: [Anime] -- a list of anime related to this anime
      }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = \v -> fromMaybe v $ stripPrefix "anime_" v} ''Anime

instance Eq Anime where
  a == b = on (==) anime_id a b

data User
  = User -- a user
      { user_id :: Int,
        name :: Text,
        location :: Text,
        joined_at :: Date
      }
  deriving (Show)

instance Eq User where
  a == b = on (==) user_id a b

deriveJSON defaultOptions {fieldLabelModifier = \v -> fromMaybe v $ stripPrefix "user_" v} ''User
