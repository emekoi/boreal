{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}

module Network.API.MAL.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.TH
import Data.Char (isUpper, toLower)
import Data.Function (on)
import Data.List (group, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, ZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Network.API.MAL.Types.Lens

data Broadcast
  = Broadcast -- the broadcasting schedule of the anime
      { day_of_the_week :: DayOfWeek, -- the day of the week that the media is released
        start_time :: Text -- time of the broadcast
      }
  deriving (Show)

deriveJSON
  defaultOptions
    { constructorTagModifier = map toLower
    }
  ''Broadcast

makeFieldsNoPrefix ''Broadcast

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

makeFieldsNoPrefix ''Picture

data SortingMethod
  = AnimeTitle -- the method of sorting entries in response lists
  | ListScore
  | ListUpdatedAt
  | AnimeStartDate
  deriving (Show)

deriveJSON
  defaultOptions
    { constructorTagModifier =
        let f c = if isUpper (head c) then "_" <> map toLower c else c
         in \v -> fromMaybe v . stripPrefix "_" . mconcat . map f . group $ v
    }
  ''SortingMethod

data Season
  = Winter -- a season in a year
  | Spring
  | Summer
  | Fall
  deriving (Show)

deriveJSON
  defaultOptions
    { constructorTagModifier = map toLower
    }
  ''Season

data AnimeSeason
  = AnimeSeason -- a specific anime season
      { season :: Season,
        year :: Int
      }
  deriving (Show)

deriveJSON defaultOptions ''AnimeSeason

makeFieldsNoPrefix ''AnimeSeason

data AnimeStatus
  = Watching
  | Completed
  | OnHold
  | Dropped
  | PlanToWatch
  deriving (Show)

deriveJSON
  defaultOptions
    { constructorTagModifier =
        let f c = if isUpper (head c) then "_" <> map toLower c else c
         in \v -> fromMaybe v . stripPrefix "_" . mconcat . map f . group $ v
    }
  ''AnimeStatus

data AlternativeTitles
  = AlternativeTitles -- a set of titles and synonyms of the anime
      { en :: Text, -- english title of the media
        ja :: Text, -- the original (native) name of the media
        synonyms :: [Text] -- a lost of synonyms of the media
      }
  deriving (Show)

deriveJSON defaultOptions ''AlternativeTitles

makeFieldsNoPrefix ''AlternativeTitles

data AnimeListStatus
  = AnimeListStatus -- a library entry
      { comments :: Maybe Text,
        is_rewatching :: Bool,
        num_episodes_watched :: Int,
        num_times_rewatched :: Maybe Int,
        priority :: Maybe Int,
        rewatch_value :: Maybe Int,
        score :: Double,
        status :: AnimeStatus,
        tags :: Maybe [Text],
        updated_at :: Date,
        start_date :: Maybe Date,
        finish_date :: Maybe Date
      }
  deriving (Show)

deriveJSON defaultOptions ''AnimeListStatus

makeFieldsNoPrefix ''AnimeListStatus

data Anime
  = Anime -- an anime in the MAL database
      { alternative_titles :: Maybe AlternativeTitles,
        average_episode_duration :: Maybe Int, -- the average duration (in seconds) of the episodes
        broadcast :: Maybe Broadcast,
        created_at :: Maybe Date,
        end_date :: Maybe Date, -- the date at which the anime ended
        anime_id :: Int, -- the identifier of this media on MAL
        main_picture :: Picture, -- the poster artwork of the anime
        mean :: Maybe Double, -- the mean score of this media on MAL
        media_type :: Maybe Text, -- the type of this media (e.g. tv)
        nsfw :: Maybe Text, -- the NSFW state for this media (e.g. white)
        num_episodes :: Maybe Int, -- the number of episodes in this anime
        num_favorites :: Maybe Int, -- the number of users that added this media to their favorites
        num_list_users :: Maybe Int, -- the number of uses that added this media to their lists
        num_scoring_users :: Maybe Int, -- (?) The number of users that voted for the scores
        popularity :: Maybe Int, -- the popularity rankings of this anime
        rank :: Maybe Int, -- the rankings of this anime
        start_date :: Maybe Date, -- the date at which the anime started
        start_season :: Maybe AnimeSeason, -- the season at which the anime started broadcasting
        status :: Maybe Text, -- an enumeration representing the broadcasting status of the anime (e.g. finished_airing)
        synopsis :: Maybe Text, -- the synopsis of the anime
        title :: Text, -- the canonical (?) title of the anime
        updated_at :: Maybe Date, -- the last time that the information is updated on MAL
        my_list_status :: AnimeListStatus,
        background :: Maybe Text, -- background story of the anime
        related_anime :: Maybe [Anime] -- a list of anime related to this anime
      }
  deriving (Show)

deriveJSON
  defaultOptions
    { fieldLabelModifier = \v -> fromMaybe v $ stripPrefix "anime_" v
    }
  ''Anime

makeFieldsNoPrefix ''Anime

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

deriveJSON
  defaultOptions
    { fieldLabelModifier = \v -> fromMaybe v $ stripPrefix "user_" v
    }
  ''User

makeFieldsNoPrefix ''User

data AuthToken
  = AuthToken
      { expires_in :: Int,
        access_token :: Text,
        refresh_token :: Text
      }
  | InvalidToken
      { error :: Text,
        message :: Text,
        hint :: Maybe Text
      }
  deriving (Show)

deriveJSON
  defaultOptions
    { sumEncoding = UntaggedValue
    }
  ''AuthToken

makeFieldsNoPrefix ''AuthToken
