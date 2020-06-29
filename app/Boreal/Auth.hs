module Boreal.Auth
  ( getAuthToken,
    saveAuthToken,
  )
where

import Boreal.Config (configFile)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:), (.=), Result (..), decodeFileStrict, object, withObject)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser, Value, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import qualified Network.API.MAL.Auth as M
import Network.API.MAL.Types (AuthToken (..))

parseAuthInfo :: Value -> Parser (AuthToken, UTCTime)
parseAuthInfo = withObject "AuthInfo" $ \o ->
  liftA2 (,) (o .: "token") (o .: "expires_at")

getAuthToken :: MonadIO m => m (Maybe AuthToken)
getAuthToken = liftIO $ do
  now <- getCurrentTime
  (decodeFileStrict =<< configFile) >>= \case
    Just cfg ->
      case parseMaybe parseAuthInfo cfg of
        Just (at, ed) ->
          if now >= ed
            then do
              at' <- M.userReAuthenticate at
              case at' of
                Success at' -> do
                  saveAuthToken at'
                  return $ Just at'
                Error _ -> return Nothing
            else return $ Just at
        Nothing -> return Nothing
    Nothing -> return Nothing

saveAuthToken :: MonadIO m => AuthToken -> m ()
saveAuthToken at = liftIO $ do
  expiration_date <- addUTCTime (fromIntegral $ expires_in at) <$> getCurrentTime
  configFile >>= \cf ->
    L.writeFile cf . encodePretty $
      object
        [ "token" .= at,
          "expires_at" .= expiration_date
        ]
