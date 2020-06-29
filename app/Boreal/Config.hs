module Boreal.Config
  ( configDir,
    configFile,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (($>))
import qualified System.Directory as S

configDir :: MonadIO m => m FilePath
configDir = liftIO $ do
  S.getXdgDirectory S.XdgConfig "boreal" >>= \d ->
    S.createDirectoryIfMissing True d $> d

configFile :: MonadIO m => m FilePath
configFile = (++ "/config.json") <$> configDir
