module Boreal.Anime
  ( printAnimeList,
  )
where

import Control.Monad (foldM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Network.API.MAL.Anime as M
-- import qualified Network.API.MAL.Auth as M

import Fmt
import Network.API.MAL.Types

printAnimeList :: MonadIO m => [Anime] -> m ()
printAnimeList = foldM_ f (1 :: Int)
  where
    f ix a = liftIO $ do
      fmtLn ("" +| ix |+ ". " +| title a |+ "")
      return $ ix + 1
