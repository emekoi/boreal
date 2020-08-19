module Boreal.Anime
  ( printAnimeList,
  )
where

import Control.Monad (foldM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Network.API.MAL.Anime as M
-- import qualified Network.API.MAL.Auth as M

import Data.Text (unpack)
import Network.API.MAL.Types

printAnimeList :: MonadIO m => [Anime] -> m ()
printAnimeList = foldM_ f (1 :: Int)
  where
    f ix a = liftIO $ do
      mapM_ putStr [show ix, ". ", unpack $ title a, "\n"]
      return $ ix + 1
