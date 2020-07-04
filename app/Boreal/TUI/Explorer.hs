module Boreal.TUI.Explorer
  ( Explorer (..),
    initExplorer,
  )
where

import Boreal.Auth
import qualified Boreal.TUI.LazyVector as L
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Result (..))
import qualified Network.API.MAL.Anime as M
import Network.API.MAL.Constants (fields)
import Network.API.MAL.Types

data Explorer
  = Explorer
      { auth_token :: AuthToken,
        animes :: L.LazyList () Anime,
        tick :: Int
      }
  deriving (Show)

initExplorer :: MonadIO m => m Explorer
initExplorer = do
  getAuthToken >>= \case
    Just at@AuthToken {..} -> do
      al <- M.getAnimeList at [fields ["my_list_status", "num_episodes"]] "@me"
      case al of
        Error err -> error err
        Success al' -> return $ Explorer at (BL.list () (L.fromList 10 al') 1) 0
    _ -> error "please login first"
