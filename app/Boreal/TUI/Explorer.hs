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
import Data.Text (Text)
import qualified Network.API.MAL.Anime as M
import Network.API.MAL.Constants (fields, paging)
import Network.API.MAL.Types

data Explorer
  = Explorer
      { auth_token :: AuthToken,
        user :: Text,
        animes :: L.LazyList () Anime,
        next_page :: Bool,
        page :: Int
      }
  deriving (Show)

initExplorer :: MonadIO m => Text -> m Explorer
initExplorer u = do
  getAuthToken >>= \case
    Just at@AuthToken {..} -> do
      al <- M.getAnimeListP at [fields ["my_list_status", "num_episodes"], paging 10 0] u
      -- alp <- M.withPaging 10 [fields ["my_list_status", "num_episodes"]] $ flip (M.getAnimeList at) u
      case al of
        Error err -> error err
        Success (al', np) -> do
          return $ Explorer at u (BL.list () (L.fromList 10 al') 1) np 10
    _ -> error "please login first"
