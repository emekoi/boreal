module Boreal.TUI.Explorer
  ( Explorer (..),
    initExplorer,
    explorerNextPage,
  )
where

import Boreal.Auth
import qualified Boreal.TUI.LazyVector as L
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Result (..))
import Data.Text (Text)
import Lens.Micro.Platform ((&), (<>~))
import qualified Network.API.MAL.Anime as M
import Network.API.MAL.Constants (fields, paging)
import Network.API.MAL.Types

data Explorer
  = Explorer
      { auth_token :: AuthToken,
        user :: Text,
        animes :: L.LazyList () Anime,
        next_page :: Bool,
        limit :: Int,
        page :: Int
      }
  deriving (Show)

initExplorer :: MonadIO m => Text -> Int -> m Explorer
initExplorer u l = do
  getAuthToken >>= \case
    Just at@AuthToken {..} -> do
      al <- M.getAnimeListP at [fields ["my_list_status", "num_episodes"], paging l 0] u
      case al of
        Error err -> error err
        Success (al', np) -> do
          return $ Explorer at u (BL.list () (L.fromList l al') 1) np l l
    _ -> error "please login first"

explorerNextPage :: MonadIO m => Explorer -> m Explorer
explorerNextPage e@Explorer {..} = do
  al <- M.getAnimeListP auth_token [fields ["my_list_status", "num_episodes"], paging limit page] user
  case al of
    Error _ -> return e
    Success (al', np) -> do
      return
        e
          { animes = animes & BL.listElementsL <>~ L.fromList limit al',
            page = page + limit,
            next_page = np
          }
