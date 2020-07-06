module Boreal.TUI.Explorer
  ( Explorer (..),
    ExplorerEvent (..),
    initExplorer,
    explorerGetNextPage,
    drawExplorer,
    explorerAttrMap,
  )
where

import Boreal.Auth
import qualified Boreal.TUI.LazyVector as L
import qualified Brick.AttrMap as A
import qualified Brick.BChan as B
import qualified Brick.Types as B
import qualified Brick.Util as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as B
import Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.List as B
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Result (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.Platform ((&), (<>~), (^.))
import qualified Network.API.MAL.Anime as M
import Network.API.MAL.Constants (fields, paging)
import Network.API.MAL.Types

data Explorer
  = Explorer
      { auth_token :: AuthToken,
        user :: Text,
        animes :: L.LazyList () Anime,
        next_page :: Maybe Int,
        limit :: Int,
        event_c :: B.BChan ExplorerEvent
      }

data ExplorerEvent
  = UpdateList
  | UpdateAnime AnimeListStatus
  deriving (Show)

initExplorer :: (MonadIO m, MonadFail m) => Text -> Int -> B.BChan ExplorerEvent -> m Explorer
initExplorer u l c = do
  getAuthToken >>= \case
    Just at@AuthToken {..} -> do
      al <- M.getAnimeListP at [fields ["my_list_status", "num_episodes"], paging l 0] u
      case al of
        Error err -> fail err
        Success (al', np) -> do
          return $
            Explorer
              { auth_token = at,
                user = u,
                animes = B.list () (L.fromList al') 1,
                next_page = if np then Just l else Nothing,
                limit = l,
                event_c = c
              }
    _ -> fail "please login first"

explorerGetNextPage :: MonadIO m => Explorer -> m Explorer
explorerGetNextPage e@Explorer {..} = do
  al <-
    M.getAnimeListP
      auth_token
      [ fields ["my_list_status", "num_episodes"],
        paging limit (fromMaybe (-1) next_page)
      ]
      user
  case al of
    Error _ -> return e
    Success (al', np) -> do
      return
        e
          { animes = animes & B.listElementsL <>~ L.fromList al',
            next_page = if np then (+ limit) <$> next_page else Nothing
          }

explorerAttr :: A.AttrName
explorerAttr = B.listSelectedAttr <> "selected"

explorerAttrMap :: [(A.AttrName, V.Attr)]
explorerAttrMap =
  [ (explorerAttr, V.withStyle (B.fg V.red) V.bold)
  ]

listDrawAnime :: Bool -> Anime -> B.Widget ()
listDrawAnime sel a =
  let selStr s =
        if sel
          then B.withAttr explorerAttr (B.str $ "<" <> s <> ">")
          else B.str s
      epsWatched = show (num_episodes_watched $ my_list_status a)
      totalEps = maybe "?" show (num_episodes a)
   in C.hCenter . selStr $ T.unpack (title a) <> " [" <> epsWatched <> "/" <> totalEps <> "]"

drawExplorer :: Explorer -> [B.Widget ()]
drawExplorer e = [ui]
  where
    l = animes e
    label = B.str "Anime " <+> cur <+> B.str " of " <+> total
    cur = case B.listSelected l of
      Nothing -> B.str "-"
      Just i -> B.str (show (i + 1))
    total = B.str . show . length $ l ^. B.listElementsL
    box =
      B.borderWithLabel label
        $ B.hLimit 100
        $ B.vLimit 15
        $ B.renderList listDrawAnime True l
    ui =
      B.vBox
        [ C.hCenter box,
          C.hCenter . B.str $ if isJust $ next_page e then "updating list" else "",
          C.hCenter $ B.str "Press Esc to exit."
        ]
