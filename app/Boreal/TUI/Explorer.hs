{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Boreal.TUI.Explorer
  ( Explorer (..),
    ExplorerEvent (..),
    initExplorer,
    explorerGetNextPage,
    drawExplorer,
    explorerAttrMap,
    dirtyAttr,
    _auth_token,
    _user,
    _animes,
    _next_page,
    _limit,
    _event_c,
    _dirty,
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
import Network.API.MAL.Types.Lens

data ExplorerEvent
  = UpdateList
  | UpdateAnime (Int, Anime)
  deriving (Show)

data Explorer
  = Explorer
      { auth_token :: AuthToken,
        user :: Text,
        animes :: L.LazyList () Anime,
        next_page :: Maybe Int,
        limit :: Int,
        event_c :: B.BChan ExplorerEvent,
        dirty :: [Anime]
      }

makeFieldsNoPrefix ''Explorer

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
                event_c = c,
                dirty = []
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

selectedAttr :: A.AttrName
selectedAttr = B.listSelectedAttr <> "selected"

dirtyAttr :: A.AttrName
dirtyAttr = B.listAttr <> "dirty"

explorerAttrMap :: [(A.AttrName, V.Attr)]
explorerAttrMap =
  [ (dirtyAttr, V.withStyle (B.fg V.red) V.bold),
    (selectedAttr, V.withStyle (B.fg V.cyan) V.bold)
  ]

renderItem :: Explorer -> Bool -> Anime -> (String -> B.Widget ())
renderItem e sel a s
  | sel = B.withAttr selectedAttr (B.str $ "<" <> s <> ">")
  | a `elem` e ^. _dirty = B.withAttr dirtyAttr (B.str s)
  | otherwise = B.str s

listDrawAnime :: Explorer -> Bool -> Anime -> B.Widget ()
listDrawAnime e sel a =
  let epsWatched = show (num_episodes_watched $ my_list_status a)
      totalEps = maybe "?" show (num_episodes a)
   in C.hCenter . renderItem e sel a $ T.unpack (title a) <> " [" <> epsWatched <> "/" <> totalEps <> "]"

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
        $ B.renderList (listDrawAnime e) True l
    ui =
      B.vBox
        [ C.hCenter box,
          C.hCenter . B.str $ if isJust $ next_page e then "updating list" else "",
          C.hCenter $ B.str "Press Esc to exit."
        ]
