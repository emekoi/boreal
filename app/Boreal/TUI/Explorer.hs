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
    _ticks,
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
import Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Core as B
import qualified Brick.Widgets.List as B
import Brick.Widgets.ProgressBar
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Result (..))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.Platform ((&), (<>~), (^.))
import qualified Network.API.MAL.Anime as M
import Network.API.MAL.Constants (fields, paging)
import Network.API.MAL.Types
import Network.API.MAL.Types.Lens

data ExplorerEvent
  = UpdateTick
  | UpdateList
  | UpdateAnime (Int, Anime)
  deriving (Show)

data Explorer
  = Explorer
      { auth_token :: AuthToken,
        user :: T.Text,
        animes :: L.LazyList () Anime,
        next_page :: Maybe Int,
        limit :: Int,
        event_c :: B.BChan ExplorerEvent,
        ticks :: Int,
        dirty :: [Anime]
      }

makeFieldsNoPrefix ''Explorer

initExplorer :: (MonadIO m, MonadFail m) => T.Text -> Int -> B.BChan ExplorerEvent -> m Explorer
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
                ticks = 0,
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
    (selectedAttr, V.withStyle (B.fg V.cyan) V.bold),
    (progressCompleteAttr, V.withStyle (B.bg V.green) V.bold),
    (progressIncompleteAttr, V.withStyle (B.bg V.red) V.bold)
  ]

wshift :: Int -> Int -> String -> String
wshift width n s = if ls < width then s else take width $ t ++ h
  where
    ls = 1 + length s
    s' = s ++ " " ++ replicate (width - ls) ' '
    (h, t) = Prelude.splitAt (n `mod` max width ls) s'

titleMaxWidth :: Int
titleMaxWidth = 50

toEllipsis :: Int -> String -> String
toEllipsis n s
  | length s > n = take (n - 3) s <> "..."
  | otherwise = s

renderItem :: Explorer -> Bool -> Anime -> (String -> B.Widget ())
renderItem e sel a s
  | sel = B.withAttr selectedAttr (B.str $ "<" <> s' <> ">")
  | a `elem` e ^. _dirty = B.withAttr dirtyAttr (B.str s)
  | otherwise = B.str $ toEllipsis titleMaxWidth s
  where
    s' = wshift (titleMaxWidth - 2) (e ^. _ticks) s

listDrawAnime :: Explorer -> Bool -> Anime -> B.Widget ()
listDrawAnime e sel a =
  let epsWatched = num_episodes_watched $ my_list_status a
      epsWatched' = fromIntegral epsWatched
      totalEps = num_episodes a
      label = " [" <> show epsWatched <> "/" <> maybe "?" show totalEps <> "]"
      progbar = B.hLimitPercent 50 $ progressBar (Just label) (epsWatched' / maybe epsWatched' fromIntegral totalEps)
   in (B.hLimit titleMaxWidth . B.padRight B.Max) (renderItem e sel a (T.unpack (title a)))
        <+> B.translateBy (B.Location {loc = (1, 0)}) progbar

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
      B.borderWithLabel label $
        B.renderList (listDrawAnime e) True l
    ui =
      B.vBox
        [ C.hCenter box,
          C.hCenter . B.str $ if isJust $ next_page e then "updating list" else "",
          C.hCenter $ B.str "Press Esc to exit."
        ]
