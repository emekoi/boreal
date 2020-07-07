module Boreal.TUI
  ( tuiMain,
  )
where

import Boreal.TUI.Explorer as E
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Types (Widget)
import Brick.Util (on)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Network.API.MAL.Anime as MAL
import Network.API.MAL.Types

type Name = ()

drawUI :: E.Explorer -> [Widget Name]
drawUI = map C.vCenter . E.drawExplorer

appEvent :: E.Explorer -> T.BrickEvent Name E.ExplorerEvent -> T.EventM Name (T.Next E.Explorer)
appEvent ex (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '+') [] -> do
      case E.animes ex ^. L.listSelectedL of
        Nothing -> M.continue ex
        Just i ->
          M.continue $ ex & _animes %~ L.listElementsL %~ ix i . _my_list_status . _num_episodes_watched +~ 1
    V.EvKey (V.KChar '-') [] -> do
      case E.animes ex ^. L.listSelectedL of
        Nothing -> M.continue ex
        Just i ->
          M.continue $ ex & _animes %~ L.listElementsL %~ ix i . _my_list_status . _num_episodes_watched -~ 1
    V.EvKey V.KEnter [] -> do
      case E.animes ex ^. L.listSelectedL of
        Nothing -> M.continue ex
        Just i ->
          (liftIO . writeBChan (E.event_c ex) . E.UpdateAnime $ (i, getAnime ex i)) >> M.continue ex
    V.EvKey V.KEsc [] -> M.halt ex
    ev -> L.handleListEvent ev (E.animes ex) >>= \x -> M.continue (ex & _animes .~ x)
  where
    getAnime e i = E.animes e ^?! L.listElementsL . ix i
appEvent e (T.AppEvent E.UpdateList) = do
  if isJust $ E.next_page e
    then M.continue =<< E.explorerGetNextPage e
    else M.continue e
appEvent e@E.Explorer {..} (T.AppEvent (E.UpdateAnime (i, a))) = do
  a' <- MAL.updateAnime auth_token a $ a ^. _my_list_status
  case a' of
    Error err -> Prelude.error err
    Success as' ->
      M.continue $ e & _animes %~ L.listElementsL %~ ix i . _my_list_status .~ as'
appEvent e _ = M.continue e

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    $ [ (L.listAttr, V.white `on` V.blue),
        (L.listSelectedAttr, V.blue `on` V.white)
      ]
      <> E.explorerAttrMap

theApp :: M.App E.Explorer E.ExplorerEvent Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

while :: Monad m => (a -> Bool) -> a -> m b -> m b
while p p' a =
  if p p'
    then a >> while p p' a
    else a

tuiMain :: Text -> IO ()
tuiMain u = do
  chan <- newBChan 10
  e <- E.initExplorer u 50 chan
  void . forkIO $ while (isJust . E.next_page) e $ do
    writeBChan chan E.UpdateList
    threadDelay 500000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just chan) theApp e
