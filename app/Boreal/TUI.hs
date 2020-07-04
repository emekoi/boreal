module Boreal.TUI
  ( tuiMain,
  )
where

import Boreal.TUI.Explorer as E
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Types
  ( Widget,
  )
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( (<+>),
    hLimit,
    str,
    vBox,
    vLimit,
    withAttr,
  )
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
-- import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.Platform ((^.))
import Network.API.MAL.Types

data UpdateList = Tick

type Name = ()

drawUI :: E.Explorer -> [Widget Name]
drawUI e = [ui]
  where
    l = E.animes e
    label = str "Anime " <+> cur <+> str " of " <+> total
    cur = case L.listSelected l of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ length $ l ^. L.listElementsL
    box =
      B.borderWithLabel label
        $ hLimit 50
        $ vLimit 15
        $ L.renderList listDrawAnime True l
    ui =
      C.vCenter $
        vBox
          [ C.hCenter box,
            C.hCenter . str $ "dummy update (" <> show (tick e) <> ")",
            C.hCenter $ str "Press Esc to exit."
          ]

appEvent :: E.Explorer -> T.BrickEvent Name UpdateList -> T.EventM Name (T.Next E.Explorer)
appEvent ex (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt ex
    ev -> L.handleListEvent ev (E.animes ex) >>= \x -> M.continue (ex {animes = x})
appEvent ex (T.AppEvent e) =
  case e of
    Tick -> do
      M.continue ex {tick = tick ex + 1}
appEvent ex _ = M.continue ex

listDrawAnime :: Bool -> Anime -> Widget Name
listDrawAnime sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
      epsWatched = show (num_episodes_watched $ my_list_status a)
      totalEps = maybe "?" show (num_episodes a)
   in C.hCenter . selStr $ T.unpack (title a) <> " [" <> epsWatched <> "/" <> totalEps <> "]"

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue),
      (L.listSelectedAttr, V.blue `on` V.white),
      (customAttr, fg V.cyan)
    ]

theApp :: M.App E.Explorer UpdateList Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

tuiMain :: IO ()
-- tuiMain =
--   E.initExplorer
--     >>= void . M.defaultMain theApp
tuiMain = do
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000 -- decides how fast your game moves
  e <- E.initExplorer
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just chan) theApp e
