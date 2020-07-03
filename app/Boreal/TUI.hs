module Boreal.TUI
  ( tuiMain,
  )
where

import Boreal.TUI.Explorer as E
import qualified Brick.AttrMap as A
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
import Control.Monad (void)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.Platform ((^.))
import Network.API.MAL.Types

drawUI :: E.Explorer -> [Widget ()]
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
            str " ",
            C.hCenter $ str "Press Esc to exit."
          ]

appEvent :: E.Explorer -> T.BrickEvent () e -> T.EventM () (T.Next E.Explorer)
appEvent ex (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt ex
    ev -> L.handleListEvent ev (E.animes ex) >>= \x -> M.continue (ex {animes = x})
appEvent ex _ = M.continue ex

listDrawAnime :: Bool -> Anime -> Widget ()
listDrawAnime sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
   in C.hCenter $ selStr (T.unpack $ title a)

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

theApp :: M.App E.Explorer e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

tuiMain :: IO ()
tuiMain =
  E.initExplorer
    >>= void . M.defaultMain theApp
