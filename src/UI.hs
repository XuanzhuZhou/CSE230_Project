{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Maze

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources

data Cell = Player1 | Player2 | Bullets | Solid | Normal | Grass

-- App definition

app :: App Game e ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO () -- !!!
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent () Tick -> EventM () (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget ()]
drawUI g =
--   [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]
    [ C.center $ padRight (Pad 2) drawGrid g ]

-- drawStats :: Game -> Widget ()
-- drawStats g = hLimit 11
--   $ vBox [ drawScore (g ^. score)
--          , padTop (Pad 2) $ drawGameOver (g ^. dead)
--          ]

-- drawScore :: Int -> Widget ()
-- drawScore n = withBorderStyle BS.unicodeBold
--   $ B.borderWithLabel (str "Score")
--   $ C.hCenter
--   $ padAll 1
--   $ str $ show n

drawGameOver :: Bool -> Widget ()
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget ()
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. _player1 = Player1
      | c == g ^. food      = Food
      | otherwise           = Empty

drawCell :: Cell -> Widget ()
drawCell Player1 = withAttr player1Attr cw 
drawCell Player2 = withAttr player2Attr cw 
drawCell Bullets = withAttr bulletsAttr cw 
drawCell Normal  = withAttr normalAttr cw 
drawCell Grass   = withAttr grassAttr cw 
drawCell Solid   = withAttr solidAttr cw 
drawCell Empty = withAttr emptyAttr cw

cw :: Widget ()
cw = str "  "

-- color for each item
-- bullets: yellow, normal: blue, grass: green, solid: grey, player1: purple, player2: red, empty: white
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (bulletsAttr, V.yellow `on` V.yellow)
  , (normalAttr, V.blue `on` V.blue)
  , (grassAttr, V.green `on` V.green)
  , (solidAttr, V.grey `on` V.grey)
  , (player1Attr, V.purple `on` V.purple)
  , (player2Attr, V.red `on` V.red)
  , (emptyAttr, V.white `on` V.white)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bulletsAttr, normalAttr, grassAttr, solidAttr, player1Attr, player2Attr, emptyAttr :: AttrName
bulletsAttr = "bulletsAttr"
normalAttr  = "normalAttr"
grassAttr   = "grassAttr"
solidAttr   = "solidAttr"
player1Attr = "player1Attr"
player2Attr = "player2Attr"
emptyAttr   = "emptyAttr"
