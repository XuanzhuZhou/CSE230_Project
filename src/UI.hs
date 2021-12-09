{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

import Maze

import Brick 
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vBox, hBox
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
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources

data Cell = Player1 | Player2 | Bullets | Solid | Normal | Grass | Empty

-- App definition
type Name = ()
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO () -- 程序的入口
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame2
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
-- handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar '.') [])) = continue $ p1_kill g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'c') [])) = continue $ p2_kill g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moves 1 North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moves 1 South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moves 1 East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moves 1 West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ moves 2 North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ moves 2 South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ moves 2 East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ moves 2 West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats2 g) <+> drawGrid g <+> padLeft (Pad 2) (drawStats1 g)]

drawStats1 :: Game -> Widget Name
drawStats1 g = hLimit 16
  $ vBox [ C.hCenter $ str "\n   PLAYER 1 \nUse ↑↓←→ to move\nUse . to kill\n\n"
         , drawScore (g ^. score1) " Score "
         , drawScore (g ^. bu_cnt1) " Bullet "
         , padTop (Pad 2) $ drawGameOver g
         ]

drawStats2 :: Game -> Widget Name
drawStats2 g = hLimit 16
  $ vBox [ C.hCenter $ str "\n   PLAYER 2 \nUse WASD to move\nUse C to kill\n\n"
         , drawScore (g ^. score2) " Socre "
         , drawScore (g ^. bu_cnt2) " Bullet "
         , padTop (Pad 2) $ drawGameOver g
         ]

drawScore :: Int -> String -> Widget Name
drawScore n s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str s)
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if g ^. dead then
    if g ^. score1 == g ^. score2 then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER\n\n Tie"
    else if g ^. score1 > g ^. score2 then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER\n\nWinner:\nPLAYER 1"
    else withAttr gameOverAttr $ C.hCenter $ str "GAME OVER\n\nWinner:\nPLAYER 2"
  else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Maze")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == (g ^. player1) = Player1
      | c == (g ^. player2) = Player2
      | c `elem` g ^. bullets   = Bullets
      | c `elem` g ^. solid   = Solid
      | c `elem` g ^. normal  = Normal
      | c `elem` g ^. grass   = Grass
      | otherwise             = Empty

drawCell :: Cell -> Widget Name
drawCell Player1 = withAttr player1Attr cw
drawCell Player2 = withAttr player2Attr cw
drawCell Bullets = withAttr bulletAttr cw
drawCell Normal  = withAttr normalAttr cw
drawCell Grass   = withAttr grassAttr cw
drawCell Solid   = withAttr solidAttr cw
drawCell Empty   = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

-- color for each item
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (bulletAttr, V.yellow `on` V.yellow)
  , (normalAttr, V.cyan `on` V.cyan)
  , (grassAttr, V.green `on` V.green)
  , (solidAttr, V.brightBlack `on` V.brightBlack)
  , (player1Attr, V.brightMagenta `on` V.brightMagenta)
  , (player2Attr, V.red `on` V.red)
  , (emptyAttr, V.white `on` V.white)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bulletAttr, normalAttr, grassAttr, solidAttr, player1Attr, player2Attr, emptyAttr :: AttrName
bulletAttr = "bulletAttr"
normalAttr  = "normalAttr"
grassAttr   = "grassAttr"
solidAttr   = "solidAttr"
player1Attr = "player1Attr"
player2Attr = "player2Attr"
emptyAttr   = "emptyAttr"
