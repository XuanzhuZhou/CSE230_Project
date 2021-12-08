{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Maze(
  initGame
  , moves
  , p1_kill
  , p2_kill
  , Game(..)
  , Direction(..)
  , dead, paused, player1, player2, score1, score2, bullets, solid, normal, grass
  , height, width
) where
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game
  { _dead    :: Bool         -- ^ game over flag
  , _paused  :: Bool         -- ^ paused flag
  , _player1 :: Coord    -- ^ coordinate of player 1
  , _player2 :: Coord    -- ^ coordinate of player 2 
  , _score1  :: Int          -- ^ score of player 1
  , _score2  :: Int          -- ^ score of player 2
  , _bullets :: [Coord] -- ^ list of bullets locations
  , _solid   :: [Coord]      -- ^ list of solid blocks
  , _normal  :: [Coord]      -- ^ list of normal blocks
  , _grass   :: [Coord]      -- ^ list of grass blocks
  } deriving (Show)

type Coord = V2 Int

data Stream a = a :| Stream a
  deriving (Eq,Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 40
width = 40

-- -- Functions

-- -- | Step forward in time
-- step :: Game -> Game
-- step s = flip execState s . runMaybeT $ do

moves  :: Int -> Direction -> Game -> Game
moves player dirc g = do
  let (V2 x y) = if player == 1 then g ^. player1 else g ^. player2
  let new_x
        | dirc == North || dirc == South = x
        | dirc == West = x-1
        | otherwise = x+1
  let new_y
        | dirc == West || dirc == East = y
        | dirc == North = y+1
        | otherwise = y-1
  let curr_bullet = V2 new_x new_y
  if blockExists x y g dirc then
    g
  else if bulletExists x y g dirc then
    if player == 1 then
      addScore player g & player1 %~ (\(V2 a b) -> V2 new_x new_y) & bullets %~ delete curr_bullet
    else
      addScore player g & player2 %~ (\(V2 a b) -> V2 new_x new_y) & bullets %~ delete curr_bullet
  else
    if player == 1 then
      g & player1 %~ (\(V2 a b) -> V2 new_x new_y)
    else
      g & player2 %~ (\(V2 a b) -> V2 new_x new_y)

blockExists :: Int -> Int -> Game -> Direction -> Bool
blockExists xm ym g d = case d of
  North -> V2 (xm-1) ym+1 `elem` g ^. normal
  South -> V2 (xm+1) ym-1 `elem` g ^. normal
  West ->  V2 (xm-1) ym `elem` g ^. normal
  East ->  V2 (xm+1) ym `elem` g ^. normal

addScore  :: Int -> Game -> Game
addScore num g =
  if num == 1 then
    g & score1 %~ (+ 10)
  else g & score2 %~ (+ 10)

delete :: Eq a => a -> [a] -> [a]
delete deleted list = [x | x <- list, x /= deleted]

bulletExists :: Int -> Int -> Game -> Direction -> Bool
bulletExists xm ym g d = case d of
  North -> V2 (xm-1) ym+1 `elem` g ^. bullets
  South -> V2 (xm+1) ym-1 `elem` g ^. bullets
  West ->  V2 (xm-1) ym `elem` g ^. bullets
  East ->  V2 (xm+1) ym `elem` g ^. bullets

-- | Define some random blocks for initialization
-- randShape1 x y = [V2 x y, V2 x y+1, V2 x y+2, V2 (x+1) y, V2 (x+2) y]
-- randShape2 x y = [V2 x y, V2 (x+1) y, V2 (x+2) y]
-- randShape3 x y = [V2 x y, V2 x y+1, V2 x y+2, V2 x y+3]
-- randShape4 x y = [V2 x y, V2 (x+1) y]
-- randShape5 x y = [V2 x y, V2 (x+1) y, V2 (x+2) y, V2 (x+2) y-1, V2 (x+2) y-2, V2 (x+2) y-3]
-- randShape6 x y = [V2 x y]
-- randSolid :: [Coord]
-- randSolid x y = randShape1 x y ++ randShape2 x y ++ randShape3 x y ++ randShape4 x y ++ randShape5 x y ++ randShape6 x y
initSolid :: [Coord]
initSolid = map (V2 0) [0..height] ++ map (V2 (width-1)) [0..height] ++ map (`V2` 0) [0..height] ++ map (`V2` (width-1)) [0..height]
initNormal :: [Coord]
initNormal = [V2 10 10, V2 10 11, V2 10 12, V2 11 10, V2 11 11, V2 11 12]
initGrass :: [Coord]
initGrass = [V2 20 21, V2 20 22, V2 20 23, V2 27 30, V2 28 30, V2 29 30]
initBullets :: [Coord]
initBullets = [V2 30 15, V2 28 25, V2 10 19, V2 5 30]

initGame :: IO Game
initGame = do
  let xm = width `div` 5
      ym = height `div` 5
      g  = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bullets = initBullets                   -- ^ list of bullets locations
        , _solid   = initSolid                     -- ^ list of solid blocks
        , _normal  = initNormal                    -- ^ list of normal blocks
        , _grass   = initGrass                     -- ^ list of grass blocks
        }
  return $ execState initState g

initState :: State Game ()
initState = do
  s <- get
  put s

------------------------------------------------------------
----------------- kill (using bullets) ---------------------
-- if rival around, game over, else delet normal blocks
p1_kill :: Game -> Game
p1_kill g = do
  let (V2 x y) = g ^. player1
  let positions  = [[x, y+1], [x, y-1], [x+1, y], [x-1, y],
                    [x+1, y+1], [x+1, y-1], [x-1, y-1], [x-1, y+1]]
  -- foldl delNormal g positions
  if (gameIsOver g 1) then g & dead %~ (\x -> True) else foldl delNormal g positions

p2_kill :: Game -> Game
p2_kill g = do
  let (V2 x y) = g ^. player2
  let positions  = [[x, y+1], [x, y-1], [x+1, y], [x-1, y],
                    [x+1, y+1], [x+1, y-1], [x-1, y-1], [x-1, y+1]]
  -- foldl delNormal g positions
  if (gameIsOver g 2) then g & dead %~ (\x -> True) else foldl delNormal g positions

---------- check game over after each kill ----------
-- check if game is over 
gameIsOver :: Game -> Int -> Bool
gameIsOver g player = do
  let (V2 x y) = if player == 1 then g ^. player1 else g ^. player2
  let (V2 rival_x rival_y) = if player == 1 then g ^. player2 else g ^. player1
  let positions  = [V2 x (y+1), V2 x (y-1), V2 (x+1) y, V2 (x-1) y,
                    V2 (x+1) (y+1), V2 (x+1) (y-1), V2 (x-1) (y-1), V2 (x-1) (y+1)]
  (V2 rival_x rival_y) `elem` positions

---------- kill normal blocks around ------------
-- delete a normal block (x, y) if it exists
delNormal :: Game -> [Int] -> Game
delNormal g pos = do
  let x = head pos
  let y = last pos
  if normalExists g x y then 
    g & normal %~ (\list -> (delete  (V2 x y) list)) 
  else 
    g

-- check if a normal block (x, y) exist
normalExists :: Game -> Int -> Int -> Bool
normalExists g x y = do
  (V2 x y) `elem` (g ^. normal)

-------------------- "kill" part ends -----------------------
-------------------------------------------------------------