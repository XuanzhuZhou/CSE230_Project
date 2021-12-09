{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Maze(
  initGame1
  , initGame2
  , moves
  , p1_kill
  , p2_kill
  , restart
  , seeRules
  , Game(..)
  , Direction(..)
  , dead, paused, player1, player2, score1, score2, bullets, bu_cnt1, bu_cnt2, rules, solid, normal, grass
  , height, width
) where

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.State
import Linear.V2 (V2(..), _x, _y)

-- Types

data Game = Game
  { _dead    :: Bool         -- ^ game over flag
  , _paused  :: Bool         -- ^ paused flag
  , _player1 :: Coord    -- ^ coordinate of player 1
  , _player2 :: Coord    -- ^ coordinate of player 2 
  , _score1  :: Int          -- ^ score of player 1
  , _score2  :: Int          -- ^ score of player 2
  , _bullets :: [Coord] -- ^ list of bullets locations
  , _bu_cnt1 :: Int          -- ^ bullets count of player 1
  , _bu_cnt2 :: Int          -- ^ bullets count of player 2
  , _rules   :: Int  
  , _solid   :: [Coord]      -- ^ list of solid blocks
  , _normal  :: [Coord]      -- ^ list of normal blocks
  , _grass   :: [Coord]      -- ^ list of grass blocks
  } deriving (Show, Eq)

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
  else if solid_blockExists x y g dirc then
    g
  else if bulletExists x y g dirc then
    if player == 1 then
      addScore player g & player1 %~ (\(V2 a b) -> V2 new_x new_y) & bullets %~ delete curr_bullet & bu_cnt1 %~ (\x -> x+1)
    else
      addScore player g & player2 %~ (\(V2 a b) -> V2 new_x new_y) & bullets %~ delete curr_bullet & bu_cnt2 %~ (\x -> x+1)
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

solid_blockExists :: Int -> Int -> Game -> Direction -> Bool
solid_blockExists xm ym g d = case d of
  North -> V2 (xm-1) ym+1 `elem` g ^. solid
  South -> V2 (xm+1) ym-1 `elem` g ^. solid
  West ->  V2 (xm-1) ym `elem` g ^. solid
  East ->  V2 (xm+1) ym `elem` g ^. solid

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

------------------------------------------------------------
----------------- kill (using bullets) ---------------------
-- if rival around, game over, else delet normal blocks
p1_kill :: Game -> Game
p1_kill g = do
  let (V2 x y) = g ^. player1
  let positions  = [[x, y+1], [x, y-1], [x+1, y], [x-1, y],
                    [x+1, y+1], [x+1, y-1], [x-1, y-1], [x-1, y+1]]
  -- foldl delNormal g positions
  if g ^. bu_cnt1 == 0 then g
  else
    if (gameIsOver g 1) then 
      g & dead %~ (\x -> True) & score1 %~ (\x -> x+1000) & bu_cnt1 %~ (\x -> x-1)
    else 
      if g ^. bu_cnt1 == 1 && g ^. bu_cnt2 == 0 && length (g ^. bullets) == 0 then
        foldl delNormal g positions & bu_cnt1 %~ (\x -> x-1) & dead %~ (\x -> True)
      else
        foldl delNormal g positions & bu_cnt1 %~ (\x -> x-1)

p2_kill :: Game -> Game
p2_kill g = do
  let (V2 x y) = g ^. player2
  let positions  = [[x, y+1], [x, y-1], [x+1, y], [x-1, y],
                    [x+1, y+1], [x+1, y-1], [x-1, y-1], [x-1, y+1]]
  -- foldl delNormal g positions
  if g ^. bu_cnt2 == 0 then g
  else
    if (gameIsOver g 2) then 
      g & dead %~ (\x -> True) & score2 %~ (\x -> x+1000) & bu_cnt2 %~ (\x -> x-1)
    else 
      if g ^. bu_cnt2 == 1 && g ^. bu_cnt1 == 0 && length (g ^. bullets) == 0 then
        foldl delNormal g positions & bu_cnt2 %~ (\x -> x-1) & dead %~ (\x -> True)
      else
        foldl delNormal g positions & bu_cnt2 %~ (\x -> x-1)

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

-- | Define some random blocks for initialization
-- INTERFACE 1--
initSolid1 :: [Coord]
-- initSolid1 = map (V2 0) [0..height] ++ map (V2 (width-1)) [0..height] ++ map (`V2` 0) [0..height] ++ map (`V2` (width-1)) [0..height]
initSolid1 =  [V2 0 39, V2 1 39, V2 2 39, V2 3 39, V2 4 39, V2 5 39, V2 6 39, V2 7 39, V2 8 39, V2 9 39, V2 10 39, V2 11 39, V2 12 39, V2 13 39, V2 14 39, V2 15 39, V2 16 39, V2 17 39, V2 18 39, V2 19 39, V2 20 39, V2 21 39, V2 22 39, V2 23 39, V2 24 39, V2 25 39, V2 26 39, V2 27 39, V2 28 39, V2 29 39, V2 30 39, V2 31 39, V2 32 39, V2 33 39, V2 34 39, V2 35 39, V2 36 39, V2 37 39, V2 38 39, V2 39 39, V2 0 38, V2 39 38, V2 0 37, V2 2 37, V2 3 37, V2 4 37, V2 5 37, V2 6 37, V2 7 37, V2 8 37, V2 9 37, V2 10 37, V2 11 37, V2 12 37, V2 13 37, V2 31 37, V2 32 37, V2 33 37, V2 34 37, V2 35 37, V2 36 37, V2 39 37, V2 0 36, V2 31 36, V2 32 36, V2 33 36, V2 34 36, V2 35 36, V2 36 36, V2 39 36, V2 0 35, V2 12 35, V2 13 35, V2 14 35, V2 15 35, V2 16 35, V2 17 35, V2 22 35, V2 23 35, V2 24 35, V2 25 35, V2 26 35, V2 27 35, V2 28 35, V2 29 35, V2 39 35, V2 0 34, V2 39 34, V2 0 33, V2 3 33, V2 4 33, V2 39 33, V2 0 32, V2 3 32, V2 4 32, V2 39 32, V2 0 31, V2 3 31, V2 4 31, V2 9 31, V2 10 31, V2 11 31, V2 12 31, V2 13 31, V2 14 31, V2 15 31, V2 16 31, V2 17 31, V2 18 31, V2 19 31, V2 20 31, V2 21 31, V2 39 31, V2 0 30, V2 3 30, V2 4 30, V2 39 30, V2 0 29, V2 3 29, V2 4 29, V2 28 29, V2 29 29, V2 30 29, V2 31 29, V2 34 29, V2 39 29, V2 0 28, V2 3 28, V2 4 28, V2 28 28, V2 29 28, V2 30 28, V2 31 28, V2 34 28, V2 39 28, V2 0 27, V2 3 27, V2 4 27, V2 39 27, V2 0 26, V2 16 26, V2 39 26, V2 0 25, V2 16 25, V2 39 25, V2 0 24, V2 16 24, V2 39 24, V2 0 23, V2 16 23, V2 21 23, V2 39 23, V2 0 22, V2 16 22, V2 21 22, V2 39 22, V2 0 21, V2 5 21, V2 16 21, V2 21 21, V2 39 21, V2 0 20, V2 5 20, V2 16 20, V2 21 20, V2 29 20, V2 39 20, V2 0 19, V2 3 19, V2 4 19, V2 5 19, V2 16 19, V2 21 19, V2 29 19, V2 39 19, V2 0 18, V2 16 18, V2 21 18, V2 39 18, V2 0 17, V2 16 17, V2 21 17, V2 29 17, V2 39 17, V2 0 16, V2 21 16, V2 29 16, V2 39 16, V2 0 15, V2 21 15, V2 29 15, V2 39 15, V2 0 14, V2 29 14, V2 39 14, V2 0 13, V2 39 13, V2 0 12, V2 39 12, V2 0 11, V2 39 11, V2 0 10, V2 39 10, V2 0 9, V2 7 9, V2 8 9, V2 9 9, V2 10 9, V2 11 9, V2 12 9, V2 13 9, V2 14 9, V2 15 9, V2 16 9, V2 17 9, V2 18 9, V2 19 9, V2 20 9, V2 39 9, V2 0 8, V2 39 8, V2 0 7, V2 39 7, V2 0 6, V2 31 6, V2 32 6, V2 33 6, V2 34 6, V2 35 6, V2 39 6, V2 0 5, V2 31 5, V2 32 5, V2 33 5, V2 34 5, V2 35 5, V2 39 5, V2 0 4, V2 39 4, V2 0 3, V2 39 3, V2 0 2, V2 39 2, V2 0 1, V2 25 1, V2 26 1, V2 27 1, V2 28 1, V2 39 1, V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 22 0, V2 23 0, V2 24 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0]
initNormal1 :: [Coord]
initNormal1 = [V2 2 36, V2 3 36, V2 4 36, V2 5 36, V2 6 36, V2 7 36, V2 8 36, V2 9 36, V2 10 36, V2 11 36, V2 12 34, V2 13 34, V2 14 34, V2 15 34, V2 16 34, V2 17 34, V2 22 34, V2 23 34, V2 24 34, V2 25 34, V2 26 34, V2 27 34, V2 28 34, V2 29 34, V2 10 28, V2 10 27, V2 11 27, V2 12 27, V2 13 27, V2 14 27, V2 15 27, V2 16 27, V2 28 27, V2 29 27, V2 30 27, V2 31 27, V2 34 27, V2 10 26, V2 15 26, V2 10 25, V2 15 25, V2 10 24, V2 15 24, V2 20 24, V2 21 24, V2 22 24, V2 35 24, V2 3 23, V2 4 23, V2 10 23, V2 15 23, V2 20 23, V2 22 23, V2 35 23, V2 3 22, V2 10 22, V2 15 22, V2 20 22, V2 22 22, V2 35 22, V2 3 21, V2 4 21, V2 10 21, V2 15 21, V2 20 21, V2 22 21, V2 35 21, V2 3 20, V2 4 20, V2 10 20, V2 15 20, V2 20 20, V2 22 20, V2 28 20, V2 35 20, V2 10 19, V2 11 19, V2 12 19, V2 13 19, V2 14 19, V2 15 19, V2 20 19, V2 22 19, V2 28 19, V2 35 19, V2 15 18, V2 20 18, V2 22 18, V2 28 18, V2 30 18, V2 31 18, V2 34 18, V2 35 18, V2 15 17, V2 20 17, V2 22 17, V2 28 17, V2 30 17, V2 31 17, V2 34 17, V2 35 17, V2 20 16, V2 22 16, V2 28 16, V2 35 16, V2 2 15, V2 3 15, V2 4 15, V2 20 15, V2 22 15, V2 28 15, V2 2 14, V2 4 14, V2 20 14, V2 22 14, V2 28 14, V2 2 13, V2 3 13, V2 4 13, V2 7 13, V2 8 13, V2 9 13, V2 10 13, V2 11 13, V2 12 13, V2 13 13, V2 14 13, V2 15 13, V2 16 13, V2 17 13, V2 18 13, V2 19 13, V2 20 13, V2 21 13, V2 22 13, V2 7 12, V2 20 12, V2 7 11, V2 20 11, V2 7 10, V2 20 10, V2 28 9, V2 29 9, V2 30 9, V2 28 8, V2 29 8, V2 30 8, V2 31 8, V2 32 8, V2 33 8, V2 34 8, V2 35 8, V2 36 8, V2 37 8, V2 38 8, V2 28 7, V2 28 6, V2 28 5, V2 29 5, V2 30 5, V2 4 4, V2 6 4, V2 9 4, V2 10 4, V2 11 4, V2 4 3, V2 5 3, V2 6 3, V2 9 3, V2 10 3, V2 11 3, V2 22 2, V2 23 2, V2 24 2, V2 29 2, V2 30 2, V2 31 2, V2 32 2, V2 33 2, V2 34 2, V2 22 1, V2 23 1, V2 24 1, V2 30 1, V2 31 1, V2 32 1, V2 33 1, V2 34 1]
initGrass1 :: [Coord]
initGrass1 = [V2 25 37, V2 26 37, V2 27 37, V2 28 37, V2 29 37, V2 30 37, V2 37 37, V2 38 37, V2 25 36, V2 26 36, V2 28 36, V2 29 36, V2 30 36, V2 37 36, V2 18 35, V2 19 35, V2 20 35, V2 21 35, V2 18 34, V2 19 34, V2 20 34, V2 21 34, V2 32 29, V2 33 29, V2 11 28, V2 12 28, V2 13 28, V2 14 28, V2 15 28, V2 16 28, V2 33 28, V2 32 27, V2 33 27, V2 3 26, V2 4 26, V2 11 26, V2 12 26, V2 13 26, V2 14 26, V2 3 25, V2 4 25, V2 11 25, V2 12 25, V2 13 25, V2 14 25, V2 3 24, V2 4 24, V2 11 24, V2 13 24, V2 14 24, V2 11 23, V2 12 23, V2 13 23, V2 14 23, V2 11 22, V2 12 22, V2 13 22, V2 14 22, V2 11 21, V2 12 21, V2 14 21, V2 11 20, V2 12 20, V2 13 20, V2 14 20, V2 32 18, V2 33 18, V2 32 17, V2 33 17, V2 8 12, V2 9 12, V2 10 12, V2 11 12, V2 12 12, V2 13 12, V2 14 12, V2 15 12, V2 16 12, V2 17 12, V2 18 12, V2 19 12, V2 21 12, V2 22 12, V2 21 11, V2 22 11, V2 21 10, V2 22 10, V2 29 7, V2 30 7, V2 16 6, V2 17 6, V2 18 6, V2 20 6, V2 21 6, V2 22 6, V2 23 6, V2 24 6, V2 25 6, V2 29 6, V2 30 6, V2 16 5, V2 17 5, V2 18 5, V2 19 5, V2 20 5, V2 21 5, V2 22 5, V2 23 5, V2 24 5, V2 25 5, V2 7 4, V2 8 4, V2 7 3, V2 8 3]
initBullets1 :: [Coord]
initBullets1 = [V2 27 36, V2 38 36, V2 36 32, V2 32 28, V2 24 27, V2 12 24, V2 24 23, V2 4 22, V2 13 21, V2 29 18, V2 3 14, V2 21 14, V2 37 12, V2 12 11, V2 18 11, V2 32 11, V2 3 9, V2 31 7, V2 19 6, V2 5 4, V2 32 4, V2 29 1]

initGame1 :: IO Game
initGame1 = do
  let xm = width `div` 5
      ym = height `div` 5
      g  = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bu_cnt1 = 0
        , _bu_cnt2 = 0
        , _rules   = 1
        , _bullets = initBullets1                   -- ^ list of bullets locations
        , _solid   = initSolid1                     -- ^ list of solid blocks
        , _normal  = initNormal1                    -- ^ list of normal blocks
        , _grass   = initGrass1                     -- ^ list of grass blocks
        }
  return $ execState initState g

-- INTERFACE 2--
initSolid2 :: [Coord]
-- initSolid2 = map (V2 0) [0..height] ++ map (V2 (width-1)) [0..height] ++ map (`V2` 0) [0..height] ++ map (`V2` (width-1)) [0..height]
initSolid2 =  [V2 0 39, V2 1 39, V2 2 39, V2 3 39, V2 4 39, V2 5 39, V2 6 39, V2 7 39, V2 8 39, V2 9 39, V2 10 39, V2 11 39, V2 12 39, V2 13 39, V2 14 39, V2 15 39, V2 16 39, V2 17 39, V2 18 39, V2 19 39, V2 20 39, V2 21 39, V2 22 39, V2 23 39, V2 24 39, V2 25 39, V2 26 39, V2 27 39, V2 28 39, V2 29 39, V2 30 39, V2 31 39, V2 32 39, V2 33 39, V2 34 39, V2 35 39, V2 36 39, V2 37 39, V2 38 39, V2 39 39, V2 0 38, V2 39 38, V2 0 37, V2 39 37, V2 0 36, V2 39 36, V2 0 35, V2 39 35, V2 0 34, V2 39 34, V2 0 33, V2 39 33, V2 0 32, V2 39 32, V2 0 31, V2 39 31, V2 0 30, V2 39 30, V2 0 29, V2 39 29, V2 0 28, V2 39 28, V2 0 27, V2 39 27, V2 0 26, V2 39 26, V2 0 25, V2 39 25, V2 0 24, V2 39 24, V2 0 23, V2 39 23, V2 0 22, V2 39 22, V2 0 21, V2 39 21, V2 0 20, V2 39 20, V2 0 19, V2 39 19, V2 0 18, V2 39 18, V2 0 17, V2 39 17, V2 0 16, V2 39 16, V2 0 15, V2 39 15, V2 0 14, V2 39 14, V2 0 13, V2 39 13, V2 0 12, V2 39 12, V2 0 11, V2 28 11, V2 29 11, V2 30 11, V2 31 11, V2 39 11, V2 0 10, V2 39 10, V2 0 9, V2 39 9, V2 0 8, V2 39 8, V2 0 7, V2 39 7, V2 0 6, V2 39 6, V2 0 5, V2 39 5, V2 0 4, V2 39 4, V2 0 3, V2 39 3, V2 0 2, V2 39 2, V2 0 1, V2 39 1, V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 22 0, V2 23 0, V2 24 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0]
initNormal2 :: [Coord]
initNormal2 = [V2 1 37, V2 2 37, V2 5 37, V2 6 37, V2 10 37, V2 11 37, V2 12 37, V2 13 37, V2 14 37, V2 18 37, V2 19 37, V2 20 37, V2 21 37, V2 22 37, V2 26 37, V2 27 37, V2 28 37, V2 29 37, V2 30 37, V2 33 37, V2 34 37, V2 1 36, V2 2 36, V2 5 36, V2 6 36, V2 14 36, V2 22 36, V2 26 36, V2 30 36, V2 33 36, V2 34 36, V2 1 35, V2 2 35, V2 5 35, V2 6 35, V2 14 35, V2 22 35, V2 26 35, V2 30 35, V2 33 35, V2 1 34, V2 2 34, V2 5 34, V2 6 34, V2 10 34, V2 11 34, V2 12 34, V2 13 34, V2 14 34, V2 18 34, V2 19 34, V2 20 34, V2 21 34, V2 22 34, V2 26 34, V2 30 34, V2 33 34, V2 34 34, V2 1 33, V2 2 33, V2 6 33, V2 10 33, V2 22 33, V2 26 33, V2 30 33, V2 33 33, V2 34 33, V2 1 32, V2 2 32, V2 5 32, V2 6 32, V2 10 32, V2 22 32, V2 26 32, V2 30 32, V2 33 32, V2 34 32, V2 1 31, V2 2 31, V2 5 31, V2 6 31, V2 10 31, V2 22 31, V2 26 31, V2 30 31, V2 33 31, V2 35 31, V2 36 31, V2 37 31, V2 38 31, V2 1 30, V2 2 30, V2 5 30, V2 6 30, V2 10 30, V2 11 30, V2 12 30, V2 13 30, V2 14 30, V2 18 30, V2 19 30, V2 20 30, V2 21 30, V2 22 30, V2 26 30, V2 27 30, V2 28 30, V2 29 30, V2 30 30, V2 33 30, V2 34 30, V2 35 30, V2 36 30, V2 37 30, V2 38 30, V2 7 28, V2 30 28, V2 31 28, V2 6 27, V2 8 27, V2 30 27, V2 31 27, V2 5 26, V2 6 26, V2 8 26, V2 9 26, V2 30 26, V2 31 26, V2 5 25, V2 6 25, V2 8 25, V2 9 25, V2 30 25, V2 31 25, V2 4 24, V2 9 24, V2 10 24, V2 30 24, V2 31 24, V2 4 23, V2 9 23, V2 10 23, V2 30 23, V2 31 23, V2 4 22, V2 5 22, V2 6 22, V2 7 22, V2 8 22, V2 9 22, V2 10 22, V2 30 22, V2 3 21, V2 4 21, V2 10 21, V2 11 21, V2 30 21, V2 31 21, V2 3 20, V2 4 20, V2 10 20, V2 11 20, V2 31 20, V2 32 20, V2 33 20, V2 34 20, V2 35 20, V2 3 19, V2 4 19, V2 10 19, V2 11 19, V2 30 19, V2 31 19, V2 32 19, V2 11 17, V2 12 17, V2 13 17, V2 14 17, V2 26 17, V2 27 17, V2 28 17, V2 29 17, V2 30 17, V2 31 17, V2 10 16, V2 11 16, V2 12 16, V2 13 16, V2 14 16, V2 26 16, V2 27 16, V2 28 16, V2 29 16, V2 30 16, V2 31 16, V2 9 15, V2 10 15, V2 26 15, V2 9 14, V2 10 14, V2 26 14, V2 27 14, V2 9 13, V2 26 13, V2 27 13, V2 28 13, V2 29 13, V2 30 13, V2 31 13, V2 9 12, V2 10 12, V2 11 12, V2 12 12, V2 13 12, V2 14 12, V2 15 12, V2 26 12, V2 27 12, V2 28 12, V2 30 12, V2 31 12, V2 14 11, V2 15 11, V2 26 11, V2 27 11, V2 14 10, V2 15 10, V2 18 10, V2 19 10, V2 22 10, V2 23 10, V2 26 10, V2 27 10, V2 14 9, V2 15 9, V2 18 9, V2 19 9, V2 21 9, V2 22 9, V2 26 9, V2 27 9, V2 11 8, V2 12 8, V2 13 8, V2 15 8, V2 18 8, V2 19 8, V2 21 8, V2 26 8, V2 28 8, V2 29 8, V2 30 8, V2 31 8, V2 10 7, V2 11 7, V2 12 7, V2 13 7, V2 14 7, V2 18 7, V2 19 7, V2 20 7, V2 26 7, V2 27 7, V2 28 7, V2 29 7, V2 30 7, V2 31 7, V2 18 6, V2 19 6, V2 20 6, V2 21 6, V2 18 5, V2 19 5, V2 21 5, V2 22 5, V2 18 4, V2 19 4, V2 22 4, V2 23 4, V2 18 3, V2 19 3, V2 23 3, V2 24 3, V2 18 2, V2 19 2, V2 24 2]
initGrass2 :: [Coord]
initGrass2 = [V2 27 35, V2 3 34, V2 4 34, V2 27 34, V2 11 33, V2 12 33, V2 13 33, V2 14 33, V2 27 33, V2 27 32, V2 6 24, V2 7 24, V2 8 24, V2 6 23, V2 7 23, V2 8 23, V2 32 21, V2 33 21, V2 34 21, V2 35 21, V2 28 15, V2 29 15, V2 30 15, V2 31 15, V2 28 14, V2 29 14, V2 30 14, V2 31 14, V2 12 11, V2 13 11, V2 12 10, V2 24 10, V2 12 9, V2 13 9]
initBullets2 :: [Coord]
initBullets2 = [V2 3 35, V2 12 35, V2 34 35, V2 36 35, V2 5 33, V2 29 33, V2 20 31, V2 27 31, V2 34 31, V2 16 26, V2 22 26, V2 32 26, V2 7 25, V2 17 22, V2 21 22, V2 31 22, V2 9 21, V2 18 21, V2 19 21, V2 20 21, V2 30 20, V2 27 15, V2 11 14, V2 10 13, V2 29 12, V2 23 11, V2 13 10, V2 20 9, V2 31 9, V2 27 8, V2 21 4]

initGame2 :: IO Game
initGame2 = do
  let xm = width `div` 5
      ym = height `div` 5
      g  = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bu_cnt1 = 0
        , _bu_cnt2 = 0
        , _rules   = 1
        , _bullets = initBullets2                   -- ^ list of bullets locations
        , _solid   = initSolid2                     -- ^ list of solid blocks
        , _normal  = initNormal2                    -- ^ list of normal blocks
        , _grass   = initGrass2                     -- ^ list of grass blocks
        }
  return $ execState initState g

initState :: State Game ()
initState = do
  s <- get
  put s

restart :: Game -> Int -> Game
restart g mapid = do 
  let xm = width `div` 5
      ym = height `div` 5
      g2  = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bu_cnt1 = 0
        , _bu_cnt2 = 0
        , _rules   = 1
        , _bullets = initBullets2                   -- ^ list of bullets locations
        , _solid   = initSolid2                     -- ^ list of solid blocks
        , _normal  = initNormal2                    -- ^ list of normal blocks
        , _grass   = initGrass2                     -- ^ list of grass blocks
        }
      g1 = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bu_cnt1 = 0
        , _bu_cnt2 = 0
        , _rules   = 1
        , _bullets = initBullets1                   -- ^ list of bullets locations
        , _solid   = initSolid1                     -- ^ list of solid blocks
        , _normal  = initNormal1                    -- ^ list of normal blocks
        , _grass   = initGrass1                     -- ^ list of grass blocks
        }
  if mapid == 1 then g1 else g2


seeRules :: Game -> Int -> Game
seeRules g n = g & rules %~ (\x -> n)