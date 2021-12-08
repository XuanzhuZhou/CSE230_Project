{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- module Maze
--   ( initGame
--   , step
--   , turn
--   , Game(..)
--   , Direction(..)
--   , dead, food, score, snake
--   , height, width
--   ) where
module Maze(
  initGame
  , moves
  , p1_kill
  , p2_kill
  -- , turn1
  , turn1
  , turn2
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
import Control.Monad.Error (MonadError(throwError))

-- Types

data Game = Game
  { _dead    :: Bool         -- ^ game over flag
  , _paused  :: Bool         -- ^ paused flag
  , _player1 :: Coord    -- ^ coordinate of player 1
  , _player2 :: Coord    -- ^ coordinate of player 2 
  , _score1  :: Int          -- ^ score of player 1
  , _score2  :: Int          -- ^ score of player 2
  -- , _bullet  :: Coord        -- ^ list of bullets locations
  , _bullets :: [Coord] -- ^ list of bullets locations
  , _solid   :: [Coord]      -- ^ list of solid blocks
  , _normal  :: [Coord]      -- ^ list of normal blocks
  , _grass   :: [Coord]      -- ^ list of grass blocks
  } deriving (Show)

type Coord = V2 Int

type Player = Seq Coord

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


-- moveCoord :: Int -> Direction-> Game -> Game
-- moveCoord num d g
--   | num==1 =
--   if d == North then g & player1 %~ (\(V2 a b) -> V2 a ((b+1) `mod` height))
--   else if d == South then g & player1 %~ (\(V2 a b) -> V2 a ((b-1) `mod` height))
--   else if d == West then g & player1 %~ (\(V2 a b) -> V2 ((a-1) `mod` width) b)
--   else g & player1 %~ (\(V2 a b) -> V2 ((a+1) `mod` width) b)
--   | d == North = g & player2 %~ (\(V2 a b) -> V2 a ((b+1) `mod` height))
--   | d == South = g & player2 %~ (\(V2 a b) -> V2 a ((b-1) `mod` height))
--   | d == West = g & player2 %~ (\(V2 a b) -> V2 ((a-1) `mod` width) b)
--   | otherwise = g & player2 %~ (\(V2 a b) -> V2 ((a+1) `mod` width) b)

-- moves  :: Int -> Direction -> Game -> Game
-- moves num d g = do
--   let (V2 x y) = if num == 1 then g ^. player1 else g ^. player2
--   let bullet_x bullet_y
--         | d == North = x y+1
--         | d == South = x y-1
--         | d == West = (x-1) y
--         | otherwise = (x+1) y
--   if blockExists x y g d then
--     g
--   else
--     if bulletExists x y g d then
--       moveCoord num d (addScore bullet_x bullet_y num d g)
--     else moveCoord num d

-- moves num South g = do
--   let (V2 x y) = if num == 1 then g ^. player1 else g ^. player2
--   let curr_bullet = V2 x (y-1)
--   if blockExists x y g South then
--     g
--   else
--     if bulletExists g South then
--       addScore num South g & player1 %~ (\(V2 a b) -> V2 a ((b-1) `mod` height)) & bullets %~ delete curr_bullet
--     else g & player1 %~ (\(V2 a b) -> V2 a ((b-1) `mod` height))

-- moves num West g = do
--   let (V2 x y) = if num == 1 then g ^. player1 else g ^. player2
--   let curr_bullet = V2 (x-1) y
--   if blockExists x y g West then
--     g
--   else
--     if bulletExists g West then
--       addScore num West g & player1 %~ (\(V2 a b) -> V2 ((a-1) `mod` width) b) & bullets %~ delete curr_bullet
--     else g & player1 %~ (\(V2 a b) -> V2 ((a-1) `mod` width) b)

-- moves num East g = do
--   let (V2 x y) = if num == 1 then g ^. player1 else g ^. player2
--   let curr_bullet = V2 (x+1) y
--   if blockExists x y g East then
--     g
  -- else
    -- if bulletExists g East then
    --   addScore num East g & player1 %~ (\(V2 a b) -> V2 ((a+1) `mod` width) b) & bullets %~ delete curr_bullet
    -- else g & player1 %~ (\(V2 a b) -> V2 ((a+1) `mod` width) b)



blockExists :: Int -> Int -> Game -> Direction -> Bool
blockExists xm ym g d = case d of
  North -> V2 (xm-1) ym+1 `elem` g ^. normal -- !!!
  South -> V2 (xm+1) ym-1 `elem` g ^. normal -- !!!
  West ->  V2 (xm-1) ym `elem` g ^. normal -- !!!
  East ->  V2 (xm+1) ym `elem` g ^. normal -- !!!
--   V2 (xm-1) ym+1 `elem` g ^. normal

-- blockExists xm ym g South =
--   V2 (xm+1) ym-1 `elem` g ^. normal

-- blockExists xm ym g East =
--   V2 (xm+1) ym `elem` g ^. normal

-- blockExists xm ym g West =
--   V2 (xm-1) ym `elem` g ^. normal

addScore  :: Int -> Game -> Game
addScore num g =
  if num == 1 then
    g & score1 %~ (+ 10)
  else g & score2 %~ (+ 10)

delete :: Eq a => a -> [a] -> [a]
delete deleted list = [x | x <- list, x /= deleted]

bulletExists :: Int -> Int -> Game -> Direction -> Bool
bulletExists xm ym g d = case d of
  North -> V2 (xm-1) ym+1 `elem` g ^. bullets -- !!!
  South -> V2 (xm+1) ym-1 `elem` g ^. bullets -- !!!
  West ->  V2 (xm-1) ym `elem` g ^. bullets -- !!!
  East ->  V2 (xm+1) ym `elem` g ^. bullets -- !!!

-- bulletExists xm ym g North =
--   V2 (xm-1) ym+1 `elem` g ^. bullets -- !!!

-- bulletExists g South =
--   V2 (xm+1) ym-1 `elem` g ^. bullets -- !!!

-- bulletExists g West =
--   V2 (xm-1) ym `elem` g ^. bullets -- !!!

-- bulletExists g East =
--   V2 (xm+1) ym `elem` g ^. bullets -- !!!
-- | Initialize a paused game with random location

turn2 South g = do
  if blockExists2 g South then
    g
  else
    g & player2 %~ (\(V2 a b) -> V2 a ((b-1) `mod` height))

turn2 West g = do
  if blockExists2 g West then
    g
  else
    g & player2 %~ (\(V2 a b) -> V2 ((a-1) `mod` width) b)
  
turn2 East g = do
  if blockExists2 g East then
    g
  else
    g & player2 %~ (\(V2 a b) -> V2 ((a+1) `mod` width) b)

turn2 _ _   = error "Player1 can't be empty!"

blockExists2 :: Game -> Direction -> Bool
blockExists2 g North = do
  let (V2 xm ym) = g ^. player2
  (V2 xm ym+1) `elem` (g ^. normal)

blockExists2 g South = do
  let (V2 xm ym) = g ^. player2
  (V2 xm ym-1) `elem` (g ^. normal)

blockExists2 g East = do
  let (V2 xm ym) = g ^. player2
  V2 (xm+1) ym `elem` (g ^. normal)

blockExists2 g West = do
  let (V2 xm ym) = g ^. player2
  V2 (xm-1) ym `elem` (g ^. normal)

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
  -- (f :| fs) <-
  --   fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 5
      ym = height `div` 5
      g  = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        -- , _bullet   = f                            -- ^ coordinate of current bullet
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

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

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