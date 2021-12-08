{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Maze(
  initGame
  , moves
  -- , turn1
  -- , turn2
  , Game(..)
  , Direction(..)
  , dead, paused, player1, player2, score1, score2, bullet, solid, normal, grass
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
  , _bullet  :: Coord        -- ^ list of bullets locations
  , _bullets :: Stream Coord -- ^ list of bullets locations
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

-- Functions
-- | Step forward in time
-- step :: Game -> Game
-- step s = flip execState s . runMaybeT $ do

--   -- Make sure the game isn't paused or over
--   MaybeT $ guard . not <$> orM [use paused, use dead]

--   -- win (one player wins)
--   win

-- win :: MaybeT (StateT Game Identity) ()
-- win = error "not implemented"

-- -- | players can get bullet if positions overlap
-- getBullet :: MaybeT (State Game) ()
-- getBullet = do
--   MaybeT . fmap guard $ (==) <$> (nextBullet <$> get) <*> use bullet
--   MaybeT . fmap Just $ do
--     modifying score1 (+ 10)
--     get >>= \g -> modifying player1 (nextBullet g <|)
--     nextBullet

-- -- | Set a valid next bullet coordinate
-- nextBullet :: State Game ()
-- nextBullet = do
--   (f :| fs) <- use bullets
--   bullets .= fs
--   use player1 >>= (\case -- all the list TBD !!!
--     True -> nextBullet
--     False -> bullet .= f) . elem f

-- -- | Move player1 & player2 around; if moving not possible, leave them where they are
-- turn1 :: Direction -> Game -> Game
-- turn1 d g@Game { _player1 = (s :|> _) } = g & player1 .~ nextPos1 d g <| s
-- turn1 _ _                               = error "Player1 can't be empty!"

-- nextPos1 :: Direction -> Game -> Coord
-- nextPos1 d g@Game { _player1 = (s :<| _) }
--   | d == North = s & _y %~ (\y -> (y + 1) `mod` height)
--   | d == South = s & _y %~ (\y -> (y - 1) `mod` height)
--   | d == East  = s & _x %~ (\x -> (x + 1) `mod` width)
--   | d == West  = s & _x %~ (\x -> (x - 1) `mod` width)
-- nextPos1 _ _      = error "Player1 can't be empty!"

-- turn2 :: Direction -> Game -> Game
-- turn2 d g@Game { _player2 = (s :|> _) } = g & player2 .~ nextPos2 d g <| s
-- turn2 _ _                               = error "Player2 can't be empty!"

-- nextPos2 :: Direction -> Game -> Coord
-- nextPos2 d g@Game { _player2 = (s :<| _) }
--   | d == North = s & _y %~ (\y -> (y + 1) `mod` height)
--   | d == South = s & _y %~ (\y -> (y - 1) `mod` height)
--   | d == East  = s & _x %~ (\x -> (x + 1) `mod` width)
--   | d == West  = s & _x %~ (\x -> (x - 1) `mod` width)
-- nextPos2 _ _      = error "Player2 can't be empty!"

moves  :: Direction -> Game -> Game
moves North g =
  if blockExists g North then
    g
  else
    g & player1 %~ (\(V2 a b) -> V2 a ((b+1) `mod` height))

moves South g = do
  if blockExists g South then
    g
  else
    g & player1 %~ (\(V2 a b) -> V2 a ((b-1) `mod` height))

moves West g = do
  if blockExists g West then
    g
  else
    g & player1 %~ (\(V2 a b) -> V2 ((a-1) `mod` width) b)

moves East g = do
  if blockExists g East then
    g
  else
    g & player1 %~ (\(V2 a b) -> V2 ((a+1) `mod` width) b)


blockExists :: Game -> Direction -> Bool
blockExists g North = do
  let (V2 xm ym) = g ^. player1
  (V2 xm ym+1) `elem` (g ^. normal)

blockExists g South = do
  let (V2 xm ym) = g ^. player1
  (V2 xm ym-1) `elem` (g ^. normal)

blockExists g East = do
  let (V2 xm ym) = g ^. player1
  V2 (xm+1) ym `elem` (g ^. normal)

blockExists g West = do
  let (V2 xm ym) = g ^. player1
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

initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 5
      ym = height `div` 5
      g  = Game{
          _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = V2 xm ym                     -- ^ coordinate of player 1
        , _player2 = V2 (xm+10) (ym+10)              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bullet   = f                            -- ^ coordinate of current bullet
        , _bullets  = fs                           -- ^ list of bullets locations
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
