{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , Game(..)
  , dead, player1, player2, score1, score2, bullet, solid, normal, grass
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
  , _player1 :: Seq Coord    -- ^ coordinate of player 1
  , _player2 :: Seq Coord    -- ^ coordinate of player 2 
  , _score1  :: Int          -- ^ score of player 1
  , _score2  :: Int          -- ^ score of player 2
  , _bullet  :: Coord        -- ^ list of bullets locations
  , _bullets :: Stream Coord -- ^ list of bullets locations
  , _solid   :: [Coord]      -- ^ list of solid blocks
  , _normal  :: [Coord]      -- ^ list of normal blocks
  , _grass   :: [Coord]      -- ^ list of grass blocks
  } deriving (Show)

type Coord = V2 Int

-- type Player = Seq Coord

data Stream a = a :| Stream a
  deriving (Eq,Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 20

-- -- Functions

-- -- | Step forward in time
-- step :: Game -> Game
-- step s = flip execState s . runMaybeT $ do

--   -- Make sure the game isn't paused or over
--   MaybeT $ guard . not <$> orM [use paused, use dead]

--   -- Unlock from last directional turn
--   MaybeT . fmap Just $ locked .= False

--   -- die (moved into boundary), eat (moved into food), or move (move into space)
--   die <|> eatFood <|> MaybeT (Just <$> modify move)

-- -- | Possibly die if next head position is in snake
-- die :: MaybeT (State Game) ()
-- die = do
--   MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use snake)
--   MaybeT . fmap Just $ dead .= True

-- -- | Possibly eat food if next head position is food
-- eatFood :: MaybeT (State Game) ()
-- eatFood = do
--   MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use food)
--   MaybeT . fmap Just $ do
--     modifying score (+ 10)
--     get >>= \g -> modifying snake (nextHead g <|)
--     nextBullet

-- -- | Set a valid next food coordinate
nextBullet :: State Game ()
nextBullet = do
  (f :| fs) <- use bullets
  bullets .= fs
  elem f <$> use player1 >>= \case
    True -> nextBullet
    False -> bullet .= f

-- -- | Move snake along in a marquee fashion
-- move :: Game -> Game
-- move g@Game { _snake = (s :|> _) } = g & snake .~ (nextHead g <| s)
-- move _                             = error "Snakes can't be empty!"

-- -- | Get next head position of the snake
-- nextHead :: Game -> Coord
-- nextHead Game { _dir = d, _snake = (a :<| _) }
--   | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
--   | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
--   | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
--   | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
-- nextHead _ = error "Snakes can't be empty!"

-- -- | Turn game direction (only turns orthogonally)
-- --
-- -- Implicitly unpauses yet locks game
-- turn :: Direction -> Game -> Game
-- turn d g = if g ^. locked
--   then g
--   else g & dir %~ turnDir d & paused .~ False & locked .~ True

-- turnDir :: Direction -> Direction -> Direction
-- turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
--             | c `elem` [East, West] && n `elem` [North, South] = n
--             | otherwise = c

-- | Initialize a paused game with random location
initSolid :: [Coord]
initSolid = map (V2 0) [0..height] ++ map (V2 (width-1)) [0..height]
initNormal :: [Coord]
initNormal = [(V2 2 1), (V2 2 2), (V2 2 3)]
initGrass :: [Coord]
initGrass = [(V2 3 1), (V2 3 2), (V2 3 3)]

initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        -- { _snake  = (S.singleton (V2 xm ym))
        -- , _food   = f
        -- , _foods  = fs
        -- , _score  = 0
        -- , _dir    = North
        -- , _dead   = False
        -- , _paused = True
        -- , _locked = False
        -- }
        { _dead    = False                         -- ^ game over flag
        , _paused  = False                         -- ^ paused flag
        , _player1 = (S.singleton (V2 xm ym))                     -- ^ coordinate of player 1
        , _player2 = (S.singleton (V2 (xm+10) (ym+10)))              -- ^ coordinate of player 2 
        , _score1  =  0                            -- ^ score of player 1
        , _score2  =  0                            -- ^ score of player 2  
        , _bullet   = f                            -- ^ coordinate of current bullet
        , _bullets  = fs                           -- ^ list of bullets locations
        , _solid   = initSolid                     -- ^ list of solid blocks
        , _normal  = initNormal                    -- ^ list of normal blocks
        , _grass   = initGrass                     -- ^ list of grass blocks
        }
  return $ execState nextBullet g
  -- return $ execState nextBullet g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
