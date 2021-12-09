module Main where 

import System.Exit
import Test.QuickCheck

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  quickCheck prop_isAllDigit
  putStrLn "\nDone Testing"
  exitWith ExitSuccess 

-- test functions
prop_isAllDigit :: Int -> Bool
prop_isAllDigit val = val == val



-------------------------- moves --------------------------
-- TestMoves ::  Game -> Bool
-- TestMoves g = do
--   let x = 5
--   let y = 6
--   (CorrectMove (SetPlayerPos 1 [x,y] ) 1 North [x, y+1]) && ( CorrectMove (SetPlayerPos 1 [x,y] ) 1 South [x, y-1] ) && ( CorrectMove (SetPlayerPos 1 [x,y] ) 1 East [x-1, y] ) && ( CorrectMove (SetPlayerPos 1 [x,y] ) 1 West [x+1, y] ) && ( CorrectMove (SetPlayerPos 2 [x,y] ) 2 North [x, y+1] ) && ( CorrectMove (SetPlayerPos 2 [x,y] ) 2 South [x, y-1] ) && ( CorrectMove (SetPlayerPos 2 [x,y] ) 2 East [x-1, y] ) && ( CorrectMove (SetPlayerPos 2 [x,y] ) 2 West [x+1, y])
--   --                           startPos        Answer

-- SetPlayerPos :: Game -> Int -> [Int] -> Game
-- SetPlayerPos g player pos = do
--   let x = head pos
--   let y = last pos
--   if player == 1 then g & player1 %~ (\V2 _ _ -> V2 x y) else g & player2 %~ (\V2 _ _ -> V2 x y)

-- CorrectMove :: Game -> Int -> Direction -> [Int] -> Bool 
-- CorrectMove g player dirc ans = do
--   let answer = (V2 (head ans) (last ans))
--   let test = if player == 1 then (moves 1 dirc g) ^. player1  else (moves 2 dirc g) ^. player2
--   test == answer

