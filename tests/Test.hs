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