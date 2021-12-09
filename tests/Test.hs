module Main where 

import Test.Tasty
import Prelude
import System.Exit
import Test.QuickCheck
import Constants as C
import Common
import Maze as M
import UI hiding(main)

main :: IO()
main = runTests 
  [  testMove
   , testGetBullet
   , testDestroyBlock
   , testGameOverCondition
  ]

scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String, Score) -> TestTree
scoreTest (f, x, r, n, msg, sc) = scoreTest' sc (return . f, x, r, n, msg)

testMove ::  Score -> TestTree
testMove sc = testGroup "Player move around" [
    scoreTest ((\_ -> M.moves 1 North C.testMoveBefore), (), C.testMoveBefore, 1, "player move north", sc)
    , scoreTest ((\_ -> M.moves 1 South C.testMoveBefore), (), C.testMoveBefore, 1, "player move south", sc)
    , scoreTest ((\_ -> M.moves 1 East C.testMoveBefore), (), C.testMoveAfterEast, 1, "player move east", sc)
    , scoreTest ((\_ -> M.moves 1 West C.testMoveBefore), (), C.testMoveAfterWest, 1, "player move west", sc)
    ]

testGetBullet ::  Score -> TestTree
testGetBullet sc = testGroup "Player get bullet" [
    scoreTest ((\_ -> M.moves 1 North C.testGetBulletBefore), (), C.testGetBulletBefore, 1, "player get bullet north", sc)
    , scoreTest ((\_ -> M.moves 1 South C.testGetBulletBefore), (), C.testGetBulletBefore, 1, "player get bullet south", sc)
    , scoreTest ((\_ -> M.moves 1 East C.testGetBulletBefore), (), C.testGetBulletAfterEast, 1, "player get bullet east", sc)
    , scoreTest ((\_ -> M.moves 1 West C.testGetBulletBefore), (), C.testGetBulletAfterWest, 1, "player get bullet west", sc)
    ]

testDestroyBlock ::  Score -> TestTree
testDestroyBlock sc = testGroup "Player destroy normal blocks" [
      scoreTest ((\_ -> M.p1_kill C.testDestroyNormalBlocksBefore), (), C.testDestroyNormalBlocksAfter, 1, "player destroy no normal blocks", sc)
    , scoreTest ((\_ -> M.p1_kill C.testDestroyNormalBlocksNorthBefore), (), C.testDestroyNormalBlocksNorthAfter, 1, "player destroy north normal blocks", sc)
    , scoreTest ((\_ -> M.p1_kill C.testDestroyNormalBlocksMoreBefore), (), C.testDestroyNormalBlocksMoreAfter, 1, "player destroy more normal blocks", sc)
    ]

testGameOverCondition ::  Score -> TestTree
testGameOverCondition sc = testGroup "Game over condition" [
      scoreTest ((\_ -> M.p1_kill C.testGameOverConditionBefore), (), C.testGameOverConditionAfterPlayer1, 1, "player1 wins", sc)
    , scoreTest ((\_ -> M.p2_kill C.testGameOverConditionBefore), (), C.testGameOverConditionAfterPlayer2, 1, "player2 wins", sc)
    ]
