module Constants where

import Maze
import UI hiding (main)
import qualified Data.Map as Map
import Linear.V2 (V2(..), _x, _y)

-- test move --
testMoveBefore :: Game
testMoveBefore = Game{
    _dead    = False                        
  , _paused  = False                        
  , _player1 = V2 10 10                      
  , _player2 = V2 20 20                    
  , _score1  =  0                            
  , _score2  =  0                           
  , _bu_cnt1 = 0
  , _bu_cnt2 = 0
  , _bullets = []                           
  , _solid   = []           
  , _normal  = [V2 10 11, V2 10 9]                           
  , _grass   = []                           
  }
testMoveAfterEast :: Game
testMoveAfterEast = Game{
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 11 10                      
, _player2 = V2 20 20                    
, _score1  =  0                            
, _score2  =  0                           
, _bu_cnt1 = 0
, _bu_cnt2 = 0
, _bullets = []                           
, _solid  = []           
, _normal   = [V2 10 11, V2 10 9]                         
, _grass   = []                           
}
testMoveAfterWest :: Game
testMoveAfterWest = Game{
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 9 10                      
, _player2 = V2 20 20                    
, _score1  =  0                            
, _score2  =  0                           
, _bu_cnt1 = 0
, _bu_cnt2 = 0
, _bullets = []                           
, _solid   = []        
, _normal  = [V2 10 11, V2 10 9]                             
, _grass   = []                           
}

-- test get bullet -- 
testGetBulletBefore :: Game
testGetBulletBefore = Game{
    _dead    = False                        
  , _paused  = False                        
  , _player1 = V2 10 10                      
  , _player2 = V2 20 20                    
  , _score1  =  0                            
  , _score2  =  0                           
  , _bu_cnt1 = 0
  , _bu_cnt2 = 0
  , _bullets = [V2 11 10]                           
  , _solid   = []           
  , _normal  = [V2 10 11, V2 10 9]                           
  , _grass   = []                           
  }
testGetBulletAfterEast :: Game
testGetBulletAfterEast = Game{
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 11 10                      
, _player2 = V2 20 20                    
, _score1  =  10                            
, _score2  =  0                           
, _bu_cnt1 = 1
, _bu_cnt2 = 0
, _bullets = []                           
, _solid  = []           
, _normal   = [V2 10 11, V2 10 9]                         
, _grass   = []                           
}
testGetBulletAfterWest :: Game
testGetBulletAfterWest = Game{
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 9 10                      
, _player2 = V2 20 20                    
, _score1  =  0                            
, _score2  =  0                           
, _bu_cnt1 = 0
, _bu_cnt2 = 0
, _bullets = [V2 11 10]                               
, _solid   = []        
, _normal  = [V2 10 11, V2 10 9]                             
, _grass   = []                           
}

-- test destroy normal blocks -- 
testDestroyNormalBlocksBefore :: Game
testDestroyNormalBlocksBefore = Game {
    _dead    = False                        
  , _paused  = False                        
  , _player1 = V2 10 10                      
  , _player2 = V2 20 20                    
  , _score1  =  10                            
  , _score2  =  0                           
  , _bu_cnt1 = 1
  , _bu_cnt2 = 0
  , _bullets = [V2 11 10]                           
  , _solid   = []           
  , _normal  = [V2 30 30, V2 31 31]                           
  , _grass   = []                           
  }
testDestroyNormalBlocksAfter :: Game
testDestroyNormalBlocksAfter = Game {
      _dead    = False                        
    , _paused  = False                        
    , _player1 = V2 10 10                      
    , _player2 = V2 20 20                    
    , _score1  =  10                            
    , _score2  =  0                           
    , _bu_cnt1 = 0
    , _bu_cnt2 = 0
    , _bullets = [V2 11 10]                           
    , _solid   = []           
    , _normal  = [V2 30 30, V2 31 31]                           
    , _grass   = []                           
    }
testDestroyNormalBlocksNorthBefore :: Game
testDestroyNormalBlocksNorthBefore = Game {
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 20 20                    
, _score1  =  10                            
, _score2  =  120                           
, _bu_cnt1 = 1
, _bu_cnt2 = 5
, _bullets = [V2 11 10]                           
, _solid   = []           
, _normal  = [V2 10 11]                           
, _grass   = []                           
}
testDestroyNormalBlocksNorthAfter :: Game
testDestroyNormalBlocksNorthAfter = Game {
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 20 20                    
, _score1  =  10                            
, _score2  =  120                           
, _bu_cnt1 = 0
, _bu_cnt2 = 5
, _bullets = [V2 11 10]                           
, _solid   = []           
, _normal  = []                           
, _grass   = []                           
}
testDestroyNormalBlocksMoreBefore :: Game
testDestroyNormalBlocksMoreBefore = Game {
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 20 20                    
, _score1  =  10                            
, _score2  =  120                           
, _bu_cnt1 = 1
, _bu_cnt2 = 5
, _bullets = [V2 11 10]                           
, _solid   = []           
, _normal  = [V2 10 11, V2 10 9, V2 11 10, V2 9 10, V2 9 9, V2 11 11, V2 9 11, V2 11 9]                           
, _grass   = []                           
}
testDestroyNormalBlocksMoreAfter :: Game
testDestroyNormalBlocksMoreAfter = Game {
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 20 20                    
, _score1  =  10                            
, _score2  =  120                           
, _bu_cnt1 = 0
, _bu_cnt2 = 5
, _bullets = [V2 11 10]                           
, _solid   = []           
, _normal  = []                           
, _grass   = []                           
}

-- test game over condition --
testGameOverConditionBefore :: Game
testGameOverConditionBefore = Game {
    _dead    = False                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 11 11                   
, _score1  =  10                            
, _score2  =  120                           
, _bu_cnt1 = 1
, _bu_cnt2 = 5
, _bullets = []                           
, _solid   = []           
, _normal =[]                         
, _grass   = []                           
}
testGameOverConditionAfterPlayer1 :: Game
testGameOverConditionAfterPlayer1 = Game {
    _dead    = True                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 11 11                   
, _score1  =  1010                            
, _score2  =  120                           
, _bu_cnt1 = 0
, _bu_cnt2 = 5
, _bullets = []                          
, _solid   = []           
, _normal  = []                           
, _grass   = []                            
}
testGameOverConditionAfterPlayer2 :: Game
testGameOverConditionAfterPlayer2 = Game {
    _dead    = True                        
, _paused  = False                        
, _player1 = V2 10 10                      
, _player2 = V2 11 11                   
, _score1  =  10                            
, _score2  =  1120                           
, _bu_cnt1 = 1
, _bu_cnt2 = 4
, _bullets = []                           
, _solid   = []           
, _normal  = []                        
, _grass   = []                          
}
