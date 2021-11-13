# CSE230_Project
### Goals
It is a fast paced multiplayer shooting game in the scene of a maze, developed in Haskell. Each participant could control their roles using a keyboard and interact with each other.

## Playfield (Maze)
The playfiled consists of lanes for players to move around and 3 kinds of blocks with different functions:   
i) Solid blocks, which can not be destroyed by bullets and players can’t go through it   
ii) Normal blocks, which can be destroyed by player’s bullets and players also can’t go through it   
iii) Grass blocks, which can not be destroyed  by player’s bullets but players can go through it    
Bullets can destroy normal blocks but can not pass through normal blocks or grass blocks. When a block is destroyed, it disappears from the playfield and becomes an empty ground.    
In our implementation, we plan to use different colors to represent different kinds of blocks.     

## Operations
There are three kinds of operations for each player.    
i) Players could be controlled using keyboards. The players signal that they are ready by pressing *enter* and *space* on the keyboard. Player A can use ↑ (top arrow),  ↓ (down arrow) ,  → (right arrow) and ← (left arrow) to move around, while *return* is the button to shoot. Player B can use key *w*, *s* ,*d*  and *a* to move round, while *space* is the button to shoot.     
ii) Players could pick up objects to earn bullets when touching the objects by moving around in the maze.     
iii) Players could also shoot bullets and cause harm to the rival and normal blocks when the bullets are available.     

## Win & Lose
A player wins if    
i) the other player is killed. A player will be killed instantly once he/she gets shooted.   
ii) have more bullets than the other player when time is up.   

## Screenshot
![Sample](https://github.com/XuanzhuZhou/CSE230_Project/blob/master/screenshot/sample.png)


