# CSE230_Project
### Goals
It is a fast paced multiplayer shooting game in the scene of a maze, developed in Haskell. Each participant could control their roles using a keyboard and interact with each other.

## Playfield (Maze)
The playfiled consists of lanes for players to move around and 3 kinds of blocks with different functions:   
i) Solid blocks, which can not be destroyed by bullets and players can’t go through it   
ii) Normal blocks, which can be destroyed by player’s bullets and players also can’t go through it   
iii) Grass blocks, which can not be destroyed by player’s bullets but players can go through it    
Bullets can destroy normal blocks but can not pass through normal blocks or grass blocks. When a block is destroyed, it disappears from the playfield and becomes an empty ground.    
In our implementation, we plan to use different colors to represent different kinds of blocks.
## Playfield (Counter)
i) Score counter, when players eat bullets, the score would increase 10 points for each bullet. When player shoot the rival, the score will increase 1000 points.
ii) Bullet Counter, when players eat bullets, the bullet counter would increase 10 points for each bullet. When players shoot their bullets, the bullet counter would decrease 10 points after shooting each point. 

## Operations
There are three kinds of operations for each player.    
i) Players could be controlled using keyboards. Player A can use ↑ (top arrow),  ↓ (down arrow) ,  → (right arrow) and ← (left arrow) to move around, while *.* is the button to shoot. Player B can use key *w*, *s* ,*d*  and *a* to move round, while *c* is the button to shoot.     
ii) Players could pick up objects to earn bullets when touching the objects by moving around in the maze.     
iii) Players could also shoot bullets and cause harm to the rival and normal blocks when the bullets are available.     

## Win & Lose
A player wins if    
i) the other player is killed. A player will be killed instantly once he/she gets shooted.   
ii) when bullets are running out and no player gets shoot, then the player who has higher bullet counter score wins.   

## Screenshot
<img src="/screenshot/sample.png" width="500"> 

## Update on Nov.29

1. What is the architecture of your application (the key components)?


2. What challenges (if any) did you have so far and how did you solve them?


3. Do you expect to meet your goals until the deadline?
Yes

4. If not, how will you modify your goals?
We're expected to meet the goals.
