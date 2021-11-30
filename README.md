# CSE230_Project

## Team Members(Name, Email, Github_id)
Xiuqi Chen, xic048@ucsd.edu, BrannyA
Manqing Zheng, maz040@ucsd.edu, manqingZheng
Xuanzhu Zhou, xuz004@ucsd.edu, XuanzhuZhou

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
<img src="/screenshot/sample.png" width="500"> 

### Update on Nov.29

1. What is the architecture of your application (the key components)?
We used several haskell libraries to implement the game, including control monad library, concurrent library, brick library and System Random library.  
There are 3 main components in this game:  

a) Concurrency Control  
Both players can move and shoot at any time while one action has no effect on the other, which can be implemented by the concurency module.  

b) Logic Judgement  
There are plenty of logic judgement requirements in this game. For instance, when a player moves, we need to judge whether it has reached the boundry or not. Also, some of the blocks can be crossed while others can not.  

c) UI  
It includes a playfield(maze) in this game, which visualizes players, blocks and bullets etc. It also accepts keyboard input from two players and pass them to the logic judgement module.  

2. What challenges (if any) did you have so far and how did you solve them?
For UI application, since we are not familiar with Brick and Graphics library, it is hard at first to implement the maze that we designed. We spent few days to learn tutorials online, gathered further understanding and planned to learn more in the next few days. It's a great experience of self learning and we believe that we will have a good knowledge of utilizing use libraries to accomplish the project well.

We also have problems with moving characters. For each step we are supposed to update data in very short time period to make moving steps smooth enough. It is not easy to applicate that so we changed our original sliding roads into grids, making movement looks natural.

When we tried to implement the Players class, we realized that we have two players who might move at the same time. A normal implementation is using two threads to represent the players. When they need to use the same resource, for example, pick up the same bullet, they should first acquire the lock of the bullet. However, multi-thread programming in Haskell is totally new for us, and we found it hard to code it out. A alternative solution to this problem is that we don't allow two players to go on the same block at the same time, so that we don't need a lock.  

3. Do you expect to meet your goals until the deadline? If not, how will you modify your goals?
We're expected to meet the goals.
We are trying to figure out multi-thread implementation of two players. Currently we are not sure about how the bricks library handle the inputs, for example, if two players try to move at the same time. If we can not solve this in few days, we are going to modify our game design.
