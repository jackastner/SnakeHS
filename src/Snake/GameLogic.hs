module Snake where 

import Data.List
import System.Random
import Control.Applicative

data SnakeGame = SnakeGame {snake :: Snake,
                            goal :: Point,
                            snakeDir :: Direction,
                            score :: Int,
                            gameOver :: Bool,
                            bounds :: Rectangle,
                            rng :: StdGen}

type Point = (Int,Int)
type Rectangle = (Point,Point)
type Snake = [Point]
type Direction = (Int,Int)

instance (Random a0, Random a1) => Random (a0,a1) where
    randomR ((x0,y0),(x1,y1)) g = ((x,y),g'')
        where (x,g')  = randomR (x0,x1) g
              (y,g'') = randomR (y0,y1) g'
    random g = ((x,y),g'')
        where (x,g')  = random g
              (y,g'') = random g'
                                  

advanceSnake :: Direction -> Snake -> Snake 
advanceSnake (x,y) s@((x0,y0):_) = (x0+x,y0+y):init s

eatsSelf :: Snake -> Bool
eatsSelf s = nub s /= s

outOfBounds :: Rectangle -> Snake -> Bool
outOfBounds ((x0,y0),(x1,y1)) ((xs,ys):_) = xs < x0 || ys < y0 || xs > x1 || ys > y1

eatsFood :: Point -> Snake -> Bool
eatsFood f (h:_) = h == f

advanceGame :: SnakeGame -> SnakeGame 
advanceGame game@(SnakeGame snake goal dir score over bounds rng) = 
                game {snake = newSnake,
                      goal = newGoal,
                      score = newScore,
                      gameOver = newGameOver,
                      rng = g}
    where newSnake = advanceSnake dir snake 
          (newGoal,g) = if eatsFood goal  newSnake 
                            then randomR bounds rng
                            else (goal,rng)
          newScore = score + (fromEnum $ eatsFood goal newSnake)
          newGameOver = eatsSelf newSnake || outOfBounds bounds newSnake
