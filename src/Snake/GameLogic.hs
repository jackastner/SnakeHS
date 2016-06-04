{-# LANGUAGE FlexibleContexts #-}
module Snake.GameLogic where 

import Data.List
import System.Random

import Control.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad

import Linear.Affine
import Linear.V2

data SnakeGame a = SnakeGame {snake :: [Point V2 a],
                            goal :: Point V2 a ,
                            snakeDir :: V2 a,
                            score :: Int,
                            gameOver :: Bool,
                            bounds :: (Point V2 a,Point V2 a),
                            rng :: StdGen}

instance Random a => Random (V2 a) where
    randomR (l,u) g = (V2 x y,g'')
        where (x,g')  = randomR (l^._x,u^._x) g
              (y,g'') = randomR (l^._y,u^._y) g'
    random g = (V2 x y,g'')
        where (x,g')  = random g
              (y,g'') = random g'

instance Random (f a) => Random (Point f a) where
    randomR (P l, P u) g = first P $ randomR (l,u) g
    random g = first P $ random g
                                  


advanceSnake :: (Num a, Affine p) => Diff p a -> [p a] -> [p a]
advanceSnake d s@(h:_) = (h.+^d):init s

eatsSelf :: Eq a => [a] -> Bool
eatsSelf s = nub s /= s

outOfBounds :: (Ord a, R2 t, R2 t1, R2 t2) => (t1 a, t2 a) -> [t a] -> Bool
outOfBounds (u,l) (h:_) = h^._x > u^._x || h^._y < u^._y || h^._x > l^._x || h^._y < l^._y


eatsFood :: Eq a => a -> [a] -> Bool
eatsFood f (h:_) = h == f

advanceGame :: (Num a, Ord a, Random a) => SnakeGame a -> SnakeGame a
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
          newScore = score + fromEnum (eatsFood goal newSnake)
          newGameOver = eatsSelf newSnake || outOfBounds bounds newSnake
