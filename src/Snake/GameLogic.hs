{-# LANGUAGE FlexibleContexts #-}
module Snake.GameLogic where 

import Data.List
import System.Random
import System.Exit

import Control.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State.Lazy

import Linear.Affine
import Linear.V2
import Linear.Vector

data SnakeGame a = SnakeGame {snake :: [Point V2 a],
                            goal :: Point V2 a ,
                            snakeDir :: V2 a,
                            score :: Int,
                            gameOver :: Bool,
                            bounds :: V2 a,
                            selectedItem :: MenuItem,
                            rng :: StdGen}

data MenuItem = NewGame | Exit deriving (Eq, Enum)

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

doMenuActionST :: (Integral a, MonadIO m, Num a, Ord a, Random a) => StateT (SnakeGame a) m ()
doMenuActionST = do
    item <- selectedItem <$> get
    when (item == NewGame) resetGameST
    when (item == Exit) (liftIO exitSuccess)
    return ()

nextMenuItemST :: (MonadIO m, Num a, Ord a, Random a) => StateT (SnakeGame a) m ()
nextMenuItemST = do
    g <- get
    let next = nextMenuItem . selectedItem $ g
    put $ g {selectedItem = next}

nextMenuItem NewGame = Exit
nextMenuItem Exit = Exit

prevMenuItemST :: (MonadIO m, Num a, Ord a, Random a) => StateT (SnakeGame a) m ()
prevMenuItemST = do
    g <- get
    let prev = prevMenuItem . selectedItem $ g
    put $ g {selectedItem = prev}

prevMenuItem NewGame = NewGame
prevMenuItem Exit = NewGame

advanceSnake :: (Num a, Affine p) => Diff p a -> [p a] -> [p a]
advanceSnake d s@(h:_) = (h.+^d):init s

eatsSelf :: Eq a => [a] -> Bool
eatsSelf s = nub s /= s

outOfBounds :: (Num a, Ord a, R2 t, R2 t1) => t1 a -> [t a] -> Bool
outOfBounds b (h:_) = h^._x < 0 || h^._y < 0 || h^._x > b^._x || h^._y > b^._y

eatsFood :: Eq a => a -> [a] -> Bool
eatsFood f (h:_) = h == f

advanceGameST :: (MonadIO m, Num a, Ord a, Random a) => StateT (SnakeGame a) m ()
advanceGameST = do
    g <- get
    when (not $ gameOver g) (put $ advanceGame g)

resetGameST :: (Integral a, MonadIO m, Num a, Ord a, Random a) => StateT (SnakeGame a) m ()
resetGameST = do
    st <- get
    put $ st {snake = [P $ (((flip div) 2) <$> (bounds st))], score = 0, gameOver = False}

advanceGame :: (Num a, Ord a, Random a) => SnakeGame a -> SnakeGame a
advanceGame game@(SnakeGame snake goal dir score over bounds item rng) =
                game {snake = newSnake,
                      goal = newGoal,
                      score = newScore,
                      gameOver = gameOver game || newGameOver,
                      rng = g}
    where newSnake = if eatsFood goal $ advanceSnake dir snake
                         then goal:snake
                         else advanceSnake dir snake
          (newGoal,g) = if eatsFood goal snake 
                            then randomR (origin, P bounds) rng
                            else (goal,rng)
          newScore = score + fromEnum (eatsFood goal newSnake)
          newGameOver = eatsSelf newSnake || outOfBounds bounds newSnake
