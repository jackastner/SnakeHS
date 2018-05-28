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

import Foreign.C.Types

import Snake.Types

doMenuActionST :: SnakeGameST ()
doMenuActionST = do
    item <- selectedItem <$> get
    when (item == NewGame) resetGameST
    when (item == Exit) (liftIO exitSuccess)
    return ()

nextMenuItemST :: SnakeGameST ()
nextMenuItemST = do
    g <- get
    let next = nextMenuItem . selectedItem $ g
    put $ g {selectedItem = next}

nextMenuItem NewGame = Exit
nextMenuItem Exit = Exit

prevMenuItemST :: SnakeGameST ()
prevMenuItemST = do
    g <- get
    let prev = prevMenuItem . selectedItem $ g
    put $ g {selectedItem = prev}

prevMenuItem NewGame = NewGame
prevMenuItem Exit = NewGame

advanceSnake :: V2 SnakeInt -> Snake -> Snake
advanceSnake d s@(h:_) = (h.+^d):init s

eatsSelf :: Snake -> Bool
eatsSelf s = nub s /= s

outOfBounds :: Bounds -> Snake -> Bool
outOfBounds b (h:_) = h^._x < 0 || h^._y < 0 || h^._x > b^._x || h^._y > b^._y

eatsFood :: GameSquare -> Snake -> Bool
eatsFood f (h:_) = h == f

advanceGameST :: SnakeGameST ()
advanceGameST = do
    g <- get
    when (not $ gameOver g) (put $ advanceGame g)

resetGameST :: SnakeGameST ()
resetGameST = do
    st <- get
    put $ st {snake = [P $ (((flip div) 2) <$> (bounds st))], score = 0, gameOver = False}

advanceGame :: SnakeGame -> SnakeGame
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
