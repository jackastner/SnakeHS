{-# LANGUAGE FlexibleContexts #-}
module Snake.EventHandler where 

import Snake.GameLogic

import Linear.V2

import SDL.Event
import SDL.Init
import SDL.Input.Keyboard.Codes
import SDL.Input.Keyboard

import Control.Monad.State.Lazy

import System.Exit
import System.Random

handleEventQueue :: (Integral a, Random a, Ord a, Num a, MonadIO m) => StateT (SnakeGame a) m ()
handleEventQueue = mapEvents handleEvent

handleEvent :: (Integral a, Ord a, Random a, Num a, MonadIO m) => Event -> StateT (SnakeGame a) m ()
handleEvent (Event _ QuitEvent) = do
    quit
    liftIO $ exitSuccess
handleEvent (Event _ (KeyboardEvent d)) = do 
    g <- get
    when (gameOver g) (handleMenuEvent d)
    when (not $ gameOver g) (handleGameEvent d)
handleEvent _         = return ()

handleMenuEvent d = do
    g <- get
    case keysymKeycode $ keyboardEventKeysym d of
        KeycodeDown -> nextMenuItemST
        KeycodeUp -> prevMenuItemST
        KeycodeReturn -> doMenuActionST
        _ -> return ()

handleGameEvent d = do
    g <- get
    case keysymKeycode $ keyboardEventKeysym d of 
        KeycodeLeft -> put $ g {snakeDir = V2 (-1) 0}
        KeycodeRight -> put $ g {snakeDir = V2 1 0}
        KeycodeDown -> put $ g {snakeDir = V2 0 1}
        KeycodeUp -> put $ g {snakeDir = V2 0 (-1)}
        _ -> return ()
