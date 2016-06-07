module Snake.EventHandler where 

import Snake.GameLogic

import Linear.V2

import SDL.Event
import SDL.Input.Keyboard.Codes
import SDL.Input.Keyboard

import Control.Monad.State.Lazy

import System.Exit

handleEventQueue :: (Num a, MonadIO m) => StateT (SnakeGame a) m ()
handleEventQueue = mapEvents handleEvent


handleEvent :: (Num a, MonadIO m) => Event -> StateT (SnakeGame a) m ()
handleEvent (Event _ QuitEvent) = liftIO $ exitSuccess
handleEvent (Event _ (KeyboardEvent d)) = do 
    g <- get
    case keysymKeycode $ keyboardEventKeysym d of 
        KeycodeLeft -> put $ g {snakeDir = V2 (-1) 0}
        KeycodeRight -> put $ g {snakeDir = V2 1 0}
        KeycodeDown -> put $ g {snakeDir = V2 0 1}
        KeycodeUp -> put $ g {snakeDir = V2 0 (-1)}
        _ -> return ()
handleEvent _         = return ()
