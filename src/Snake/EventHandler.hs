module Snake.EventHandler where 

import Snake.GameLogic

import SDL.Event

import Control.Monad.State.Lazy

import System.Exit

handleEventQueue :: MonadIO m => StateT (SnakeGame a) m ()
handleEventQueue = mapEvents handleEvent


handleEvent :: MonadIO m => Event -> StateT (SnakeGame a) m ()
handleEvent (Event _ QuitEvent) = liftIO $ exitSuccess
handleEvent _         = return ()
