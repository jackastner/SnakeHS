import Snake.Render
import Snake.GameLogic
import Snake.EventHandler

import Linear.Affine
import Linear.V2
import Linear.Vector

import SDL.Video

import System.Random

import Control.Monad.State.Lazy
import Control.Concurrent.Thread.Delay --evil

import Data.StateVar (($=))

import Foreign.C.Types

main = evalStateT (playGame defaultOptions) initialGame

    
playGame :: MonadIO m => SnakeRenderOptions CInt -> StateT (SnakeGame CInt) m ()
playGame o = do w <- createSnakeWindow
                r <- createRenderer w (-1) defaultRenderer

                (*^) (scale o) . (+) 1 . bounds <$> get >>= ($=) (windowSize w)

                forever $ do
                    handleEventQueue
                    advanceGameST
                    drawSnakeGameST r o
                    present r
                    liftIO $ delay 100000
           
initialGame =  SnakeGame [P $ V2 0 0, P $ V2 0 1, P $ V2 0 2] (P $ V2 5 5) (V2 1 1) 0 False (V2 10 10)  (mkStdGen 0)
