import Snake.Render
import Snake.GameLogic
import Snake.EventHandler
import Snake.Types

import Linear.Affine
import Linear.V2
import Linear.Vector

import SDL.Video
import SDL.Init
import SDL.Time
import SDL.Font as Font

import System.Random

import Control.Monad.State.Lazy

import Data.StateVar (($=))
import Data.Text (pack)

import Foreign.C.Types

main = do
  initializeAll
  Font.initialize
  w <- createSnakeWindow
  r <- createRenderer w (-1) defaultRenderer
  o <- defaultOptions r

  initialGame >>= evalStateT (playGame w r o)
    
playGame :: Window -> Renderer -> SnakeRenderOptions -> SnakeGameST ()
playGame w r o = do
  (*^) (scale o) . (+) 1 . bounds <$> get >>= (windowSize w $=)

  forever $ do
      handleEventQueue
      advanceGameST
      drawSnakeGameST r o
      pack . ("Snake " ++) .  show . score <$> get >>= (windowTitle w $=)
      present r
      delay 50
           
initialGame = SnakeGame [P $ V2 15 15] (P $ V2 5 5) (V2 0 0) 0 False (V2 30 30) Exit <$> getStdGen
