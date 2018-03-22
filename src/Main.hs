import Snake.Render
import Snake.GameLogic
import Snake.EventHandler

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
  g <- initialGame
  o <- defaultOptions
  evalStateT (playGame o) g
    
playGame :: MonadIO m => SnakeRenderOptions CInt -> StateT (SnakeGame CInt) m ()
playGame o = do
  w <- createSnakeWindow
  r <- createRenderer w (-1) defaultRenderer

  (*^) (scale o) . (+) 1 . bounds <$> get >>= (windowSize w $=)

  forever $ do
      handleEventQueue
      advanceGameST
      over <- gameOver <$> get
      drawSnakeGameST r o
      pack . ("Snake " ++) .  show . score <$> get >>= (windowTitle w $=)
      present r
      delay 50

           
initialGame = SnakeGame [P $ V2 15 15] (P $ V2 5 5) (V2 0 0) 0 False (V2 30 30) Exit <$> getStdGen
