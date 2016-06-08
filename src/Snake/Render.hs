module Snake.Render where

import Data.Text (pack)
import Data.StateVar (($=))
import Data.Word

import Control.Monad.IO.Class
import Control.Monad.State.Lazy

import SDL.Video

import Linear.V4
import Linear.V2
import Linear.Affine

import Foreign.C.Types

import Snake.GameLogic

data SnakeRenderOptions a = SnakeRenderOptions {
    backgroundColor :: V4 Word8,
    snakeColor :: V4 Word8,
    goalColor  :: V4 Word8,
    scale      :: a}

defaultOptions :: Num a => SnakeRenderOptions a
defaultOptions = SnakeRenderOptions {
    backgroundColor = V4 0x00 0x00 0x00 0xff,
    snakeColor      = V4 0xff 0xff 0xff 0xff,
    goalColor       = V4 0x00 0xff 0x00 0xff,
    scale           = 10}

createSnakeWindow :: MonadIO m => m Window 
createSnakeWindow = createWindow (pack "Snake") defaultWindow

drawSnakeGameST :: MonadIO m => Renderer -> SnakeRenderOptions CInt -> StateT (SnakeGame CInt) m ()
drawSnakeGameST r o = do
    g <- get
    drawSnakeGame r o g
    return ()

drawSnakeGame r o g = do
    rendererDrawColor r $= backgroundColor o
    clear r
    
    rendererDrawColor r $= snakeColor o
    drawSnake r o g

    rendererDrawColor r $= goalColor o
    drawGoal r o g

drawSnake r o g = mapM_ (drawGameSquare r o) $ snake g
drawGoal  r o g = drawGameSquare r o $ goal g
drawGameSquare r SnakeRenderOptions {scale=s} p = fillRect r (Just $ Rectangle ((*s) <$> p) (V2 s s))
