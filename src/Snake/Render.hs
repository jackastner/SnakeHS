module Snake.Render where

import qualified Data.Text as T
import Data.StateVar
import Data.Word

import SDL.Video

import Linear.V4
import Linear.V2
import Linear.Affine

import Snake.GameLogic

data SnakeRenderOptions a = SnakeRenderOptions {
    backgroundColor :: V4 Word8,
    snakeColor :: V4 Word8,
    goalColor  :: V4 Word8,
    scale      :: a}

defaultOptions :: Num a => SnakeRenderOptions a
defaultOptions = SnakeRenderOptions {
    backgroundColor = V4 0xff 0xff 0xff 0xff,
    snakeColor      = V4 0x00 0x00 0x00 0xff,
    goalColor       = V4 0x00 0xff 0x00 0xff,
    scale           = 10}

createSnakeWindow :: IO Window
createSnakeWindow = createWindow (T.pack "Snake") defaultWindow

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
