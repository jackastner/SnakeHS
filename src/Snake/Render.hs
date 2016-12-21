module Snake.Render where

import Paths_SnakeHS

import Data.Text (pack)
import Data.StateVar (($=))
import Data.Word

import Control.Monad.IO.Class
import Control.Monad.State.Lazy

import SDL.Video
import SDL.Font as Font

import Linear.V4
import Linear.V2
import Linear.Affine

import Foreign.C.Types

import Snake.GameLogic
import Snake.Types

-- change to font available on your system
defaultFontPath = "/usr/share/fonts/truetype/freefont/FreeMono.ttf"

defaultOptions :: Renderer -> IO SnakeRenderOptions
defaultOptions r = do
  font <- Font.load defaultFontPath 24
  sprites <- getDataFileName "sprites/snake.bmp" >>= loadBMP >>= createTextureFromSurface r
  return $ SnakeRenderOptions {
    spriteSheet     = sprites,
    backgroundColor = V4 0x00 0x00 0x00 0xff,
    snakeColor      = V4 0xff 0xff 0xff 0xff,
    goalColor       = V4 0x00 0xff 0x00 0xff,
    scale           = 10,
    menuFont        = font,
    menuColor       = V4 0xff 0xff 0xff 0xff,
    highlight       = V4 0xff 0x00 0x00 0x00 }

createSnakeWindow :: IO Window
createSnakeWindow = createWindow (pack "Snake") defaultWindow

drawSnakeGameST :: Renderer -> SnakeRenderOptions -> SnakeGameST ()
drawSnakeGameST r o = do
    g <- get
    when (not $ gameOver g) (drawSnakeGame r o g)
    when (gameOver g) (drawMenu r o g)
    return ()

drawMenu r o g = do
  let (V2 w h) = bounds g
  let centerW = (w * (scale o)) `div` 2
  let centerH = (h * (scale o)) `div` 2

  let color = (if selectedItem g == NewGame then highlight o else menuColor o)
  surface <- Font.solid (menuFont o) color (pack "New Game")
  texture <- createTextureFromSurface r surface
  infoNewGame <- queryTexture texture
  let hNewGame = textureHeight infoNewGame
  let wNewGame = textureWidth infoNewGame
  copy r texture Nothing (Just $ Rectangle (P $ V2 (centerW - (wNewGame `div` 2)) (centerH - hNewGame)) (V2 wNewGame hNewGame))

  let color = (if selectedItem g == Exit then highlight o else menuColor o)
  surface <- Font.solid (menuFont o) color (pack "Exit")
  texture <- createTextureFromSurface r surface
  infoExit <- queryTexture texture
  let hExit = textureHeight infoExit
  let wExit = textureWidth infoExit
  copy r texture Nothing (Just $ Rectangle (P $ V2 (centerW - (wExit `div` 2)) centerH) (V2 wExit hExit))

drawSnakeGame r o g = do
    rendererDrawColor r $= backgroundColor o
    clear r
    
    drawSnake r o g

    rendererDrawColor r $= goalColor o
    drawGoal r o g

drawSnake r o g = mapM_ (drawSnakeSquare r o) $ snake g
drawGoal  r o g = drawGameSquare r o $ goal g
drawGameSquare r SnakeRenderOptions {scale=s} p = fillRect r (Just $ Rectangle ((*s) <$> p) (V2 s s))
drawSnakeSquare r SnakeRenderOptions {scale=s,spriteSheet=sprites} p = copy r sprites (Nothing)  (Just $ Rectangle ((*s) <$> p) (V2 s s))
    where spriteStart = Rectangle (P $ V2 0 0) (V2 10 10)
