import Snake.Render
import Snake.GameLogic

import Linear.Affine
import Linear.V2

import SDL.Video

import System.Random

main = do w <- createSnakeWindow
          r <- createRenderer w (-1) defaultRenderer
          let o = defaultOptions
          
          drawSnakeGame r o initialGame

          present r

          getLine
          
          destroyRenderer r
          destroyWindow w
           
initialGame =  SnakeGame [P $ V2 0 0] (P $ V2 5 5) (V2 1 1) 0 False (P $ V2 0 0, P $ V2 10 10)  (mkStdGen 0)
