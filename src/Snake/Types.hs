module Snake.Types where 

import Data.Word

import SDL.Font as Font
import SDL.Video

import Linear.V4
import Linear.V2
import Linear.Vector
import Linear.Affine

import Foreign.C.Types

import System.Random

import Control.Lens
import Control.Arrow
import Control.Monad.State.Lazy

type SnakeInt = CInt

type SnakeGameST = StateT SnakeGame IO

type GameSquare = Point V2 SnakeInt

type Snake = [GameSquare]

type Direction = V2 SnakeInt

type Bounds = V2 SnakeInt

data SnakeGame = SnakeGame {snake :: Snake,
                            goal :: GameSquare,
                            snakeDir :: Direction,
                            score :: Int,
                            gameOver :: Bool,
                            bounds :: Bounds,
                            selectedItem :: MenuItem,
                            rng :: StdGen}

data MenuItem = NewGame | Exit deriving (Eq, Enum)

instance Random a => Random (V2 a) where
    randomR (l,u) g = (V2 x y,g'')
        where (x,g')  = randomR (l^._x,u^._x) g
              (y,g'') = randomR (l^._y,u^._y) g'
    random g = (V2 x y,g'')
        where (x,g')  = random g
              (y,g'') = random g'

instance Random (f a) => Random (Point f a) where
    randomR (P l, P u) g = first P $ randomR (l,u) g
    random g = first P $ random g

data SnakeRenderOptions = SnakeRenderOptions {
    spriteSheet :: Texture,
    backgroundColor :: V4 Word8,
    snakeColor :: V4 Word8,
    goalColor  :: V4 Word8,
    scale      :: SnakeInt,
    menuFont   :: Font.Font,
    menuColor  :: V4 Word8,
    highlight  :: V4 Word8}
