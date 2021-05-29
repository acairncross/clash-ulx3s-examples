{-# LANGUAGE RecordWildCards #-}

module Snake where

import Clash.Prelude hiding (Either (..))
import Control.Lens ((.~), ix)
import Data.Function ((&))
import Data.Word
import Data.Biapplicative
import Data.Bool (bool)

-- Much of this stolen from the version of Snake in https://github.com/basile-henry/clash-io

type ScreenWidth = 16
type ScreenHeight = 12
type MaxSnakeLength = 10

data Status
  = Alive
  | Dead
  deriving (Show, Eq, Generic, NFDataX)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq, Generic, NFDataX)

data Tile
  = Space
  | Snake
  | Food
  deriving (Show, Eq, Generic, NFDataX)

data Inputs = MkInputs
  { snakeUp :: Bool
  , snakeDown :: Bool
  , snakeLeft :: Bool
  , snakeRight :: Bool
  }
  deriving (Show, Eq, Generic, NFDataX)

defaultInputs :: Inputs
defaultInputs = MkInputs False False False False

data GameState = GameState
  { snakeCoords :: Vec MaxSnakeLength (Index ScreenWidth, Index ScreenHeight)
  , snakeLength :: Index MaxSnakeLength
  , snakeStatus :: Status
  , snakeDirection :: Direction
  , snakeSpeed :: Unsigned 4
  , waitFrames :: Unsigned 4
  , foodCoords :: (Index ScreenWidth, Index ScreenHeight)
  , randomness :: BitVector 16
  , frameBuffer :: Vec ScreenHeight (Vec ScreenWidth Tile)
  , lastInputs :: Inputs
  }
  deriving (Show, Eq, Generic, NFDataX)

initState :: GameState
initState = GameState
  { snakeCoords = zip (repeat 8) (repeat 8)
  , snakeLength = 1
  , snakeStatus = Alive
  , snakeDirection = Right
  , snakeSpeed = 10
  , waitFrames = 10
  , foodCoords = (10, 5)
  , randomness = 0x1234
  , frameBuffer
    = repeat (repeat Space)
      & ix 8 . ix 8 .~ Snake
      & ix 5 . ix 10 .~ Food
  , lastInputs = MkInputs False False False False
  }

oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Down = Up
oppositeDirection Left = Right
oppositeDirection Right = Left

inputsToDirection :: Inputs -> Maybe Direction
inputsToDirection MkInputs{..}
  | snakeUp = Just Up
  | snakeDown = Just Down
  | snakeLeft = Just Left
  | snakeRight = Just Right
  | otherwise = Nothing

anyInputs :: Inputs -> Bool
anyInputs MkInputs{..} = anyVec (snakeUp :> snakeDown :> snakeLeft :> snakeRight :> Nil)
  where
    anyVec = (/= 0) . v2bv . map (bool 0 1)

satInc :: SaturatingNum a => SaturationMode -> a -> a
satInc mode x = satAdd mode x 1

satDec :: SaturatingNum a => SaturationMode -> a -> a
satDec mode x = satSub mode x 1

directionToDeltaFns :: (SaturatingNum a, SaturatingNum b) => Direction -> (a -> a, b -> b)
directionToDeltaFns Up    = (id,              satDec SatBound)
directionToDeltaFns Down  = (id,              satInc SatBound)
directionToDeltaFns Left  = (satDec SatBound, id             )
directionToDeltaFns Right = (satInc SatBound, id             )

isBorder
  :: (Index ScreenWidth, Index ScreenHeight)
  -> Bool
isBorder (x, y)
  = x == minBound || x == maxBound
 || y == minBound || y == maxBound

updateState :: Inputs -> GameState -> GameState
updateState inp state@GameState{..}
  | snakeStatus == Dead
    = state
  | waitFrames /= 0
    = state
      { waitFrames = pred waitFrames
      , lastInputs = kb
      }
  | snakeHeadCoords' == foodCoords
    = state
      { foodCoords = foodCoords'
      , snakeCoords = snakeCoords'
      , snakeLength = 1 + snakeLength
      , snakeDirection = snakeDirection'
      , waitFrames = snakeSpeed'
      , snakeSpeed = snakeSpeed'
      , lastInputs = defaultInputs
      , randomness = randomness'
      , frameBuffer
        = frameBuffer
          & ix headY' . ix headX' .~ Snake
          & ix foodY' . ix foodX' .~ Food
      }
  | isColliding
    = state { snakeStatus = Dead }
  | otherwise
    = state
      { snakeCoords = snakeCoords'
      , snakeDirection = snakeDirection'
      , waitFrames = snakeSpeed'
      , lastInputs = defaultInputs
      , frameBuffer
        = frameBuffer
          & ix tailY . ix tailX .~ Space
          & ix headY' . ix headX' .~ Snake
      }
  where
    kb
      | anyInputs inp = inp
      | otherwise = lastInputs

    snakeSpeed'
      | snakeSpeed > 1 = pred snakeSpeed
      | otherwise = snakeSpeed

    snakeDirection' = case inputsToDirection kb of
      Nothing -> snakeDirection
      Just dir -> if dir == oppositeDirection snakeDirection then snakeDirection else dir

    snakeHeadCoords'@(headX', headY') =
      let deltaFns = directionToDeltaFns snakeDirection'
      in deltaFns <<*>> head snakeCoords

    (tailX, tailY) = snakeCoords !! snakeLength

    randomness' = lfsr randomness

    foodCoords'@(foodX', foodY') =
      let (x, y) = bitCoerce $ truncateB randomness'
      in (x, y)

    snakeCoords' = snakeHeadCoords' +>> snakeCoords

    isColliding
      -- Wall collision
      = isBorder snakeHeadCoords' 
      -- Self collision
      || (frameBuffer !! headY' !! headX' == Snake && (headX', headY') /= (tailX, tailY))

type Color = (Word8, Word8, Word8)

draw :: GameState -> Index ScreenWidth -> Index ScreenHeight -> Color
draw GameState{..} x y =
  case frameBuffer !! y !! x of
    Space -> if isBorder (x, y)
      then white
      else gray
    Snake -> case snakeStatus of
      Alive -> blue
      Dead -> yellow
    Food -> red

white, blue, yellow, red, gray :: Color
white  = (0xff, 0xff, 0xff)
blue   = (0x40, 0x80, 0xf0)
yellow = (0xf0, 0xe0, 0x40)
red    = (0x80, 0x00, 0x00)
gray   = (0x30, 0x30, 0x30) 

-- LFSR from Clash examples
lfsr :: BitVector 16 -> BitVector 16
lfsr s = (bitCoerce fb) ++# slice d15 d1 s
  where
    fb :: Bit
    fb = s!(5 :: Index 16) `xor` s!(3 :: Index 16) `xor` s!(2 :: Index 16) `xor` s!(0 :: Index 16)
