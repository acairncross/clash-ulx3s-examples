{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.State
import RetroClash.Sim.SDL

import Snake

main :: IO ()
main =
  flip evalStateT initState $
  withMainWindow @ScreenWidth @ScreenHeight videoParams $ \events keyDown -> do
    guard $ not $ keyDown ScancodeEscape
    modify $ updateState $ MkInputs
      { snakeUp = keyDown ScancodeUp
      , snakeRight = keyDown ScancodeRight
      , snakeDown = keyDown ScancodeDown
      , snakeLeft = keyDown ScancodeLeft
      }
    gets $ rasterizePattern . draw
  where
    videoParams = MkVideoParams
      { windowTitle = "Snake"
      , screenScale = 16
      , screenRefreshRate = 60
      , reportFPS = True
      }
