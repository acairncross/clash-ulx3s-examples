#!/usr/bin/env cabal
{- cabal:
build-depends:
  base < 5,
  JuicyPixels,
  lens
-}

import Control.Lens (toListOf)
import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Codec.Picture as Pic

main :: IO ()
main = do
  [imgPath] <- getArgs
  Right dyImg <- Pic.readImage imgPath
  let img = Pic.convertRGB8 dyImg
  let pixels = toListOf Pic.imagePixels img
  forM_ pixels $ \(Pic.PixelRGB8 r g b) ->
    putStrLn $ printf "%08b_%08b_%08b" r g b
