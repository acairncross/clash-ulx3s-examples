{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module Top where

import Clash.Prelude
import Clash.Annotations.TH (makeTopEntity)
import Control.Monad.State
import Data.Function ((&))
import Data.Maybe (isJust)
import RetroClash.VGA
import RetroClash.Video (scale, center)

import Clocks
import DVI
import ECP5
import qualified RAM
import qualified Snake
import StackMachine
import UART
import Utils

counter'
  :: forall n dom
   . HiddenClockResetEnable dom
  => KnownNat n
  => Signal dom (BitVector n)
counter' = register 0 ((1+) <$> counter')

slowCounter'
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 8)
slowCounter' = mealyState (\() -> slowCounterT) 0 (pure ())
  where
    slowCounterT :: State (BitVector 32) (BitVector 8)
    slowCounterT = do
      internalCounter <- get
      let internalCounter' = internalCounter + 1
      put internalCounter'
      return $ truncateB (internalCounter' `shiftR` 22)

counter
  :: "clk_25mhz" ::: Clock Dom25
  -> "led" ::: Signal Dom25 (BitVector 8)
counter clk =
  withClockResetEnable clk resetGen enableGen $
    fmap (truncateB . (`shiftR` 22)) (counter' @32)
    -- slowCounter'

makeTopEntity 'counter

ram
  :: "clk_25mhz" ::: Clock Dom25
  -> "ftdi_txd" ::: Signal Dom25 Bit
  -> "ftdi_rxd" ::: Signal Dom25 Bit
ram clk input =
  withClockResetEnable clk resetGen enableGen $
    input
    & override 8 high
    & uartRx (SNat @(HzToPeriod 115200))
    & RAM.ramRx
    & RAM.ram
    & fst . uartTx (SNat @(HzToPeriod 115200))

makeTopEntity 'ram

pll
  :: "clk_25mhz" ::: Clock Dom25
  -> ("clk_60mhz" ::: Clock Dom60, "clk_300mhz" ::: Clock Dom300)
pll clk =
  let (clkPixel, clkShift, _) =
        ecp5pll @_ @Dom25 (SSymbol @"tmds_pll") clk resetGen
          :: (Clock Dom60, Clock Dom300, Signal Dom60 Bool)
  in (clkPixel, clkShift)

makeTopEntity 'pll

stackMachine
  :: "clk_25mhz" ::: Clock Dom25
  -> "ftdi_txd" ::: Signal Dom25 Bit
  -> "ftdi_rxd" ::: Signal Dom25 Bit
stackMachine clk input =
  withClockResetEnable clk resetGen enableGen $
    input
    & override 8 high
    & uartRx (SNat @(HzToPeriod 115200))
    & processorRx
    & processor
    & fst . uartTx (SNat @(HzToPeriod 115200))

makeTopEntity 'stackMachine

dvi
  :: "clk_25mhz" ::: Clock Dom25
  -> "gpdi_dp" ::: Signal Dom600 (BitVector 4)
dvi clkIn =
  let (clkShift, clkPixel, locked) = ecp5pll (SSymbol @"tmds_pll") clkIn resetGen
        :: (Clock Dom300, Clock Dom60, Signal Dom300 Bool)

      (de, hsync, vsync, color) = withClockResetEnable clkPixel resetGen enableGen vgaPattern

      sdrOut :: Signal Dom300 (Vec 4 (BitVector 2))
      sdrOut = tmdsTx d2 clkPixel clkShift de locked (pure 0) (pure 0) color (fmap bitCoerce . bundle $ (vsync, hsync))

      sdrOutUnbundled :: Vec 4 (Signal Dom300 Bit, Signal Dom300 Bit)
      sdrOutUnbundled = map (unbundle . fmap bitCoerce) $ unbundle sdrOut

      ddrOutUnbundled :: Vec 4 (Signal Dom600 Bit)
      ddrOutUnbundled = map (\(x, y) -> oddrx1 y x clkShift resetGen) sdrOutUnbundled

      ddrOut :: Signal Dom600 (BitVector 4)
      ddrOut = fmap v2bv $ bundle ddrOutUnbundled

  in ddrOut

makeTopEntity 'dvi

snake
  :: "clk_25mhz" ::: Clock Dom25
  -> "btn" ::: Signal Dom25 (BitVector 7)
  -> "gpdi_dp" ::: Signal Dom250 (BitVector 4)
snake clk btns =
  let boardVgaOut@VGAOut{..} = withClockResetEnable clk resetGen enableGen (board btns)
      (clkShift, clkPixel, locked) = ecp5pll (SSymbol @"tmds_pll") clk resetGen :: (Clock Dom250, Clock Dom25, Signal Dom250 Bool)
      dviOut =
        tmdsTx d1 clkPixel clkShift (vgaDE vgaSync)
          locked
          (bitCoerce <$> vgaB)
          (bitCoerce <$> vgaG)
          (bitCoerce <$> vgaR)
          -- complement the sync signals because they're active low
          (bitCoerce <$> bundle (complement <$> vgaVSync vgaSync, complement <$> vgaHSync vgaSync))
  in bitCoerce <$> dviOut
  where
    -- board :: Signal Dom25 (BitVector 7) -> VGAOut Dom25 8 8 8
    board btns = vgaOut vgaSync rgb
      where
        dirBtns :: Signal Dom25 (BitVector 4)
        dirBtns = slice d6 d3 <$> btns

        up, down, left, right :: Signal Dom25 Bool
        (up, down, left, right) = unbundle $ fmap (bitCoerce . map (== high) . reverse . bv2v) dirBtns

        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        inputs = Snake.MkInputs <$> up <*> down <*> left <*> right

        st = regEn Snake.initState frameEnd $ Snake.updateState <$> inputs <*> st

        rgb :: Signal Dom25 (Unsigned 8, Unsigned 8, Unsigned 8)
        rgb = fmap (maybe (0, 0, 0) bitCoerce) $
            liftA2 <$> (Snake.draw <$> st) <*> x <*> y
          where
            (x, _) = scale (SNat @20) . center $ vgaX
            (y, _) = scale (SNat @20) . center $ vgaY

makeTopEntity 'snake
