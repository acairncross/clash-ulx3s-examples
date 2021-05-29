{-# LANGUAGE MagicHash #-}

module Top where

import Clash.Prelude
import Clash.Annotations.TH (makeTopEntity)
import Control.Monad.State
import Data.Function ((&))

import Clocks
import DVI
import ECP5
import qualified RAM
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
