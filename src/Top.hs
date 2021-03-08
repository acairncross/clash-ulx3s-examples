{-# LANGUAGE MagicHash #-}

module Top where

import Clash.Prelude
import Clash.Annotations.TH (makeTopEntityWithName)
import Control.Monad.State
import Data.Function ((&))

import Clocks
import DVI
import ECP5
import RAM
import StackMachine
import UART
import Utils

counter
  :: forall n dom
   . HiddenClockResetEnable dom
  => KnownNat n
  => Signal dom (BitVector n)
counter = register 0 ((1+) <$> counter)

slowCounter
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 8)
slowCounter = mealyState (\() -> slowCounterT) 0 (pure ())
  where
    slowCounterT :: State (BitVector 32) (BitVector 8)
    slowCounterT = do
      internalCounter <- get
      let internalCounter' = internalCounter + 1
      put internalCounter'
      return $ truncateB (internalCounter' `shiftR` 22)

topCounter
  :: "clk_25mhz" ::: Clock Dom25
  -> "led" ::: Signal Dom25 (BitVector 8)
topCounter clk =
  withClockResetEnable clk resetGen enableGen $
    fmap (truncateB . (`shiftR` 22)) (counter @32)
    -- slowCounter

makeTopEntityWithName 'topCounter "counter"

topRam
  :: "clk_25mhz" ::: Clock Dom25
  -> "ftdi_txd" ::: Signal Dom25 Bit
  -> "ftdi_rxd" ::: Signal Dom25 Bit
topRam clk input =
  withClockResetEnable clk resetGen enableGen $
    input
    & override 8 high
    & uartRx (SNat @(HzToPeriod 115200))
    & ramRx
    & ram
    & fst . uartTx (SNat @(HzToPeriod 115200))

makeTopEntityWithName 'topRam "ram"

topStackMachine
  :: "clk_25mhz" ::: Clock Dom25
  -> "ftdi_txd" ::: Signal Dom25 Bit
  -> "ftdi_rxd" ::: Signal Dom25 Bit
topStackMachine clk input =
  withClockResetEnable clk resetGen enableGen $
    input
    & override 8 high
    & uartRx (SNat @(HzToPeriod 115200))
    & processorRx
    & processor
    & fst . uartTx (SNat @(HzToPeriod 115200))

makeTopEntityWithName 'topStackMachine "stackMachine"

topDvi
  :: "clk_25mhz" ::: Clock Dom25
  -> "gpdi_dp" ::: Signal Dom250 (BitVector 4)
topDvi clk =
  let (clkShift, locked) = ecp5pll (SSymbol @"tmds_pll") clk resetGen

      (de, hsync, vsync, color) = withClockResetEnable clk resetGen enableGen vgaPattern

      sdrOut :: Signal Dom125 (Vec 4 (BitVector 2))
      sdrOut = tmdsTx clk clkShift de locked (pure 0) (pure 0) color (fmap bitCoerce . bundle $ (vsync, hsync))

      sdrOutUnbundled :: Vec 4 (Signal Dom125 Bit, Signal Dom125 Bit)
      sdrOutUnbundled = map (unbundle . fmap bitCoerce) $ unbundle sdrOut

      ddrOutUnbundled :: Vec 4 (Signal Dom250 Bit)
      ddrOutUnbundled = map (\(x, y) -> oddrx1 y x clkShift resetGen) sdrOutUnbundled

      ddrOut :: Signal Dom250 (BitVector 4)
      ddrOut = fmap v2bv $ bundle ddrOutUnbundled

  in ddrOut

makeTopEntityWithName 'topDvi "dvi"
