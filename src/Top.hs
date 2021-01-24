{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}

-- createDomain creates an ophan instance of KnownDomain
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Top where

import Clash.Prelude
import Clash.Annotations.TH (makeTopEntity)
import Data.Function ((&))

import RAM
import StackMachine
import UART

-- Domain with 25MHz clock
createDomain vXilinxSystem{vName="Dom25", vPeriod=hzToPeriod 25e6}

-- Divide 1s by rate, rounding up - type level version of hzToPeriod
type HzToPeriod (rate :: Nat) = (Seconds 1 + rate - 1) `Div` rate

type Seconds      (s  :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps

counter
  :: forall n dom
   . HiddenClockResetEnable dom
  => KnownNat n
  => Signal dom (BitVector n)
counter = register 0 ((1+) <$> counter)

override :: HiddenClockResetEnable dom => BitVector 8 -> Bit -> Signal dom Bit -> Signal dom Bit
override numCycles x = mealy f 0
  where
    f :: BitVector 8 -> Bit -> (BitVector 8, Bit)
    f count y = if count == numCycles then (count, y) else (count+1, x)

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

makeTopEntity 'topRam

topCounter
  :: "clk_25mhz" ::: Clock Dom25
  -> "led" ::: Signal Dom25 (BitVector 8)
topCounter clk =
  withClockResetEnable clk resetGen enableGen $
    fmap (truncateB . (`shiftR` 22)) (counter @32)

makeTopEntity 'topCounter

topEntity
  :: "clk_25mhz" ::: Clock Dom25
  -> "ftdi_txd" ::: Signal Dom25 Bit
  -- -> "led" ::: Signal Dom25 (BitVector 8)
  -> "ftdi_rxd" ::: Signal Dom25 Bit
topEntity clk input =
  withClockResetEnable clk resetGen enableGen $
    input
    & override 8 high
    & uartRx (SNat @(HzToPeriod 115200))
    & processorRx
    & processor
    & fst . uartTx (SNat @(HzToPeriod 115200))

makeTopEntity 'topEntity
