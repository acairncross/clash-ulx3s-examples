{-# LANGUAGE NumericUnderscores #-}

-- createDomain creates an ophan instance of KnownDomain
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import Clash.Prelude
import Control.Monad.State

-- Utils ----------------------------------------------------------------------
mealyState
  :: HiddenClockResetEnable dom
  => NFDataX s
  => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

shiftBitR :: forall n. KnownNat n => BitVector n -> Bit -> BitVector n
shiftBitR bs b =
  let (bs', _) = bitCoerce $ pack b ++# bs :: (BitVector n, Bit)
  in bs'

stickify
  :: HiddenClockResetEnable dom
  => NFDataX a
  => Signal dom (Maybe a)
  -> Signal dom (Maybe a)
stickify xm = let ym = liftA2 (<|>) xm (register Nothing ym) in ym

override :: HiddenClockResetEnable dom => BitVector 8 -> Bit -> Signal dom Bit -> Signal dom Bit
override numCycles x = mealy f 0
  where
    f :: BitVector 8 -> Bit -> (BitVector 8, Bit)
    f count y = if count == numCycles then (count, y) else (count+1, x)

-- Clock domains --------------------------------------------------------------

-- Domain with 25MHz clock
createDomain vXilinxSystem{vName="Dom25", vPeriod=hzToPeriod 25e6}
-- Domain with 250MHz clock
createDomain vXilinxSystem{vName="Dom250", vPeriod=hzToPeriod 250e6}

-- Divide 1s by rate, rounding up - type level version of hzToPeriod
type HzToPeriod (rate :: Nat) = (Seconds 1 + rate - 1) `Div` rate

type Seconds      (s  :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps
