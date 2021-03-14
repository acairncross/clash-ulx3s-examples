{-# LANGUAGE NumericUnderscores #-}

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
