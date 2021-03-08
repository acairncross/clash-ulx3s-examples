{-# LANGUAGE NumericUnderscores #-}

-- createDomain creates an ophan instance of KnownDomain
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clocks where

import Clash.Prelude

import Utils (exactHzToPeriod)

-- Domain with 25MHz clock
createDomain vXilinxSystem{vName="Dom25", vPeriod=exactHzToPeriod 25_000_000}
createDomain vXilinxSystem{vName="Dom125", vPeriod=exactHzToPeriod 125_000_000}
createDomain vXilinxSystem{vName="Dom250", vPeriod=exactHzToPeriod 250_000_000}

-- Divide 1s by rate, rounding up - type level version of hzToPeriod
type HzToPeriod (rate :: Nat) = (Seconds 1 + rate - 1) `Div` rate

type Seconds      (s  :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps
