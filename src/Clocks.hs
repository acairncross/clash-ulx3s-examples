{-# LANGUAGE NumericUnderscores #-}

-- createDomain creates an ophan instance of KnownDomain
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clocks where

import Clash.Prelude

-- Domain with 25MHz clock
createDomain vXilinxSystem{vName="Dom25", vPeriod=hzToPeriod 25_000_000}
createDomain vXilinxSystem{vName="Dom125", vPeriod=hzToPeriod 125_000_000}
createDomain vXilinxSystem{vName="Dom250", vPeriod=hzToPeriod 250_000_000}

createDomain vXilinxSystem{vName="Dom60", vPeriod=hzToPeriod 62_500_000}
createDomain vXilinxSystem{vName="Dom300", vPeriod=hzToPeriod 312_500_000}
createDomain vXilinxSystem{vName="Dom600", vPeriod=hzToPeriod 625_000_000}

createDomain vXilinxSystem{vName="Dom80", vPeriod=hzToPeriod 80_000_000}
createDomain vXilinxSystem{vName="Dom400", vPeriod=hzToPeriod 400_000_000}
createDomain vXilinxSystem{vName="Dom800", vPeriod=hzToPeriod 800_000_000}

createDomain vXilinxSystem{vName="Dom65", vPeriod=hzToPeriod 65_000_000}
createDomain vXilinxSystem{vName="Dom325", vPeriod=hzToPeriod 325_000_000}
createDomain vXilinxSystem{vName="Dom650", vPeriod=hzToPeriod 650_000_000}

-- Divide 1s by rate - type level version of hzToPeriod
type HzToPeriod (rate :: Nat) = Seconds 1 `Div` rate

type Seconds      (s  :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps
