module ECP5 (ecp5pll, oddrx1) where

import Clash.Clocks (Clocks (..))
import Clash.Explicit.Signal (unsafeSynchronizer)
import Clash.Prelude

import Clash.Annotations.Primitive (Primitive (InlinePrimitive), HDL (Verilog, VHDL))
import qualified Data.String.Interpolate as I
import Data.String.Interpolate.Util (unindent)

ecp5pll
  :: forall domOut domIn name
   . (KnownDomain domIn, KnownDomain domOut)
  => SSymbol name
  -> Clock domIn
  -> Reset domIn
  -> (Clock domOut, Signal domOut Bool)
ecp5pll !_ = knownDomain @domIn `seq` knownDomain @domOut `seq` clocks
{-# NOINLINE ecp5pll #-}

-- Note: Untested for VHDL
{-# ANN ecp5pll (InlinePrimitive [Verilog, VHDL] $ unindent [I.i|
  [ { "BlackBox" :
      { "name" : "ECP5.ecp5pll"
      , "kind" : "Declaration"
      , "format" : "Haskell"
      , "templateFunction" : "ECP5.Primitive.ecp5pllTF"
      }
    }
  ]
|]) #-}

oddrx1
  :: forall domIn domOut
   . KnownDomain domIn
  => KnownDomain domOut
  => DomainPeriod domIn ~ (2 * DomainPeriod domOut)
  => Signal domIn Bit
  -> Signal domIn Bit
  -> Clock domIn
  -> Reset domIn
  -> Signal domOut Bit
oddrx1 x1 y1 !_ !_ =
  let x2 = unsafeSynchronizer (clockGen @domIn) (clockGen @domOut) x1
      y2 = unsafeSynchronizer (clockGen @domIn) (clockGen @domOut) y1
  in withClockResetEnable (clockGen @domOut) (resetGen @domOut) enableGen $
      mealyB mergeT False (x2, y2)
  where
    mergeT b (x, y) = (not b, if b then y else x)
{-# NOINLINE oddrx1 #-}
{-# ANN oddrx1 (InlinePrimitive [Verilog] $ unindent [I.i|
  [ { "BlackBox" :
      { "name" : "ECP5.oddrx1"
      , "kind" : "Declaration"
      , "template" :
"ODDRX1F
(
  .D0(~ARG[3]),
  .D1(~ARG[4]),
  .RST(~ARG[6]),
  .SCLK(~ARG[5]),
  .Q(~RESULT)
);"
      }
    }
  ]
|]) #-}
