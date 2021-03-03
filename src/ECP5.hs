module ECP5 (ecp5pll) where

import Clash.Clocks (Clocks (..))
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
