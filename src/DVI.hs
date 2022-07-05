{-# LANGUAGE NumericUnderscores #-}

module DVI where

import Control.Monad.State
import Clash.Explicit.Prelude (dualFlipFlopSynchronizer)
import Clash.Prelude

import RAM.TH
import Utils

xnor :: Bits a => a -> a -> a
xnor x y = complement (x `xor` y)

popCountBV :: forall n a. KnownNat n => 1 <= n => Integral a => BitVector n -> a
popCountBV bv =
  let v = bv2v bv
  in sum (map (fromIntegral . pack) v)

-- | Given a TMDS encoded pixel, output the channels in serial, including a
-- pixel clock.
tmdsPiso
  :: forall dom dataRate
   . HiddenClockResetEnable dom
  => 1 <= dataRate
  => dataRate <= 2
  => SNat dataRate
  -> Signal dom (BitVector 10)
  -> Signal dom (BitVector 10)
  -> Signal dom (BitVector 10)
  -> Signal dom (Vec 4 (BitVector dataRate))
tmdsPiso dataRate@SNat bs gs rs =
  mealyState tmdsPisoT (0, 0, 0, 0b11111_00000) (bundle (bs, gs, rs))
  where
    lsbs :: SNat a -> BitVector (a+b) -> BitVector a
    lsbs SNat = truncateB

    tmdsPisoT
      :: (BitVector 10, BitVector 10, BitVector 10)
      -> State (BitVector 10, BitVector 10, BitVector 10, BitVector 10) (Vec 4 (BitVector dataRate))
    tmdsPisoT (bIn, gIn, rIn) = get >>= \(b, g, r, c) -> do
      let c' = c `rotateR` snatToNum dataRate
      let c'mid = v2bv $ take d2 $ drop d4 $ bv2v c'
      let b' = if c'mid == 0b10 then bIn else b `rotateR` snatToNum dataRate
      let g' = if c'mid == 0b10 then gIn else g `rotateR` snatToNum dataRate
      let r' = if c'mid == 0b10 then rIn else r `rotateR` snatToNum dataRate
      put (b', g', r', c')
      return $ bitCoerce ((leToPlus @dataRate @10 (lsbs dataRate c, lsbs dataRate r, lsbs dataRate g, lsbs dataRate b)) :: (BitVector dataRate, BitVector dataRate, BitVector dataRate, BitVector dataRate))

-- | A 1920x1080 test pattern
vgaPattern
  :: HiddenClockResetEnable dom
  => (Signal dom Bool, Signal dom Bit, Signal dom Bit, Signal dom (BitVector 8))
  -- ^ Data enable, HSync, VSync, Brightness
vgaPattern = unbundle $ mealyState vgaPatternT (0, 0) (pure ())
  where
    vgaPatternT :: () -> State (BitVector 12, BitVector 12) (Bool, Bit, Bit, BitVector 8)
    vgaPatternT () = get >>= \(y, x) -> do
      put $ if x+1 == 1950 then (if y+1 == 1096 then 0 else y+1, 0) else (y, x+1)
      return
        ( x < 1920 && y < 1080
        , if x >= 1930 && x < 1940 then 1 else 0
        , if y >= 1085 && y < 1091 then 1 else 0
        , if (x .&. 0x4 > 0 && not (y .&. 0x4 > 0)) || (not (x .&. 0x4 > 0) && y .&. 0x4 > 0) then 255 else 0
        )

tileRom
  :: forall dom
   . HiddenClockResetEnable dom
  => Signal dom (BitVector 12)
  -- ^ x
  -> Signal dom (BitVector 12)
  -- ^ y
  -> (Signal dom (BitVector 8), Signal dom (BitVector 8), Signal dom (BitVector 8))
  -- ^ Red, Green, Blue (delayed by 1 cycle)
tileRom x y =
  let dataOut :: Signal dom (BitVector 24)
      dataOut =
        romFile
          (SNat @12) -- 4Kb
          $(tilePathTH)
          (liftA2 (\x' y' -> (shiftL (y' .&. 0xf) 4) .|. (x' .&. 0xf)) x y)
  in unbundle (fmap bitCoerce dataOut :: Signal dom (BitVector 8, BitVector 8, BitVector 8))

-- | A TMDS transmitter
tmdsTx
  :: forall domPixel domBit dataRate
   . KnownDomain domPixel
  => KnownDomain domBit
  => DomainPeriod domPixel ~ ((Div 10 dataRate) * DomainPeriod domBit)
  => 1 <= dataRate
  => dataRate <= 2
  => SNat dataRate
  -> Clock domPixel
  -- ^ Pixel clock
  -> Clock domBit
  -- ^ Bit clock
  -> Signal domPixel Bool
  -- ^ Data enable
  -> Signal domBit Bool
  -- ^ Bit clock locked
  -> Signal domPixel (BitVector 8)
  -- ^ Blue
  -> Signal domPixel (BitVector 8)
  -- ^ Green
  -> Signal domPixel (BitVector 8)
  -- ^ Red
  -> Signal domPixel (BitVector 2)
  -- ^ Ctrl
  -> Signal domBit (Vec 4 (BitVector dataRate))
  -- ^ Blue, Green, Red, Clock
tmdsTx dataRate clkPixel clkBit de locked blueIn greenIn redIn ctrl =
  let blueOut = dualFlipFlopSynchronizer clkPixel clkBit resetGen (toEnable locked) 0 $
        tmdsChannel clkPixel de ctrl blueIn

      greenOut = dualFlipFlopSynchronizer clkPixel clkBit resetGen (toEnable locked) 0 $
        tmdsChannel clkPixel de (pure 0b00) greenIn

      redOut = dualFlipFlopSynchronizer clkPixel clkBit resetGen (toEnable locked) 0 $
        tmdsChannel clkPixel de (pure 0b00) redIn

  in withClockResetEnable clkBit resetGen enableGen $ tmdsPiso dataRate blueOut greenOut redOut

-- | A single TMDS channel, or encoder. A TMDS transmitter comprises 3 of these.
tmdsChannel
  :: KnownDomain dom
  => Clock dom
  -- ^ Pixel clock
  -> Signal dom Bool
  -- ^ Data enable
  -> Signal dom (BitVector 2)
  -- ^ Ctrl, HSync and VSync for the first channel, low otherwise
  -> Signal dom (BitVector 8)
  -- ^ Data in
  -> Signal dom (BitVector 10)
  -- ^ Data out
tmdsChannel clk de ctrl din =
  withClockResetEnable clk resetGen enableGen $
    mealyState tmdsEncode 0 (bundle (de, ctrl, din))

tmdsEncode :: (Bool, BitVector 2, BitVector 8) -> State (Signed 8) (BitVector 10)
tmdsEncode (de, ctrl, din) = get >>= \cnt -> do
  let
    count0s :: BitVector 8 -> BitVector 4
    count0s = popCountBV . complement

    count1s :: BitVector 8 -> BitVector 4
    count1s = popCountBV

    count0sMinus1s :: BitVector 8 -> Signed 8
    count0sMinus1s bv =
      (bitCoerce . zeroExtend . count0s $ bv) - (bitCoerce . zeroExtend . count1s $ bv)

    count1sMinus0s :: BitVector 8 -> Signed 8
    count1sMinus0s bv =
      (bitCoerce . zeroExtend . count1s $ bv) - (bitCoerce . zeroExtend . count0s $ bv)

    -- Transition minimization method (XOR vs XNOR)
    tmMethod :: Bit
    tmMethod =
      if count1s din > 4 || (count1s din == 4 && lsb din == 0) then 0 else 1

    q_m :: BitVector 8
    q_m = v2bv $
      scanr (if tmMethod == 0 then xnor else xor) (last $ bv2v din) (init $ bv2v din)

  if de
    then do
      if cnt == 0 || count1s q_m == count0s q_m
        then do
          if tmMethod == 0
            then put $ cnt + count0sMinus1s q_m
            else put $ cnt + count1sMinus0s q_m
          return $ bitCoerce (complement tmMethod, tmMethod, if tmMethod == 1 then q_m else complement q_m)
        else if (cnt > 0 && count1s q_m > count0s q_m) || (cnt < 0 && count0s q_m > count1s q_m)
          then do
            let doubleTmMethod = if tmMethod == 0 then 0 else 2 :: Signed 8
            put $ cnt + doubleTmMethod + count0sMinus1s q_m
            return $ bitCoerce (1 :: Bit, tmMethod, complement q_m)
          else do
            let doubleNegTmMethod = if tmMethod == 0 then 2 else 0 :: Signed 8
            put $ cnt - doubleNegTmMethod + count1sMinus0s q_m
            return $ bitCoerce (0 :: Bit, tmMethod, q_m)
    else do
      put 0
      return $ case ctrl of
        0b00 -> 0b1101010100
        0b01 -> 0b0010101011
        0b10 -> 0b0101010100
        0b11 -> 0b1010101011
        _ -> error "Impossible"
