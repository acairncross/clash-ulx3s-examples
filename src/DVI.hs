module DVI where

import Control.Monad.State
import Clash.Prelude hiding (d0, d7)

import Utils

xnor :: Bits a => a -> a -> a
xnor x y = complement (x `xor` y)

popCountBV :: forall n a. KnownNat n => 1 <= n => Integral a => BitVector n -> a
popCountBV bv =
  let v = bv2v bv
  in sum (map (fromIntegral . pack) v)

-- | A TMDS transmitter
tmdsTx
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 8)
tmdsTx = undefined

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
      if count1s din > 4 || (count1s din == 4 && not (testBit din 0)) then 0 else 1

    q_m :: BitVector 8
    q_m = v2bv $
      scanr (if tmMethod == 0 then xnor else xor) (last $ bv2v din) (init $ bv2v din)

  if de
    then do
      put 0
      return $ case ctrl of
        0b00 -> 0b0010101011
        0b01 -> 0b1101010100
        0b10 -> 0b0010101010
        0b11 -> 0b1101010101
        _ -> error "Impossible"
    else do
      if cnt == 0 || count1s q_m == count0s q_m
        then do
          if tmMethod == 0
            then put $ cnt + count0sMinus1s q_m
            else put $ cnt + count1sMinus0s q_m
          return $ bitCoerce (complement tmMethod, tmMethod, (if tmMethod == 1 then q_m else complement q_m))
        else if (cnt > 0 && count1s q_m > count0s q_m) || (cnt < 0 && count0s q_m > count1s q_m)
          then do
            let doubleTmMethod = if tmMethod == 0 then 0 else 2 :: Signed 8
            put $ cnt + doubleTmMethod + count0sMinus1s q_m
            return $ bitCoerce (1 :: Bit, tmMethod, complement q_m)
          else do
            let doubleNegTmMethod = if tmMethod == 0 then 2 else 0 :: Signed 8
            put $ cnt - doubleNegTmMethod + count1sMinus0s q_m
            return $ bitCoerce (0 :: Bit, tmMethod, q_m)
