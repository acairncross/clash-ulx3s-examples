module RAM (RamOp(..), defaultRamOp, ram, ramRx) where

import Clash.Prelude hiding (blockRam)
import Clash.Explicit.Prelude (blockRam)

data RamOp = RamOp
  { ramClockEnable :: Bool
  , ramReadAddr :: BitVector 14
  , ramWriteEnable :: Bool
  , ramWriteAddr :: BitVector 14
  , ramWriteData :: BitVector 8
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

defaultRamOp :: RamOp
defaultRamOp = RamOp
  { ramClockEnable = False
  , ramReadAddr = 0
  , ramWriteEnable = False
  , ramWriteAddr = 0
  , ramWriteData = 0
  }

ram
  :: HiddenClockResetEnable dom
  => Signal dom RamOp
  -> Signal dom (Maybe (BitVector 8))
ram op =
  let valid = register False (ramClockEnable <$> op)
      dataOut =
        blockRam
          hasClock
          (toEnable $ ramClockEnable <$> op)
          (replicate (SNat @2048) 0)
          (ramReadAddr <$> op)
          ((\(we, addr, din) -> if we then Just (addr, din) else Nothing) <$> bundle (ramWriteEnable <$> op, ramWriteAddr <$> op, ramWriteData <$> op))
  in (\b x -> if b then Just x else Nothing) <$> valid <*> dataOut

ramRx
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 8))
  -> Signal dom RamOp
ramRx = mealy ramRxT (0 :: Index 5, 0 :: BitVector 38)

ramRxT :: (Index 5, BitVector 38) -> Maybe (BitVector 8) -> ((Index 5, BitVector 38), RamOp)
ramRxT (i, acc) Nothing = ((i, acc), defaultRamOp)
ramRxT (i, acc) (Just x)
  | i < maxBound = ((i+1, (acc `shiftL` 8) .|. zeroExtend x), defaultRamOp)
  | otherwise = ((0, 0), let ramOpBV = bitCoerce ((acc `shiftL` 8) .|. zeroExtend x) in unpack ramOpBV)
