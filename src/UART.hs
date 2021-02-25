module UART (uartRx, uartTx) where

import Clash.Prelude
import Control.Monad.State
import Data.Word

import Utils

data RxState n
  = RxIdle
  | RxBit Word32 (RxBit n)
  deriving (Generic, NFDataX, Eq, Show)

data RxBit n
  = RxStartBit
  | RxDataBit (BitVector n) (Index n)
  | RxStopBit (BitVector n)
  deriving (Generic, NFDataX, Eq, Show)

uartRxT :: KnownNat n => Word32 -> Bit -> State (RxState n) (Maybe (BitVector n))
uartRxT clocksPerBaud input = get >>= \case
  RxIdle -> do
    when (input == low) $ put (RxBit 0 RxStartBit)
    return Nothing
  RxBit cnt rxBit -> do
    let cnt1 = cnt + 1
    let baudHalfDone = cnt1 == clocksPerBaud `shiftR` 1
    let baudDone = cnt1 == clocksPerBaud
    let cnt' = if baudDone then 0 else cnt1
    case rxBit of
      RxStartBit -> do
        let rxBit' = if baudDone then RxDataBit 0 0 else RxStartBit
        put $ RxBit cnt' rxBit'
        return Nothing
      RxDataBit datum i -> do
        if baudHalfDone
          then put $ RxBit cnt' (RxDataBit (shiftBitR datum input) i)
          else if baudDone
            then put $ RxBit cnt' (if i == maxBound then RxStopBit datum else RxDataBit datum (i+1))
            else put $ RxBit cnt' rxBit
        return Nothing
      RxStopBit datum ->
        if baudHalfDone
          then put RxIdle >> return (Just datum)
          else put (RxBit cnt' rxBit) >> return Nothing
    
-- | Receives an 8N1 UART input. Expects LSB first.
uartRx
  :: forall dom baudDuration
   . HiddenClockResetEnable dom
  => SNat baudDuration
  -- ^ Duration of baud in picoseconds
  -> Signal dom Bit
  -- ^ UART Rx
  -> Signal dom (Maybe (BitVector 8))
  -- ^ Output byte
uartRx baudDuration urx =
  let clocksPerBaud = fromIntegral $
        snatToInteger baudDuration `div` snatToInteger (clockPeriod @dom)
      urx'' = register high . register high $ urx
  in mealyState (uartRxT clocksPerBaud) RxIdle urx''

data TxState n
  = TxIdle
  | TxBit Word32 (TxBit n)
  deriving (Generic, NFDataX, Eq, Show)

data TxBit n
  = TxStartBit (BitVector n)
  | TxDataBit (BitVector n) (Index n)
  | TxStopBit
  deriving (Generic, NFDataX, Eq, Show)

uartTxT :: KnownNat n => Word32 -> Maybe (BitVector n) -> State (TxState n) (Bit, Bool)
uartTxT clocksPerBaud input = get >>= \case
  TxIdle -> case input of
    Just input' -> put (TxBit 0 (TxStartBit input')) >> return (low, True)
    Nothing -> return (high, False)
  TxBit cnt txBit -> do
    let cnt1 = cnt + 1
    let baudDone = cnt1 == clocksPerBaud
    let cnt' = if baudDone then 0 else cnt1
    case txBit of
      TxStartBit datum -> do
        put $ if baudDone
          then TxBit cnt' (TxDataBit datum 0)
          else TxBit cnt' txBit
        return (low, True)
      TxDataBit datum i -> do
        put $ if baudDone
          then TxBit cnt' $
            if i == maxBound then TxStopBit else TxDataBit (rotateR datum 1) (i+1)
          else TxBit cnt' (TxDataBit datum i)
        return (lsb datum, True)
      TxStopBit ->
        if baudDone
          then put TxIdle >> return (high, False)
          else put (TxBit cnt' TxStopBit) >> return (high, True)

uartTx
  :: forall dom baudDuration
   . HiddenClockResetEnable dom
  => SNat baudDuration
  -> Signal dom (Maybe (BitVector 8))
  -> (Signal dom Bit, Signal dom Bool)
uartTx baudDuration utx =
  let clocksPerBaud = fromIntegral $
        snatToInteger baudDuration `div` snatToInteger (clockPeriod @dom)
      utx'' = register Nothing . register Nothing $ utx
  in unbundle $ mealyState (uartTxT clocksPerBaud) TxIdle utx''
