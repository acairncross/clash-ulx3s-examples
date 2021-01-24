{-# LANGUAGE NumericUnderscores #-}

module StackMachine where

import Clash.Prelude hiding (last)
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

import Control.Monad.State

import Utils

-- Core -----------------------------------------------------------------------
data Instr
  = Push (Signed 8)
  | Pop
  | Add
  | Mul
  deriving (Generic, NFDataX, Eq, Show)

-- Note: This overspecifies the Pop, Add and Mul representations to make the
-- derived BitPack instance more usable.
{-# ANN module (DataReprAnn $(liftQ [t|Instr|]) 16
  [ ConstrRepr 'Push 0b1111_1111_0000_0000 0b0000_0000_0000_0000 [0b0000_0000_1111_1111]
  , ConstrRepr 'Pop  0b1111_1111_1111_1111 0b0000_0001_0000_0000 []
  , ConstrRepr 'Add  0b1111_1111_1111_1111 0b0000_0010_0000_0000 []
  , ConstrRepr 'Mul  0b1111_1111_1111_1111 0b0000_0011_0000_0000 []
  ]) #-}
deriveBitPack [t|Instr|]

processorT
  :: KnownNat n
  => Maybe Instr
  -> State (Index n, Vec n (Signed 8)) (Maybe (BitVector 8))
processorT instrM = get >>= \(sp, stack) ->
  case instrM of
    Nothing -> return Nothing
    Just instr -> case instr of
      Push x -> put (sp+1, replace sp x stack) >> return Nothing
      Pop -> let sp' = sp-1 in put (sp', stack) >> return (Just $ pack (stack !! sp'))
      Add -> let sp' = sp-1 in put (sp', replace (sp-2) ((stack !! (sp-2)) + (stack !! (sp-1))) stack) >> return Nothing
      Mul -> let sp' = sp-1 in put (sp', replace (sp-2) ((stack !! (sp-2)) * (stack !! (sp-1))) stack) >> return Nothing

processor
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe Instr)
  -> Signal dom (Maybe (BitVector 8))
processor = mealyState processorT (0, replicate d16 0)

processorRxT
  :: Maybe (BitVector 8)
  -> State (BitVector 16, Bool) (Maybe (BitVector 16))
processorRxT inputM = get >>= \(acc, last) ->
  case inputM of
    Nothing -> return Nothing
    Just input -> if last
      then put (0, False) >> return (Just $ zeroExtend input .|. acc)
      else put (zeroExtend input `shiftL` 8, True) >> return Nothing

-- | Receive bytes and and emit instructions
processorRx
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 8))
  -> Signal dom (Maybe Instr)
processorRx input =
  (fmap . fmap) bitCoerce $ mealyState processorRxT (0, False) input
