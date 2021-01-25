{-# LANGUAGE NumericUnderscores #-}

module StackMachine where

import Clash.Prelude hiding (last)
import Clash.Annotations.BitRepresentation

import Control.Monad.State

import Utils

-- Core -----------------------------------------------------------------------
data Instr
  = Push (Signed 8)
  | Pop
  | Add
  | Mul
  deriving (Generic, NFDataX, Eq, Show)

{-# ANN module (DataReprAnn $(liftQ [t|Instr|]) 16
  [ ConstrRepr 'Push 0b0000_0011_0000_0000 0b0000_0000_0000_0000 [0b0000_0000_1111_1111]
  , ConstrRepr 'Pop  0b0000_0011_0000_0000 0b0000_0001_0000_0000 []
  , ConstrRepr 'Add  0b0000_0011_0000_0000 0b0000_0010_0000_0000 []
  , ConstrRepr 'Mul  0b0000_0011_0000_0000 0b0000_0011_0000_0000 []
  ]) #-}

instance BitPack Instr where
  type BitSize Instr = 16
  pack instr = case instr of
    Push n -> zeroExtend (pack n)
    Pop -> 0b01 `shiftL` 8
    Add -> 0b10 `shiftL` 8
    Mul -> 0b11 `shiftL` 8

  unpack bv = case truncateB (bv `shiftR` 8) :: BitVector 2 of
    0b00 -> Push $ bitCoerce (truncateB bv)
    0b01 -> Pop
    0b10 -> Add
    0b11 -> Mul
    _ -> error "impossible"

processorT
  :: KnownNat n
  => Maybe Instr
  -> State (Index n, Vec n (Signed 8)) (Maybe (BitVector 8))
processorT instrM = get >>= \(sp, stack) ->
  case instrM of
    Nothing -> return Nothing
    Just instr -> case instr of
      Push x -> do
        put (sp+1, replace sp x stack)
        return Nothing
      Pop -> do
        let sp' = sp-1
        put (sp', stack)
        return (Just $ pack (stack !! sp'))
      Add -> do
        let lhs = stack !! (sp-2)
        let rhs = stack !! (sp-1)
        put (sp-1, replace (sp-2) (lhs + rhs) stack)
        return Nothing
      Mul -> do
        let lhs = stack !! (sp-2)
        let rhs = stack !! (sp-1)
        put (sp-1, replace (sp-2) (lhs * rhs) stack)
        return Nothing

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
