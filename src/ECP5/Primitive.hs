{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ECP5.Primitive (ecp5pllTF) where

import Prelude

import Control.Monad (guard)
import Control.Monad.State (State)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)

-- clash-prelude
import Clash.Signal (periodToHz)

-- clash-lib (blackbox types/functions)
import Clash.Backend (Backend (..), mkUniqueIdentifier)
import Clash.Netlist.BlackBox.Util (exprToString)
import Clash.Netlist.Id (IdType (Basic))
import Clash.Netlist.Types
import Data.Semigroup.Monad (Mon (getMon))
import Data.Text.Prettyprint.Doc.Extra (Doc)
import qualified Data.Text as TextS

ecp5pllTF :: TemplateFunction
ecp5pllTF = TemplateFunction used valid ecp5pllTemplate
  where
    used = [0..3] -- TODO Use reset
    valid = const True

-- Get the period of a KnownDomain constraint
extractPeriod :: (Expr, HWType, Bool) -> Integer
extractPeriod (_, Void (Just (KnownDomain _ period _ _ _ _)), _) = period
extractPeriod _ = error "expected blackbox argument to be a KnownDomain constraint"

ecp5pllTemplate :: Backend s => BlackBoxContext -> State s Doc
ecp5pllTemplate bbCtx = do
  -- Inputs
  let [ knownDomIn, knownDomOut, nameArg, clkArg, _rstArg ] = bbInputs bbCtx

  let inPeriod = extractPeriod knownDomIn
  let outPeriod = extractPeriod knownDomOut

  let (instNameE, _, _) = nameArg
  let Just instName = TextS.pack <$> exprToString instNameE

  let (clk, clkTy, _) = clkArg

  -- Result
  let (Identifier result Nothing, resTy@(Product _ _ [clkOutTy, _])) = bbResult bbCtx

  clkOut <- mkUniqueIdentifier Basic "clkOut"
  locked <- mkUniqueIdentifier Basic "locked"
  blockName <- mkUniqueIdentifier Basic "ecp5pll_block"

  let inHz = round $ periodToHz (fromIntegral inPeriod) * 1e-6 :: Int
  let outHz = round $ periodToHz (fromIntegral outPeriod) * 1e-6 :: Int
  let PllParams { refClkDiv, feedbackDiv, outputDiv } =
        fromMaybe (error "failed to find suitable PLL parameters")
          $ calculatePllParams (fromIntegral inHz) (fromIntegral outHz)

  getMon $ blockDecl blockName
    [ NetDecl Nothing locked Bit
    , NetDecl Nothing clkOut clkOutTy
    , InstDecl Comp Nothing "EHXPLLL" instName
        [ stringParam "PLLRST_ENA" "DISABLED" -- TODO Enable
        , stringParam "INTFB_WAKE" "DISABLED"
        , stringParam "STDBY_ENABLE" "DISABLED"
        , stringParam "DPHASE_SOURCE" "DISABLED"
        , stringParam "OUTDIVIDER_MUXA" "DIVA"
        , stringParam "OUTDIVIDER_MUXB" "DIVB"
        , stringParam "OUTDIVIDER_MUXC" "DIVC"
        , stringParam "OUTDIVIDER_MUXD" "DIVD"
        , intParam "CLKI_DIV" (toInteger refClkDiv)
        , stringParam "CLKOP_ENABLE" "ENABLED"
        , intParam "CLKOP_DIV" (toInteger outputDiv)
        , intParam "CLKOP_CPHASE" 0
        , intParam "CLKOP_FPHASE" 0
        , stringParam "FEEDBK_PATH" "CLKOP"
        , intParam "CLKFB_DIV" (toInteger feedbackDiv)
        ]
        [ (Identifier "RST" Nothing, In, Bit, Literal Nothing (BitLit L)) -- TODO Enable
        , (Identifier "STDBY" Nothing, In, Bit, Literal Nothing (BitLit L))
        , (Identifier "CLKI" Nothing, In, clkTy, clk)
        , (Identifier "CLKOP" Nothing, Out, clkOutTy, Identifier clkOut Nothing)
        , (Identifier "CLKFB" Nothing, Out, clkOutTy, Identifier clkOut Nothing)
        , (Identifier "PHASESEL0" Nothing, In, Bit, Literal Nothing (BitLit L))
        , (Identifier "PHASESEL1" Nothing, In, Bit, Literal Nothing (BitLit L))
        , (Identifier "PHASEDIR" Nothing, In, Bit, Literal Nothing (BitLit H))
        , (Identifier "PHASESTEP" Nothing, In, Bit, Literal Nothing (BitLit H))
        , (Identifier "PHASELOADREG" Nothing, In, Bit, Literal Nothing (BitLit H))
        , (Identifier "PLLWAKESYNC" Nothing, In, Bit, Literal Nothing (BitLit L))
        , (Identifier "ENCLKOP" Nothing, In, Bit, Literal Nothing (BitLit L))
        , (Identifier "LOCK" Nothing, Out, Bit, Identifier locked Nothing)
        ]
    , Assignment result
        ( DataCon resTy (DC (resTy, 0))
          [ Identifier clkOut Nothing
          , Identifier locked Nothing
          ]
        )
    ]
  where
    stringParam :: Identifier -> String -> (Expr, HWType, Expr)
    stringParam ident value =
      (Identifier ident Nothing, String, Literal Nothing (StringLit value))

    intParam :: Identifier -> Integer -> (Expr, HWType, Expr)
    intParam ident value =
      (Identifier ident Nothing, Integer, Literal Nothing (NumLit value))

-- Generating PLL parameters from input/output frequencies

data PllParams = PllParams
  { refClkDiv :: !Int
  , feedbackDiv :: !Int
  , outputDiv :: !Int
  , primaryCPhase :: !Int
  , fOut :: !Float -- ^ In MHz
  , fVco :: !Float -- ^ In MHz
  } deriving Show

pfdMin :: Float
pfdMin = 3.125

pfdMax :: Float
pfdMax = 400

vcoMin :: Float
vcoMin = 400

vcoMax :: Float
vcoMax = 800

vcoOpt :: Float
vcoOpt = (vcoMin + vcoMax) / 2

-- Copied from Project Trellis's ecppll
calculatePllParams :: Float -> Float -> Maybe PllParams
calculatePllParams inputf outputf = minOrNothing $ do
  inputDiv <- [1..128]
  let fpfd = inputf / fromIntegral inputDiv
  guard $ fpfd >= pfdMin && fpfd <= pfdMax

  feedbackDiv <- [1..80]
  outputDiv <- [1..128]
  let fvco = fpfd * fromIntegral feedbackDiv * fromIntegral outputDiv
  guard $ fvco >= vcoMin && fvco <= vcoMax
  let fout = fvco / fromIntegral outputDiv
  return PllParams
    { refClkDiv = inputDiv
    , feedbackDiv = feedbackDiv
    , outputDiv = outputDiv
    , fOut = fout
    , fVco = fvco
    , primaryCPhase =
        let nsPhase = 1 / (fout * 1e6) * 0.5 in floor $ nsPhase * fvco * 1e6
    }
  where
    minOrNothing :: [PllParams] -> Maybe PllParams
    minOrNothing [] = Nothing
    minOrNothing xs = Just $ minimumBy (compare `on` pllBadness) xs

    pllBadness :: PllParams -> (Float, Float)
    pllBadness params = (abs $ fOut params - outputf, abs $ fVco params - vcoOpt)
