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
import Clash.Backend (Backend (..))
import Clash.Netlist.BlackBox.Util (exprToString)
import Clash.Netlist.Id (addRaw, makeBasic)
import Clash.Netlist.Types
import Clash.Netlist.Util (instPort)
import Data.Semigroup.Monad (Mon (getMon))
import Data.Text.Prettyprint.Doc.Extra (Doc)
import qualified Data.Text as TextS

ecp5pllTF :: TemplateFunction
ecp5pllTF = TemplateFunction used valid ecp5pllTemplate
  where
    used = [0..4]
    valid = const True

-- Get the period of a KnownDomain constraint
extractPeriod :: (Expr, HWType, Bool) -> Integer
extractPeriod (_, Void (Just (KnownDomain _ period _ _ _ _)), _) = period
extractPeriod _ = error "expected blackbox argument to be a KnownDomain constraint"

ecp5pllTemplate :: Backend s => BlackBoxContext -> State s Doc
ecp5pllTemplate bbCtx = do
  -- Inputs
  let [ knownDomIn, knownDomOut, nameArg, clkArg, rstArg ] = bbInputs bbCtx

  let inPeriod = extractPeriod knownDomIn
  let outPeriod = extractPeriod knownDomOut

  let (instNameE, _, _) = nameArg
  let Just instNameT = TextS.pack <$> exprToString instNameE
  instName <- makeBasic instNameT

  let (clk, clkTy, _) = clkArg
  let (rst, rstTy, _) = rstArg

  -- Result
  let [(Identifier result Nothing, resTy@(Product _ _ [clkOutTy, _]))] = bbResults bbCtx

  clkOut <- makeBasic "clkOut"
  locked <- makeBasic "locked"
  blockName <- makeBasic "ecp5pll_block"

  let inHz = round $ periodToHz (fromIntegral inPeriod) * 1e-6 :: Int
  let outHz = round $ periodToHz (fromIntegral outPeriod) * 1e-6 :: Int
  let PllParams { refClkDiv, feedbackDiv, outputDiv } =
        fromMaybe (error "failed to find suitable PLL parameters")
          $ calculatePllParams (fromIntegral inHz) (fromIntegral outHz)

  primName <- addRaw "EHXPLLL"

  getMon $ blockDecl blockName
    [ NetDecl Nothing locked Bit
    , NetDecl Nothing clkOut clkOutTy
    , InstDecl Comp Nothing [] primName instName
        [ stringParam "PLLRST_ENA" "ENABLED"
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
        ( NamedPortMap
        [ (instPort "RST", In, rstTy, rst)
        , (instPort "STDBY", In, Bit, Literal Nothing (BitLit L))
        , (instPort "CLKI", In, clkTy, clk)
        , (instPort "CLKOP", Out, clkOutTy, Identifier clkOut Nothing)
        , (instPort "CLKFB", Out, clkOutTy, Identifier clkOut Nothing)
        , (instPort "PHASESEL0", In, Bit, Literal Nothing (BitLit L))
        , (instPort "PHASESEL1", In, Bit, Literal Nothing (BitLit L))
        , (instPort "PHASEDIR", In, Bit, Literal Nothing (BitLit H))
        , (instPort "PHASESTEP", In, Bit, Literal Nothing (BitLit H))
        , (instPort "PHASELOADREG", In, Bit, Literal Nothing (BitLit H))
        , (instPort "PLLWAKESYNC", In, Bit, Literal Nothing (BitLit L))
        , (instPort "ENCLKOP", In, Bit, Literal Nothing (BitLit L))
        , (instPort "LOCK", Out, Bit, Identifier locked Nothing)
        ])
    , Assignment result
        ( DataCon resTy (DC (resTy, 0))
          [ Identifier clkOut Nothing
          , Identifier locked Nothing
          ]
        )
    ]
  where
    stringParam :: TextS.Text -> String -> (Expr, HWType, Expr)
    stringParam ident value =
      (instPort ident, String, Literal Nothing (StringLit value))

    intParam :: TextS.Text -> Integer -> (Expr, HWType, Expr)
    intParam ident value =
      (instPort ident, Integer, Literal Nothing (NumLit value))

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
