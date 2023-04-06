{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ECP5.Primitive (ecp5pllTF) where

import Prelude

import Control.Monad (guard, forM)
import Control.Monad.State (State)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)

-- clash-prelude
import Clash.Signal (periodToHz)

-- clash-lib (blackbox types/functions)
import Clash.Backend (Backend (..))
import Clash.Netlist.BlackBox.Util (exprToString)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Netlist.Util (instPort, stripVoid)
import Data.Monoid (Ap (getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc)
import qualified Data.Text as TextS

ecp5pllTF :: TemplateFunction
ecp5pllTF = TemplateFunction used valid ecp5pllTemplate
  where
    used = [1..5]
    valid = const True

-- Get the period of a KnownDomain constraint
extractPeriod :: HWType -> Integer
extractPeriod (KnownDomain _ period _ _ _ _) = period
extractPeriod _ = error "expected HWType to be a KnownDomain constraint"

-- Get the periods of one or more KnownDomain constraints
extractPeriods :: HWType -> [Integer]
extractPeriods (KnownDomain _ period _ _ _ _) = [period]
extractPeriods (Product _ _ knownDomains) = map extractPeriod knownDomains
extractPeriods _ = error "expected HWType to be a Product type"

ecp5pllTemplate :: Backend s => BlackBoxContext -> State s Doc
ecp5pllTemplate bbCtx = do
  -- Inputs
  let [ _clocks, knownDomInArg, knownDomOutArgs, nameArg, clkInArg, rstArg ] = bbInputs bbCtx

  let (_, extractPeriod . stripVoid -> inPeriod, _) = knownDomInArg
  let (_, extractPeriods . stripVoid -> outPeriods, _) = knownDomOutArgs

  let (instNameE, _, _) = nameArg
  let Just instNameT = TextS.pack <$> exprToString instNameE
  instName <- Id.makeBasic instNameT
  blockName <- Id.makeBasic "ecp5pll_block"
  primName <- Id.addRaw "EHXPLLL"

  let (clkIn, clkInTy, _) = clkInArg
  let (rst, rstTy, _) = rstArg

  -- Result
  let [(Identifier result Nothing, resTy@(Product _ _ (init -> clkOutTys)))] = bbResults bbCtx
  let primaryClkOutTy:secondaryClkOutTys = clkOutTys

  -- Convert periods to frequencies in Hz
  let inFreq = round $ periodToHz (fromIntegral inPeriod) * 1e-6 :: Int
  let primaryOutFreq:secondaryOutFreqs =
        map (\period -> round $ periodToHz (fromIntegral period) * 1e-6) outPeriods :: [Int]

  -- Primary PLL params
  let PllParams { refClkDiv, feedbackDiv, outputDiv, primaryCPhase, fVco } =
        fromMaybe (error "failed to find suitable PLL parameters")
          $ calculatePllParams (fromIntegral inFreq) (fromIntegral primaryOutFreq)

  -- Secondary PLL params
  let secondaryPllParams =
        map (\freq -> generateSecondaryOutput fVco (fromIntegral freq) 0 primaryCPhase) secondaryOutFreqs

  locked <- Id.makeBasic "locked"
  primaryClkOut <- Id.makeBasic "clkOut0"
  secondaryClkOuts <- forM (zip ([1..3] :: [Int]) secondaryPllParams) $ \(i, _) ->
    Id.makeBasic ("clkOut" <> TextS.pack (show i))

  getAp $ blockDecl blockName $ concat
    [ [ NetDecl Nothing locked Bit
      , NetDecl Nothing primaryClkOut primaryClkOutTy
      ]
    , [ NetDecl Nothing clkOut clkTy | (clkOut, clkTy) <- zip secondaryClkOuts secondaryClkOutTys ]
    , [ InstDecl Comp Nothing [] primName instName
        ( [ stringParam "PLLRST_ENA" "ENABLED"
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
          , intParam "CLKOP_CPHASE" (toInteger primaryCPhase)
          , intParam "CLKOP_FPHASE" 0
          ] ++
          concat
          [ [ stringParam "CLKOS_ENABLE" "ENABLED"
            , intParam ("CLKOS" <> clkSuffix <> "_DIV") (toInteger divisor)
            , intParam ("CLKOS" <> clkSuffix <> "_CPHASE") (toInteger cPhase)
            , intParam ("CLKOS" <> clkSuffix <> "_FPHASE") (toInteger fPhase)
            ]
          | (clkSuffix, SecondaryPllParams { divisor, cPhase, fPhase })
            <- zip ["", "2", "3"] secondaryPllParams
          ] ++
          [ stringParam "FEEDBK_PATH" "CLKOP"
          , intParam "CLKFB_DIV" (toInteger feedbackDiv)
          ]
        )
        ( NamedPortMap $
          [ (instPort "RST", In, rstTy, rst)
          , (instPort "STDBY", In, Bit, Literal Nothing (BitLit L))
          , (instPort "CLKI", In, clkInTy, clkIn)
          , (instPort "CLKOP", Out, primaryClkOutTy, Identifier primaryClkOut Nothing)
          ] ++
          [ (instPort ("CLKOS" <> clkSuffix), Out, clkOutTy, Identifier clkOut Nothing)
          | (clkSuffix, clkOut, clkOutTy) <- zip3 ["", "2", "3"] secondaryClkOuts secondaryClkOutTys
          ] ++
          [ (instPort "CLKFB", Out, primaryClkOutTy, Identifier primaryClkOut Nothing)
          , (instPort "PHASESEL0", In, Bit, Literal Nothing (BitLit L))
          , (instPort "PHASESEL1", In, Bit, Literal Nothing (BitLit L))
          , (instPort "PHASEDIR", In, Bit, Literal Nothing (BitLit H))
          , (instPort "PHASESTEP", In, Bit, Literal Nothing (BitLit H))
          , (instPort "PHASELOADREG", In, Bit, Literal Nothing (BitLit H))
          , (instPort "PLLWAKESYNC", In, Bit, Literal Nothing (BitLit L))
          , (instPort "ENCLKOP", In, Bit, Literal Nothing (BitLit L))
          , (instPort "LOCK", Out, Bit, Identifier locked Nothing)
          ]
        )
      , Assignment result
        ( DataCon resTy (DC (resTy, 0)) $
          [ Identifier primaryClkOut Nothing ] ++
          map (\clkOut -> Identifier clkOut Nothing) secondaryClkOuts ++
          [ Identifier locked Nothing ]
        )
      ]
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

-- Copied from Project Trellis's ecppll.  Doesn't always agree with ecppll
-- because of some floating point error reason. e.g. for a 25MHz input and 50MHz
-- output, ecppll gets a cphase of 4, rounded down from a floating point value
-- very close to 5, whereas this actually gets 5.
calculatePllParams :: Float -> Float -> Maybe PllParams
calculatePllParams inputf outputf = minOrNothing $ do
  inputDiv <- [1..128]
  let fpfd = inputf / fromIntegral inputDiv
  guard $ fpfd >= pfdMin && fpfd <= pfdMax

  feedbackDiv <- [1..80]
  outputDiv <- [1..128]
  let fout = fpfd * fromIntegral feedbackDiv
  let fvco = fout * fromIntegral outputDiv
  guard $ fvco >= vcoMin && fvco <= vcoMax
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

data SecondaryPllParams = SecondaryPllParams
  { divisor :: !Int
  , cPhase :: !Int
  , fPhase :: !Int
  , freq :: !Float
  }

generateSecondaryOutput :: Float -> Float -> Float -> Int -> SecondaryPllParams
generateSecondaryOutput fvco frequency phase primaryCPhase =
  let div' = fvco / frequency
      freq = fvco / div'

      nsShift = 1 / (freq * 1e6) * phase / 360.0
      phaseCount = nsShift * (fvco * 1e6)
      cPhase = floor phaseCount
      fPhase = floor ((phaseCount - fromIntegral cPhase) * 8)

  in SecondaryPllParams
    { divisor = floor div'
    , cPhase = cPhase + primaryCPhase
    , fPhase = fPhase
    , freq = freq
    }
