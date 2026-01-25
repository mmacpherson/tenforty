module TenForty.PhaseOut
  ( -- * Phase-Out Specification
    PhaseOutSpec(..)

    -- * Standard Phase-Outs
  , childTaxCredit2025
  , earnedIncomeCredit2025
  , studentLoanInterest2025
  , iraDeduction2025

    -- * Evaluation
  , evalPhaseOut
  , phaseOutReduction

    -- * Expression Construction
  , applyPhaseOut
  ) where

import TenForty.Types
import TenForty.Expr


data PhaseOutSpec = PhaseOutSpec
  { poBase      :: Amount Dollars
  , poThreshold :: ByStatus (Amount Dollars)
  , poRate      :: Amount Rate
  , poFloor     :: Amount Dollars
  , poRoundTo   :: Maybe (Amount Dollars)
  }
  deriving stock (Show, Eq)

childTaxCredit2025 :: PhaseOutSpec
childTaxCredit2025 = PhaseOutSpec
  { poBase      = 2000
  , poThreshold = byStatus 200000 400000 200000 200000 400000
  , poRate      = 0.05
  , poFloor     = 0
  , poRoundTo   = Just 1000
  }

earnedIncomeCredit2025 :: PhaseOutSpec
earnedIncomeCredit2025 = PhaseOutSpec
  { poBase      = 0
  , poThreshold = byStatus 17640 24610 17640 17640 24610
  , poRate      = 0.0765
  , poFloor     = 0
  , poRoundTo   = Nothing
  }

studentLoanInterest2025 :: PhaseOutSpec
studentLoanInterest2025 = PhaseOutSpec
  { poBase      = 2500
  , poThreshold = byStatus 80000 165000 80000 80000 165000
  , poRate      = 1.0
  , poFloor     = 0
  , poRoundTo   = Nothing
  }

iraDeduction2025 :: PhaseOutSpec
iraDeduction2025 = PhaseOutSpec
  { poBase      = 7000
  , poThreshold = byStatus 79000 126000 79000 79000 126000
  , poRate      = 1.0
  , poFloor     = 0
  , poRoundTo   = Nothing
  }


evalPhaseOut :: PhaseOutSpec -> FilingStatus -> Amount Dollars -> Amount Dollars
evalPhaseOut spec status agi =
  let threshold = forStatus (poThreshold spec) status
      excess = max 0 (agi - threshold)
      reduction = phaseOutReduction spec excess
      result = max (poFloor spec) (poBase spec - reduction)
  in case poRoundTo spec of
       Nothing -> result
       Just r  -> Amount (fromIntegral @Int (floor (unAmount result / unAmount r)) * unAmount r)

phaseOutReduction :: PhaseOutSpec -> Amount Dollars -> Amount Dollars
phaseOutReduction spec excess =
  let roundedExcess = case poRoundTo spec of
        Nothing -> excess
        Just r  -> Amount (fromIntegral @Int (ceiling (unAmount excess / unAmount r)) * unAmount r)
  in Amount (unAmount roundedExcess * unAmount (poRate spec))


applyPhaseOut :: PhaseOutSpec -> Expr Dollars -> Expr Dollars
applyPhaseOut spec agiExpr =
  PhaseOut
    (lit (poBase spec))
    (ByStatusE $ fmap lit (poThreshold spec))
    (lit (poRate spec))
    agiExpr
