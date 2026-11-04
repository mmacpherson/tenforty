-- | State tax tables built from published row rules.
--
-- A state table whose rows are the rate schedule at each row's midpoint,
-- rounded half up to the dollar, is a run of uniform grids, as on Form 1040
-- (FederalTaxTable). Each segment covers incomes below its bound not claimed
-- by an earlier segment: either a uniform grid of a given step anchored at
-- zero, priced at the row midpoint, or a single row printed at a fixed amount
-- (an opening row that breaks the midpoint rule). Above the last segment the
-- caller supplies the exact computation.
module StateTaxTable
  ( TableSegment (..),
    stateTaxTable,
    stateTaxTableBy,
  )
where

import TenForty
import TenForty.Expr (Expr (BracketTax, TaxTableQuantize), TaxTableQuantizeMode (..))

data TableSegment
  = -- | Rows of @step@ dollars anchored at zero, priced at row start + offset.
    GridRows {segmentLessThan :: Double, segmentStep :: Int, segmentMidpointOffset :: Double}
  | -- | One printed row whose amount is fixed.
    FixedRow {segmentLessThan :: Double, segmentTax :: Double}

-- | A table whose rows price the bracket table @rateSchedule@.
stateTaxTable :: [TableSegment] -> TableId -> Expr Dollars -> Expr Dollars -> Expr Dollars
stateTaxTable segments rateSchedule = stateTaxTableBy segments (BracketTax rateSchedule)

-- | A table whose rows price @schedule@, for states whose printed schedule is
-- not the bracket integral (whole-dollar bracket bases, for example).
stateTaxTableBy :: [TableSegment] -> (Expr Dollars -> Expr Dollars) -> Expr Dollars -> Expr Dollars -> Expr Dollars
stateTaxTableBy segments schedule aboveTable amount =
  foldr selectSegment aboveTable segments
  where
    selectSegment segment =
      ifPos (dollars (segmentLessThan segment) .-. amount) (rowTax segment)
    rowTax (FixedRow _ tax) = dollars tax
    rowTax (GridRows _ step offset)
      | step <= 0 = error "stateTaxTable: step must be a positive integer number of dollars"
      | isNaN offset || isInfinite offset || offset < 0 || offset >= fromIntegral step =
          error "stateTaxTable: midpoint offset must be finite and inside the row"
      | otherwise =
          TaxTableQuantize TableRound 1 0 $
            schedule (TaxTableQuantize TableFloor (fromIntegral step) offset amount)
