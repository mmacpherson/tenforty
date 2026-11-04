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
  )
where

import TenForty

data TableSegment
  = -- | Rows of @step@ dollars anchored at zero, priced at row start + offset.
    GridRows {segmentLessThan :: Double, segmentStep :: Int, segmentMidpointOffset :: Double}
  | -- | One printed row whose amount is fixed.
    FixedRow {segmentLessThan :: Double, segmentTax :: Double}

stateTaxTable :: [TableSegment] -> TableId -> Expr Dollars -> Expr Dollars -> Expr Dollars
stateTaxTable segments rateSchedule aboveTable amount =
  foldr selectSegment aboveTable segments
  where
    selectSegment segment =
      ifPos (dollars (segmentLessThan segment) .-. amount) (rowTax segment)
    rowTax (FixedRow _ tax) = dollars tax
    rowTax (GridRows _ step offset) =
      case mkTaxTable step (Amount offset) rateSchedule of
        Right table -> taxTableBandTax table amount
        Left err -> error ("stateTaxTable: " ++ err)
