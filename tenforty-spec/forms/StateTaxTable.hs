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
import TenForty.TaxTable (taxTableBandIncome)

data TableSegment
  = -- | Rows of @step@ dollars anchored at zero, priced at row start + offset.
    GridRows {segmentLessThan :: Rational, segmentStep :: Int, segmentMidpointOffset :: Rational}
  | -- | One printed row whose amount is fixed.
    FixedRow {segmentLessThan :: Rational, segmentTax :: Rational}

-- | A table whose rows price the bracket table @rateSchedule@.
stateTaxTable :: [TableSegment] -> TableId -> Expr Dollars -> Expr Dollars -> Expr Dollars
stateTaxTable segments rateSchedule =
  segmentedTable segments rateSchedule taxTableBandTax

-- | A table whose rows price @schedule@ at the row midpoint, rounded half up,
-- for states whose printed schedule is not the bracket integral of
-- @rateSchedule@ (whole-dollar bracket bases, for example), as OR-40 does.
stateTaxTableBy :: [TableSegment] -> TableId -> (Expr Dollars -> Expr Dollars) -> Expr Dollars -> Expr Dollars -> Expr Dollars
stateTaxTableBy segments rateSchedule schedule =
  segmentedTable segments rateSchedule (\table amount -> roundE (schedule (taxTableBandIncome table amount)))

segmentedTable :: [TableSegment] -> TableId -> (TaxTable -> Expr Dollars -> Expr Dollars) -> Expr Dollars -> Expr Dollars -> Expr Dollars
segmentedTable segments rateSchedule rowPrice aboveTable amount =
  foldr selectSegment aboveTable segments
  where
    selectSegment segment =
      ifPos (dollars (segmentLessThan segment) .-. amount) (rowTax segment)
    rowTax (FixedRow _ tax) = dollars tax
    rowTax (GridRows _ step offset) =
      case mkTaxTable step (Amount offset) rateSchedule of
        Right table -> rowPrice table amount
        Left err -> error ("stateTaxTable: " ++ err)
