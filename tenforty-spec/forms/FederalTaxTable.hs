-- | Form 1040 line 16 tax on an amount: the Tax Table below $100,000, the Tax
-- Computation Worksheet at $100,000 or more.
--
-- Instructions for Form 1040, line 16 (2024 p. 33, 2025 p. 36): "If your
-- taxable income is less than $100,000, you must use the Tax Table ... If
-- your taxable income is $100,000 or more, use the Tax Computation
-- Worksheet". The Qualified Dividends and Capital Gain Tax Worksheet lines 22
-- and 24 (2024 p. 36, 2025 p. 38) apply the same rule to their amounts.
--
-- The Tax Table (2024 i1040tt pp. 3-14, 2025 Pub. 1040 pp. 2-13) has rows
-- of 0-5, 5-15 and 15-25 dollars, then 25-dollar rows to 3,000, then
-- 50-dollar rows to 100,000. Each row's amount is the rate-schedule tax at
-- the row midpoint, rounded half up to the dollar; every published row is
-- checked in tests/federal_tax_table_test.py.
--
-- The three opening rows are not cells of any uniform grid anchored at zero,
-- so each is its own segment: a single-cell table whose cell contains the row
-- and whose output offset is the row midpoint, selected only inside the row.
-- The Tax Computation Worksheet is the unrounded rate schedule.
module FederalTaxTable (form1040TaxOnAmount) where

import TenForty

data TableSegment = TableSegment
  { segmentLessThan :: Double,
    segmentStep :: Int,
    segmentMidpointOffset :: Double
  }

taxTableCeiling :: Double
taxTableCeiling = 100000

taxTableSegments :: [TableSegment]
taxTableSegments =
  [ TableSegment 5 5 2.5,
    TableSegment 15 15 10,
    TableSegment 25 25 20,
    TableSegment 3000 25 12.5,
    TableSegment taxTableCeiling 50 25
  ]

form1040TaxOnAmount :: TableId -> Expr Dollars -> Expr Dollars
form1040TaxOnAmount rateSchedule amount =
  foldr selectSegment (bracketTax rateSchedule amount) taxTableSegments
  where
    selectSegment segment =
      ifPos (dollars (segmentLessThan segment) .-. amount) (segmentTax segment)
    segmentTax segment =
      case mkTaxTable (segmentStep segment) (Amount (segmentMidpointOffset segment)) rateSchedule of
        Right table -> taxTableBandTax table amount
        Left err -> error ("form1040TaxOnAmount: " ++ err)
