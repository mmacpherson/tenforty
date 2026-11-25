module TenForty.TaxTable
  ( TaxTable,
    mkTaxTable,
    taxTableBandTax,
    taxTableBandIncome,
    tableBandMidpoint,
    tableRoundToDollar,
  )
where

import TenForty.Expr
import TenForty.Types

data TaxTable = TaxTable Int (Amount Dollars) TableId

mkTaxTable :: Int -> Amount Dollars -> TableId -> Either String TaxTable
mkTaxTable step midpointOffset@(Amount offset) rateTable
  | step <= 0 = Left "mkTaxTable: step must be a positive integer number of dollars"
  | offset < 0 || offset >= fromIntegral step = Left "mkTaxTable: midpoint offset must be finite and inside the band"
  | otherwise = Right (TaxTable step midpointOffset rateTable)

taxTableBandTax :: TaxTable -> Expr Dollars -> Expr Dollars
taxTableBandTax table@(TaxTable _ _ rateTable) income =
  TaxTableQuantize TableRound 1 0 $
    BracketTax rateTable (taxTableBandIncome table income)

taxTableBandIncome :: TaxTable -> Expr Dollars -> Expr Dollars
taxTableBandIncome (TaxTable step (Amount midpointOffset) _) =
  TaxTableQuantize TableFloor (fromIntegral step) midpointOffset

-- | The midpoint of the published table row holding an amount: rows are @step@
-- dollars wide and start at multiples of @step@. Table quantization, so
-- autodiff takes the planning tangent of the row formula, not a zero slope.
tableBandMidpoint :: Int -> Expr Dollars -> Expr Dollars
tableBandMidpoint step =
  TaxTableQuantize TableFloor (fromIntegral step) (fromIntegral step / 2)

-- | A table entry rounded to whole dollars (half up), as table quantization.
tableRoundToDollar :: Expr Dollars -> Expr Dollars
tableRoundToDollar = TaxTableQuantize TableRound 1 0
