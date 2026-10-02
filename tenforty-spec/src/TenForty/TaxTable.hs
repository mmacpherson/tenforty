module TenForty.TaxTable (TaxTable, mkTaxTable, taxTableBandTax) where

import TenForty.Expr
import TenForty.Types

data TaxTable = TaxTable Int (Amount Dollars) TableId

mkTaxTable :: Int -> Amount Dollars -> TableId -> Either String TaxTable
mkTaxTable step midpointOffset@(Amount offset) rateTable
  | step <= 0 = Left "mkTaxTable: step must be a positive integer number of dollars"
  | isNaN offset || isInfinite offset || offset < 0 || offset >= fromIntegral step = Left "mkTaxTable: midpoint offset must be finite and inside the band"
  | otherwise = Right (TaxTable step midpointOffset rateTable)

taxTableBandTax :: TaxTable -> Expr Dollars -> Expr Dollars
taxTableBandTax (TaxTable step (Amount midpointOffset) rateTable) income =
  TaxTableQuantize TableRound 1 0 $
    BracketTax rateTable (TaxTableQuantize TableFloor (fromIntegral step) midpointOffset income)
