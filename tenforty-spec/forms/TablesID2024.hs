module TablesID2024 (
    idTaxRate2024,
    idStandardDeduction2024,
    idTaxThreshold2024,
) where

import TenForty.Types

{- | 2024 Idaho State flat tax rate on income above threshold
Source: Idaho State Tax Commission, Individual Income Tax Rate Schedule
https://tax.idaho.gov/taxes/income-tax/individual-income/individual-income-tax-rate-schedule/
Rate: 5.695%
-}
idTaxRate2024 :: Double
idTaxRate2024 = 0.05695

{- | 2024 Idaho standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Idaho Form 40 Instructions (2024), Standard Deduction Worksheet
https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_10-23-2024.pdf
Single or Married Filing Separately: $14,600
Married Filing Jointly or Qualifying Surviving Spouse: $29,200
Head of Household: $21,900
-}
idStandardDeduction2024 :: ByStatus (Amount Dollars)
idStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200

{- | 2024 Idaho tax threshold - income below this is taxed at 0%
Order: Single, MFJ, MFS, HoH, QW
Source: Idaho State Tax Commission, Individual Income Tax Rate Schedule
https://tax.idaho.gov/taxes/income-tax/individual-income/individual-income-tax-rate-schedule/
Single or Married Filing Separately: $4,673
Married Filing Jointly, Head of Household, or Qualifying Surviving Spouse: $9,346
-}
idTaxThreshold2024 :: ByStatus (Amount Dollars)
idTaxThreshold2024 = byStatus 4673 9346 4673 9346 9346
