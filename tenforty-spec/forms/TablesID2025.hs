module TablesID2025 (
    idTaxRate2025,
    idStandardDeduction2025,
    idTaxThreshold2025,
) where

import TenForty.Types

{- | 2025 Idaho State flat tax rate on income above threshold
Source: Idaho State Tax Commission, Individual Income Tax Rate Schedule
https://tax.idaho.gov/taxes/income-tax/individual-income/individual-income-tax-rate-schedule/
Rate: 5.3%
Rate reduced from 5.695% in 2024 per Idaho HB 40, signed March 6, 2025
-}
idTaxRate2025 :: Double
idTaxRate2025 = 0.053

{- | 2025 Idaho standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Idaho Form 40 Instructions (2025)
Single or Married Filing Separately: $15,000
Married Filing Jointly or Qualifying Surviving Spouse: $30,000
Head of Household: $22,500
-}
idStandardDeduction2025 :: ByStatus (Amount Dollars)
idStandardDeduction2025 = byStatus 15000 30000 15000 22500 30000

{- | 2025 Idaho tax threshold - income below this is taxed at 0%
Order: Single, MFJ, MFS, HoH, QW
Source: Idaho State Tax Commission, Individual Income Tax Rate Schedule
https://tax.idaho.gov/taxes/income-tax/individual-income/individual-income-tax-rate-schedule/
Single or Married Filing Separately: $4,811
Married Filing Jointly, Head of Household, or Qualifying Surviving Spouse: $9,622
-}
idTaxThreshold2025 :: ByStatus (Amount Dollars)
idTaxThreshold2025 = byStatus 4811 9622 4811 9622 9622
