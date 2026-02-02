module TablesMS2024 (
    msTaxRate2024,
    msExemption2024,
    msStandardDeduction2024,
    msAdditionalExemption2024,
    msTaxThreshold2024,
) where

import TenForty.Types

{- | 2024 Mississippi State flat tax rate on income over $10,000
Source: Mississippi DOR Form 80-100 instructions (2024), page 9
"Excess of $10,000 @ 4.7%"
-}
msTaxRate2024 :: Double
msTaxRate2024 = 0.047

{- | 2024 Mississippi personal exemption amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Mississippi DOR Form 80-100 instructions (2024), page 11
Line 5: Single - $6,000
Line 1: Married - Joint or Combined Return - $12,000
Line 2: Married - Filing Separate Returns - $12,000
Line 4: Head of Family - $8,000
Line 3: Married - Spouse Died 2024 - $12,000 (treated as QW)
-}
msExemption2024 :: ByStatus (Amount Dollars)
msExemption2024 = byStatus 6000 12000 12000 8000 12000

{- | 2024 Mississippi standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Mississippi DOR Form 80-100 instructions (2024), page 11
Single: $2,300
Married Filing Joint or Combined: $4,600
Married Filing Separate: $2,300
Head of Family: $4,600
Spouse Died 2024: $4,600
-}
msStandardDeduction2024 :: ByStatus (Amount Dollars)
msStandardDeduction2024 = byStatus 2300 4600 2300 4600 4600

{- | 2024 Mississippi additional exemption for dependents, age 65+, or blind
Source: Mississippi DOR Form 80-100 instructions (2024), page 10
Line 10: Each dependent, age 65+, or blind gets $1,500 exemption
-}
msAdditionalExemption2024 :: Amount Dollars
msAdditionalExemption2024 = 1500

{- | 2024 Mississippi tax threshold - income below this is taxed at 0%
Source: Mississippi DOR Form 80-100 instructions (2024), page 9
"Excess of $10,000 @ 4.7%"
-}
msTaxThreshold2024 :: Amount Dollars
msTaxThreshold2024 = 10000
