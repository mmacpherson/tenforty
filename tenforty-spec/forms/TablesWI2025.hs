module TablesWI2025 (
    -- * Wisconsin State Income Tax Brackets
    wisconsinBrackets2025,
    wisconsinBracketsTable2025,

    -- * Standard Deduction (sliding scale)
    wiStandardDeductionMax2025,
    wiStandardDeductionPhaseoutStart2025,
    wiStandardDeductionPhaseoutEnd2025,

    -- * Personal Exemptions
    wiPersonalExemption2025,
    wiAgeExemption2025,

    -- * Retirement Income Exclusion (new in 2025)
    wiRetirementExclusionAge2025,
    wiRetirementExclusionMax2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Wisconsin State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR, 2025 Wisconsin Act 15 (effective for tax year 2025)
Note: 2025 Wisconsin Act 15 expanded the 4.4% bracket significantly:
  - Single/HoH: 4.4% bracket now ends at $50,480 (was $28,640 in 2024)
  - MFJ: 4.4% bracket now ends at $67,300 (was $38,190 in 2024)
  - MFS: 4.4% bracket now ends at $33,650 (was $19,095 in 2024)
The 3.5%, 5.3%, and 7.65% bracket thresholds remain unchanged from 2024.
-}
wisconsinBrackets2025 :: NonEmpty Bracket
wisconsinBrackets2025 =
    Bracket (byStatus 14320 19090 9545 14320 19090) 0.035
        :| [ Bracket (byStatus 50480 67300 33650 50480 67300) 0.044
           , Bracket (byStatus 315310 420420 210210 315310 420420) 0.053
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0765
           ]

wisconsinBracketsTable2025 :: Table
wisconsinBracketsTable2025 =
    case mkBracketTable wisconsinBrackets2025 of
        Right bt -> TableBracket "wi_brackets_2025" bt
        Left err -> error $ "Invalid Wisconsin brackets: " ++ err

{- | 2025 Wisconsin State maximum standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2025)
Note: Values will be inflation-adjusted from 2024; using estimated values
based on typical inflation adjustments. Official values TBD.
For implementation purposes, using 2024 values as conservative estimate.
-}
wiStandardDeductionMax2025 :: ByStatus (Amount Dollars)
wiStandardDeductionMax2025 = byStatus 9930 17880 8490 12820 17880

{- | 2025 Wisconsin standard deduction phase-out start (income levels)
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2025)
Note: Using 2024 values as conservative estimate pending official publication.
-}
wiStandardDeductionPhaseoutStart2025 :: ByStatus (Amount Dollars)
wiStandardDeductionPhaseoutStart2025 = byStatus 14310 20090 9540 14310 20090

{- | 2025 Wisconsin standard deduction phase-out end (income levels)
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2025)
Note: Using 2024 values as conservative estimate pending official publication.
-}
wiStandardDeductionPhaseoutEnd2025 :: ByStatus (Amount Dollars)
wiStandardDeductionPhaseoutEnd2025 = byStatus 97060 110493 52466 97060 110493

{- | 2025 Wisconsin personal exemption amount
Source: Wisconsin DOR Form 1 Instructions (2025)
Note: $700 per taxpayer, spouse (if MFJ), and each dependent.
Unchanged from 2024.
-}
wiPersonalExemption2025 :: Amount Dollars
wiPersonalExemption2025 = 700

{- | 2025 Wisconsin age exemption (65+)
Source: Wisconsin DOR Form 1 Instructions (2025)
Note: Additional $250 per taxpayer/spouse age 65 or older.
Unchanged from 2024.
-}
wiAgeExemption2025 :: Amount Dollars
wiAgeExemption2025 = 250

{- | 2025 Wisconsin retirement income exclusion minimum age
Source: 2025 Wisconsin Act 15 (signed July 3, 2025)
Note: Taxpayers age 67+ can exclude retirement income.
-}
wiRetirementExclusionAge2025 :: Int
wiRetirementExclusionAge2025 = 67

{- | 2025 Wisconsin retirement income exclusion maximum amounts
Order: Single, MFJ, MFS, HoH, QW
Source: 2025 Wisconsin Act 15 (signed July 3, 2025)
Note: $24,000 per individual age 67+, $48,000 for MFJ if both spouses 67+.
No income limits or phase-outs apply.
-}
wiRetirementExclusionMax2025 :: ByStatus (Amount Dollars)
wiRetirementExclusionMax2025 = byStatus 24000 48000 24000 24000 48000
