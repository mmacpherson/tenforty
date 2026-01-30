module TablesWI2024 (
    -- * Wisconsin State Income Tax Brackets
    wisconsinBrackets2024,
    wisconsinBracketsTable2024,

    -- * Standard Deduction (sliding scale)
    wiStandardDeductionMax2024,
    wiStandardDeductionPhaseoutStart2024,
    wiStandardDeductionPhaseoutEnd2024,

    -- * Personal Exemptions
    wiPersonalExemption2024,
    wiAgeExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Wisconsin State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2024), Tax Rate Schedule
Note: Wisconsin uses same brackets for Single and HoH; QW uses MFJ brackets.
The 4.4% bracket was expanded in 2024 (previously ended at $28,640 for Single).
-}
wisconsinBrackets2024 :: NonEmpty Bracket
wisconsinBrackets2024 =
    Bracket (byStatus 14320 19090 9545 14320 19090) 0.035
        :| [ Bracket (byStatus 28640 38190 19095 28640 38190) 0.044
           , Bracket (byStatus 315310 420420 210210 315310 420420) 0.053
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0765
           ]

wisconsinBracketsTable2024 :: Table
wisconsinBracketsTable2024 =
    case mkBracketTable wisconsinBrackets2024 of
        Right bt -> TableBracket "wi_brackets_2024" bt
        Left err -> error $ "Invalid Wisconsin brackets: " ++ err

{- | 2024 Wisconsin State maximum standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2024), Standard Deduction Table (pg 16-17)
Note: Wisconsin uses a sliding-scale deduction that phases out based on income.
These are the maximum amounts (at lowest income levels).
-}
wiStandardDeductionMax2024 :: ByStatus (Amount Dollars)
wiStandardDeductionMax2024 = byStatus 9930 17880 8490 12820 17880

{- | 2024 Wisconsin standard deduction phase-out start (income levels)
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2024), Standard Deduction Table
Note: Below these income levels, taxpayers receive the maximum standard deduction.
-}
wiStandardDeductionPhaseoutStart2024 :: ByStatus (Amount Dollars)
wiStandardDeductionPhaseoutStart2024 = byStatus 14310 20090 9540 14310 20090

{- | 2024 Wisconsin standard deduction phase-out end (income levels)
Order: Single, MFJ, MFS, HoH, QW
Source: Wisconsin DOR Form 1 Instructions (2024), Standard Deduction Table
Note: Above these income levels, the standard deduction is zero.
-}
wiStandardDeductionPhaseoutEnd2024 :: ByStatus (Amount Dollars)
wiStandardDeductionPhaseoutEnd2024 = byStatus 97060 110493 52466 97060 110493

{- | 2024 Wisconsin personal exemption amount
Source: Wisconsin DOR Form 1 Instructions (2024), Line 38 instructions (pg 21)
Note: $700 per taxpayer, spouse (if MFJ), and each dependent.
-}
wiPersonalExemption2024 :: Amount Dollars
wiPersonalExemption2024 = 700

{- | 2024 Wisconsin age exemption (65+)
Source: Wisconsin DOR Form 1 Instructions (2024), Line 38 instructions (pg 21)
Note: Additional $250 per taxpayer/spouse age 65 or older.
-}
wiAgeExemption2024 :: Amount Dollars
wiAgeExemption2024 = 250
