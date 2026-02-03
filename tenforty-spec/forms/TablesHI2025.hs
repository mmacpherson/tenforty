module TablesHI2025 (
    -- * Hawaii State Income Tax Brackets
    hawaiiBrackets2025,
    hawaiiBracketsTable2025,

    -- * Standard Deduction
    hiStandardDeduction2025,

    -- * Personal Exemption
    hiPersonalExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Hawaii state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Hawaii Form N-11 Instructions 2025, Tax Rate Schedules (pg 48)
Brackets widened under Act 46, SLH 2024 (Green Affordability Plan II)
-}
hawaiiBrackets2025 :: NonEmpty Bracket
hawaiiBrackets2025 =
    Bracket (byStatus 9600 19200 9600 14400 19200) 0.014
        :| [ Bracket (byStatus 14400 28800 14400 21600 28800) 0.032
           , Bracket (byStatus 19200 38400 19200 28800 38400) 0.055
           , Bracket (byStatus 24000 48000 24000 36000 48000) 0.064
           , Bracket (byStatus 36000 72000 36000 54000 72000) 0.068
           , Bracket (byStatus 48000 96000 48000 72000 96000) 0.072
           , Bracket (byStatus 125000 250000 125000 187500 250000) 0.076
           , Bracket (byStatus 175000 350000 175000 262500 350000) 0.079
           , Bracket (byStatus 225000 450000 225000 337500 450000) 0.0825
           , Bracket (byStatus 275000 550000 275000 412500 550000) 0.09
           , Bracket (byStatus 325000 650000 325000 487500 650000) 0.10
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.11
           ]

hawaiiBracketsTable2025 :: Table
hawaiiBracketsTable2025 =
    case mkBracketTable hawaiiBrackets2025 of
        Right bt -> TableBracket "hi_brackets_2025" bt
        Left err -> error $ "Invalid Hawaii brackets: " ++ err

{- | 2025 Hawaii state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Hawaii Tax Year 2025 Information (tax.hawaii.gov/tax-year-information/)
Standard deduction amounts unchanged from 2024
-}
hiStandardDeduction2025 :: ByStatus (Amount Dollars)
hiStandardDeduction2025 = byStatus 4400 8800 4400 6424 8800

-- | 2025 Hawaii personal exemption amount ($1,144 per exemption)
-- Source: Hawaii Form N-11 Instructions 2025, pg 20
-- Personal exemption unchanged from 2024
hiPersonalExemption2025 :: Amount Dollars
hiPersonalExemption2025 = 1144
