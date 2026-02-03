module TablesME2025 (
    -- * Maine State Income Tax Brackets
    maineBrackets2025,
    maineBracketsTable2025,

    -- * Standard Deduction
    meStandardDeduction2025,

    -- * Personal Exemption
    mePersonalExemption2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Maine state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Maine Revenue Services, Individual Income Tax 2025 Rates
(https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/ind_tax_rate_sched_2025.pdf)
Published September 15, 2024
COLA adjustments: 1.274 for lower brackets, 1.269 for upper brackets
-}
maineBrackets2025 :: NonEmpty Bracket
maineBrackets2025 =
    Bracket (byStatus 26800 53600 26800 40200 53600) 0.058
        :| [ Bracket (byStatus 63450 126900 63450 95150 126900) 0.0675
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0715
           ]

maineBracketsTable2025 :: Table
maineBracketsTable2025 =
    case mkBracketTable maineBrackets2025 of
        Right bt -> TableBracket "me_brackets_2025" bt
        Left err -> error $ "Invalid Maine brackets: " ++ err

{- | 2025 Maine state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Maine Revenue Services, Individual Income Tax 2025 Rates
Standard deduction equals federal standard deduction for 2025:
\$15,000 (Single/MFS), $30,000 (MFJ), $22,500 (HoH)
-}
meStandardDeduction2025 :: ByStatus (Amount Dollars)
meStandardDeduction2025 = byStatus 15000 30000 15000 22500 30000

{- | 2025 Maine personal exemption amount ($5,150 per exemption)
Source: Maine Revenue Services, Individual Income Tax 2025 Rates
COLA adjustment: 1.25 × $4,120 (base amount) = $5,150
-}
mePersonalExemption2025 :: Amount Dollars
mePersonalExemption2025 = 5150
