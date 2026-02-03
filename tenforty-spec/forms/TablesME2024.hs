module TablesME2024 (
    -- * Maine State Income Tax Brackets
    maineBrackets2024,
    maineBracketsTable2024,

    -- * Standard Deduction
    meStandardDeduction2024,

    -- * Personal Exemption
    mePersonalExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Maine state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Maine Revenue Services, Individual Income Tax 2024 Rates
(https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/ind_tax_rate_sched_2024.pdf)
Published September 15, 2023
-}
maineBrackets2024 :: NonEmpty Bracket
maineBrackets2024 =
    Bracket (byStatus 26050 52100 26050 39050 52100) 0.058
        :| [ Bracket (byStatus 61600 123250 61600 92450 123250) 0.0675
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0715
           ]

maineBracketsTable2024 :: Table
maineBracketsTable2024 =
    case mkBracketTable maineBrackets2024 of
        Right bt -> TableBracket "me_brackets_2024" bt
        Left err -> error $ "Invalid Maine brackets: " ++ err

{- | 2024 Maine state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Maine Revenue Services, Individual Income Tax 2024 Rates
Standard deduction equals federal standard deduction for 2024:
\$14,600 (Single/MFS), $29,200 (MFJ), $21,900 (HoH)
-}
meStandardDeduction2024 :: ByStatus (Amount Dollars)
meStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200

{- | 2024 Maine personal exemption amount ($5,000 per exemption)
Source: Maine Revenue Services, Individual Income Tax 2024 Rates
-}
mePersonalExemption2024 :: Amount Dollars
mePersonalExemption2024 = 5000
