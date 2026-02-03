module TablesMT2025 (
    -- * Montana Ordinary Income Tax Brackets
    montanaOrdinaryBrackets2025,
    montanaOrdinaryBracketsTable2025,

    -- * Montana Net Long-Term Capital Gains Tax Brackets
    montanaCapitalGainsBrackets2025,
    montanaCapitalGainsBracketsTable2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Montana ordinary income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Montana Tax Tables 2025, https://www.taxformcalculator.com/montana/tax-tables/2025.html

Montana uses two-bracket system for ordinary income (all income that is not
net long-term capital gain). Rates: 4.7% up to threshold, then 5.9%.

Thresholds by filing status (adjusted for inflation from 2024):
- Single/MFS: $21,100
- MFJ/QW: $42,200
- HoH: $31,700
-}
montanaOrdinaryBrackets2025 :: NonEmpty Bracket
montanaOrdinaryBrackets2025 =
    Bracket (byStatus 21100 42200 21100 31700 42200) 0.047
        :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.059]

montanaOrdinaryBracketsTable2025 :: Table
montanaOrdinaryBracketsTable2025 =
    case mkBracketTable montanaOrdinaryBrackets2025 of
        Right bt -> TableBracket "mt_ordinary_brackets_2025" bt
        Left err -> error $ "Invalid Montana ordinary brackets: " ++ err

{- | 2025 Montana net long-term capital gains tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Montana Code Annotated 15-30-2103, inflation-adjusted for 2025

Montana taxes net long-term capital gains at lower rates: 3.0% up to threshold
(less ordinary income), then 4.1%.

The capital gains brackets are based on the same thresholds as ordinary income:
- Single/MFS: $21,100
- MFJ/QW: $42,200
- HoH: $31,700

If ordinary income exceeds the threshold, all capital gains are taxed at 4.1%.
-}
montanaCapitalGainsBrackets2025 :: NonEmpty Bracket
montanaCapitalGainsBrackets2025 =
    Bracket (byStatus 21100 42200 21100 31700 42200) 0.030
        :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.041]

montanaCapitalGainsBracketsTable2025 :: Table
montanaCapitalGainsBracketsTable2025 =
    case mkBracketTable montanaCapitalGainsBrackets2025 of
        Right bt -> TableBracket "mt_capital_gains_brackets_2025" bt
        Left err -> error $ "Invalid Montana capital gains brackets: " ++ err
