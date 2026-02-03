module TablesMT2024 (
    -- * Montana Ordinary Income Tax Brackets
    montanaOrdinaryBrackets2024,
    montanaOrdinaryBracketsTable2024,

    -- * Montana Net Long-Term Capital Gains Tax Brackets
    montanaCapitalGainsBrackets2024,
    montanaCapitalGainsBracketsTable2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Montana ordinary income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Montana Form 2 Instructions 2024, pg 14
https://taxsim.nber.org/historical_state_tax_forms/MT/2024/Form_2_2024_Instrux.pdf

Montana uses two-bracket system for ordinary income (all income that is not
net long-term capital gain). Rates: 4.7% up to threshold, then 5.9%.

Thresholds by filing status:
- Single/MFS: $20,500
- MFJ/QW: $41,000
- HoH: $30,750
-}
montanaOrdinaryBrackets2024 :: NonEmpty Bracket
montanaOrdinaryBrackets2024 =
    Bracket (byStatus 20500 41000 20500 30750 41000) 0.047
        :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.059]

montanaOrdinaryBracketsTable2024 :: Table
montanaOrdinaryBracketsTable2024 =
    case mkBracketTable montanaOrdinaryBrackets2024 of
        Right bt -> TableBracket "mt_ordinary_brackets_2024" bt
        Left err -> error $ "Invalid Montana ordinary brackets: " ++ err

{- | 2024 Montana net long-term capital gains tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Montana Form 2 Instructions 2024, pg 13
https://taxsim.nber.org/historical_state_tax_forms/MT/2024/Form_2_2024_Instrux.pdf

Montana taxes net long-term capital gains at lower rates: 3.0% up to threshold
(less ordinary income), then 4.1%.

The capital gains brackets are based on the same thresholds as ordinary income:
- Single/MFS: $20,500
- MFJ/QW: $41,000
- HoH: $30,750

If ordinary income exceeds the threshold, all capital gains are taxed at 4.1%.
-}
montanaCapitalGainsBrackets2024 :: NonEmpty Bracket
montanaCapitalGainsBrackets2024 =
    Bracket (byStatus 20500 41000 20500 30750 41000) 0.030
        :| [Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.041]

montanaCapitalGainsBracketsTable2024 :: Table
montanaCapitalGainsBracketsTable2024 =
    case mkBracketTable montanaCapitalGainsBrackets2024 of
        Right bt -> TableBracket "mt_capital_gains_brackets_2024" bt
        Left err -> error $ "Invalid Montana capital gains brackets: " ++ err
