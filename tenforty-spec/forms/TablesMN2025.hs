module TablesMN2025 (
    -- * Minnesota State Income Tax Brackets
    mnBrackets2025,
    mnBracketsTable2025,

    -- * Standard Deduction
    mnStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Minnesota state income tax brackets
Order: Single, MFJ, MFS, HoH, QW

Source: Minnesota Department of Revenue press release (Dec 16, 2024)
https://www.revenue.state.mn.us/press-release/2024-12-16/minnesota-income-tax-brackets-standard-deduction-and-dependent-exemption

Brackets adjusted by 2.886% from 2024 to account for inflation.

Single: 5.35% up to $32,570, 6.80% $32,571-$106,990, 7.85% $106,991-$198,630, 9.85% over $198,630
MFJ: 5.35% up to $47,620, 6.80% $47,621-$189,180, 7.85% $189,181-$330,410, 9.85% over $330,410
MFS: 5.35% up to $23,810, 6.80% $23,811-$94,590, 7.85% $94,591-$165,205, 9.85% over $165,205
HoH: 5.35% up to $40,100, 6.80% $40,101-$161,130, 7.85% $161,131-$264,050, 9.85% over $264,050
QW: Same as MFJ
-}
mnBrackets2025 :: NonEmpty Bracket
mnBrackets2025 =
    Bracket (byStatus 32570 47620 23810 40100 47620) 0.0535
        :| [ Bracket (byStatus 106990 189180 94590 161130 189180) 0.0680
           , Bracket (byStatus 198630 330410 165205 264050 330410) 0.0785
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0985
           ]

mnBracketsTable2025 :: Table
mnBracketsTable2025 =
    case mkBracketTable mnBrackets2025 of
        Right bt -> TableBracket "mn_brackets_2025" bt
        Left err -> error $ "Invalid Minnesota brackets: " ++ err

{- | 2025 Minnesota state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW

Source: Minnesota Department of Revenue press release (Dec 16, 2024)
https://www.revenue.state.mn.us/press-release/2024-12-16/minnesota-income-tax-brackets-standard-deduction-and-dependent-exemption

Standard deduction: $14,950 (Single/MFS), $29,900 (MFJ), $14,950 (MFS), $22,500 (HoH), $29,900 (QW)

Note: Standard deduction is reduced if income exceeds $238,950 ($119,475 if MFS).
This reduction is not implemented in this basic spec.
-}
mnStandardDeduction2025 :: ByStatus (Amount Dollars)
mnStandardDeduction2025 = byStatus 14950 29900 14950 22500 29900
