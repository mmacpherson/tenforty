module TablesMN2024 (
    -- * Minnesota State Income Tax Brackets
    mnBrackets2024,
    mnBracketsTable2024,

    -- * Standard Deduction
    mnStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Minnesota state income tax brackets
Order: Single, MFJ, MFS, HoH, QW

Source: Minnesota Department of Revenue press release (Dec 12, 2023)
https://www.revenue.state.mn.us/press-release/2023-12-12/minnesota-income-tax-brackets-standard-deduction-and-dependent-exemption

Single: 5.35% up to $31,690, 6.80% $31,691-$104,090, 7.85% $104,091-$193,240, 9.85% over $193,240
MFJ: 5.35% up to $46,330, 6.80% $46,331-$184,040, 7.85% $184,041-$321,450, 9.85% over $321,450
MFS: 5.35% up to $23,165, 6.80% $23,166-$92,020, 7.85% $92,021-$160,725, 9.85% over $160,725
HoH: 5.35% up to $39,010, 6.80% $39,011-$156,760, 7.85% $156,761-$256,880, 9.85% over $256,880
QW: Same as MFJ
-}
mnBrackets2024 :: NonEmpty Bracket
mnBrackets2024 =
    Bracket (byStatus 31690 46330 23165 39010 46330) 0.0535
        :| [ Bracket (byStatus 104090 184040 92020 156760 184040) 0.0680
           , Bracket (byStatus 193240 321450 160725 256880 321450) 0.0785
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0985
           ]

mnBracketsTable2024 :: Table
mnBracketsTable2024 =
    case mkBracketTable mnBrackets2024 of
        Right bt -> TableBracket "mn_brackets_2024" bt
        Left err -> error $ "Invalid Minnesota brackets: " ++ err

{- | 2024 Minnesota state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW

Source: Minnesota Department of Revenue press release (Dec 12, 2023)
https://www.revenue.state.mn.us/press-release/2023-12-12/minnesota-income-tax-brackets-standard-deduction-and-dependent-exemption

Standard deduction: $14,575 (Single/MFS), $29,150 (MFJ), $14,575 (MFS), $21,862.50 (HoH), $29,150 (QW)
-}
mnStandardDeduction2024 :: ByStatus (Amount Dollars)
mnStandardDeduction2024 = byStatus 14575 29150 14575 21862.50 29150
