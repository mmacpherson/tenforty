module TablesDC2024 (
    dcBrackets2024,
    dcBracketsTable2024,
    dcStandardDeduction2024,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | District of Columbia 2024 tax brackets (uniform for all filing statuses)
Source: DC Office of Tax and Revenue, DC Code Title 47, Chapter 18
https://otr.cfo.dc.gov/page/dc-individual-and-fiduciary-income-tax-rates
https://otr.cfo.dc.gov/sites/default/files/dc/sites/otr/publication/attachments/2024_D40_Booklet_011525.pdf

The District of Columbia uses a 7-bracket progressive tax system with rates ranging from 4% to 10.75%.
All filing statuses use the same bracket thresholds.

Bracket thresholds (uniform for all filing statuses):
- 4.00% on income $0 to $10,000
- 6.00% on income $10,000 to $40,000
- 6.50% on income $40,000 to $60,000
- 8.50% on income $60,000 to $250,000
- 9.25% on income $250,000 to $500,000
- 9.75% on income $500,000 to $1,000,000
- 10.75% on income over $1,000,000

Order: Single, MFJ, MFS, HoH, QW
-}
dcBrackets2024 :: NonEmpty Bracket
dcBrackets2024 =
    Bracket (byStatus 10000 10000 10000 10000 10000) 0.04
        :| [ Bracket (byStatus 40000 40000 40000 40000 40000) 0.06
           , Bracket (byStatus 60000 60000 60000 60000 60000) 0.065
           , Bracket (byStatus 250000 250000 250000 250000 250000) 0.085
           , Bracket (byStatus 500000 500000 500000 500000 500000) 0.0925
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0975
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

dcBracketsTable2024 :: Table
dcBracketsTable2024 =
    case mkBracketTable dcBrackets2024 of
        Right bt -> TableBracket "dc_brackets_2024" bt
        Left err -> error $ "Invalid District of Columbia brackets: " ++ err

{- | District of Columbia 2024 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: 2024 D-40 Booklet, page 2 (What's New for 2024)
https://otr.cfo.dc.gov/sites/default/files/dc/sites/otr/publication/attachments/2024_D40_Booklet_011525.pdf

Standard deductions for tax year 2024:
- Single/MFS: $14,600
- MFJ/QW: $29,200
- HoH: $21,900
-}
dcStandardDeduction2024 :: ByStatus (Amount Dollars)
dcStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200
