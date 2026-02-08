module TablesDC2025 (
    dcBrackets2025,
    dcBracketsTable2025,
    dcStandardDeduction2025,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

{- | District of Columbia 2025 tax brackets (unchanged from 2024)
Source: DC Office of Tax and Revenue, DC Code Title 47, Chapter 18
https://otr.cfo.dc.gov/page/dc-individual-and-fiduciary-income-tax-rates
https://otr.cfo.dc.gov/sites/default/files/dc/sites/otr/publication/attachments/2025_D40ES_Booklet_121824.pdf

The tax brackets for tax years 2022-2025 are unchanged. The District of Columbia
uses a 7-bracket progressive system with uniform thresholds across all filing statuses.

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
dcBrackets2025 :: NonEmpty Bracket
dcBrackets2025 =
    Bracket (byStatus 10000 10000 10000 10000 10000) 0.04
        :| [ Bracket (byStatus 40000 40000 40000 40000 40000) 0.06
           , Bracket (byStatus 60000 60000 60000 60000 60000) 0.065
           , Bracket (byStatus 250000 250000 250000 250000 250000) 0.085
           , Bracket (byStatus 500000 500000 500000 500000 500000) 0.0925
           , Bracket (byStatus 1000000 1000000 1000000 1000000 1000000) 0.0975
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.1075
           ]

dcBracketsTable2025 :: Table
dcBracketsTable2025 =
    case mkBracketTable dcBrackets2025 of
        Right bt -> TableBracket "dc_brackets_2025" bt
        Left err -> error $ "Invalid District of Columbia brackets: " ++ err

{- | District of Columbia 2025 standard deduction by filing status
Order: Single, MFJ, MFS, HoH, QW
Source: 2025 D-40ES Estimated Tax Booklet, Worksheet 2, Line 2b
https://otr.cfo.dc.gov/sites/default/files/dc/sites/otr/publication/attachments/2025_D40ES_Booklet_121824.pdf

Standard deductions for tax year 2025 (OBBBA, per federal conformity):
- Single/MFS: $15,750
- MFJ/QW: $31,500
- HoH: $23,625
-}
dcStandardDeduction2025 :: ByStatus (Amount Dollars)
dcStandardDeduction2025 = byStatus 15750 31500 15750 23625 31500
