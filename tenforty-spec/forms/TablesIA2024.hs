module TablesIA2024 (
    -- * Iowa State Income Tax Brackets
    iowaBrackets2024,
    iowaBracketsTable2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Iowa State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Iowa Department of Revenue press release dated 2023-10-25
https://revenue.iowa.gov/press-release/2023-10-25/idr-announces-2024-individual-income-tax-brackets-and-interest-rates

Single/Other:
  0-6,210: 4.4%
  6,210-31,050: 4.82%
  31,050+: 5.7%

MFJ:
  0-12,420: 4.4%
  12,420-62,100: 4.82%
  62,100+: 5.7%

Iowa does not specify separate brackets for MFS, HoH, or QW in the official documentation.
Using Single brackets for MFS, HoH, and QW (same as "Single and Other Filers" category).
-}
iowaBrackets2024 :: NonEmpty Bracket
iowaBrackets2024 =
    Bracket (byStatus 6210 12420 6210 6210 12420) 0.044
        :| [ Bracket (byStatus 31050 62100 31050 31050 62100) 0.0482
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.057
           ]

iowaBracketsTable2024 :: Table
iowaBracketsTable2024 =
    case mkBracketTable iowaBrackets2024 of
        Right bt -> TableBracket "ia_brackets_2024" bt
        Left err -> error $ "Invalid Iowa brackets: " ++ err
