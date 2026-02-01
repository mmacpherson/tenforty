module TablesNY2024 (
    -- * New York State Income Tax Brackets
    newYorkBrackets2024,
    newYorkBracketsTable2024,

    -- * Standard Deduction
    nyStandardDeduction2024,

    -- * Dependent Exemption
    nyDependentExemption2024,

    -- * NYC Income Tax Brackets
    nycBrackets2024,
    nycBracketsTable2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 New York State income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: NY IT-201 Instructions, Tax Rate Schedule (pg 45)
-}
newYorkBrackets2024 :: NonEmpty Bracket
newYorkBrackets2024 =
    Bracket (byStatus 8500 17150 8500 12800 17150) 0.04
        :| [ Bracket (byStatus 11700 23600 11700 17650 23600) 0.045
           , Bracket (byStatus 13900 27900 13900 20900 27900) 0.0525
           , Bracket (byStatus 80650 161550 80650 107650 161550) 0.055
           , Bracket (byStatus 215400 323200 215400 269300 323200) 0.06
           , Bracket (byStatus 1077550 2155350 1077550 1616450 2155350) 0.0685
           , Bracket (byStatus 5000000 5000000 5000000 5000000 5000000) 0.0965
           , Bracket (byStatus 25000000 25000000 25000000 25000000 25000000) 0.103
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.109
           ]

newYorkBracketsTable2024 :: Table
newYorkBracketsTable2024 =
    case mkBracketTable newYorkBrackets2024 of
        Right bt -> TableBracket "ny_brackets_2024" bt
        Left err -> error $ "Invalid New York brackets: " ++ err

{- | 2024 New York State standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: NY IT-201 Instructions pg 11
-}
nyStandardDeduction2024 :: ByStatus (Amount Dollars)
nyStandardDeduction2024 = byStatus 8000 16050 8000 11200 16050

-- | 2024 New York dependent exemption amount ($1,000 per dependent)
nyDependentExemption2024 :: Amount Dollars
nyDependentExemption2024 = 1000

{- | 2024 New York City income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: NY IT-201 Instructions pg 40
-}
nycBrackets2024 :: NonEmpty Bracket
nycBrackets2024 =
    Bracket (byStatus 12000 21600 12000 14400 21600) 0.03078
        :| [ Bracket (byStatus 25000 45000 25000 30000 45000) 0.03762
           , Bracket (byStatus 50000 90000 50000 60000 90000) 0.03819
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.03876
           ]

nycBracketsTable2024 :: Table
nycBracketsTable2024 =
    case mkBracketTable nycBrackets2024 of
        Right bt -> TableBracket "nyc_brackets_2024" bt
        Left err -> error $ "Invalid NYC brackets: " ++ err
