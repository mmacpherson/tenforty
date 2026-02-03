module TablesHI2024 (
    -- * Hawaii State Income Tax Brackets
    hawaiiBrackets2024,
    hawaiiBracketsTable2024,

    -- * Standard Deduction
    hiStandardDeduction2024,

    -- * Personal Exemption
    hiPersonalExemption2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Hawaii state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Hawaii Form N-11 Instructions 2024, Tax Rate Schedules (pg 48)
-}
hawaiiBrackets2024 :: NonEmpty Bracket
hawaiiBrackets2024 =
    Bracket (byStatus 2400 4800 2400 3600 4800) 0.014
        :| [ Bracket (byStatus 4800 9600 4800 7200 9600) 0.032
           , Bracket (byStatus 9600 19200 9600 14400 19200) 0.055
           , Bracket (byStatus 14400 28800 14400 21600 28800) 0.064
           , Bracket (byStatus 19200 38400 19200 28800 38400) 0.068
           , Bracket (byStatus 24000 48000 24000 36000 48000) 0.072
           , Bracket (byStatus 36000 72000 36000 54000 72000) 0.076
           , Bracket (byStatus 48000 96000 48000 72000 96000) 0.079
           , Bracket (byStatus 150000 300000 150000 225000 300000) 0.0825
           , Bracket (byStatus 175000 350000 175000 262500 350000) 0.09
           , Bracket (byStatus 200000 400000 200000 300000 400000) 0.10
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.11
           ]

hawaiiBracketsTable2024 :: Table
hawaiiBracketsTable2024 =
    case mkBracketTable hawaiiBrackets2024 of
        Right bt -> TableBracket "hi_brackets_2024" bt
        Left err -> error $ "Invalid Hawaii brackets: " ++ err

{- | 2024 Hawaii state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Hawaii Form N-11 Instructions 2024, pg 20
Standard deductions doubled under Act 45, SLH 2024
-}
hiStandardDeduction2024 :: ByStatus (Amount Dollars)
hiStandardDeduction2024 = byStatus 4400 8800 4400 6424 8800

{- | 2024 Hawaii personal exemption amount ($1,144 per exemption)
Source: Hawaii Form N-11 Instructions 2024, pg 20
-}
hiPersonalExemption2024 :: Amount Dollars
hiPersonalExemption2024 = 1144
