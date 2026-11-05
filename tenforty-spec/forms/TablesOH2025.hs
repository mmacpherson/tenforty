module TablesOH2025
  ( -- * Ohio State Income Tax Brackets (Nonbusiness Income)
    ohioBrackets2025,
    ohioBracketsTable2025,

    -- * Ohio Personal Exemption Amounts
    ohioPersonalExemption2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 Ohio State income tax brackets for nonbusiness income
-- All filing statuses use the same brackets.
-- Source: Ohio Department of Taxation, 2025 IT-1040 Instructions
-- https://dam.assets.ohio.gov/image/upload/v1767095693/tax.ohio.gov/forms/ohio_individual/individual/2025/it1040-booklet.pdf
--
-- Rates: 0% on income $0-$26,050; 2.75% on $26,051-$100,000; 3.125% on income over $100,000
-- Base tax: $342 for income above $26,050 (added to bracket tax computation)
-- Top rate: 3.125%, 2025 booklet p.18 and ORC 5747.02(A)(3)(b).
ohioBrackets2025 :: NonEmpty Bracket
ohioBrackets2025 =
  Bracket (byStatus 26050 26050 26050 26050 26050) 0.0
    :| [ Bracket (byStatus 100000 100000 100000 100000 100000) 0.0275,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.03125
       ]

ohioBracketsTable2025 :: Table
ohioBracketsTable2025 =
  case mkBracketTable ohioBrackets2025 of
    Right bt -> TableBracket "oh_brackets_2025" bt
    Left err -> error $ "Invalid Ohio brackets: " ++ err

-- | 2025 Ohio personal exemption amounts (per taxpayer/spouse/dependent)
-- Exemption amounts vary by modified adjusted gross income (MAGI):
-- - $2,400 if MAGI ≤ $40,000
-- - $2,150 if $40,000 < MAGI ≤ $80,000
-- - $1,900 if $80,000 < MAGI < $750,000; $0 at $750,000 and above
-- Source: Ohio Revised Code Section 5747.025
-- https://codes.ohio.gov/ohio-revised-code/section-5747.025
-- 2025 IT 1040 booklet p.17 preserves the 2024 amounts and adds the $750,000 cliff.
-- Applied to the taxpayer/spouse base plus the total dependent count in the spec.
ohioPersonalExemption2025 :: [(Amount Dollars, Amount Dollars)]
ohioPersonalExemption2025 =
  [ (40000, 2400),
    (80000, 2150),
    (1e12, 1900)
  ]
