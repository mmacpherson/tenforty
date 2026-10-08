module TablesME2024
  ( -- * Maine State Income Tax Brackets
    maineBrackets2024,
    maineBracketsTable2024,

    -- * Standard Deduction
    meStandardDeduction2024,

    -- * Personal Exemption
    mePersonalExemption2024,
    mePersonalExemptionBase2024,
    mePersonalExemptionPhaseoutThreshold2024,
    mePersonalExemptionPhaseoutRange2024,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2024 Maine state income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Maine Revenue Services, Individual Income Tax 2024 Rates
-- (https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/ind_tax_rate_sched_2024.pdf)
-- Published September 15, 2023
maineBrackets2024 :: NonEmpty Bracket
maineBrackets2024 =
  Bracket (byStatus 26050 52100 26050 39050 52100) 0.058
    :| [ Bracket (byStatus 61600 123250 61600 92450 123250) 0.0675,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0715
       ]

maineBracketsTable2024 :: Table
maineBracketsTable2024 =
  case mkBracketTable maineBrackets2024 of
    Right bt -> TableBracket "me_brackets_2024" bt
    Left err -> error $ "Invalid Maine brackets: " ++ err

-- | 2024 Maine state standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Maine Revenue Services, Individual Income Tax 2024 Rates
-- Standard deduction equals federal standard deduction for 2024:
-- \$14,600 (Single/MFS), $29,200 (MFJ), $21,900 (HoH)
meStandardDeduction2024 :: ByStatus (Amount Dollars)
meStandardDeduction2024 = byStatus 14600 29200 14600 21900 29200

-- | 2024 Maine personal exemption amount ($5,000 per exemption)
-- Source: Maine Revenue Services, Individual Income Tax 2024 Rates
mePersonalExemption2024 :: Amount Dollars
mePersonalExemption2024 = 5000

-- | Line 18 before phase-out: line 13 count times the amount when neither
-- the filer nor spouse can be claimed as a dependent: one, or two on a joint return. QSS
-- gets one (line 13 table, PDF p.4 = printed p.4; 36 M.R.S. 5126-A(1)).
-- Dependents feed a credit, not line 18.
-- Order: Single, MFJ, MFS, HoH, QW
mePersonalExemptionBase2024 :: ByStatus (Amount Dollars)
mePersonalExemptionBase2024 = byStatus e (2 * e) e e e
  where
    e = mePersonalExemption2024

-- | Line 18 phase-out worksheet, line 2 ($323,900 Single; $388,650 MFJ/QSS; $194,325 MFS; $356,300 HoH).
-- Source: 2024 Form 1040ME instructions, PDF p.4 = printed p.4,
-- https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/24_1040me_book_gen_instr.pdf
mePersonalExemptionPhaseoutThreshold2024 :: ByStatus (Amount Dollars)
mePersonalExemptionPhaseoutThreshold2024 = byStatus 323900 388650 194325 356300 388650

-- | Line 18 phase-out worksheet, line 4: $62,500 MFS, $125,000 otherwise
-- (36 M.R.S. 5126-A(2)).
mePersonalExemptionPhaseoutRange2024 :: ByStatus (Amount Dollars)
mePersonalExemptionPhaseoutRange2024 = byStatus 125000 125000 62500 125000 125000
