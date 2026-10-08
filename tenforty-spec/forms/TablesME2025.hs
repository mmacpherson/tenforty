module TablesME2025
  ( -- * Maine State Income Tax Brackets
    maineBrackets2025,
    maineBracketsTable2025,

    -- * Standard Deduction
    meStandardDeduction2025,

    -- * Personal Exemption
    mePersonalExemption2025,
    mePersonalExemptionBase2025,
    mePersonalExemptionPhaseoutThreshold2025,
    mePersonalExemptionPhaseoutRange2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 Maine state income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Maine Revenue Services, Individual Income Tax 2025 Rates
-- (https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/ind_tax_rate_sched_2025.pdf)
-- Published September 15, 2024
-- COLA adjustments: 1.274 for lower brackets, 1.269 for upper brackets
maineBrackets2025 :: NonEmpty Bracket
maineBrackets2025 =
  Bracket (byStatus 26800 53600 26800 40200 53600) 0.058
    :| [ Bracket (byStatus 63450 126900 63450 95150 126900) 0.0675,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0715
       ]

maineBracketsTable2025 :: Table
maineBracketsTable2025 =
  case mkBracketTable maineBrackets2025 of
    Right bt -> TableBracket "me_brackets_2025" bt
    Left err -> error $ "Invalid Maine brackets: " ++ err

-- | 2025 Maine state standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Maine Revenue Services, Individual Income Tax 2025 Rates
-- Standard deduction equals federal standard deduction for 2025:
-- \$15,750 (Single/MFS), $31,500 (MFJ), $23,625 (HoH)
meStandardDeduction2025 :: ByStatus (Amount Dollars)
meStandardDeduction2025 = byStatus 15750 31500 15750 23625 31500

-- | 2025 Maine personal exemption amount ($5,150 per exemption)
-- Source: Maine Revenue Services, Individual Income Tax 2025 Rates
-- COLA adjustment: 1.25 × $4,120 (base amount) = $5,150
mePersonalExemption2025 :: Amount Dollars
mePersonalExemption2025 = 5150

-- | Line 18 before phase-out: line 13 count times the amount when neither
-- the filer nor spouse can be claimed as a dependent: one, or two on a joint return. QSS
-- gets one (line 13 table, PDF p.5 = printed p.4; 36 M.R.S. 5126-A(1)).
-- Dependents feed a credit, not line 18.
-- Order: Single, MFJ, MFS, HoH, QW
mePersonalExemptionBase2025 :: ByStatus (Amount Dollars)
mePersonalExemptionBase2025 = byStatus e (2 * e) e e e
  where
    e = mePersonalExemption2025

-- | Line 18 phase-out worksheet, line 2 ($333,450 Single; $400,100 MFJ/QSS; $200,050 MFS; $366,750 HoH).
-- Source: 2025 Form 1040ME instructions, PDF p.6 = printed p.5,
-- https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/25_1040me_gen_instr_w_cover_pg.pdf
mePersonalExemptionPhaseoutThreshold2025 :: ByStatus (Amount Dollars)
mePersonalExemptionPhaseoutThreshold2025 = byStatus 333450 400100 200050 366750 400100

-- | Line 18 phase-out worksheet, line 4: $62,500 MFS, $125,000 otherwise
-- (36 M.R.S. 5126-A(2)).
mePersonalExemptionPhaseoutRange2025 :: ByStatus (Amount Dollars)
mePersonalExemptionPhaseoutRange2025 = byStatus 125000 125000 62500 125000 125000
