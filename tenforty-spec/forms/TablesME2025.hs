module TablesME2025
  ( -- * Maine State Income Tax Brackets
    maineBrackets2025,
    maineBracketsTable2025,
    maineSchedule2025,

    -- * Standard Deduction
    meStandardDeduction2025,
    meStandardDeductionPhaseoutThreshold2025,
    meStandardDeductionPhaseoutRange2025,

    -- * Personal Exemption
    mePersonalExemption2025,
    mePersonalExemptionBase2025,
    mePersonalExemptionPhaseoutThreshold2025,
    mePersonalExemptionPhaseoutRange2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import MaineTax (MaineSchedule (..))
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

-- | 2025 Tax Rate Schedules #1-#3 as printed, with whole-dollar bracket bases
-- (line 20 at \$100,000 and over, and every tax table row).
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Maine Revenue Services, Individual Income Tax 2025 Rates (prepared
-- February 3, 2026),
-- https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/ind_tax_rate_sched_2025.pdf
maineSchedule2025 :: MaineSchedule
maineSchedule2025 =
  MaineSchedule
    { lowRate = 0.058,
      middleRate = 0.0675,
      topRate = 0.0715,
      middleStart = byStatus 26800 53600 26800 40200 53600,
      middleBase = byStatus 1554 3109 1554 2332 3109,
      topStart = byStatus 63450 126900 63450 95150 126900,
      topBase = byStatus 4028 8057 4028 6041 8057
    }

-- | 2025 Maine state standard deduction amounts, before the line 17 phase-out
-- Order: Single, MFJ, MFS, HoH, QW
-- Not the OBBBA federal 15,750/31,500/23,625. The 2025 forms were printed under
-- IRC conformity as of 2024-12-31 (36 M.R.S. 111(1-A)), before OBBBA. P.L. 2025,
-- c. 650 later moved conformity to 2025-12-31 but also fixed the 2025 basic
-- amount in 36 M.R.S. 5124-C(1-B): \$15,000 (Single/MFS), x1.5 HoH, x2 MFJ/QSS.
-- Either way the amount is \$15,000 / \$30,000 / \$22,500.
-- Source: Maine Revenue Services, Individual Income Tax 2025 Rates; 2025 Form
-- 1040ME instructions, Standard Deduction Chart (PDF p.5).
meStandardDeduction2025 :: ByStatus (Amount Dollars)
meStandardDeduction2025 = byStatus 15000 30000 15000 22500 30000

-- | Line 17 phase-out worksheet, line 2 (\$100,000 Single/MFS; \$200,050 MFJ/QSS;
-- \$150,000 HoH).
-- Source: 2025 Form 1040ME instructions, Worksheet for Standard/Itemized
-- Deductions (PDF p.5),
-- https://www.maine.gov/revenue/sites/maine.gov.revenue/files/inline-files/25_1040me_gen_instr_w_cover_pg.pdf
meStandardDeductionPhaseoutThreshold2025 :: ByStatus (Amount Dollars)
meStandardDeductionPhaseoutThreshold2025 = byStatus 100000 200050 100000 150000 200050

-- | Line 17 phase-out worksheet, line 4: \$75,000 Single/MFS, \$150,000 MFJ/QSS,
-- \$112,500 HoH (36 M.R.S. 5124-C(2)).
meStandardDeductionPhaseoutRange2025 :: ByStatus (Amount Dollars)
meStandardDeductionPhaseoutRange2025 = byStatus 75000 150000 75000 112500 150000

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
