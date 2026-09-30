module TablesWI2025
  ( -- * Wisconsin State Income Tax Brackets
    wisconsinBrackets2025,
    wisconsinBracketsTable2025,

    -- * Standard Deduction (sliding scale)
    wiStandardDeductionMax2025,
    wiStandardDeductionPhaseoutStart2025,
    wiStandardDeductionPhaseoutRate2025,

    -- * Personal Exemptions
    wiFilerExemptions2025,
    wiDependentExemption2025,
    wiAgeExemption2025,

    -- * Retirement Income Exclusion (new in 2025)
    wiRetirementExclusionAge2025,
    wiRetirementExclusionMax2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 Wisconsin State income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Wisconsin DOR FAQ "What are the individual income tax rates?"
-- (https://revenue.wi.gov/Pages/FAQS/pcs-taxrates.aspx) and DOR "Wisconsin Tax
-- Update - Fall 2025" slide 7; reconciles with the 2025 Form 1 Instructions
-- p. 44 Tax Computation Worksheet. Retrieved 2026-09-30.
-- Head of household uses the Single schedule (Form 1 Instructions p. 38 tax
-- table column "Single or Head of household"; p. 44 worksheet section A).
-- Wisconsin has no qualifying-surviving-spouse status: a federal QSS "may file
-- your Wisconsin return as head of household" (Form 1 Instructions, Filing
-- Status), so QW takes the head-of-household brackets, standard deduction and
-- exemptions.
-- 2025 Wisconsin Act 15 widened the 4.4% bracket; every threshold was also
-- indexed from 2024.
wisconsinBrackets2025 :: NonEmpty Bracket
wisconsinBrackets2025 =
  Bracket (byStatus 14680 19580 9790 14680 14680) 0.035
    :| [ Bracket (byStatus 50480 67300 33650 50480 50480) 0.044,
         Bracket (byStatus 323290 431060 215530 323290 323290) 0.053,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0765
       ]

wisconsinBracketsTable2025 :: Table
wisconsinBracketsTable2025 =
  case mkBracketTable wisconsinBrackets2025 of
    Right bt -> TableBracket "wi_brackets_2025" bt
    Left err -> error $ "Invalid Wisconsin brackets: " ++ err

-- | 2025 Wisconsin sliding-scale standard deduction, Wis. Stat. 71.05(22)(dp):
-- the maximum, less the phase-down rate times Wisconsin income over the
-- phase-out start, but not less than zero. Head of household never falls below
-- the Single amount at the same income (71.05(22)(dp)1.).
-- Order: Single, MFJ, MFS, HoH, QW
-- Statute: https://docs.legis.wisconsin.gov/statutes/statutes/71/i/05/22
--   (dp)1. base amounts: Single $7,200 over $10,380 at 12%; HoH $9,300 over
--   $10,380 at 22.515%. (dp)2. base amounts (2015 base year): MFJ $19,010 over
--   $21,360 and MFS $9,030 over $10,140, both at 19.778%. (dt) indexes every
--   dollar amount by August CPI-U and rounds to the nearest $10.
-- Indexed 2025 amounts: Wisconsin Legislative Fiscal Bureau,
-- Paper #325 to the Joint Committee on Finance, "Overview of Broad-Based General
-- Fund Tax Reductions" (2025-27 budget), Table 1 "Current Law Sliding Scale
-- Standard Deduction, Tax Year 2025", mirrored at
-- https://taxsim.nber.org/historical_state_tax_forms/WI/2025/325%20-%20General%20Fund%20Taxes%20--%20Income%20and%20Franchise%20Taxes_Overview%20of%20Broad-Based%20General%20Fund%20Tax%20Reductions.pdf
-- (retrieved 2026-09-30). The maxima match the 2025 Form 1 Instructions p. 35,
-- https://www.revenue.wi.gov/TaxForms2025/2025-Form1-Inst.pdf
-- The DOR Standard Deduction Table (Form 1 Instructions pp. 35-37) prices this
-- formula at the midpoint of $500 income bands; the graph computes the exact
-- statutory formula instead.
wiStandardDeductionMax2025 :: ByStatus (Amount Dollars)
wiStandardDeductionMax2025 = byStatus 13560 25110 11930 17520 17520

wiStandardDeductionPhaseoutStart2025 :: ByStatus (Amount Dollars)
wiStandardDeductionPhaseoutStart2025 = byStatus 19550 28210 13390 19550 19550

-- | Statutory phase-down rates, Wis. Stat. 71.05(22)(dp) (not indexed).
wiStandardDeductionPhaseoutRate2025 :: ByStatus (Amount Rate)
wiStandardDeductionPhaseoutRate2025 = byStatus 0.12 0.19778 0.19778 0.22515 0.22515

-- | 2025 Wisconsin personal exemptions for the filer (and spouse on a joint
-- return), Form 1 line 10a: $700 each. Wis. Stat. 71.05(23)(b)1. denies the
-- spouse exemption when filing separately or as head of household.
-- Assumes no filer can be claimed as someone else's dependent (the API has no
-- such input); a claimable filer would get $0 here.
-- Order: Single, MFJ, MFS, HoH, QW
wiFilerExemptions2025 :: ByStatus (Amount Dollars)
wiFilerExemptions2025 = byStatus 700 1400 700 700 700

-- | 2025 Wisconsin exemption per dependent, Form 1 line 10a; Wis. Stat.
-- 71.05(23)(b)2.
wiDependentExemption2025 :: Amount Dollars
wiDependentExemption2025 = 700

-- | 2025 Wisconsin age exemption (65+), Form 1 line 10b; Wis. Stat.
-- 71.05(23)(b)3. Not applied: the API carries no age input.
wiAgeExemption2025 :: Amount Dollars
wiAgeExemption2025 = 250

-- | 2025 Wisconsin retirement income exclusion minimum age
-- Source: 2025 Wisconsin Act 15 (signed July 3, 2025)
-- Note: Taxpayers age 67+ can exclude retirement income.
wiRetirementExclusionAge2025 :: Int
wiRetirementExclusionAge2025 = 67

-- | 2025 Wisconsin retirement income exclusion maximum amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: 2025 Wisconsin Act 15 (signed July 3, 2025)
-- Note: $24,000 per individual age 67+, $48,000 for MFJ if both spouses 67+.
-- No income limits or phase-outs apply.
wiRetirementExclusionMax2025 :: ByStatus (Amount Dollars)
wiRetirementExclusionMax2025 = byStatus 24000 48000 24000 24000 48000
