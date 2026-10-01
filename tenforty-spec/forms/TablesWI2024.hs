module TablesWI2024
  ( -- * Wisconsin State Income Tax Brackets
    wisconsinBrackets2024,
    wisconsinBracketsTable2024,

    -- * Standard Deduction (sliding scale)
    wiStandardDeductionMax2024,
    wiStandardDeductionPhaseoutStart2024,
    wiStandardDeductionPhaseoutRate2024,

    -- * Personal Exemptions
    wiFilerExemptions2024,
    wiDependentExemption2024,
    wiAgeExemption2024,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2024 Wisconsin State income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Wisconsin DOR FAQ "What are the individual income tax rates?" (2024
-- schedule, Wayback snapshot 2025-03-05 of
-- https://www.revenue.wi.gov/Pages/FAQS/pcs-taxrates.aspx); reconciles with the
-- 2024 Form 1 Instructions p. 44 Tax Computation Worksheet. Retrieved 2026-09-30.
-- Head of household uses the Single schedule (Form 1 Instructions p. 38 tax
-- table column "Single or Head of household"; p. 44 worksheet section A).
-- Wisconsin has no qualifying-surviving-spouse status: a federal QSS "may file
-- your Wisconsin return as head of household" (Form 1 Instructions, Filing
-- Status), so QW takes the head-of-household brackets, standard deduction and
-- exemptions.
wisconsinBrackets2024 :: NonEmpty Bracket
wisconsinBrackets2024 =
  Bracket (byStatus 14320 19090 9550 14320 14320) 0.035
    :| [ Bracket (byStatus 28640 38190 19090 28640 28640) 0.044,
         Bracket (byStatus 315310 420420 210210 315310 315310) 0.053,
         Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0765
       ]

wisconsinBracketsTable2024 :: Table
wisconsinBracketsTable2024 =
  case mkBracketTable wisconsinBrackets2024 of
    Right bt -> TableBracket "wi_brackets_2024" bt
    Left err -> error $ "Invalid Wisconsin brackets: " ++ err

-- | 2024 Wisconsin sliding-scale standard deduction, Wis. Stat. 71.05(22)(dp):
-- the maximum, less the phase-down rate times Wisconsin income over the
-- phase-out start, but not less than zero. Head of household never falls below
-- the Single amount at the same income (71.05(22)(dp)1.).
-- Order: Single, MFJ, MFS, HoH, QW
-- Statute: https://docs.legis.wisconsin.gov/statutes/statutes/71/i/05/22
--   (dp)1. base amounts: Single $7,200 over $10,380 at 12%; HoH $9,300 over
--   $10,380 at 22.515%. (dp)2. base amounts (2015 base year): MFJ $19,010 over
--   $21,360 and MFS $9,030 over $10,140, both at 19.778%. (dt) indexes every
--   dollar amount by August CPI-U and rounds to the nearest $10.
-- Indexed 2024 amounts: Wisconsin Legislative Fiscal Bureau,
-- Informational Paper 2 "Individual Income Tax" (January 2025), Table 1
-- "Sliding Scale Standard Deduction (Tax Year 2024)",
-- https://docs.legis.wisconsin.gov/misc/lfb/informational_papers/january_2025/0002_individual_income_tax_informational_paper_2.pdf
-- (retrieved 2026-09-30). The maxima match the 2024 Form 1 Instructions p. 35,
-- https://www.revenue.wi.gov/TaxForms2024/2024-Form1-Inst.pdf
-- The DOR Standard Deduction Table (Form 1 Instructions pp. 35-37) prices this
-- formula at the midpoint of $500 income bands; the graph computes the exact
-- statutory formula instead.
wiStandardDeductionMax2024 :: ByStatus (Amount Dollars)
wiStandardDeductionMax2024 = byStatus 13230 24490 11630 17090 17090

wiStandardDeductionPhaseoutStart2024 :: ByStatus (Amount Dollars)
wiStandardDeductionPhaseoutStart2024 = byStatus 19070 27520 13060 19070 19070

-- | Statutory phase-down rates, Wis. Stat. 71.05(22)(dp) (not indexed).
wiStandardDeductionPhaseoutRate2024 :: ByStatus (Amount Rate)
wiStandardDeductionPhaseoutRate2024 = byStatus 0.12 0.19778 0.19778 0.22515 0.22515

-- | 2024 Wisconsin personal exemptions for the filer (and spouse on a joint
-- return), Form 1 line 10a: $700 each. Wis. Stat. 71.05(23)(b)1. denies the
-- spouse exemption when filing separately or as head of household.
-- Assumes no filer can be claimed as someone else's dependent (the API has no
-- such input); a claimable filer would get $0 here.
-- Order: Single, MFJ, MFS, HoH, QW
wiFilerExemptions2024 :: ByStatus (Amount Dollars)
wiFilerExemptions2024 = byStatus 700 1400 700 700 700

-- | 2024 Wisconsin exemption per dependent, Form 1 line 10a; Wis. Stat.
-- 71.05(23)(b)2.
wiDependentExemption2024 :: Amount Dollars
wiDependentExemption2024 = 700

-- | 2024 Wisconsin age exemption (65+), Form 1 line 10b; Wis. Stat.
-- 71.05(23)(b)3. Not applied: the API carries no age input.
wiAgeExemption2024 :: Amount Dollars
wiAgeExemption2024 = 250
