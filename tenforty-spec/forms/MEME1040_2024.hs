{-# LANGUAGE OverloadedStrings #-}

module MEME1040_2024
  ( meme1040_2024,
  )
where

import FormRefs
import MaineTax
import TablesME2024
import TenForty

meme1040_2024 :: Either FormError Form
meme1040_2024 = form "me_1040me" 2024 $ do
  defineTable maineBracketsTable2024

  -- Line 1: Federal AGI (imported from US 1040)
  let federalAgi = roundE (importForm us1040L11)
  l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" federalAgi

  -- Lines 2-5: Maine additions to income
  l2 <- roundE <$> keyInput "L2" "me_addition_municipal_bond_interest" "Interest on non-ME municipal bonds"
  l3 <- roundE <$> keyInput "L3" "me_addition_529_excess" "529 plan contributions exceeding annual limit"
  l4 <- roundE <$> keyInput "L4" "me_addition_pension_military" "Pension/military income included in federal return"
  l5 <- roundE <$> keyInput "L5" "me_addition_other" "Other Maine additions"

  l6 <-
    interior "L6" "me_total_additions" $
      sumOf [l2, l3, l4, l5]

  -- Line 7: Maine income
  l7 <-
    interior "L7" "me_income" $
      l1 .+. l6

  -- Lines 8-14: Maine subtractions from income
  l8 <- roundE <$> keyInput "L8" "me_sub_pension_exclusion" "Pension income (up to $10,000)"
  l9 <- roundE <$> keyInput "L9" "me_sub_military_pay" "Military pay and retirement"
  l10 <- roundE <$> keyInput "L10" "me_sub_social_security" "Social security and railroad retirement benefits"
  l11 <- roundE <$> keyInput "L11" "me_sub_state_refund" "State/local income tax refunds"
  l12 <- roundE <$> keyInput "L12" "me_sub_529_earnings" "Maine 529 plan earnings"
  l13 <- roundE <$> keyInput "L13" "me_sub_student_loan" "Student loan payments"
  l14 <- roundE <$> keyInput "L14" "me_sub_other" "Other Maine subtractions"

  l15 <-
    interior "L15" "me_total_subtractions" $
      sumOf [l8, l9, l10, l11, l12, l13, l14]

  -- Line 16: Maine adjusted gross income
  l16 <-
    keyOutput "L16" "me_agi" "Maine adjusted gross income" $
      l7 .-. l15

  -- Line 17-19: Deductions
  -- Use larger of itemized or standard deduction
  itemized <- roundE <$> keyInput "L17_itemized" "me_itemized" "Maine itemized deductions"
  stdDed <-
    interior "StdDed" "me_std_deduction" $
      byStatusE (fmap lit meStandardDeduction2024)
  deductionBeforePhaseout <-
    interior "DeductionBeforePhaseout" "deduction_before_phaseout" $
      greaterOf itemized stdDed

  -- Line 17 phase-out worksheet (Worksheet for Standard/Itemized
  -- Deductions): four-place line 5 rounded half up, whole-dollar line 7.
  l19 <-
    interior "L19" "deduction" $
      phaseoutWorksheet
        meStandardDeductionPhaseoutThreshold2024
        meStandardDeductionPhaseoutRange2024
        l16
        deductionBeforePhaseout

  -- Line 20: Maine AGI minus deductions
  l20 <-
    interior "L20" "me_agi_minus_deductions" $
      l16 `subtractNotBelowZero` l19

  -- Line 21: Personal exemption deduction (official Form 1040ME line 18).
  -- \$5,000 per line-13 exemption: one, or two on a joint return; dependents
  -- feed a credit instead. The legacy input is TOTAL pre-phase-out dollars
  -- (worksheet line 6) including that base, so take the greater of the two
  -- rather than adding them. Returns where the filer or spouse can be
  -- claimed as another's dependent are outside the supported domain: line 13
  -- then drops below the base (0 for a claimable Single/MFS/HoH/QSS filer; 0
  -- or 1 on MFJ), which max() cannot express (tenforty-avr.3).
  -- 2024 Form 1040ME instructions, PDF p.4 = printed p.4 (line 13, line 18 and
  -- its phase-out worksheet); 36 M.R.S. 5126-A.
  exemptionTotal <- roundE <$> keyInput "L21" "total_exemptions" "Total exemption dollars before phase-out, including taxpayer/spouse"
  mandatoryExemptions <-
    interior "MandatoryExemptions" "mandatory_exemptions" $
      byStatusE (fmap lit mePersonalExemptionBase2024)
  exemptionsBeforePhaseout <-
    interior "L21_total" "total_exemptions_before_phaseout" $
      greaterOf mandatoryExemptions exemptionTotal

  -- Phase-out worksheet (Worksheet for Phaseout of Personal Exemption
  -- Deduction Amount): four-place line 5 rounded half up, whole-dollar line 7.
  l21 <-
    interior "L21_allowed" "exemptions_after_phaseout" $
      phaseoutWorksheet
        mePersonalExemptionPhaseoutThreshold2024
        mePersonalExemptionPhaseoutRange2024
        l16
        exemptionsBeforePhaseout

  -- Line 22: Maine taxable income
  _l22 <-
    keyOutput "L22" "me_taxable_income" "Maine taxable income" $
      l20 `subtractNotBelowZero` l21

  -- Line 23: Same as L22 (for tax computation)
  l23 <- interior "L23" "me_taxable_for_computation" $ line "L22"

  -- Line 24 (official line 20): the tax table below \$100,000, the printed
  -- rate schedule at and above it (MaineTax).
  l24 <-
    keyOutput "L24" "me_bracket_tax" "Maine income tax (tax table or rate schedule)" $
      maineIncomeTax maineSchedule2024 "me_brackets_2024" l23

  -- Lines 25-28: Credits
  l25 <- roundE <$> keyInput "L25" "me_tax_credit_property" "Property tax fairness credit"
  l26 <- roundE <$> keyInput "L26" "me_tax_credit_seed_capital" "Seed capital investment credit"
  l27 <- roundE <$> keyInput "L27" "me_tax_credit_child_care" "Child care credit"
  l28 <- roundE <$> keyInput "L28" "me_tax_credit_other_nonrefundable" "Other Maine nonrefundable credits"

  l29 <-
    interior "L29" "me_total_credits" $
      sumOf [l25, l26, l27, l28]

  -- Line 30: Tax after credits
  l30 <-
    interior "L30" "me_tax_after_credits" $
      l24 `subtractNotBelowZero` l29

  -- Line 31: Other taxes
  l31 <- roundE <$> keyInput "L31" "me_other_taxes" "Other Maine taxes"

  -- Line 32: Total Maine tax
  l32 <-
    keyOutput "L32" "me_total_tax" "Total Maine tax" $
      l30 .+. l31

  -- Lines 33-35: Refundable credits
  l33 <- roundE <$> keyInput "L33" "me_sales_tax_credit" "Sales tax fairness credit"
  l34 <- roundE <$> keyInput "L34" "me_eic" "Maine earned income credit"
  l35 <- roundE <$> keyInput "L35" "me_refundable_credit_other" "Other Maine refundable credits"

  l36 <-
    keyOutput "L36" "me_total_refundable_credits" "Total refundable credits" $
      sumOf [l33, l34, l35]

  -- Lines 37-39: Payments
  l37 <- roundE <$> keyInput "L37" "me_withholding" "Maine income tax withheld"
  l38 <- roundE <$> keyInput "L38" "me_estimated_payments" "Estimated tax payments"
  l39 <- roundE <$> keyInput "L39" "me_extension_payment" "Amount paid with extension"

  l40 <-
    keyOutput "L40" "me_total_payments" "Total Maine tax payments" $
      sumOf [l37, l38, l39]

  -- Total payments + refundable credits
  l41 <-
    keyOutput "L41" "me_total_payments_and_credits" "Total payments and refundable credits" $
      l40 .+. l36

  -- Refund or amount owed
  _overpayment <-
    keyOutput "L42" "me_overpayment" "Amount overpaid" $
      l41 `excessOf` l32

  _amountOwed <-
    keyOutput "L43" "me_amount_owed" "Amount you owe" $
      l32 `subtractNotBelowZero` l41

  outputs
    [ "L1",
      "L16",
      "L22",
      "L24",
      "L32",
      "L36",
      "L40",
      "L41",
      "L42",
      "L43"
    ]
