{-# LANGUAGE OverloadedStrings #-}

module MAForm1_2025
  ( maForm1_2025,
  )
where

import FormRefs
import TablesMA2025
import TenForty

maForm1_2025 :: Either FormError Form
maForm1_2025 = form "ma_1" 2025 $ do
  -- Line 3: Wages, salaries, tips (from US 1040)
  -- Line 4: Taxable pensions and annuities (from US 1040)
  -- Lines 5-9: Various income sources
  -- Line 10: Total income (approximated by federal AGI for this implementation)
  let federalAgi = importForm us1040L11
  l10 <-
    keyOutput
      "L10"
      "ma_total_income"
      "Total Massachusetts income"
      federalAgi

  -- Lines 11-16: Deductions (Social Security, alimony, rental deduction, etc.)
  -- For simplicity, we accept total deductions as an input
  deductions <- keyInput "L16" "ma_deductions" "Total Massachusetts deductions"

  -- Line 17: 5.0% Income After Deductions
  l17 <-
    keyOutput "L17" "ma_income_after_deductions" "5.0% income after deductions" $
      l10 `subtractNotBelowZero` deductions

  -- Line 2: Exemptions
  -- Line 2a: Personal exemption (based on filing status)
  l2a <-
    interior "L2a" "personal_exemption" $
      byStatusE (fmap lit maPersonalExemption2025)

  -- Line 2b: Dependent exemptions ($1,000 per dependent)
  l2b <- keyInput "L2b" "dependent_exemptions" "Dependent exemptions"

  -- Line 2c: Age 65 or over exemption
  l2c <- keyInput "L2c" "age_65_exemption" "Age 65 or over exemption"

  -- Line 2d: Blindness exemption
  l2d <- keyInput "L2d" "blindness_exemption" "Blindness exemption"

  -- Line 2e: Medical/dental expenses exemption
  l2e <- keyInput "L2e" "medical_exemption" "Medical/dental expenses exemption"

  -- Line 2f: Adoption agency fee exemption
  l2f <- keyInput "L2f" "adoption_exemption" "Adoption agency fee exemption"

  -- Line 18: Total exemptions
  l18 <-
    interior "L18" "total_exemptions" $
      sumOf [l2a, l2b, l2c, l2d, l2e, l2f]

  -- Line 19: 5.0% Income After Exemptions (taxable income for base rate)
  l19 <-
    keyOutput "L19" "ma_taxable_income" "Massachusetts taxable income (5.0% income after exemptions)" $
      l17 `subtractNotBelowZero` l18

  -- Line 20: Interest and dividend income (taxed separately at 5.0%)
  l20 <- keyInput "L20" "interest_dividend_income" "Interest and dividend income"

  -- Line 21: Combined income for tax computation
  l21 <-
    interior "L21" "combined_income" $
      l19 .+. l20

  -- Line 22: 5.0% Tax
  l22 <-
    keyOutput "L22" "ma_base_tax" "5.0% tax" $
      l21 .*. rate maBaseRate2025

  -- Line 23a: Short-term capital gains (taxed at 8.5%)
  l23a <- keyInput "L23a" "short_term_gains" "8.5% income from short-term capital gains"

  l23a_tax <-
    interior "L23a_tax" "short_term_gains_tax" $
      l23a .*. rate maShortTermCapitalGainsRate2025

  -- Line 23b: Long-term collectibles gains (taxed at 12%)
  l23b <- keyInput "L23b" "collectibles_gains" "12% income from long-term collectibles"

  l23b_tax <-
    interior "L23b_tax" "collectibles_tax" $
      l23b .*. rate maLongTermCollectiblesRate2025

  -- Line 24: Schedule D long-term capital gains (taxed at 5.0%)
  l24 <- keyInput "L24" "long_term_gains" "Long-term capital gains"

  l24_tax <-
    interior "L24_tax" "long_term_gains_tax" $
      l24 .*. rate maBaseRate2025

  -- Line 25-26: Other taxes and recaptures
  l25 <- keyInput "L25" "credit_recapture" "Credit recapture amount"
  l26 <- keyInput "L26" "installment_sale_tax" "Additional tax on installment sales"

  -- Line 27 Massachusetts AGI worksheet, 2025 instructions, PDF
  -- pp.13-14: ordinary Form 1 deductions are NOT the Schedule Y subset here.
  -- L10 retains the pre-existing federal-AGI approximation (b72.39); these worksheet
  -- facts are raw-form inputs, not new public natural inputs.
  -- https://www.mass.gov/doc/2025-form-1-instructions/download
  abandonedBuildingAddback <- keyInput "NTS_Addback" "abandoned_building_addback" "Abandoned building renovation deduction addback"
  -- MA-only eligible items, not a duplicate of federal adjustments: those
  -- adjustments are excluded by the supported-domain guard below.
  ntsScheduleY <- keyInput "NTS_ScheduleY" "eligible_schedule_y" "MA-only Schedule Y lines 2 through 9b and 10"
  ntsFivePercentIncome <-
    interior "NTS_FivePercentIncome" "nts_five_percent_income" $
      greaterOf (l10 .+. abandonedBuildingAddback) (dollars 0) `subtractNotBelowZero` ntsScheduleY
  -- Schedule B35 precedes B36 excess exemptions; B38/B39 feed L20/L23.
  -- Schedule D19 precedes D20 excess exemptions; D21/D22 feed L24.
  -- These taxable buckets cannot establish the worksheet's pre-exemption
  -- amounts. Apply NTS only with no represented investment/capital facts;
  -- otherwise preserve the existing computation pending b72.40's worksheet.
  -- Absolute values prevent raw, federal ST/LT, or carryover cancellation.
  ntsCapitalFacts <-
    interior "NTS_CapitalFacts" "nts_capital_facts" $
      sumOf $
        fmap
          (\amount -> greaterOf amount (dollars 0 .-. amount))
          [l20, l23a, l23b, l24]
  -- The represented W-2-only model assumes federal box 1 equals MA wages.
  -- Form 1 L3 instead uses box 16 for state/local pension contributors:
  -- unrepresented section 414(h) pickups can still cause false NTS (b72.39).
  -- Other income/adjustments require reconciliation under b72.39/b72.40;
  -- check individual signed facts, not their net sum.
  let ntsNonWageIncomeMagnitude = importForm us1040NonWageIncomeMagnitude
  ntsMassachusettsAgi <-
    interior "NTS_MassachusettsAGI" "nts_massachusetts_agi" ntsFivePercentIncome
  -- L2b is already dollars: $1,000 per dependent, the same increment that
  -- applies to the joint/HoH NTS threshold. Single gets no dependent increment.
  -- MFS is explicitly ineligible; QW equivalence remains r91.9, not guessed.
  ntsThreshold <-
    interior "NTS_Threshold" "nts_threshold" $
      byStatusE (byStatus (dollars 8000) (dollars 16400 .+. l2b) (dollars 0) (dollars 14400 .+. l2b) (dollars 0))
  ntsEligibleStatus <-
    interior "NTS_EligibleStatus" "nts_eligible_status" $
      byStatusE (byStatus (dollars 1) (dollars 1) (dollars 0) (dollars 1) (dollars 0))
  noTaxStatus <-
    keyOutput "L27" "no_tax_status" "No Tax Status qualification (1 qualifies, 0 does not)" $
      ifPos (ntsCapitalFacts .+. ntsNonWageIncomeMagnitude) (dollars 0) $
        ifPos ntsEligibleStatus (ifGte ntsThreshold ntsMassachusettsAgi (dollars 1) (dollars 0)) (dollars 0)

  -- Line 28a: Total income tax (before surtax)
  l28a <-
    interior "L28a" "income_tax_before_surtax" $
      ifPos noTaxStatus (l25 .+. l26) (sumOf [l22, l23a_tax, l23b_tax, l24_tax, l25, l26])

  -- Line 28b: 4% Surtax on income over threshold
  -- Total taxable income for surtax = L21 + L23a + L23b + L24
  totalTaxableForSurtax <-
    interior "total_taxable_for_surtax" "total_taxable_for_surtax" $
      sumOf [l21, l23a, l23b, l24]

  surtaxableIncome <-
    interior "surtaxable_income" "surtaxable_income" $
      totalTaxableForSurtax `subtractNotBelowZero` lit maSurtaxThreshold2025

  l28b <-
    keyOutput "L28b" "ma_surtax" "4% surtax" $
      ifPos noTaxStatus (dollars 0) (surtaxableIncome .*. rate maSurtaxRate2025)

  -- Line 28: Total tax (income tax + surtax)
  l28 <-
    keyOutput "L28" "ma_total_tax" "Total Massachusetts tax" $
      l28a .+. l28b

  -- Credits (lines 29-32)
  l29 <- keyInput "L29" "limited_income_credit" "Limited income credit"
  l30 <- keyInput "L30" "other_state_tax_credit" "Taxes due any other state"
  l31 <- keyInput "L31" "other_credits" "Other credits"

  l32 <-
    interior "L32" "total_credits" $
      -- NTS skips 29-31, except recapture/installment tax requires line 31.
      ifPos noTaxStatus (ifPos (l25 .+. l26) l31 (dollars 0)) (sumOf [l29, l30, l31])

  -- Tax after credits
  taxAfterCredits <-
    interior "tax_after_credits" "tax_after_credits" $
      l28 `subtractNotBelowZero` l32

  -- Line 33-35: Additional amounts
  l33 <- keyInput "L33" "voluntary_contributions" "Voluntary contributions"
  l34 <- keyInput "L34" "use_tax" "Massachusetts use tax"
  l35 <- keyInput "L35" "health_care_penalty" "Health care penalty"

  -- Line 36: Total amount due
  _ <-
    interior "L36" "total_amount_due" $
      sumOf [taxAfterCredits, l33, l34, l35]

  -- Payments (lines 37-39)
  l37 <- keyInput "L37" "ma_withholding" "Massachusetts tax withheld"
  l38 <- keyInput "L38" "estimated_payments" "Estimated tax payments"
  l39 <- keyInput "L39" "refundable_credits" "Refundable credits"

  _l40 <-
    interior "L40" "total_payments" $
      sumOf [l37, l38, l39]

  outputs
    [ "L10",
      "L17",
      "L19",
      "L22",
      "L27",
      "L28b",
      "L28"
    ]
