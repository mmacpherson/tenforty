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
  -- Lines 3-10: Massachusetts 5% income, built from the federal income lines
  -- rather than federal AGI (tenforty-b72.39), so federal Schedule 1 Part II
  -- adjustments do not reach Massachusetts: M.G.L. c.62 2(d)(1)(K) disallows
  -- the one-half self-employment tax deduction, and the instructions disallow
  -- IRA/Keogh/SEP/SIMPLE deductions. Massachusetts allows its own deductions on
  -- lines 11-15 and Schedule Y. Interest, dividends and capital gains are not 5%
  -- income here: they go through Schedules B and D below. Taxable Social
  -- Security is exempt, and state income tax refunds (Schedule 1 line 1) are
  -- not Massachusetts income. Interest is treated as non-Massachusetts-bank
  -- (Schedule B); long-term gains as non-collectible (no public input).
  -- Blind derivation: docs/validation/derivations/ma-2024-2025.md.
  wages <-
    interior "L3" "wages" $
      importForm us1040L1z
  pensions <-
    interior "L4" "pensions" $
      importForm us1040L4b .+. importForm us1040L5b
  otherFivePercentIncome <-
    interior "L5to9" "other_five_percent_income" $
      importForm usSchedule1L10 .-. importForm usSchedule1L1
  l10 <-
    keyOutput "L10" "ma_total_income" "Total Massachusetts 5% income" $
      sumOf [wages, pensions, otherFivePercentIncome]

  -- Line 11: Social Security, Medicare and self-employment tax paid, not more
  -- than \$2,000 per person (instructions line 11; c.62 3.B(a)(3)-(4)). The
  -- statute names FICA and RRTA; the DOR instruction includes self-employment
  -- tax "as reported on your U.S. Form 1040", which this follows. w2_income is
  -- a household aggregate, so a joint return takes the one-earner reading
  -- (11a = min(\$2,000, paid), 11b = 0), as schedule_se_ss_wages does; a
  -- two-earner couple over-pays by at most 5% of \$2,000.
  ficaPaid <-
    interior "L11_fica" "fica_paid" $
      (smallerOf (greaterOf wages (dollars 0)) (lit maSocialSecurityWageBase2025) .*. rate maSocialSecurityRate)
        .+. (greaterOf wages (dollars 0) .*. rate maMedicareRate)
  l11 <-
    interior "L11" "contribution_deduction" $
      smallerOf (ficaPaid .+. importForm usScheduleSeL10) (lit maContributionDeductionCap2025)

  -- Lines 12-15: other Massachusetts deductions, accepted as a raw total.
  otherDeductions <- keyInput "L16" "ma_deductions" "Massachusetts deductions other than line 11 (lines 12-15)"
  deductions <-
    interior "L16_total" "total_deductions" $
      l11 .+. otherDeductions

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

  -- Schedules B and D (tenforty-b72.6). Part 1 is interest and dividends; short-
  -- term gains are 8.5% income. A net short-term loss offsets interest and
  -- dividends first, then long-term gains; a net long-term loss offsets short-
  -- term gains first, then interest and dividends. Together the two offset at
  -- most \$2,000 of interest and dividends (Schedule B lines 20-32; c.62 2(c)).
  -- Capital losses never offset 5% income; unused losses carry forward, which
  -- this does not model (no carryover input).
  interestDividends <-
    interior "SchB_L9" "interest_dividends" $
      greaterOf (importForm us1040L2b .+. importForm us1040L3b) (dollars 0)
  netShortTerm <- interior "SchB_ShortTerm" "net_short_term" $ importForm usScheduleDL7
  netLongTerm <- interior "SchD_LongTerm" "net_long_term" $ importForm usScheduleDL15
  let shortTermGain = greaterOf netShortTerm (dollars 0)
      shortTermLoss = greaterOf (dollars 0 .-. netShortTerm) (dollars 0)
      longTermGain = greaterOf netLongTerm (dollars 0)
      longTermLoss = greaterOf (dollars 0 .-. netLongTerm) (dollars 0)
      lossOffsetCap = smallerOf interestDividends (lit maCapitalLossInterestDividendCap2025)
  shortTermLossAgainstInterest <-
    interior "SchB_L20" "short_term_loss_against_interest_dividends" $
      smallerOf shortTermLoss lossOffsetCap
  shortTermLossAgainstLongTerm <-
    interior "SchB_L22" "short_term_loss_against_long_term" $
      smallerOf (shortTermLoss .-. shortTermLossAgainstInterest) longTermGain
  longTermLossAgainstShortTerm <-
    interior "SchB_L25" "long_term_loss_against_short_term" $
      smallerOf longTermLoss shortTermGain
  longTermLossAgainstInterest <-
    interior "SchB_L32" "long_term_loss_against_interest_dividends" $
      smallerOf
        (longTermLoss .-. longTermLossAgainstShortTerm)
        (lossOffsetCap .-. shortTermLossAgainstInterest)
  interestDividendsAfterLosses <-
    interior "SchB_InterestDividendsAfterLosses" "interest_dividends_after_losses" $
      interestDividends .-. shortTermLossAgainstInterest .-. longTermLossAgainstInterest
  schB35 <-
    interior "SchB_L35" "schedule_b_income" $
      interestDividendsAfterLosses .+. (shortTermGain .-. longTermLossAgainstShortTerm)

  -- Excess exemptions (Form 1 line 18 over line 17) reduce Schedule B line 36
  -- first, then Schedule D line 20. Married filing separately gets none.
  excessExemptions <-
    interior "ExcessExemptions" "excess_exemptions" $
      let excess = l18 `subtractNotBelowZero` l17
       in byStatusE (byStatus excess excess (dollars 0) excess excess)
  schB36 <-
    interior "SchB_L36" "schedule_b_excess_exemptions" $
      smallerOf excessExemptions schB35
  schB37 <-
    interior "SchB_L37" "schedule_b_taxable" $
      schB35 .-. schB36

  -- Line 20: Schedule B line 38, interest and dividends taxed at 5.0%
  l20 <-
    interior "L20" "interest_dividend_income" $
      smallerOf schB37 interestDividendsAfterLosses

  -- Line 21: Combined income for tax computation
  l21 <-
    interior "L21" "combined_income" $
      l19 .+. l20

  -- Line 22: 5.0% Tax
  l22 <-
    keyOutput "L22" "ma_base_tax" "5.0% tax" $
      l21 .*. rate maBaseRate2025

  -- Line 23a: Schedule B line 39, short-term gains taxed at 8.5%
  l23a <-
    interior "L23a" "short_term_gains" $
      schB37 .-. l20

  l23a_tax <-
    interior "L23a_tax" "short_term_gains_tax" $
      l23a .*. rate maShortTermCapitalGainsRate2025

  -- Line 23b: Long-term collectibles gains (taxed at 12%)
  l23b <- keyInput "L23b" "collectibles_gains" "12% income from long-term collectibles"

  l23b_tax <-
    interior "L23b_tax" "collectibles_tax" $
      l23b .*. rate maLongTermCollectiblesRate2025

  -- Line 24: Schedule D line 21 (after line 20 excess exemptions), taxed at 5.0%
  schD20 <-
    interior "SchD_L20" "schedule_d_excess_exemptions" $
      smallerOf (excessExemptions .-. schB36) (longTermGain .-. shortTermLossAgainstLongTerm)
  l24 <-
    interior "L24" "long_term_gains" $
      longTermGain .-. shortTermLossAgainstLongTerm .-. schD20

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
