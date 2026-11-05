{-# LANGUAGE OverloadedStrings #-}

module OHIT1040_2025
  ( ohIT1040_2025,
  )
where

import FormRefs
import TablesOH2025
import TenForty

ohIT1040_2025 :: Either FormError Form
ohIT1040_2025 = form "oh_it1040" 2025 $ do
  defineTable ohioBracketsTable2025

  -- Line 1: Federal Adjusted Gross Income (imported from US 1040)
  let federalAgi = importForm us1040L11
  l1 <- keyOutput "L1" "federal_agi" "Federal adjusted gross income" (roundE federalAgi)

  -- Line 2: Additions to federal AGI (from Schedule of Adjustments)
  l2 <- keyInput "L2" "oh_additions" "Ohio additions to federal AGI"

  -- Line 3: Deductions from federal AGI (from Schedule of Adjustments)
  l3 <- keyInput "L3" "oh_deductions" "Ohio deductions from federal AGI"

  -- Line 4: Ohio Adjusted Gross Income
  l4 <-
    keyOutput "L4" "oh_agi" "Ohio adjusted gross income" $
      (l1 .+. roundE l2) `subtractNotBelowZero` roundE l3

  -- Line 5a: Business income (from Schedule IT BUS)
  l5a <- keyInput "L5a" "business_income" "Business income"

  -- Line 5b: Nonbusiness income
  l5b <-
    interior "L5b" "nonbusiness_income" $
      l4 `subtractNotBelowZero` l5a

  -- Line 6: Senior citizen credit/lump sum distribution credit (pre-computed)
  l6 <- keyInput "L6" "senior_lump_credit" "Senior citizen credit/lump sum distribution credit"

  -- Line 7: Income after credits
  l7 <-
    interior "L7" "income_after_credits" $
      l5b `subtractNotBelowZero` l6

  dependents <- keyInput "Dependents" "dependents" "Total dependent exemption count"
  exemptionCount <-
    interior "ExemptionCount" "exemption_count" $
      max0 (roundE dependents) .+. byStatusE (fmap lit (byStatus 1 2 1 1 1))
  let exemptionAmount =
        foldr
          (\(ceiling, amount) rest -> ifGte (lit ceiling) l4 (lit amount) rest)
          (dollars 1900)
          (take 2 ohioPersonalExemption2025)
  l8 <-
    keyOutput "L8" "personal_exemptions" "MAGI-based personal and dependent exemptions" $
      ifGte l4 (dollars 750000) (dollars 0) (exemptionCount .*. (exemptionAmount ./. dollars 1))

  -- Line 9: Ohio taxable nonbusiness income
  l9 <-
    keyOutput "L9" "oh_taxable_nonbusiness_income" "Ohio taxable nonbusiness income" $
      l7 `subtractNotBelowZero` l8

  -- Line 10: Ohio nonbusiness income tax (from tax rate schedule)
  -- Base tax of $342 applies to income above $26,050 (2025)
  ohBaseTax <-
    interior "oh_base_tax" "ohio_base_tax" $
      ifPos (l9 .-. dollars 26050) (dollars 342) (dollars 0)

  l10 <-
    keyOutput "L10" "oh_nonbusiness_tax" "Ohio nonbusiness income tax" $
      roundE $
        ifPos
          (l9 .-. dollars 100000)
          (dollars 2394.32 .+. ((l9 .-. dollars 100000) .*. bracketRateAt ohioBracketsTable2025 l9))
          (bracketTax "oh_brackets_2025" l9 .+. ohBaseTax)

  -- Line 11: Business income tax (3% flat rate on business income)
  let businessTaxRate = 0.03
  l11 <-
    interior "L11" "oh_business_tax" $
      l5a .*. rate businessTaxRate

  -- Line 12: Total Ohio income tax
  l12 <-
    keyOutput "L12" "oh_total_tax" "Total Ohio income tax" $
      l10 .+. l11

  exemptionCredit <-
    interior "ExemptionCredit" "exemption_credit" $
      ifNeg (l4 .-. l8 .-. dollars 30000) (exemptionCount .*. rate 20) (dollars 0)
  filerQualifyingIncome <- keyInput "FilerQualifyingIncome" "filer_qualifying_income" "Filer Ohio joint-credit qualifying income"
  spouseQualifyingIncome <- keyInput "SpouseQualifyingIncome" "spouse_qualifying_income" "Spouse Ohio joint-credit qualifying income"
  priorJointCredits <- keyInput "PreJointCredits" "pre_joint_credits" "Other Schedule of Credits lines 2 through 8"
  let jointCreditBase = l12 `subtractNotBelowZero` (exemptionCredit .+. roundE priorJointCredits)
      jointCreditRate =
        ifPos (l4 .-. l8 .-. dollars 75000) (rate 0.05) $
          ifPos (l4 .-. l8 .-. dollars 50000) (rate 0.10) $
            ifPos (l4 .-. l8 .-. dollars 25000) (rate 0.15) (rate 0.20)
      jointCredit = minE (dollars 650) (roundE (jointCreditBase .*. jointCreditRate))
      eligibleJointCredit =
        ifGte (roundE filerQualifyingIncome) (dollars 500) $
          ifGte (roundE spouseQualifyingIncome) (dollars 500) jointCredit (dollars 0)
  l13 <-
    keyOutput "L13" "joint_filing_credit" "Joint filing credit" $
      byStatusE $
        byStatus (dollars 0) (ifNeg (l4 .-. dollars 750000) (eligibleJointCredit (dollars 0)) (dollars 0)) (dollars 0) (dollars 0) (dollars 0)

  -- Line 14: Lump sum retirement credit (from Schedule of Credits)
  l14 <- keyInput "L14" "lump_sum_retirement_credit" "Lump sum retirement credit"

  -- Line 15: Senior citizen credit (from Schedule of Credits)
  l15 <- keyInput "L15" "senior_citizen_credit" "Senior citizen credit"

  -- Line 16: Child care credit (from IT RC)
  l16 <- keyInput "L16" "child_care_credit" "Child care credit"

  -- Line 17: Displaced worker training credit
  l17 <- keyInput "L17" "displaced_worker_credit" "Displaced worker training credit"

  -- Line 18: Campaign contribution credit
  l18 <- keyInput "L18" "campaign_contribution_credit" "Campaign contribution credit"

  -- Line 19: Opportunity Zone investment credit
  l19 <- keyInput "L19" "opportunity_zone_credit" "Opportunity Zone investment credit"

  -- Line 20: Other nonrefundable credits
  l20 <- keyInput "L20" "other_nonrefundable_credits" "Other nonrefundable credits"

  -- Line 21: Total credits
  l21 <-
    interior "L21" "total_credits" $
      sumOf [exemptionCredit, roundE priorJointCredits, l13, roundE l14, roundE l15, roundE l16, roundE l17, roundE l18, roundE l19, roundE l20]

  -- Line 22: Tax after credits
  l22 <-
    interior "L22" "tax_after_credits" $
      l12 `subtractNotBelowZero` l21

  -- Line 23: Use tax
  l23 <- keyInput "L23" "use_tax" "Use tax"

  -- Line 24: School district income tax (from SD 100)
  l24 <- keyInput "L24" "school_district_tax" "School district income tax"

  -- Line 25: Total tax liability
  l25 <-
    keyOutput "L25" "total_tax_liability" "Total tax liability" $
      sumOf [l22, l23, l24]

  -- Line 26: Ohio income tax withheld
  l26 <- keyInput "L26" "oh_tax_withheld" "Ohio income tax withheld"

  -- Line 27: Estimated payments
  l27 <- keyInput "L27" "estimated_payments" "Estimated payments"

  -- Line 28: Extension payment
  l28 <- keyInput "L28" "extension_payment" "Payment with extension"

  -- Line 29: Pass-through entity withholding
  l29 <- keyInput "L29" "pte_withholding" "Pass-through entity withholding"

  -- Line 30: Other refundable credits
  l30 <- keyInput "L30" "other_refundable_credits" "Other refundable credits"

  -- Line 31: Total payments and refundable credits
  l31 <-
    keyOutput "L31" "total_payments" "Total payments and refundable credits" $
      sumOf [l26, l27, l28, l29, l30]

  -- Line 32: Amount owed
  _amountOwed <-
    keyOutput "L32" "amount_owed" "Amount you owe" $
      l25 `subtractNotBelowZero` l31

  -- Line 33: Overpayment/refund
  _overpayment <-
    keyOutput "L33" "overpayment" "Overpayment" $
      l31 `excessOf` l25

  -- The rate-schedule bracket that holds l9, as tax per dollar: the
  -- same table as the tax, with a threshold amount in the lower bracket.
  _ <-
    keyOutput "L9BracketRate" "oh_bracket_rate" "Rate-schedule bracket at taxable income" $
      dollars 1 .*. bracketRateAt ohioBracketsTable2025 l9

  outputs
    [ "L9BracketRate",
      "L1",
      "L4",
      "L8",
      "L9",
      "L10",
      "L12",
      "L13",
      "L25",
      "L31",
      "L32",
      "L33"
    ]
