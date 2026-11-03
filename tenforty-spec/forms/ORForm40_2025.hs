{-# LANGUAGE OverloadedStrings #-}

module ORForm40_2025
  ( orForm40_2025,
  )
where

import FormRefs
import TablesOR2025
import TenForty

orForm40_2025 :: Either FormError Form
orForm40_2025 = form "or_40" 2025 $ do
  defineTable oregonBracketsTable2025

  -- Line 7: Federal AGI (imported from US 1040)
  let federalAgi = importForm us1040L11
  l7 <- keyOutput "L7" "federal_agi" "Federal adjusted gross income" federalAgi

  -- Lines 8-12: Oregon additions to income
  l8 <- keyInput "L8" "or_addition_interest" "Interest from obligations of other states"
  l9 <- keyInput "L9" "or_addition_federal_sub" "Federal income tax subtracted on last year's OR return"
  l10 <- keyInput "L10" "or_addition_depreciation" "Depreciation adjustments"
  l11 <- keyInput "L11" "or_addition_other" "Other additions to income"

  l12 <-
    interior "L12" "or_total_additions" $
      sumOf [l8, l9, l10, l11]

  -- Lines 13-19: Oregon subtractions from income
  l13 <- keyInput "L13" "or_sub_state_refund" "State income tax refund from prior year"
  l14 <- keyInput "L14" "or_sub_us_interest" "Interest from U.S. obligations"
  l15 <- keyInput "L15" "or_sub_socsec" "Social Security included in federal AGI"
  l16 <- keyInput "L16" "or_sub_federal_pension" "Federal pension income subtraction"
  l17 <- keyInput "L17" "or_sub_military_pension" "Oregon active duty pay/military pension subtraction"

  excessPremiumRepayment <- keyInput "ExcessPremiumRepayment" "excess_premium_repayment" "Excess advance premium tax credit repayment"
  additionalFederalIncomeTax <- keyInput "AdditionalFederalIncomeTax" "additional_federal_income_tax" "Other federal income taxes eligible for subtraction"
  premiumTaxCredit <- keyInput "PremiumTaxCredit" "premium_tax_credit" "Total premium tax credit from Form 8962 line 24"
  refundableAdoptionCredit <- keyInput "RefundableAdoptionCredit" "refundable_adoption_credit" "Refundable adoption credit from federal Form 1040 line 30"
  let eligibleFederalTax =
        max0 $
          (importForm us1040L22 `subtractNotBelowZero` excessPremiumRepayment)
            .+. additionalFederalIncomeTax
            .-. importForm usForm8863L9
            .-. premiumTaxCredit
            .-. refundableAdoptionCredit
      phaseoutStart = byStatusE $ fmap lit orFederalTaxSubtractionPhaseoutStart2025
      phaseoutBand = byStatusE $ lit <$> byStatus 5000 10000 5000 10000 10000
      cap = byStatusE $ fmap lit orFederalTaxSubtractionLimit2025
      capFraction =
        ifNeg (federalAgi .-. phaseoutStart) (lit 1) $
          ifNeg (federalAgi .-. phaseoutStart .-. phaseoutBand) (lit 0.8) $
            ifNeg (federalAgi .-. phaseoutStart .-. phaseoutBand .-. phaseoutBand) (lit 0.6) $
              ifNeg (federalAgi .-. phaseoutStart .-. phaseoutBand .-. phaseoutBand .-. phaseoutBand) (lit 0.4) $
                ifNeg (federalAgi .-. byStatusE (fmap lit orFederalTaxSubtractionPhaseoutEnd2025)) (lit 0.2) (lit 0)
  l18 <-
    keyOutput "L18" "federal_tax_subtraction" "Federal income tax subtraction (Table 4)" $
      smallerOf eligibleFederalTax (cap .*. capFraction)

  l19 <- keyInput "L19" "or_sub_other" "Other subtractions from income"

  l20 <-
    interior "L20" "or_total_subtractions" $
      sumOf [l13, l14, l15, l16, l17, l18, l19]

  -- Line 21: Oregon taxable income before deductions
  l21 <-
    interior "L21" "or_income_before_deductions" $
      l7 .+. l12 .-. l20

  -- Line 22: Deduction (larger of OR itemized or OR standard)
  itemized <- keyInput "L22_itemized" "or_itemized" "Oregon itemized deductions (from Schedule OR-A)"
  stdDed <-
    interior "StdDed" "or_std_deduction" $
      byStatusE (fmap lit orStandardDeduction2025)
  l22 <- interior "L22" "deduction" $ greaterOf itemized stdDed

  -- Line 23: Oregon taxable income
  _l23 <-
    keyOutput "L23" "or_taxable_income" "Oregon taxable income" $
      l21 `subtractNotBelowZero` l22

  -- Line 24: Same as L23 (for tax computation)
  l24 <- interior "L24" "or_taxable_for_computation" $ line "L23"

  let chartCeiling = byStatusE $ lit <$> byStatus 125000 250000 125000 250000 250000
      chartBase = byStatusE $ lit <$> byStatus 4065 3756 4065 3756 3756
      topBase = byStatusE $ lit <$> byStatus 10627 21256 10627 21256 21256
      chartTax =
        ifPos
          (l24 .-. chartCeiling)
          (topBase .+. (l24 .-. chartCeiling) .*. rate 0.099)
          (chartBase .+. (l24 .-. lit 50000) .*. rate 0.0875)
  l25 <-
    keyOutput "L25" "or_bracket_tax" "Tax from Oregon tax table or rate chart" $
      ifNeg
        (l24 .-. lit 50000)
        (oregonTaxTable2025 l24)
        (roundE chartTax)

  regularExemptions <- keyInput "RegularExemptions" "dependent_count" "Total number of dependents for the regular exemption credit"
  let regularBase = byStatusE $ lit <$> byStatus 1 2 1 1 1
      regularLimit = byStatusE $ lit <$> byStatus 100000 200000 100000 200000 200000
      regularCount = regularBase .+. max0 regularExemptions
  exemptionCredit <-
    interior "ExemptionCredit" "exemption_credit" $
      ifPos (federalAgi .-. regularLimit) (lit 0) (regularCount .*. rate 256)

  -- Lines 26-29: Credits
  l26 <- keyInput "L26" "or_working_family_credit" "Working family household and dependent care credit"
  l27 <- keyInput "L27" "or_political_contribution_credit" "Political contribution credit"
  l28 <- keyInput "L28" "or_other_credits" "Other Oregon credits"

  l29 <-
    interior "L29" "or_total_credits" $
      sumOf [exemptionCredit, l26, l27, l28]

  -- Line 30: Tax after credits
  l30 <-
    interior "L30" "or_tax_after_credits" $
      l25 `subtractNotBelowZero` l29

  -- Line 31: Other Oregon taxes
  l31 <- keyInput "L31" "or_other_taxes" "Other Oregon taxes"

  -- Line 32: Total Oregon tax
  _l32 <-
    keyOutput "L32" "or_total_tax" "Total Oregon tax" $
      l30 .+. l31

  outputs
    [ "L7",
      "L23",
      "L25",
      "L32"
    ]
