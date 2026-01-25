{-# LANGUAGE OverloadedStrings #-}

module CAScheduleCA_2024
  ( caScheduleCA_2024
  ) where

import TenForty


caScheduleCA_2024 :: Either FormError Form
caScheduleCA_2024 = form "ca_schedule_ca" 2024 $ do

  -- Schedule CA (540): California Adjustments - Residents
  -- Adjusts Federal AGI to California AGI

  -- Part I: Income Adjustment Schedule
  -- Column A = Federal amounts, Column B = Subtractions, Column C = Additions

  -- Section A: Subtractions from Federal Income (reduce CA taxable income)

  -- Line 7: Wages, salaries, tips (CA adjustments - usually none)
  sWages <- keyInput "S7" "wages_subtraction" "Wage subtraction (if any)"

  -- Line 8: Interest income adjustments
  -- Federal bond interest is taxable federally but not in CA
  sFederalBondInt <- keyInput "S8" "federal_bond_interest" "US savings bond/Treasury interest"

  -- Line 9: Dividend adjustments
  sDividends <- keyInput "S9" "dividend_subtraction" "Dividend subtraction"

  -- Line 10: State tax refund (not applicable as subtraction)

  -- Line 11: Alimony received (CA may differ from federal)
  sAlimony <- keyInput "S11" "alimony_subtraction" "Alimony subtraction"

  -- Line 12: Business income adjustments
  sBusiness <- keyInput "S12" "business_subtraction" "Business income subtraction"

  -- Line 13: Capital gain/loss adjustments
  sCapGain <- keyInput "S13" "cap_gain_subtraction" "Capital gain subtraction"

  -- Line 14: Other gains adjustments
  sOtherGains <- keyInput "S14" "other_gains_subtraction" "Other gains subtraction"

  -- Line 15a: IRA distributions (may differ)
  sIRA <- keyInput "S15a" "ira_subtraction" "IRA distribution subtraction"

  -- Line 16a: Pensions and annuities
  sPension <- keyInput "S16a" "pension_subtraction" "Pension subtraction"

  -- Line 17-19: Pass-through income adjustments
  sPassthrough <- keyInput "S17_19" "passthrough_subtraction" "Pass-through entity subtraction"

  -- Line 20a: Social Security benefits (CA doesn't tax)
  -- Import from federal 1040 if available, or manual input
  sSocSec <- keyInput "S20a" "ss_subtraction" "Social Security benefits (CA exempt)"

  -- Line 21: CA lottery winnings (CA doesn't tax)
  sCALottery <- keyInput "S21" "ca_lottery" "California lottery winnings"

  -- Line 22-24: Other subtractions
  sOther <- keyInput "S22_24" "other_subtractions" "Other subtractions"

  -- Total Part I Subtractions (Column B)
  _totalSubtractions <- keyOutput "TOTAL_SUB" "total_subtractions"
    "Total subtractions (to CA 540 Line 14)" $
    sumOf [ sWages, sFederalBondInt, sDividends, sAlimony, sBusiness
          , sCapGain, sOtherGains, sIRA, sPension, sPassthrough
          , sSocSec, sCALottery, sOther ]


  -- Section B: Additions to Federal Income (increase CA taxable income)

  -- Line 7: Wage additions
  aWages <- keyInput "A7" "wages_addition" "Wage addition"

  -- Line 8: Interest adjustments - out-of-state municipal bond interest
  aOutOfStateMuni <- keyInput "A8" "out_of_state_muni" "Out-of-state municipal bond interest"

  -- Line 9: Dividend additions
  aDividends <- keyInput "A9" "dividend_addition" "Dividend addition"

  -- Line 10: State/local tax refund (may be taxable in CA)
  aStateRefund <- keyInput "A10" "state_refund" "State/local tax refund addition"

  -- Line 11: Alimony addition
  aAlimony <- keyInput "A11" "alimony_addition" "Alimony addition"

  -- Line 12: Business income addition (depreciation differences)
  aBusiness <- keyInput "A12" "business_addition" "Business income addition"

  -- Line 13: Capital gain addition
  aCapGain <- keyInput "A13" "cap_gain_addition" "Capital gain addition"

  -- Line 14: Other gains addition
  aOtherGains <- keyInput "A14" "other_gains_addition" "Other gains addition"

  -- Line 15a: IRA distribution addition
  aIRA <- keyInput "A15a" "ira_addition" "IRA distribution addition"

  -- Line 16a: Pension addition
  aPension <- keyInput "A16a" "pension_addition" "Pension addition"

  -- Line 17-19: Pass-through income additions
  aPassthrough <- keyInput "A17_19" "passthrough_addition" "Pass-through entity addition"

  -- QBI Deduction add-back (California does not allow federal QBI deduction)
  -- Import from Form 8995 if available
  aQBI <- interior "A_QBI" "qbi_addback" $ importForm "us_form_8995" "L16"

  -- Line 22-24: Other additions
  aOther <- keyInput "A22_24" "other_additions" "Other additions"

  -- Total Part I Additions (Column C)
  _totalAdditions <- keyOutput "TOTAL_ADD" "total_additions"
    "Total additions (to CA 540 Line 16)" $
    sumOf [ aWages, aOutOfStateMuni, aDividends, aStateRefund, aAlimony
          , aBusiness, aCapGain, aOtherGains, aIRA, aPension, aPassthrough
          , aQBI, aOther ]


  -- Part II: Adjustments to Federal AGI

  -- Educator expenses difference
  adjEducator <- keyInput "ADJ_EDUCATOR" "educator_adj" "Educator expenses adjustment"

  -- HSA deduction difference
  adjHSA <- keyInput "ADJ_HSA" "hsa_adj" "HSA deduction adjustment"

  -- Self-employment tax deduction difference
  adjSE <- keyInput "ADJ_SE" "se_adj" "Self-employment tax adjustment"

  -- Student loan interest difference
  adjStudentLoan <- keyInput "ADJ_STUDENT" "student_loan_adj" "Student loan interest adjustment"

  -- IRA deduction difference
  adjIRADed <- keyInput "ADJ_IRA" "ira_ded_adj" "IRA deduction adjustment"

  -- Other adjustments
  adjOther <- keyInput "ADJ_OTHER" "other_adj" "Other adjustments"

  -- Net Adjustments (subtractions are negative, additions are positive)
  -- Positive = increases CA AGI, Negative = decreases CA AGI
  _netAdjustments <- keyOutput "NET_ADJ" "net_adjustments" "Net adjustments to federal AGI" $
    adjEducator .+. adjHSA .+. adjSE .+. adjStudentLoan .+. adjIRADed .+. adjOther

  outputs ["TOTAL_SUB", "TOTAL_ADD", "NET_ADJ"]
