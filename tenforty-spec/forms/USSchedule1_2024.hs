{-# LANGUAGE OverloadedStrings #-}

module USSchedule1_2024
  ( usSchedule1_2024
  ) where

import TenForty


usSchedule1_2024 :: Either FormError Form
usSchedule1_2024 = form "us_schedule_1" 2024 $ do

  -- Part I: Additional Income
  l1 <- keyInput "L1" "taxable_refunds" "Taxable refunds of state/local income taxes"
  l2a <- keyInput "L2a" "alimony_received" "Alimony received (pre-2019 agreements)"
  l2b <- keyInput "L2b" "alimony_received_date" "Date of original divorce/separation agreement"
  l3 <- keyInput "L3" "business_income" "Business income or loss from Schedule C"
  l4 <- keyInput "L4" "other_gains" "Other gains or losses from Form 4797"
  l5 <- keyInput "L5" "rental_income" "Rental real estate, royalties, partnerships from Schedule E"
  l6 <- keyInput "L6" "farm_income" "Farm income or loss from Schedule F"
  l7 <- keyInput "L7" "unemployment" "Unemployment compensation"

  -- Line 8: Other Income
  l8a <- keyInput "L8a" "net_operating_loss" "Net operating loss deduction"
  l8b <- keyInput "L8b" "gambling_income" "Gambling income"
  l8c <- keyInput "L8c" "cancellation_of_debt" "Cancellation of debt"
  l8d <- keyInput "L8d" "foreign_earned_income" "Foreign earned income exclusion from Form 2555"
  l8e <- keyInput "L8e" "taxable_hsa" "Taxable HSA distribution"
  l8f <- keyInput "L8f" "alaska_pfd" "Alaska Permanent Fund dividends"
  l8g <- keyInput "L8g" "jury_pay" "Jury duty pay given to employer"
  l8h <- keyInput "L8h" "prizes_awards" "Prizes and awards"
  l8i <- keyInput "L8i" "activity_not_for_profit" "Activity not engaged in for profit income"
  l8j <- keyInput "L8j" "stock_options" "Stock options"
  l8k <- keyInput "L8k" "income_from_form_8814" "Income from Form 8814"
  l8l <- keyInput "L8l" "income_from_form_4797" "Income from Form 4797, Part II"
  l8m <- keyInput "L8m" "rental_personal_property" "Rental of personal property income"
  l8n <- keyInput "L8n" "olympic_medal" "Olympic and Paralympic medal and prize money"
  l8o <- keyInput "L8o" "sec_951_income" "Section 951(a) inclusion"
  l8p <- keyInput "L8p" "sec_951a_income" "Section 951A(a) inclusion"
  l8q <- keyInput "L8q" "sec_461l_excess" "Section 461(l) excess business loss adjustment"
  l8r <- keyInput "L8r" "taxable_scholarship" "Taxable scholarship or fellowship grants"
  l8s <- keyInput "L8s" "nec_1099_income" "NEC 1099 income"
  l8z <- keyInput "L8z" "other_income" "Other income (list type and amount)"

  l9 <- interior "L9" "other_income_total" $
    l8a .+. l8b .+. l8c .+. l8d .+. l8e .+. l8f .+. l8g .+. l8h .+.
    l8i .+. l8j .+. l8k .+. l8l .+. l8m .+. l8n .+. l8o .+. l8p .+.
    l8q .+. l8r .+. l8s .+. l8z

  _l10 <- keyOutput "L10" "additional_income" "Total additional income" $
    l1 .+. l2a .+. l2b .+. l3 .+. l4 .+. l5 .+. l6 .+. l7 .+. l9

  -- Part II: Adjustments to Income
  l11 <- keyInput "L11" "educator_expenses" "Educator expenses"
  l12 <- keyInput "L12" "business_expenses_reservists" "Certain business expenses of reservists"
  l13 <- keyInput "L13" "hsa_deduction" "Health savings account deduction"
  l14 <- keyInput "L14" "moving_expenses_military" "Moving expenses for armed forces"
  l15 <- interior "L15" "self_employment_tax_deduction" $ importForm "us_schedule_se" "L11"
  l16 <- keyInput "L16" "sep_simple_deduction" "Self-employed SEP, SIMPLE, and qualified plans"
  l17 <- keyInput "L17" "self_employed_health" "Self-employed health insurance deduction"
  l18 <- keyInput "L18" "early_withdrawal_penalty" "Penalty on early withdrawal of savings"
  l19a <- keyInput "L19a" "alimony_paid" "Alimony paid"
  l19b <- keyInput "L19b" "alimony_recipient_ssn" "Recipient's SSN"
  l19c <- keyInput "L19c" "alimony_divorce_date" "Date of original divorce/separation agreement"
  l20 <- keyInput "L20" "ira_deduction" "IRA deduction"
  l21 <- keyInput "L21" "student_loan_interest" "Student loan interest deduction"
  l22 <- keyInput "L22" "reserved_future" "Reserved for future use"
  l23 <- keyInput "L23" "archer_msa" "Archer MSA deduction"

  -- Line 24: Other Adjustments
  l24a <- keyInput "L24a" "jury_duty_remitted" "Jury duty pay remitted to employer"
  l24b <- keyInput "L24b" "deductible_expenses_rental" "Deductible expenses related to rental income"
  l24c <- keyInput "L24c" "nol_deduction" "Nontaxable amount of Olympic/Paralympic medals"
  l24d <- keyInput "L24d" "reforestation_amortization" "Reforestation amortization and expenses"
  l24e <- keyInput "L24e" "repayment_supplemental_unemployment" "Repayment of supplemental unemployment"
  l24f <- keyInput "L24f" "contributions_408p" "Contributions to section 501(c)(18)(D) pension"
  l24g <- keyInput "L24g" "contributions_govt_def_comp" "Contributions to governmental section 457(b)"
  l24h <- keyInput "L24h" "attorney_fees_whistleblower" "Attorney fees related to whistleblower awards"
  l24i <- keyInput "L24i" "attorney_fees_discrimination" "Attorney fees related to discrimination suits"
  l24j <- keyInput "L24j" "housing_deduction_clergy" "Housing deduction from Form 2106"
  l24k <- keyInput "L24k" "excess_deductions_estate" "Excess deductions of section 67(e) expenses"
  l24z <- keyInput "L24z" "other_adjustments" "Other adjustments (list type and amount)"

  l25 <- interior "L25" "other_adjustments_total" $
    l24a .+. l24b .+. l24c .+. l24d .+. l24e .+. l24f .+. l24g .+.
    l24h .+. l24i .+. l24j .+. l24k .+. l24z

  _l26 <- keyOutput "L26" "total_adjustments" "Total adjustments to income" $
    l11 .+. l12 .+. l13 .+. l14 .+. l15 .+. l16 .+. l17 .+. l18 .+.
    l19a .+. l19b .+. l19c .+. l20 .+. l21 .+. l22 .+. l23 .+. l25

  outputs ["L9", "L10", "L25", "L26"]
