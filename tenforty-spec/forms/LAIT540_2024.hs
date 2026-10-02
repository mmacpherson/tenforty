{-# LANGUAGE OverloadedStrings #-}

module LAIT540_2024
  ( laIT540_2024,
  )
where

import FormRefs
import TablesLA2024
import TenForty

laIT540_2024 :: Either FormError Form
laIT540_2024 = form "la_it540" 2024 $ do
  defineTable louisianaBracketsTable2024

  -- Line 7: Federal Adjusted Gross Income (from US 1040 Line 11)
  let federalAgi = importForm us1040L11
  l7 <-
    keyOutput "L7" "federal_agi" "Federal adjusted gross income" $
      federalAgi .+. dollars 0

  -- Line 8A-8C: Federal itemized deductions adjustment
  -- 8B: Medical and dental expenses from Federal Schedule A Line 4
  l8b <- keyInput "L8B" "medical_dental" "Federal medical and dental expenses"
  -- 8C: Amount to subtract (by filing status)
  -- Single/MFS: $14,600, MFJ/QW: $29,200, HoH: $21,900
  l8c <-
    interior "L8C" "medical_threshold" $
      byStatusE (fmap lit (byStatus 14600 29200 14600 21900 29200))
  -- 8D: Difference (L8B - L8C, not below zero)
  l8d <-
    interior "L8D" "itemized_adjustment" $
      l8b `subtractNotBelowZero` l8c

  -- Line 9: Louisiana Tax Table Income (federal AGI minus adjustment)
  l9 <-
    keyOutput "L9" "la_tax_table_income" "Louisiana tax table income" $
      l7 `subtractNotBelowZero` l8d

  -- Line 6A-6F: Exemptions
  -- 6A: Taxpayer exemption (always 1)
  -- 6B: Spouse exemption (1 if MFJ, 0 otherwise)
  -- 6C: Dependent exemptions (user input)
  -- 6D: Total exemptions (6A + 6B + 6C)
  -- 6E: Dependents for adoption deduction (subtract from 6D)
  -- 6F: Net exemptions (6D - 6E)

  -- The tax table applies the combined personal exemption-standard deduction
  -- ($4,500 Single/MFS; $9,000 MFJ/QSS/HoH) plus $1,000 per additional
  -- exemption. We accept that total exemption amount as input.
  -- Source: 2024 Louisiana Tax Table headers, PDF pages 1, 3 and 7,
  -- https://dam.ldr.la.gov/taxforms/IT540(2024)D13%20TT.pdf
  totalExemptions <- keyInput "L6F_amount" "exemption_amount" "Total personal exemption and dependent deductions"

  -- Taxable income: the form prints no taxable-income line; this is the
  -- income left once the exemptions are deducted.
  _ <-
    keyOutput "L10_taxable" "la_taxable_income" "Louisiana taxable income" $
      l9 `subtractNotBelowZero` totalExemptions

  -- Line 10: Louisiana Tax. The exemptions are deducted from the lowest
  -- bracket first and then the remaining brackets in increasing order (La. R.S.
  -- 47:32(A)(1), 294, 295(B)), so they are worth the tax on their own amount
  -- at the lowest rates, not the tax at the taxpayer's top rate.
  -- Source: Revenue Information Bulletin 25-012, PDF page 1, note 1,
  -- https://dam.ldr.la.gov/lawspolicies/RIB-25-012-Louisiana-Individual-Income-Tax-Reform-1.pdf
  -- The 2024 tax table reproduces this: Single, tax table income $50,000 to
  -- \$50,250, one exemption, $1,466 = tax on $50,125 ($1,549.06) less tax on
  -- \$4,500 ($83.25).
  l10_income_tax <-
    interior "L10_income_tax" "tax_on_tax_table_income" $
      bracketTax "la_brackets_2024" l9
  l10_exemption_tax <-
    interior "L10_exemption_tax" "tax_on_exemptions" $
      bracketTax "la_brackets_2024" totalExemptions
  l10 <-
    keyOutput "L10" "la_tax" "Louisiana income tax" $
      l10_income_tax `subtractNotBelowZero` l10_exemption_tax

  -- Line 11: Nonrefundable Priority 1 Credits
  l11 <- keyInput "L11" "priority1_credits" "Nonrefundable Priority 1 Credits"

  -- Line 12: Tax after credits
  l12 <-
    keyOutput "L12" "tax_after_credits" "Tax after nonrefundable credits" $
      l10 `subtractNotBelowZero` l11

  -- Line 13: Louisiana Income Tax Withheld
  l13 <- keyInput "L13" "la_tax_withheld" "Louisiana income tax withheld"

  -- Line 14: Other Tax Payments
  l14 <- keyInput "L14" "other_payments" "Other tax payments"

  -- Line 15: Refundable Credits
  l15 <- keyInput "L15" "refundable_credits" "Refundable credits"

  -- Line 16: Total Payments and Credits
  l16 <-
    keyOutput "L16" "total_payments" "Total payments and credits" $
      sumOf [l13, l14, l15]

  -- Line 17: Amount Due
  _ <-
    keyOutput "L17" "tax_due" "Tax Due" $
      l12 `subtractNotBelowZero` l16

  -- Line 18: Overpayment
  _ <-
    keyOutput "L18" "overpayment" "Overpayment" $
      l16 `excessOf` l12

  outputs
    [ "L7",
      "L9",
      "L10_taxable",
      "L10",
      "L12",
      "L16",
      "L17",
      "L18"
    ]
