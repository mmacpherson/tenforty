{-# LANGUAGE OverloadedStrings #-}

module CA540_2024
  ( ca540_2024
  ) where

import TenForty
import TablesCA2024


ca540_2024 :: Either FormError Form
ca540_2024 = form "ca_540" 2024 $ do
  defineTable californiaBracketsTable2024

  -- Line 13: Federal AGI (imported from US 1040)
  let federalAgi = importForm "us_1040" "L11"
  l13 <- keyOutput "L13" "federal_agi" "Federal adjusted gross income" federalAgi

  -- Line 14: California adjustments - subtractions (from Schedule CA)
  l14 <- interior "L14" "ca_subtractions" $ importForm "ca_schedule_ca" "TOTAL_SUB"

  -- Line 15: Federal AGI minus subtractions
  l15 <- interior "L15" "agi_minus_subtractions" $ l13 .-. l14

  -- Line 16: California adjustments - additions (from Schedule CA)
  l16 <- interior "L16" "ca_additions" $ importForm "ca_schedule_ca" "TOTAL_ADD"

  -- Line 17: California AGI
  l17 <- keyOutput "L17" "ca_agi" "California adjusted gross income" $
    l15 .+. l16

  -- Line 18: Deductions (larger of itemized or standard)
  itemized <- keyInput "L18_itemized" "ca_itemized" "California itemized deductions"
  stdDed <- interior "StdDed" "ca_std_deduction" $
    byStatusE (fmap lit caStandardDeduction2024)
  l18 <- interior "L18" "deduction" $ greaterOf itemized stdDed

  -- Line 19: Taxable income (CA AGI minus deductions, not below zero)
  l19 <- keyOutput "L19" "ca_taxable_income" "California taxable income" $
    l17 `subtractNotBelowZero` l18

  -- Line 31: Tax from California tax brackets
  l31 <- keyOutput "L31" "ca_bracket_tax" "Tax from California tax brackets" $
    bracketTax "ca_brackets_2024" l19

  -- Line 32: Exemption credits (simplified as input; phaseout is complex)
  l32 <- keyInput "L32" "exemption_credits" "Exemption credits"

  -- Line 33: Tax after exemption credits
  l33 <- interior "L33" "tax_after_exemptions" $
    l31 `subtractNotBelowZero` l32

  -- Line 34: Other tax from Schedule G-1, FTB 5870A
  l34 <- keyInput "L34" "other_tax_schedule_g" "Other tax from Schedule G-1"

  -- Line 35: Subtotal (L33 + L34)
  l35 <- interior "L35" "tax_subtotal" $ l33 .+. l34

  -- Lines 40-47: Nonrefundable credits (simplified as single total)
  l47 <- keyInput "L47" "total_credits" "Total nonrefundable credits"

  -- Line 48: Tax after credits
  l48 <- interior "L48" "tax_after_credits" $
    l35 `subtractNotBelowZero` l47

  -- Line 61: Alternative minimum tax
  l61 <- keyInput "L61" "amt" "Alternative minimum tax"

  -- Line 62: Mental Health Services Tax (1% on taxable income > $1M)
  mentalHealthTaxCalc <- interior "L62_calc" "mental_health_tax_calc" $
    (l19 .-. lit caMentalHealthThreshold2024) .*. rate caMentalHealthRate2024
  l62 <- keyOutput "L62" "mental_health_tax" "Mental Health Services Tax" $
    max0 mentalHealthTaxCalc

  -- Line 63: Other taxes and credit recapture
  l63 <- keyInput "L63" "other_taxes" "Other taxes and credit recapture"

  -- Line 64: Total tax
  l64 <- keyOutput "L64" "ca_total_tax" "California total tax" $
    l48 .+. l61 .+. l62 .+. l63

  -- Payments section (lines 71-74, 91-111)
  l71 <- keyInput "L71" "ca_withholding" "California income tax withheld"
  l72 <- keyInput "L72" "estimated_payments" "Estimated tax payments"
  l73 <- keyInput "L73" "extension_payment" "Amount paid with extension"
  l74 <- keyInput "L74" "excess_sdi" "Excess SDI or VPDI withheld"

  l75 <- interior "L75" "total_payments_before_credits" $
    sumOf [l71, l72, l73, l74]

  -- Refundable credits
  -- Line 91: CA EITC (from FTB 3514)
  l91 <- interior "L91" "ca_eitc" $ importForm "ca_ftb_3514" "L18"
  l92 <- keyInput "L92" "young_child_credit" "Young Child Tax Credit"
  l93 <- keyInput "L93" "foster_youth_credit" "Foster Youth Tax Credit"
  l94 <- keyInput "L94" "other_refundable_credits" "Other refundable credits"

  l95 <- interior "L95" "total_refundable_credits" $
    sumOf [l91, l92, l93, l94]

  l96 <- keyOutput "L96" "total_payments" "Total payments and refundable credits" $
    l75 .+. l95

  -- Refund or amount owed
  _l110 <- keyOutput "L110" "overpayment" "Amount overpaid" $
    l96 `excessOf` l64

  _l111 <- keyOutput "L111" "amount_owed" "Amount you owe" $
    l64 `subtractNotBelowZero` l96

  outputs
    [ "L13", "L17", "L19", "L31", "L35", "L48", "L62", "L64"
    , "L75", "L95", "L96", "L110", "L111"
    ]
