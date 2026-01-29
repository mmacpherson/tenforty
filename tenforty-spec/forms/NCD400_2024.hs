{-# LANGUAGE OverloadedStrings #-}

module NCD400_2024 (
    ncd400_2024,
) where

import TablesNC2024
import TenForty

ncd400_2024 :: Either FormError Form
ncd400_2024 = form "nc_d400" 2024 $ do
    defineTable ncChildDeductionTable2024

    -- Line 6: Federal Adjusted Gross Income
    -- Imported from US 1040 Line 11
    let federalAgi = importForm "us_1040" "L11"
    l6 <- keyOutput "L6" "federal_agi" "Federal Adjusted Gross Income" $ federalAgi .+. dollars 0

    -- Line 7: Additions to Federal AGI
    l7 <- keyInput "L7" "additions" "Additions to Federal Adjusted Gross Income"

    -- Line 8: L6 + L7
    l8 <- interior "L8" "agi_plus_additions" $ l6 .+. l7

    -- Line 9: Deductions from Federal AGI
    l9 <- keyInput "L9" "deductions_from_agi" "Deductions from Federal Adjusted Gross Income"

    -- Line 10: Child Deduction Amount
    -- Number of qualifying children
    l10a <- keyInput "L10a" "num_children" "Number of qualifying children"
    -- Look up deduction per child based on AGI (L6)
    childDedPerChild <- interior "ChildDedPerChild" "child_deduction_per_child" $
        tableLookup "nc_child_deduction_2024" l6

    -- Cast count (L10a) to Dollars so we can use it in multiplication with Rate?
    -- No, tableLookup returns Dollars.
    -- Mul :: Expr Dollars -> Expr Rate -> Expr Dollars.
    -- l10a is Dollars (abused as count). childDedPerChild is Dollars.
    -- Dollars * Dollars is not allowed.
    -- We need to convert one to Rate.
    -- Or simpler: "Number of children" is just a multiplier.
    -- Let's treat count as Rate (since 1.0 = 100%, 2.0 = 200%).
    -- If we have 2 kids, and deduction is $1000.
    -- 2 * $1000 = $2000.
    -- In DSL: (l10a / 1) * childDedPerChild.
    -- Div :: Dollars -> Dollars -> Rate.
    -- l10a ./. dollars 1 :: Rate.

    let l10a_rate = l10a ./. dollars 1

    l10 <- keyOutput "L10" "child_deduction" "Child Deduction Amount" $
        childDedPerChild .*. l10a_rate

    -- Line 11: Standard Deduction or Itemized Deduction
    -- User provides itemized deduction or 0 to use standard
    l11_input <- keyInput "L11" "itemized_deduction" "Itemized Deduction (enter 0 for Standard)"

    stdDed <- interior "StdDed" "standard_deduction" $
        byStatusE (fmap lit ncStandardDeduction2024)

    l11 <- interior "L11_final" "deduction_amount" $
        greaterOf l11_input stdDed

    -- Line 12a: Total Deductions (L9 + L10 + L11)
    l12a <- interior "L12a" "total_deductions" $
        sumOf [l9, l10, l11]

    -- Line 12: NC Taxable Income Base (L8 - L12a)
    l12 <- interior "L12" "nc_taxable_income_base" $
        l8 `subtractNotBelowZero` l12a

    -- Line 13: Part-year resident ratio (default 1.0)
    l13_input <- keyInput "L13" "part_year_ratio" "Part-year resident ratio (enter 1.0 if full year)"
    let l13 = ifPos l13_input l13_input (dollars 1)
    let l13_rate = l13 ./. dollars 1

    -- Line 14: North Carolina Taxable Income (L12 * L13)
    l14 <- keyOutput "L14" "nc_taxable_income" "North Carolina Taxable Income" $
        l12 .*. l13_rate

    -- Line 15: North Carolina Income Tax (L14 * 4.5%)
    l15 <- keyOutput "L15" "nc_income_tax" "North Carolina Income Tax" $
        roundE (l14 .*. lit ncTaxRate2024)

    -- Line 16: Tax Credits
    l16 <- keyInput "L16" "tax_credits" "Tax Credits"

    -- Line 17: Tax after credits (L15 - L16)
    l17 <- interior "L17" "tax_after_credits" $
        l15 `subtractNotBelowZero` l16

    -- Line 18: Consumer Use Tax
    l18 <- keyInput "L18" "use_tax" "Consumer Use Tax"

    -- Line 19: Total Tax (L17 + L18)
    l19 <- keyOutput "L19" "total_tax" "Total Tax" $
        l17 .+. l18

    -- Line 20: NC Income Tax Withheld
    l20a <- keyInput "L20a" "withholding_yours" "NC Tax Withheld (Yours)"
    l20b <- keyInput "L20b" "withholding_spouse" "NC Tax Withheld (Spouse)"
    l20 <- interior "L20" "total_withholding" $ l20a .+. l20b

    -- Line 21: Other Tax Payments
    l21a <- keyInput "L21a" "estimated_tax" "2024 Estimated Tax"
    l21b <- keyInput "L21b" "paid_with_extension" "Paid with Extension"
    l21c <- keyInput "L21c" "partnership_payments" "Partnership Payments"
    l21d <- keyInput "L21d" "scorp_payments" "S Corporation Payments"
    l21 <- interior "L21" "other_payments" $
        sumOf [l21a, l21b, l21c, l21d]

    -- Line 22: Amended Return Payments (omitted for now, usually 0)
    let l22 = dollars 0

    -- Line 23: Total Payments (L20 + L21 + L22)
    l23 <- keyOutput "L23" "total_payments" "Total Payments" $
        sumOf [l20, l21, l22]

    -- Line 24: Amended Return Refunds (omitted)
    let l24 = dollars 0

    -- Line 25: Net Payments (L23 - L24)
    l25 <- interior "L25" "net_payments" $
        l23 `subtractNotBelowZero` l24

    -- Line 26: Tax Due
    l26 <- keyOutput "L26" "tax_due" "Tax Due" $
        l19 `subtractNotBelowZero` l25

    -- Line 28: Overpayment
    l28 <- keyOutput "L28" "overpayment" "Overpayment" $
        l25 `excessOf` l19

    outputs
        [ "L6"
        , "L10"
        , "L14"
        , "L15"
        , "L19"
        , "L23"
        , "L26"
        , "L28"
        ]
