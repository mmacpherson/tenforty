{-# LANGUAGE OverloadedStrings #-}

module MS80105_2024
  ( ms80105_2024,
  )
where

import FormRefs
import TablesMS2024
import TenForty

ms80105_2024 :: Either FormError Form
ms80105_2024 = form "ms_80105" 2024 $ do
  -- Form 80-100 pp. 10-14: round each page-2 entry before the sums.
  wages <- keyInput "L38" "wages" "Mississippi taxable wages"
  business <- keyInput "L39" "business_income" "Mississippi net business income or loss"
  capital <- interior "L40" "capital_gain_loss" (roundE (importForm usScheduleDL21))
  rental <- keyInput "L41" "rental_income" "Mississippi rental and Schedule E income or loss"
  farm <- keyInput "L42" "farm_income" "Prepared Mississippi farm income or loss"
  interest <- keyInput "L43" "interest_income" "Mississippi taxable interest"
  dividends <- keyInput "L44" "dividend_income" "Total Mississippi dividends including the qualified subset"
  alimonyReceived <- keyInput "L45" "alimony_received" "Prepared taxable alimony received"
  pensions <- keyInput "L46" "taxable_pensions" "Prepared Mississippi taxable pension distributions"
  unemployment <- keyInput "L47" "unemployment" "Prepared taxable unemployment compensation"
  otherIncome <- keyInput "L48" "other_income" "Prepared Mississippi Schedule N income or loss"
  totalIncome <- interior "L49" "total_income" (sumOf (fmap roundE [wages, business, capital, rental, farm, interest, dividends, alimonyReceived, pensions, unemployment, otherIncome]))
  l50 <- keyInput "L50" "ira" "Prepared IRA deduction"
  l51 <- keyInput "L51" "sep_simple" "Prepared SEP SIMPLE and qualified-plan deduction"
  l52 <- keyInput "L52" "savings_penalty" "Prepared early savings withdrawal penalty"
  l53 <- keyInput "L53" "alimony" "Prepared qualifying alimony paid"
  l54 <- keyInput "L54" "moving" "Prepared qualifying moving expenses"
  l55 <- keyInput "L55" "reserve_pay" "Prepared National Guard or reserve pay exclusion"
  l56 <- keyInput "L56" "mpact" "Prepared MPACT deduction"
  l57 <- keyInput "L57" "macs" "Prepared MACS deduction"
  l58 <- keyInput "L58" "se_health" "Prepared self-employed health insurance deduction"
  l59 <- keyInput "L59" "hsa" "Prepared health savings account deduction"
  l60 <- keyInput "L60" "catastrophe" "Prepared catastrophe savings deduction"
  l62 <- keyInput "L62" "first_home" "Prepared first-home savings deduction"
  l63 <- keyInput "L63" "agricultural_disaster" "Prepared agricultural disaster adjustment"
  l64 <- keyInput "L64" "able" "Prepared ABLE adjustment"
  l61 <- interior "L61" "self_employment_tax_deduction" (roundE (importForm usScheduleSeL11))
  totalAdjustments <- interior "L65" "total_adjustments" (sumOf (fmap roundE [l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63, l64]))
  page2Agi <- interior "L66" "page2_ms_agi" (totalIncome .-. totalAdjustments)

  usePreparedAgi <- keyInput "UsePreparedAGI" "use_prepared_agi" "Use prepared raw household MS AGI instead of the page-2 inputs"
  preparedAgi <- keyInput "PreparedHouseholdAGI" "prepared_household_agi" "Prepared total MS AGI before entry rounding"
  spouseAgi <- keyInput "SpouseAGI" "spouse_adjusted_gross_income" "Spouse Column B MS AGI before deductions and exemptions"
  let householdAgi = ifPos usePreparedAgi preparedAgi page2Agi
      jointOnly value = byStatusE (byStatus (dollars 0) value (dollars 0) (dollars 0) (dollars 0))
  taxpayerAgi <- interior "L13A" "taxpayer_ms_agi" (roundE (householdAgi .-. jointOnly spouseAgi))
  spouseColumnAgi <- interior "L13B" "spouse_ms_agi" (jointOnly (roundE spouseAgi))
  l13 <- keyOutput "L13" "ms_agi" "Mississippi adjusted gross income" (taxpayerAgi .+. spouseColumnAgi)

  dependents <- keyInput "Dependents" "dependents" "Total federally claimed dependents"
  filer65 <- keyInput "Filer65" "filer_65_or_older" "Filer age-65 exemption box"
  spouse65 <- keyInput "Spouse65" "spouse_65_or_older" "Spouse age-65 exemption box (married statuses)"
  l10 <- keyInput "L10" "additional_exemptions" "Prepared total additional exemptions, including any blindness boxes"
  let spouseAgeCount = byStatusE (byStatus (dollars 0) (ifPos spouse65 (dollars 1) (dollars 0)) (ifPos spouse65 (dollars 1) (dollars 0)) (dollars 0) (dollars 0))
      additionalCount = sumOf [max0 (roundE dependents), ifPos filer65 (dollars 1) (dollars 0), spouseAgeCount]
      computedAdditional = lit msAdditionalExemption2024 .*. (additionalCount ./. dollars 1)
  additionalExemptions <- interior "L10Effective" "additional_exemption_amount" (greaterOf computedAdditional (roundE l10))
  l11 <- interior "L11" "filing_status_exemption" (byStatusE (fmap lit msExemption2024))
  l12 <- interior "L12" "total_exemptions" (l11 .+. additionalExemptions)
  l15 <- interior "L15" "exemption_allowance" (l12 .*. byStatusE (fmap rate (byStatus 1 1 0.5 1 1)))

  l14a <- keyInput "L14a" "itemized_deductions" "Prepared Mississippi itemized deductions"
  standardDeduction <- interior "L14b_std" "standard_deduction" (byStatusE (fmap lit msStandardDeduction2024))
  deductions <- interior "L14b" "deductions" (greaterOf (roundE l14a) standardDeduction)
  allowances <- interior "Allowances" "exemptions_and_deductions" (l15 .+. deductions)
  l16 <- keyOutput "L16" "ms_taxable_income" "Aggregate Mississippi taxable income (not below zero)" (l13 `subtractNotBelowZero` allowances)

  -- Exact minimum over the allocations permitted on pp. 5/7: preserve each
  -- original AGI column's available zero-rate capacity, spending allowances
  -- on the excess first. The archive proves this also handles signed AGIs.
  let threshold = lit msTaxThreshold2024
      zeroRateCapacity = minE threshold (max0 taxpayerAgi) .+. minE threshold (max0 spouseColumnAgi)
      minimumBalance = l16 `subtractNotBelowZero` zeroRateCapacity

  usePrepared <- keyInput "UsePreparedAllocation" "use_prepared_allocation" "Use raw prepared spouse deduction/exemption allocation instead of the lawful minimum"
  spouseDeduction <- keyInput "PreparedSpouseDeduction" "prepared_spouse_deduction" "Column B deduction from a prepared joint return"
  spouseExemption <- keyInput "PreparedSpouseExemption" "prepared_spouse_exemption" "Column B exemption from a prepared joint return"
  let preparedTaxpayer = taxpayerAgi .-. (deductions .-. roundE spouseDeduction) .-. (l15 .-. roundE spouseExemption)
      preparedSpouse = spouseColumnAgi .-. roundE spouseDeduction .-. roundE spouseExemption
      combinedBalance = (preparedTaxpayer .+. preparedSpouse) `subtractNotBelowZero` threshold
      separateBalance = (preparedTaxpayer `subtractNotBelowZero` threshold) .+. (preparedSpouse `subtractNotBelowZero` threshold)
      preparedBalance = ifPos preparedTaxpayer (ifPos preparedSpouse separateBalance combinedBalance) combinedBalance
      preparedChoice = ifPos usePrepared preparedBalance minimumBalance
      chosenBalance = byStatusE (byStatus minimumBalance preparedChoice minimumBalance minimumBalance minimumBalance)
  taxedBalance <- interior "TaxedBalance" "taxed_balance" chosenBalance
  l17 <- keyOutput "L17" "ms_computed_tax" "Computed whole-dollar tax" (roundE (taxedBalance .*. rate msTaxRate2024))
  l18 <- keyOutput "L18" "ms_tax" "Computed tax before credits" l17

  l19 <- keyInput "L19" "interest_penalty" "Interest and penalty"
  l20 <- keyInput "L20" "credits" "Credits from Schedule C"
  l21 <- keyOutput "L21" "tax_after_credits" "Tax after credits" ((l18 .+. roundE l19) `subtractNotBelowZero` roundE l20)
  l22 <- keyInput "L22" "total_payments" "Total payments"
  _ <- keyOutput "L23a" "refund" "Amount to be refunded" (roundE l22 `excessOf` l21)
  _ <- keyOutput "L25" "amount_due" "Amount due" (l21 `subtractNotBelowZero` roundE l22)

  outputs ["L13", "L16", "L17", "L18", "L21", "L23a", "L25"]
