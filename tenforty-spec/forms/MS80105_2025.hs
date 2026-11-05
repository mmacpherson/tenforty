{-# LANGUAGE OverloadedStrings #-}

module MS80105_2025
  ( ms80105_2025,
  )
where

import FormRefs
import TablesMS2025
import TenForty

ms80105_2025 :: Either FormError Form
ms80105_2025 = form "ms_80105" 2025 $ do
  -- Form 80-100 pp. 5-7: separate MS AGI columns before rounding entries.
  -- The natural MFJ lowering requires the actual Column B line 13 fact.
  let federalAgi = importForm us1040L11
      jointOnly value = byStatusE (byStatus (dollars 0) value (dollars 0) (dollars 0) (dollars 0))
  spouseAgi <- keyInput "SpouseAGI" "spouse_adjusted_gross_income" "Spouse Column B MS AGI before deductions and exemptions"
  taxpayerAgi <- interior "L13A" "taxpayer_ms_agi" (roundE (federalAgi .-. jointOnly spouseAgi))
  spouseColumnAgi <- interior "L13B" "spouse_ms_agi" (jointOnly (roundE spouseAgi))
  l13 <- keyOutput "L13" "ms_agi" "Mississippi adjusted gross income" (taxpayerAgi .+. spouseColumnAgi)

  dependents <- keyInput "Dependents" "dependents" "Total federally claimed dependents"
  filer65 <- keyInput "Filer65" "filer_65_or_older" "Filer age-65 exemption box"
  spouse65 <- keyInput "Spouse65" "spouse_65_or_older" "Spouse age-65 exemption box (married statuses)"
  l10 <- keyInput "L10" "additional_exemptions" "Prepared total additional exemptions, including any blindness boxes"
  let spouseAgeCount = byStatusE (byStatus (dollars 0) (ifPos spouse65 (dollars 1) (dollars 0)) (ifPos spouse65 (dollars 1) (dollars 0)) (dollars 0) (dollars 0))
      additionalCount = sumOf [max0 (roundE dependents), ifPos filer65 (dollars 1) (dollars 0), spouseAgeCount]
      computedAdditional = lit msAdditionalExemption2025 .*. (additionalCount ./. dollars 1)
  additionalExemptions <- interior "L10Effective" "additional_exemption_amount" (greaterOf computedAdditional (roundE l10))
  l11 <- interior "L11" "filing_status_exemption" (byStatusE (fmap lit msExemption2025))
  l12 <- interior "L12" "total_exemptions" (l11 .+. additionalExemptions)
  l15 <- interior "L15" "exemption_allowance" (l12 .*. byStatusE (fmap rate (byStatus 1 1 0.5 1 1)))

  l14a <- keyInput "L14a" "itemized_deductions" "Prepared Mississippi itemized deductions"
  standardDeduction <- interior "L14b_std" "standard_deduction" (byStatusE (fmap lit msStandardDeduction2025))
  deductions <- interior "L14b" "deductions" (greaterOf (roundE l14a) standardDeduction)
  allowances <- interior "Allowances" "exemptions_and_deductions" (l15 .+. deductions)
  l16 <- keyOutput "L16" "ms_taxable_income" "Aggregate Mississippi taxable income (not below zero)" (l13 `subtractNotBelowZero` allowances)

  -- Exact minimum over the allocations permitted on pp. 5/7: preserve each
  -- original AGI column's available zero-rate capacity, spending allowances
  -- on the excess first. The archive proves this also handles signed AGIs.
  let threshold = lit msTaxThreshold2025
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
  l17 <- keyOutput "L17" "ms_computed_tax" "Computed whole-dollar tax" (roundE (taxedBalance .*. rate msTaxRate2025))
  l18 <- keyOutput "L18" "ms_tax" "Computed tax before credits" l17

  l19 <- keyInput "L19" "interest_penalty" "Interest and penalty"
  l20 <- keyInput "L20" "credits" "Credits from Schedule C"
  l21 <- keyOutput "L21" "tax_after_credits" "Tax after credits" ((l18 .+. roundE l19) `subtractNotBelowZero` roundE l20)
  l22 <- keyInput "L22" "total_payments" "Total payments"
  _ <- keyOutput "L23a" "refund" "Amount to be refunded" (roundE l22 `excessOf` l21)
  _ <- keyOutput "L25" "amount_due" "Amount due" (l21 `subtractNotBelowZero` roundE l22)

  outputs ["L13", "L16", "L17", "L18", "L21", "L23a", "L25"]
