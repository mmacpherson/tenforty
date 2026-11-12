module TablesOR2025
  ( -- * Oregon State Income Tax Brackets
    oregonBrackets2025,
    oregonBracketsTable2025,
    oregonTaxTable2025,

    -- * Standard Deduction
    orStandardDeduction2025,

    -- * Federal Tax Subtraction
    orFederalTaxSubtractionLimit2025,
    orFederalTaxSubtractionPhaseoutStart2025,
    orFederalTaxSubtractionPhaseoutEnd2025,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import TenForty.Expr qualified as E
import TenForty.Table
import TenForty.TaxTable (mkTaxTable, taxTableBandIncome)
import TenForty.Types

-- | 2025 Oregon state income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Oregon Form OR-40-N/P Instructions 2025, p. 21
-- https://www.oregon.gov/dor/forms/FormsPubs/form-or-40-n_or-40-p-inst_101-048-1_2025.pdf
-- Note: Brackets under $125,000 are indexed for inflation annually.
oregonBrackets2025 :: NonEmpty Bracket
oregonBrackets2025 =
  Bracket (byStatus 4400 8800 4400 8800 8800) 0.0475
    :| [ Bracket (byStatus 11100 22200 11100 22200 22200) 0.0675,
         Bracket (byStatus 125000 250000 125000 250000 250000) 0.0875,
         TopBracket 0.099
       ]

oregonBracketsTable2025 :: Table
oregonBracketsTable2025 =
  case mkBracketTable oregonBrackets2025 of
    Right bt -> TableBracket "or_brackets_2025" bt
    Left err -> error $ "Invalid Oregon brackets: " ++ err

-- | 2025 Oregon state standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Oregon Form OR-40 Instructions 2025, Table 5 (p. 16)
-- Single: $2,835
-- MFJ: $5,670
-- MFS: $2,835 (or $0 if spouse itemizes)
-- HoH: $4,560
-- QW: $5,670
orStandardDeduction2025 :: ByStatus (Amount Dollars)
orStandardDeduction2025 = byStatus 2835 5670 2835 4560 5670

-- | 2025 Oregon federal tax subtraction limit
-- Source: Oregon Form OR-40 Instructions 2025, p. 14, "2025 federal tax liability subtraction"
-- The 2025 federal tax subtraction is limited to $8,500 ($4,250 if married filing separately).
orFederalTaxSubtractionLimit2025 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionLimit2025 = byStatus 8500 8500 4250 8500 8500

-- | 2025 Oregon federal tax subtraction AGI phaseout start
-- Source: Oregon Form OR-40 Instructions 2025, Table 4 (p. 15)
-- Single/MFS: $125,000
-- MFJ/HoH/QW: $250,000
orFederalTaxSubtractionPhaseoutStart2025 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionPhaseoutStart2025 = byStatus 125000 250000 125000 250000 250000

-- | 2025 Oregon federal tax subtraction AGI phaseout end
-- Source: Oregon Form OR-40 Instructions 2025, Table 4 (p. 15)
-- Single/MFS: $145,000 (phaseout complete)
-- MFJ/HoH/QW: $290,000 (phaseout complete)
orFederalTaxSubtractionPhaseoutEnd2025 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionPhaseoutEnd2025 = byStatus 145000 290000 145000 290000 290000

-- | Every published OR-40 table row is reproduced by midpoint pricing
-- with each bracket increment rounded separately. The companion CSV pins
-- all 502 rows in both columns; see docs/validation/derivations/or-2024-2025.md.
oregonTaxTable2025 :: E.Expr Dollars -> E.Expr Dollars
oregonTaxTable2025 income =
  E.ifNeg (income E..-. E.lit 20) (E.lit 0) $
    E.ifNeg (income E..-. E.lit 50) (E.lit 2) $
      E.ifNeg (income E..-. E.lit 100) (E.lit 4) $
        roundedIncrements (E.lit 0) (NE.toList oregonBrackets2025)
  where
    midpoint = taxTableBandIncome taxTable income
    taxTable =
      case mkTaxTable 100 50 "or_brackets_2025" of
        Right table -> table
        Left err -> error $ "Invalid Oregon tax table: " ++ err
    roundedIncrements _ [] = E.lit 0
    roundedIncrements lower (bracket : remaining) =
      let upper = maybe midpoint (E.ByStatusE . fmap E.lit) (bracketThreshold bracket)
          capped = maybe midpoint (const (E.minE midpoint upper)) (bracketThreshold bracket)
          taxable = E.maxE (E.lit 0) (capped E..-. lower)
       in E.roundE (E.lit 1 E..*. ((taxable E..*. E.lit (bracketRate bracket * 10000)) E../. E.lit 10000))
            E..+. roundedIncrements upper remaining
