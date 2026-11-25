module TablesOR2024
  ( -- * Oregon State Income Tax Brackets
    oregonBrackets2024,
    oregonBracketsTable2024,
    oregonTaxTable2024,

    -- * Standard Deduction
    orStandardDeduction2024,

    -- * Federal Tax Subtraction
    orFederalTaxSubtractionLimit2024,
    orFederalTaxSubtractionPhaseoutStart2024,
    orFederalTaxSubtractionPhaseoutEnd2024,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import TenForty.Expr qualified as E
import TenForty.Table
import TenForty.TaxTable (mkTaxTable, taxTableBandIncome)
import TenForty.Types

-- | 2024 Oregon state income tax brackets
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Oregon Department of Revenue 2024 Tax Rate Charts
-- https://www.oregon.gov/dor/programs/individuals/Documents/2024%20rate%20charts.pdf
oregonBrackets2024 :: NonEmpty Bracket
oregonBrackets2024 =
  Bracket (byStatus 4300 8600 4300 8600 8600) 0.0475
    :| [ Bracket (byStatus 10750 21500 10750 21500 21500) 0.0675,
         Bracket (byStatus 125000 250000 125000 250000 250000) 0.0875,
         TopBracket 0.099
       ]

oregonBracketsTable2024 :: Table
oregonBracketsTable2024 =
  case mkBracketTable oregonBrackets2024 of
    Right bt -> TableBracket "or_brackets_2024" bt
    Left err -> error $ "Invalid Oregon brackets: " ++ err

-- | 2024 Oregon state standard deduction amounts
-- Order: Single, MFJ, MFS, HoH, QW
-- Source: Oregon Form OR-40 Instructions 2024, Table 5 (p. 16)
-- Single: $2,745
-- MFJ: $5,495
-- MFS: $2,745 (or $0 if spouse itemizes)
-- HoH: $4,420
-- QW: $5,495
orStandardDeduction2024 :: ByStatus (Amount Dollars)
orStandardDeduction2024 = byStatus 2745 5495 2745 4420 5495

-- | 2024 Oregon federal tax subtraction limit
-- Source: Oregon Form OR-40 Instructions 2024, p. 14
-- The 2024 federal tax subtraction is limited to $8,250 ($4,125 if married filing separately).
orFederalTaxSubtractionLimit2024 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionLimit2024 = byStatus 8250 8250 4125 8250 8250

-- | 2024 Oregon federal tax subtraction AGI phaseout start
-- Source: Oregon Form OR-40 Instructions 2024, Table 4 (p. 15)
-- Single/MFS: $125,000
-- MFJ/HoH/QW: $250,000
orFederalTaxSubtractionPhaseoutStart2024 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionPhaseoutStart2024 = byStatus 125000 250000 125000 250000 250000

-- | 2024 Oregon federal tax subtraction AGI phaseout end
-- Source: Oregon Form OR-40 Instructions 2024, Table 4 (p. 15)
-- Single/MFS: $145,000 (phaseout complete)
-- MFJ/HoH/QW: $290,000 (phaseout complete)
orFederalTaxSubtractionPhaseoutEnd2024 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionPhaseoutEnd2024 = byStatus 145000 290000 145000 290000 290000

-- | Every published OR-40 table row is reproduced by midpoint pricing
-- with each bracket increment rounded separately. The companion CSV pins
-- all 502 rows in both columns; see docs/validation/derivations/or-2024-2025.md.
oregonTaxTable2024 :: E.Expr Dollars -> E.Expr Dollars
oregonTaxTable2024 income =
  E.ifNeg (income E..-. E.lit 20) (E.lit 0) $
    E.ifNeg (income E..-. E.lit 50) (E.lit 2) $
      E.ifNeg (income E..-. E.lit 100) (E.lit 4) $
        roundedIncrements (E.lit 0) (NE.toList oregonBrackets2024)
  where
    midpoint = taxTableBandIncome taxTable income
    taxTable =
      case mkTaxTable 100 50 "or_brackets_2024" of
        Right table -> table
        Left err -> error $ "Invalid Oregon tax table: " ++ err
    roundedIncrements _ [] = E.lit 0
    roundedIncrements lower (bracket : remaining) =
      let upper = maybe midpoint (E.ByStatusE . fmap E.lit) (bracketThreshold bracket)
          capped = maybe midpoint (const (E.minE midpoint upper)) (bracketThreshold bracket)
          taxable = E.maxE (E.lit 0) (capped E..-. lower)
       in E.roundE (E.lit 1 E..*. ((taxable E..*. E.lit (bracketRate bracket * 10000)) E../. E.lit 10000))
            E..+. roundedIncrements upper remaining
