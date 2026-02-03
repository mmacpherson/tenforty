module TablesOR2024 (
    -- * Oregon State Income Tax Brackets
    oregonBrackets2024,
    oregonBracketsTable2024,

    -- * Standard Deduction
    orStandardDeduction2024,

    -- * Federal Tax Subtraction
    orFederalTaxSubtractionLimit2024,
    orFederalTaxSubtractionPhaseoutStart2024,
    orFederalTaxSubtractionPhaseoutEnd2024,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2024 Oregon state income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: Oregon Department of Revenue 2024 Tax Rate Charts
https://www.oregon.gov/dor/programs/individuals/Documents/2024%20rate%20charts.pdf
and https://www.tax-brackets.org/oregontaxtable (verified against official sources)
-}
oregonBrackets2024 :: NonEmpty Bracket
oregonBrackets2024 =
    Bracket (byStatus 4400 8800 4400 8600 8800) 0.0475
        :| [ Bracket (byStatus 11050 22100 11050 21500 22100) 0.0675
           , Bracket (byStatus 125000 250000 125000 250000 250000) 0.0875
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.099
           ]

oregonBracketsTable2024 :: Table
oregonBracketsTable2024 =
    case mkBracketTable oregonBrackets2024 of
        Right bt -> TableBracket "or_brackets_2024" bt
        Left err -> error $ "Invalid Oregon brackets: " ++ err

{- | 2024 Oregon state standard deduction amounts
Order: Single, MFJ, MFS, HoH, QW
Source: Oregon Form OR-40 Instructions 2024, Table 5 (pg 17)
Single: $2,745
MFJ: $5,495
MFS: $2,745 (or $0 if spouse itemizes)
HoH: $4,420
QW: $5,495
-}
orStandardDeduction2024 :: ByStatus (Amount Dollars)
orStandardDeduction2024 = byStatus 2745 5495 2745 4420 5495

{- | 2024 Oregon federal tax subtraction limit
Source: Oregon Publication OR-17 (2024), pg 71, "Federal income tax liability"
The 2024 federal tax subtraction is limited to $8,250 ($4,125 if married filing separately).
-}
orFederalTaxSubtractionLimit2024 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionLimit2024 = byStatus 8250 8250 4125 8250 8250

{- | 2024 Oregon federal tax subtraction AGI phaseout start
Source: Oregon Publication OR-17 (2024), Table 9 (pg 72)
Single/MFS: $125,000
MFJ/HoH/QW: $250,000
-}
orFederalTaxSubtractionPhaseoutStart2024 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionPhaseoutStart2024 = byStatus 125000 250000 125000 250000 250000

{- | 2024 Oregon federal tax subtraction AGI phaseout end
Source: Oregon Publication OR-17 (2024), Table 9 (pg 72)
Single/MFS: $145,000 (phaseout complete)
MFJ/HoH/QW: $290,000 (phaseout complete)
-}
orFederalTaxSubtractionPhaseoutEnd2024 :: ByStatus (Amount Dollars)
orFederalTaxSubtractionPhaseoutEnd2024 = byStatus 145000 290000 145000 290000 290000
