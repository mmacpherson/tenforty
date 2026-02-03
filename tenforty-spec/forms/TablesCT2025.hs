module TablesCT2025 (
    -- * Connecticut State Income Tax Brackets
    ctBrackets2025,
    ctBracketsTable2025,

    -- * Personal Exemption Phaseout
    ctPersonalExemptionBase2025,
    ctPersonalExemptionPhaseoutStart2025,
    ctPersonalExemptionPhaseoutEnd2025,
) where

import Data.List.NonEmpty (NonEmpty (..))

import TenForty.Table
import TenForty.Types

{- | 2025 Connecticut income tax brackets
Order: Single, MFJ, MFS, HoH, QW
Source: CT Form CT-1040 Tax Calculation Schedule (Rev. 12/25), Table A
Brackets are unchanged from 2024 per CT DRS confirmation.
-}
ctBrackets2025 :: NonEmpty Bracket
ctBrackets2025 =
    Bracket (byStatus 10000 20000 10000 16000 20000) 0.02
        :| [ Bracket (byStatus 50000 100000 50000 80000 100000) 0.045
           , Bracket (byStatus 100000 200000 100000 160000 200000) 0.055
           , Bracket (byStatus 200000 400000 200000 320000 400000) 0.06
           , Bracket (byStatus 250000 500000 250000 400000 500000) 0.065
           , Bracket (byStatus 500000 1000000 500000 800000 1000000) 0.069
           , Bracket (byStatus 1e12 1e12 1e12 1e12 1e12) 0.0699
           ]

ctBracketsTable2025 :: Table
ctBracketsTable2025 =
    case mkBracketTable ctBrackets2025 of
        Right bt -> TableBracket "ct_brackets_2025" bt
        Left err -> error $ "Invalid Connecticut brackets: " ++ err

{- | 2025 Connecticut personal exemption base amounts (before phaseout)
Order: Single, MFJ, MFS, HoH, QW
Source: CT Form CT-1040 Tax Calculation Schedule (Rev. 12/25), Table A
Values unchanged from 2024.
Single: $15,000 (for AGI $0-$30,000)
MFJ/QW: $24,000 (for AGI $0-$48,000)
MFS: $12,000 (for AGI $0-$24,000)
HoH: $19,000 (for AGI $0-$38,000)
-}
ctPersonalExemptionBase2025 :: ByStatus (Amount Dollars)
ctPersonalExemptionBase2025 = byStatus 15000 24000 12000 19000 24000

{- | 2025 Connecticut personal exemption phaseout start thresholds
Order: Single, MFJ, MFS, HoH, QW
Source: CT Form CT-1040 Tax Calculation Schedule (Rev. 12/25), Table A
Values unchanged from 2024.
Single: phaseout starts at $30,000
MFJ/QW: phaseout starts at $48,000
MFS: phaseout starts at $24,000
HoH: phaseout starts at $38,000
-}
ctPersonalExemptionPhaseoutStart2025 :: ByStatus (Amount Dollars)
ctPersonalExemptionPhaseoutStart2025 = byStatus 30000 48000 24000 38000 48000

{- | 2025 Connecticut personal exemption phaseout end thresholds
Order: Single, MFJ, MFS, HoH, QW
Source: CT Form CT-1040 Tax Calculation Schedule (Rev. 12/25), Table A
Values unchanged from 2024.
Single: phaseout ends at $44,000 (exemption reaches $0)
MFJ/QW: phaseout ends at $71,000 (exemption reaches $0)
MFS: phaseout ends at $35,000 (exemption reaches $0)
HoH: phaseout ends at $56,000 (exemption reaches $0)
-}
ctPersonalExemptionPhaseoutEnd2025 :: ByStatus (Amount Dollars)
ctPersonalExemptionPhaseoutEnd2025 = byStatus 44000 71000 35000 56000 71000
