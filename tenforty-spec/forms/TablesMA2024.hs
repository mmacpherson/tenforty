module TablesMA2024 (
    -- * Massachusetts State Income Tax Rates
    maBaseRate2024,
    maSurtaxRate2024,
    maSurtaxThreshold2024,
    maShortTermCapitalGainsRate2024,
    maLongTermCollectiblesRate2024,

    -- * Personal Exemptions
    maPersonalExemption2024,
    maDependentExemption2024,
    maAge65Exemption2024,
    maBlindnessExemption2024,
) where

import TenForty.Types

{- | 2024 Massachusetts base income tax rate
Source: MA Form 1 Instructions (2024), pg 14, Line 22
Massachusetts has a flat 5.0% tax rate on most income.
-}
maBaseRate2024 :: Double
maBaseRate2024 = 0.05

{- | 2024 Massachusetts surtax rate (additional tax on high earners)
Source: MA Form 1 Instructions (2024), pg 1, 4% Surtax section
An additional 4% tax applies to taxable income over the surtax threshold.
-}
maSurtaxRate2024 :: Double
maSurtaxRate2024 = 0.04

{- | 2024 Massachusetts surtax threshold
Source: MA Form 1 Instructions (2024), pg 1, 4% Surtax section; pg 15, Line 28b
For 2024, the surtax applies to taxable income exceeding $1,053,750.
-}
maSurtaxThreshold2024 :: Amount Dollars
maSurtaxThreshold2024 = 1053750

{- | 2024 Massachusetts short-term capital gains tax rate
Source: MA Form 1 Instructions (2024), pg 14, Line 23a
Short-term capital gains from sales or exchanges of capital assets are taxed at 8.5%.
-}
maShortTermCapitalGainsRate2024 :: Double
maShortTermCapitalGainsRate2024 = 0.085

{- | 2024 Massachusetts long-term collectibles tax rate
Source: MA Form 1 Instructions (2024), pg 14, Line 23b
Long-term gains on collectibles are taxed at 12%.
-}
maLongTermCollectiblesRate2024 :: Double
maLongTermCollectiblesRate2024 = 0.12

{- | 2024 Massachusetts personal exemption amounts
Source: MA Form 1 Instructions (2024), pg 8, Line 2a
Single/MFS: $4,400
HoH: $6,800
MFJ: $8,800
Order: Single, MFJ, MFS, HoH, QW (QW uses MFJ amount)
-}
maPersonalExemption2024 :: ByStatus (Amount Dollars)
maPersonalExemption2024 = byStatus 4400 8800 4400 6800 8800

{- | 2024 Massachusetts dependent exemption amount
Source: MA Form 1 Instructions (2024), pg 8, Line 2b
Each dependent provides a $1,000 exemption.
-}
maDependentExemption2024 :: Amount Dollars
maDependentExemption2024 = 1000

{- | 2024 Massachusetts age 65 or over exemption amount
Source: MA Form 1 Instructions (2024), pg 8, Line 2c
\$700 for each taxpayer who is 65 or over before the end of the tax year.
-}
maAge65Exemption2024 :: Amount Dollars
maAge65Exemption2024 = 700

{- | 2024 Massachusetts blindness exemption amount
Source: MA Form 1 Instructions (2024), pg 9, Line 2d
\$2,200 for each taxpayer who is legally blind.
-}
maBlindnessExemption2024 :: Amount Dollars
maBlindnessExemption2024 = 2200
