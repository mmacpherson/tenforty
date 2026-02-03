module TablesMA2025 (
    -- * Massachusetts State Income Tax Rates
    maBaseRate2025,
    maSurtaxRate2025,
    maSurtaxThreshold2025,
    maShortTermCapitalGainsRate2025,
    maLongTermCollectiblesRate2025,

    -- * Personal Exemptions
    maPersonalExemption2025,
    maDependentExemption2025,
    maAge65Exemption2025,
    maBlindnessExemption2025,
) where

import TenForty.Types

{- | 2025 Massachusetts base income tax rate
Source: MA Form 1 Instructions (2025), pg 14, Line 22
Massachusetts has a flat 5.0% tax rate on most income (unchanged from 2024).
-}
maBaseRate2025 :: Double
maBaseRate2025 = 0.05

{- | 2025 Massachusetts surtax rate (additional tax on high earners)
Source: MA Form 1 Instructions (2025), pg 1, 4% Surtax section
An additional 4% tax applies to taxable income over the surtax threshold (unchanged from 2024).
-}
maSurtaxRate2025 :: Double
maSurtaxRate2025 = 0.04

{- | 2025 Massachusetts surtax threshold
Source: MA Form 1 Instructions (2025), pg 1, 4% Surtax section; pg 15, Line 28b
For 2025, the surtax applies to taxable income exceeding $1,083,150 (inflation-adjusted from $1,053,750 in 2024).
-}
maSurtaxThreshold2025 :: Amount Dollars
maSurtaxThreshold2025 = 1083150

{- | 2025 Massachusetts short-term capital gains tax rate
Source: MA Form 1 Instructions (2025), pg 14, Line 23a
Short-term capital gains from sales or exchanges of capital assets are taxed at 8.5% (unchanged from 2024).
-}
maShortTermCapitalGainsRate2025 :: Double
maShortTermCapitalGainsRate2025 = 0.085

{- | 2025 Massachusetts long-term collectibles tax rate
Source: MA Form 1 Instructions (2025), pg 14, Line 23b
Long-term gains on collectibles are taxed at 12% (unchanged from 2024).
-}
maLongTermCollectiblesRate2025 :: Double
maLongTermCollectiblesRate2025 = 0.12

{- | 2025 Massachusetts personal exemption amounts
Source: MA Form 1 Instructions (2025), pg 8, Line 2a
Single/MFS: $4,400
HoH: $6,800
MFJ: $8,800
(Unchanged from 2024)
Order: Single, MFJ, MFS, HoH, QW (QW uses MFJ amount)
-}
maPersonalExemption2025 :: ByStatus (Amount Dollars)
maPersonalExemption2025 = byStatus 4400 8800 4400 6800 8800

{- | 2025 Massachusetts dependent exemption amount
Source: MA Form 1 Instructions (2025), pg 8, Line 2b
Each dependent provides a $1,000 exemption (unchanged from 2024).
-}
maDependentExemption2025 :: Amount Dollars
maDependentExemption2025 = 1000

{- | 2025 Massachusetts age 65 or over exemption amount
Source: MA Form 1 Instructions (2025), pg 8, Line 2c
\$700 for each taxpayer who is 65 or over before the end of the tax year (unchanged from 2024).
-}
maAge65Exemption2025 :: Amount Dollars
maAge65Exemption2025 = 700

{- | 2025 Massachusetts blindness exemption amount
Source: MA Form 1 Instructions (2025), pg 9, Line 2d
\$2,200 for each taxpayer who is legally blind (unchanged from 2024).
-}
maBlindnessExemption2025 :: Amount Dollars
maBlindnessExemption2025 = 2200
