module TablesMI2025 (
    -- * Michigan Income Tax Rate
    miTaxRate2025,

    -- * Personal Exemption
    miPersonalExemption2025,

    -- * Special Exemptions
    miSpecialExemption2025,
    miDisabledVeteranExemption2025,
) where

import TenForty.Types

-- | 2025 Michigan flat income tax rate (4.25%)
miTaxRate2025 :: Amount Rate
miTaxRate2025 = 0.0425

-- | 2025 Michigan personal exemption allowance per person
-- Source: Michigan Department of Treasury (Form 446, Rev. 01-25)
miPersonalExemption2025 :: Amount Dollars
miPersonalExemption2025 = 5800

-- | 2025 Michigan special exemption (deaf, blind, disabled)
miSpecialExemption2025 :: Amount Dollars
miSpecialExemption2025 = 3400

-- | 2025 Michigan qualified disabled veteran exemption
miDisabledVeteranExemption2025 :: Amount Dollars
miDisabledVeteranExemption2025 = 500
