module TablesMI2024 (
    -- * Michigan Income Tax Rate
    miTaxRate2024,

    -- * Personal Exemption
    miPersonalExemption2024,

    -- * Special Exemptions
    miSpecialExemption2024,
    miDisabledVeteranExemption2024,
) where

import TenForty.Types

-- | 2024 Michigan flat income tax rate (4.25%)
miTaxRate2024 :: Amount Rate
miTaxRate2024 = 0.0425

-- | 2024 Michigan personal exemption allowance per person
-- Source: Michigan Department of Treasury
miPersonalExemption2024 :: Amount Rate
miPersonalExemption2024 = 5600

-- | 2024 Michigan special exemption (deaf, blind, disabled)
miSpecialExemption2024 :: Amount Dollars
miSpecialExemption2024 = 3300

-- | 2024 Michigan qualified disabled veteran exemption
miDisabledVeteranExemption2024 :: Amount Dollars
miDisabledVeteranExemption2024 = 500
