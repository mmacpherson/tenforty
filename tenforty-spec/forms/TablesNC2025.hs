module TablesNC2025 (
    -- * North Carolina Income Tax Rate
    ncTaxRate2025,

    -- * Standard Deductions
    ncStandardDeduction2025,

    -- * Child Deduction Tables
    ncChildDeductionTable2025,
) where

import Data.List.NonEmpty (NonEmpty (..))
import TenForty.Table
import TenForty.Types

-- | 2025 North Carolina flat income tax rate (4.5%)
-- Assumed same as 2024
ncTaxRate2025 :: Amount Rate
ncTaxRate2025 = 0.045

-- | 2025 North Carolina standard deduction
-- Assumed same as 2024
ncStandardDeduction2025 :: ByStatus (Amount Dollars)
ncStandardDeduction2025 = byStatus 12750 25500 12750 19125 25500

-- | Child Deduction Amount per dependent child based on AGI and filing status
-- Source: NC D-400 Instructions 2024 (Assumed same for 2025)
ncChildDeductionTable2025 :: Table
ncChildDeductionTable2025 = TableLookup "nc_child_deduction_2025" $
    LookupTable $
        LookupEntry 0 20000 (byStatus 3000 3000 3000 3000 3000)
            :| [ LookupEntry 20000 30000 (byStatus 2500 3000 2500 3000 3000)
               , LookupEntry 30000 40000 (byStatus 2000 3000 2000 2500 3000)
               , LookupEntry 40000 45000 (byStatus 1500 2500 1500 2500 2500)
               , LookupEntry 45000 50000 (byStatus 1500 2500 1500 2000 2500)
               , LookupEntry 50000 60000 (byStatus 1000 2500 1000 2000 2500)
               , LookupEntry 60000 70000 (byStatus 500 2000 500 2000 2000)
               , LookupEntry 70000 75000 (byStatus 0 2000 0 1500 2000)
               , LookupEntry 75000 80000 (byStatus 0 2000 0 1000 2000)
               , LookupEntry 80000 90000 (byStatus 0 1500 0 1000 1500)
               , LookupEntry 90000 100000 (byStatus 0 1500 0 500 1500)
               , LookupEntry 100000 105000 (byStatus 0 1000 0 500 1000)
               , LookupEntry 105000 120000 (byStatus 0 1000 0 0 1000)
               , LookupEntry 120000 140000 (byStatus 0 500 0 0 500)
               , LookupEntry 140000 (1/0) (byStatus 0 0 0 0 0)
               ]
