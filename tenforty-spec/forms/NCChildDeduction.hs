module NCChildDeduction
  ( ncChildDeductionPerChild,
  )
where

import TenForty

-- 2024/2025 D-401 p.13, Child Deduction Table; federal AGI, inclusive ceilings.
ncChildDeductionPerChild :: Expr Dollars -> Expr Dollars
ncChildDeductionPerChild agi =
  byStatusE $ fmap (incomeRows agi) (byStatus single joint single headOfHousehold joint)
  where
    single = [(20000, 3000), (30000, 2500), (40000, 2000), (50000, 1500), (60000, 1000), (70000, 500)]
    joint = [(40000, 3000), (60000, 2500), (80000, 2000), (100000, 1500), (120000, 1000), (140000, 500)]
    headOfHousehold = [(30000, 3000), (45000, 2500), (60000, 2000), (75000, 1500), (90000, 1000), (105000, 500)]

incomeRows :: Expr Dollars -> [(Rational, Rational)] -> Expr Dollars
incomeRows agi =
  foldr (\(ceiling, amount) rest -> ifGte (dollars ceiling) agi (dollars amount) rest) (dollars 0)
