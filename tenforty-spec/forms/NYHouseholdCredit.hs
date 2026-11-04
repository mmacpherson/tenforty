module NYHouseholdCredit
  ( singleHouseholdCredit,
    jointHouseholdCredit,
    separateHouseholdCredit,
  )
where

import TenForty

-- IT-201-I (2024 and 2025), p.12 tables1–3 and p.13 notes3/5.
singleHouseholdCredit :: Expr Dollars -> Expr Dollars
singleHouseholdCredit agi =
  incomeRows
    agi
    [ (5000, dollars 75),
      (6000, dollars 60),
      (7000, dollars 50),
      (20000, dollars 45),
      (25000, dollars 40),
      (28000, dollars 20)
    ]

jointHouseholdCredit :: Expr Dollars -> Expr Dollars -> Expr Dollars
jointHouseholdCredit agi count =
  incomeRows
    agi
    [ (5000, householdRow count [90, 105, 120, 135, 150, 165, 180] 15),
      (6000, householdRow count [75, 90, 105, 120, 135, 150, 165] 15),
      (7000, householdRow count [65, 80, 95, 110, 125, 140, 155] 15),
      (20000, householdRow count [60, 75, 90, 105, 120, 135, 150] 15),
      (22000, householdRow count [60, 70, 80, 90, 100, 110, 120] 10),
      (25000, householdRow count [50, 60, 70, 80, 90, 100, 110] 10),
      (28000, householdRow count [40, 45, 50, 55, 60, 65, 70] 5),
      (32000, householdRow count [20, 25, 30, 35, 40, 45, 50] 5)
    ]

separateHouseholdCredit :: Expr Dollars -> Expr Dollars -> Expr Dollars
separateHouseholdCredit combinedAgi combinedCount =
  incomeRows
    combinedAgi
    [ (5000, householdRow combinedCount [45, 53, 60, 68, 75, 83, 90] 8),
      (6000, householdRow combinedCount [38, 45, 53, 60, 68, 75, 83] 8),
      (7000, householdRow combinedCount [33, 40, 48, 55, 63, 70, 78] 8),
      (20000, householdRow combinedCount [30, 38, 45, 53, 60, 68, 75] 8),
      (22000, householdRow combinedCount [30, 35, 40, 45, 50, 55, 60] 5),
      (25000, householdRow combinedCount [25, 30, 35, 40, 45, 50, 55] 5),
      (28000, householdRow combinedCount [20, 23, 25, 28, 30, 33, 35] 3),
      (32000, householdRow combinedCount [10, 13, 15, 18, 20, 23, 25] 3)
    ]

incomeRows :: Expr Dollars -> [(Double, Expr Dollars)] -> Expr Dollars
incomeRows agi =
  foldr (\(upper, credit) rest -> ifGte (dollars upper) agi credit rest) (dollars 0)

householdRow :: Expr Dollars -> [Double] -> Double -> Expr Dollars
householdRow count = go 1
  where
    go :: Double -> [Double] -> Double -> Expr Dollars
    go _ [] _ = error "A household-credit row must contain credit columns"
    go index [credit] increment =
      dollars credit .+. (maxE (dollars 0) (count .-. dollars index) .*. rate increment)
    go index (credit : rest) increment =
      ifGte (dollars index) count (dollars credit) (go (index + 1) rest increment)
