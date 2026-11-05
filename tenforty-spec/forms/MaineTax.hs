-- | Form 1040ME line 20 and the line 17 and line 18 phase-out worksheets.
--
-- Rulings recorded on tenforty-tj2.20 (2026-11-03):
--
-- * Line 20 below \$100,000 is the published Maine Income Tax Table: \$100
--   rows priced at the row midpoint on the printed rate schedule, rounded half
--   up. Its two opening rows break that rule and are printed as fixed amounts:
--   0-50 is \$0 and 50-100 is \$3, as in the 2024 table and the 2025 booklet.
--   (The 2025 standalone table prints one 0-100 row at \$3; the booklet,
--   which line 20 names, is followed.) At \$100,000 and over, the printed rate
--   schedule applied to line 19, rounded to whole dollars. The table's
--   "100,000 and over" row, the schedule at line 19 minus \$50, is a permitted
--   reading that is not taken.
--
-- * The printed schedules carry whole-dollar bracket bases, so they are not
--   the bracket integral of the marginal rates.
--
-- * Worksheet line 5 is entered to four places, rounded half up; lines 7 and
--   8 are whole dollars (instructions: "Use whole dollar amounts").
--
-- Sources: docs/validation/derivations/me-tax-table-2024-2025.md and
-- me-2024-2025.md.
module MaineTax
  ( MaineSchedule (..),
    maineIncomeTax,
    phaseoutWorksheet,
  )
where

import StateTaxTable
import TenForty

-- | One year's printed Tax Rate Schedules #1-#3, by filing status.
data MaineSchedule = MaineSchedule
  { lowRate :: Double,
    middleRate :: Double,
    topRate :: Double,
    middleStart :: ByStatus (Amount Dollars),
    middleBase :: ByStatus (Amount Dollars),
    topStart :: ByStatus (Amount Dollars),
    topBase :: ByStatus (Amount Dollars)
  }

printedScheduleTax :: MaineSchedule -> Expr Dollars -> Expr Dollars
printedScheduleTax schedule income =
  ifGte
    income
    (byStatus' topStart)
    (byStatus' topBase .+. (income .-. byStatus' topStart) .*. rate (topRate schedule))
    ( ifGte
        income
        (byStatus' middleStart)
        (byStatus' middleBase .+. (income .-. byStatus' middleStart) .*. rate (middleRate schedule))
        (income .*. rate (lowRate schedule))
    )
  where
    byStatus' field = byStatusE (fmap lit (field schedule))

maineIncomeTax :: MaineSchedule -> TableId -> Expr Dollars -> Expr Dollars
maineIncomeTax schedule rateSchedule taxableIncome =
  stateTaxTableBy
    [FixedRow 50 0, FixedRow 100 3, GridRows 100000 100 50]
    rateSchedule
    (printedScheduleTax schedule)
    (roundE (printedScheduleTax schedule taxableIncome))
    taxableIncome

-- | Worksheet lines 3-8: the allowed part of @amount@ (line 6) at Maine AGI
-- @agi@ (line 1). Line 5 is held in basis points so that each product and
-- quotient below is one correctly rounded operation on whole numbers, and an
-- exact half (line 7 = 1,964.50 at 2024 Single AGI 373,007) rounds up.
phaseoutWorksheet :: ByStatus (Amount Dollars) -> ByStatus (Amount Dollars) -> Expr Dollars -> Expr Dollars -> Expr Dollars
phaseoutWorksheet threshold range agi amount = amount .-. line7
  where
    line3 = agi `excessOf` byStatusE (fmap lit threshold)
    line5BasisPoints =
      minE
        (roundE (quotient (line3 .*. rate 10000) (byStatusE (fmap lit range))))
        (dollars 10000)
    line7 = roundE (quotient (amount .*. (line5BasisPoints ./. dollars 1)) (dollars 10000))
    quotient numerator denominator = dollars 1 .*. (numerator ./. denominator)
