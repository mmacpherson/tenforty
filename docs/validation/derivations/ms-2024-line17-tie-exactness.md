# Mississippi 2024 line 17: no half-dollar tie misrounds (tenforty-s48h.77)

Evidence kind: exact integer arithmetic about the graph's rounding of one product,
not a tax-value derivation. Argument by Astra (independent review of
tenforty-s48h.77, 2026-11-11), checked and recorded by tenforty-opus-worker
2026-11-11. It supports keeping MS 2024 in graph scope while MS 2025 leaves.

## Scope of the claim

This concerns one rounding class only: Form 80-105 line 17, the whole-dollar
round-half-up of `taxed balance x rate` (2024 rate 4.7%, 2025 rate 4.4%; 2024 and
2025 Form 80-100 instructions, printed pp. 21/22 "Tax Rates" and rounding, and
pp. 26/27 Schedule of Tax Computation;
https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf,
https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf).
It is not a claim that every floating-point computation in the MS graph is exact,
nor about arbitrarily large returns beyond the bounds checked below.

The graph computes line 17 as `floor(b * r + 0.5)` in IEEE-754 binary64, where `b`
is the taxed balance and `r` is binary64(rate) (`MS80105_2024.hs`, `l17`).

## 2024: where the ties are

Let `c` be the taxed balance in integer cents. The exact product is
`47c / 100000` dollars, a half-dollar tie exactly when `47c = 50000 (mod 100000)`.
Since `gcd(47, 100000) = 1` and the inverse of 47 modulo 100000 maps 50000 to
50000 (47 is odd, so `47 * 50000 = 50000 mod 100000`), the only solutions are
`c = 50000 (mod 100000)`: a dollar balance of 500 (mod 1000). Cents-valued
balances add no ties beyond those whole-dollar ones.

## 2024: why those ties round correctly

binary64(0.047) exceeds 47/1000 by exactly 1/9007199254740992000 (computed with
exact rationals). For `b = 1000j + 500` the exact real product with the stored
rate is therefore strictly above the tie `47j + 23.5`, which is itself exactly
representable. Correctly rounded multiplication is monotone, so the computed
product is at least the tie, and `floor(x + 0.5)` gives `47j + 24`, the half-up
result. Contrast 2025: binary64(0.044) is below 44/1000 by 23/9007199254740992000,
so a computed product can land just under its tie and round down, which is
tenforty-s48h.77.

Bounded check, by exact integer arithmetic against the f64 model: all 1,000 ties
from $500 to $999,500 (and every cents-valued balance from $0.00 to $1,000,000.00)
give `floor(b * 0.047 + 0.5) = (47b + 500) // 1000`; no misround. The same sweep
at 4.4% finds 4,000 ties and 259 misrounds, all exactly $1 low.

## Joint returns are covered

The joint path does not round two spouse taxes. `MS80105_2024.hs` (lines 83-103)
forms one taxed balance (the lawful-minimum `minimumBalance`, or a prepared
allocation from `roundE`-rounded spouse amounts), and line 17 rounds one product
of that balance. Natural MFJ facts are actual whole-dollar line-13 entries
(`state_facts.MS`), page-2 entries are rounded to whole dollars, and allowances,
`min`/`max` and subtraction keep the balance an integer at these magnitudes. So the
tie analysis for an integer balance covers MFJ as well as Single wage returns.
