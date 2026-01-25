# State Tax Forms Prioritization

## Prioritization Framework

Three criteria for prioritizing state implementations:

| Criterion | Why It Matters |
|-----------|----------------|
| **Tax Burden** | High-tax states = users care more about accuracy |
| **Population** | More potential users |
| **Complexity** | Simpler = faster to implement, good for testing framework |

---

## State Analysis

### Tier 1: High Value (High Tax + High Population)

| State | Population | Tax Burden | Complexity | Notes |
|-------|------------|------------|------------|-------|
| **California** | 39M (#1) | 13.3% top | High | **DONE** - brackets, credits, Schedule CA |
| **New York** | 19M (#4) | 10.9% top | High | NYC adds local tax, many credits |
| **New Jersey** | 9M (#11) | 10.75% top | Medium | Graduated brackets, property tax focus |
| **Hawaii** | 1.4M (#40) | 11% top | Medium | Highest rates but small population |

### Tier 2: High Population (Worth the Effort)

| State | Population | Tax Burden | Complexity | Notes |
|-------|------------|------------|------------|-------|
| **Texas** | 30M (#2) | 0% | None | No income tax - stub only |
| **Florida** | 22M (#3) | 0% | None | No income tax - stub only |
| **Pennsylvania** | 13M (#5) | 3.07% flat | Very Low | Flat rate, minimal forms |
| **Illinois** | 12M (#6) | 4.95% flat | Low | Flat rate |
| **Ohio** | 12M (#7) | 3.99% top | Medium | Graduated, local taxes exist |
| **Georgia** | 11M (#8) | 5.49% top | Medium | Moving to flat rate |
| **North Carolina** | 10M (#9) | 4.75% flat | Low | Recently went flat |
| **Michigan** | 10M (#10) | 4.25% flat | Low | Flat rate |

### Tier 3: Simple States (Good for Testing)

| State | Population | Tax Burden | Complexity | Notes |
|-------|------------|------------|------------|-------|
| **Utah** | 3.4M | 4.65% flat | Very Low | Single rate, clean structure |
| **Colorado** | 5.8M | 4.4% flat | Very Low | Flat rate, simple |
| **Indiana** | 6.8M | 3.05% flat | Very Low | Flat rate |
| **Massachusetts** | 7M (#15) | 5% flat* | Low | *Millionaire surtax adds 4% |

### Tier 4: No Income Tax States

| State | Population | Notes |
|-------|------------|-------|
| Texas | 30M | No income tax |
| Florida | 22M | No income tax |
| Washington | 7.7M | No income tax (has capital gains tax) |
| Tennessee | 7M | No income tax |
| Nevada | 3.2M | No income tax |
| South Dakota | 0.9M | No income tax |
| Wyoming | 0.6M | No income tax |
| Alaska | 0.7M | No income tax |
| New Hampshire | 1.4M | Interest/dividends only (phasing out) |

---

## Recommended Implementation Order

### Phase 1: Framework Validation (Simple States)
1. **Utah** - Flat 4.65%, minimal complexity, validates flat-rate pattern
2. **Colorado** - Flat 4.4%, similar structure
3. **Pennsylvania** - Flat 3.07%, large population justifies effort

### Phase 2: High-Value States
4. **New York** - 19M population, high tax burden, complex but important
5. **Massachusetts** - 7M population, mostly flat + surtax edge case

### Phase 3: Population Coverage
6. **Illinois** - 12M, flat rate
7. **Ohio** - 12M, graduated brackets
8. **Georgia** - 11M, transitioning system
9. **North Carolina** - 10M, flat rate
10. **Michigan** - 10M, flat rate
11. **New Jersey** - 9M, high tax

### Phase 4: Remaining States
- Oregon, Virginia, Arizona, etc.

---

## Complexity Indicators

**Very Low Complexity:**
- Flat rate (single percentage of federal AGI or state AGI)
- Few or no state-specific credits
- No local income taxes
- Examples: UT, CO, PA, IN, MI, NC

**Low Complexity:**
- Flat rate with one or two special cases
- Limited credits
- Examples: MA (millionaire surtax), IL

**Medium Complexity:**
- Graduated brackets
- Some state-specific credits
- Examples: OR, GA, OH, NJ

**High Complexity:**
- Graduated brackets with many rates
- Multiple state-specific credits
- Local tax integration
- Significant federal/state differences
- Examples: CA, NY, HI

---

## Implementation Stats

| Complexity | Forms per State | Effort |
|------------|-----------------|--------|
| Very Low (flat) | 2-3 | 1-2 hours |
| Low | 3-4 | 2-4 hours |
| Medium | 4-6 | 4-8 hours |
| High | 6-10 | 8-16 hours |

---

## Current Status

| State | Status | Forms |
|-------|--------|-------|
| California | **DONE** | CA 540, Schedule CA, FTB 3514, FTB 3506 |
| All others | TODO | - |

---

## Score Matrix

Composite score = (Population Rank / 50 × 0.4) + (Tax Burden Rank / 50 × 0.3) + (1 - Complexity × 0.3)

| State | Pop Score | Tax Score | Simple Score | **Total** |
|-------|-----------|-----------|--------------|-----------|
| California | 0.40 | 0.30 | 0.00 | **0.70** ✓ Done |
| New York | 0.37 | 0.28 | 0.00 | **0.65** |
| Utah | 0.15 | 0.12 | 0.30 | **0.57** |
| Pennsylvania | 0.36 | 0.06 | 0.30 | **0.72** |
| Massachusetts | 0.28 | 0.15 | 0.20 | **0.63** |
| Colorado | 0.25 | 0.09 | 0.30 | **0.64** |

**Suggested next: Pennsylvania or Utah** (high simplicity, decent coverage)
