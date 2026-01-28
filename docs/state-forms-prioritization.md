# State Tax Forms Prioritization

This document outlines the prioritization strategy for implementing US state tax return support in TenForty.

## Prioritization Framework

We evaluate states based on three primary axes to determine the implementation order, aiming to maximize the utility and impact of the software:

1.  **Population Size**: Indicates the number of citizens who can benefit from the software. (Weight: High)
2.  **Tax Burden**: In states with higher tax burdens, the utility of accurate, transparent tax software is often greater for the individual. (Weight: Medium)
3.  **Complexity**: Lower complexity allows for faster implementation and easier validation of the core graph engine. (Weight: Medium/Variable)

**Internal Priority Logic:**
We generally prioritize states with **High Population** (maximizing reach), **High Tax Burden** (maximizing individual utility), and **Low Complexity** (enabling rapid contribution and validation).

---

## Priority Tier List

### Tier 1: Strategic Targets (High Impact)
*States where implementation will have the broadest immediate benefit.*

| Priority | State | Population (2024 Est.) | Tax Burden | Complexity | Rationale |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **1** | **California** | ~39.4M | High (13.3% top) | High | **Implemented.** Largest population; establishes high-complexity benchmark. |
| **2** | **New York** | ~19.9M | High (10.9% top) | High | Large population with complex tax needs. |
| **3** | **Pennsylvania** | ~13.1M | Low (3.07% flat) | Very Low | High population; flat tax structure allows for rapid support. |
| **4** | **Illinois** | ~12.7M | Med (4.95% flat) | Low | Significant population; straightforward flat tax structure. |
| **5** | **North Carolina** | ~11.0M | Med (4.75% flat) | Low | Large and growing population; flat tax structure. |
| **6** | **Michigan** | ~10.1M | Med (4.25% flat) | Low | Major population center; flat tax structure. |
| **7** | **New Jersey** | ~9.5M | High (10.75% top)| Medium | High tax burden and significant population. |

### Tier 2: Broad Reach (High Population)
*States with significant populations or moderate complexity.*

| Priority | State | Population (2024 Est.) | Tax Burden | Complexity | Rationale |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **8** | **Ohio** | ~11.9M | Med (3.5% top) | Medium | Large population; graduated brackets. |
| **9** | **Georgia** | ~11.2M | Med (5.39% flat) | Low | Large population; recently transitioned to flat tax. |
| **10** | **Virginia** | ~8.8M | Med (5.75% top) | Medium | Solid population base. |
| **11** | **Arizona** | ~7.6M | Low (2.5% flat) | Low | Flat tax (new); growing population. |
| **12** | **Massachusetts** | ~7.1M | High (5% + surtax)| Low/Med | High tax awareness; flat rate base with surtax complexity. |
| **13** | **Maryland** | ~6.3M | High (5.75% top) | Medium | Significant tax burden; proximity to DC. |
| **14** | **Missouri** | ~6.2M | Med (4.8% top) | Medium | Moderate population and complexity. |
| **15** | **Wisconsin** | ~6.0M | Med (7.65% top) | Medium | Graduated brackets; significant population. |
| **16** | **Minnesota** | ~5.8M | High (9.85% top) | High | High tax burden; graduated rates. |

### Tier 3: Efficiency & Validation (Flat/Simple Tax)
*States that are excellent for validating the engine due to simpler tax structures (Flat Tax).*

| Priority | State | Population (2024 Est.) | Tax Burden | Complexity | Rationale |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **17** | **Colorado** | ~6.0M | Med (4.4% flat) | Very Low | Simple structure; good test case. |
| **18** | **Indiana** | ~6.9M | Low (3.0% flat) | Very Low | Simple flat tax. |
| **19** | **Kentucky** | ~4.6M | Low (4.0% flat) | Very Low | Flat tax structure. |
| **20** | **Louisiana** | ~4.6M | Low (3.0% flat) | Very Low | Recently flat. |
| **21** | **Utah** | ~3.5M | Med (4.55% flat) | Very Low | Excellent for "Flat Tax" pattern validation. |
| **22** | **Iowa** | ~3.2M | Med (3.8% flat) | Very Low | Recently flat. |
| **23** | **Mississippi** | ~2.9M | Med (4.7% flat) | Low | Flat tax on income over $10k. |
| **24** | **Idaho** | ~2.0M | Med (5.7% flat) | Very Low | Flat tax. |

### Tier 4: Remaining Income Tax States (Population Order)
*States with graduated taxes or lower populations, to be implemented as community contributions allow.*

| Priority | State | Population (2024 Est.) | Tax Burden | Complexity |
| :--- | :--- | :--- | :--- | :--- |
| **25** | **South Carolina** | ~5.5M | Med (6.3% top) | Medium |
| **26** | **Alabama** | ~5.2M | Med (5.0% top) | Medium |
| **27** | **Oregon** | ~4.3M | High (9.9% top) | High |
| **28** | **Oklahoma** | ~4.1M | Low (4.75% top) | Medium |
| **29** | **Connecticut** | ~3.7M | High (6.99% top)| High |
| **30** | **Arkansas** | ~3.1M | Low (4.4% top) | Medium |
| **31** | **Kansas** | ~3.0M | Med (5.7% top) | Medium |
| **32** | **New Mexico** | ~2.1M | Low (5.9% top) | Medium |
| **33** | **Nebraska** | ~2.0M | Med (5.84% top) | Medium |
| **34** | **West Virginia** | ~1.8M | Low (5.12% top) | Medium |
| **35** | **Hawaii** | ~1.4M | High (11% top) | Medium |
| **36** | **Maine** | ~1.4M | High (7.15% top)| Medium |
| **37** | **Montana** | ~1.1M | Med (5.9% top) | Medium |
| **38** | **Rhode Island** | ~1.1M | Med (5.99% top) | Medium |
| **39** | **Delaware** | ~1.1M | Med (6.6% top) | Medium |
| **40** | **North Dakota** | ~0.8M | Very Low (2.5%) | Low |
| **41** | **District of Columbia**| ~0.7M | High (10.75% top)| High |
| **42** | **Vermont** | ~0.6M | High (8.75% top) | High |

---

## No Income Tax States (Separate Case)

These states do not have a general state income tax on wages. Support for these is minimal (often just a "stub" or confirmation of no filing requirement) but necessary for complete national coverage.

| State | Population (2024 Est.) | Notes |
| :--- | :--- | :--- |
| **Texas** | ~31.3M | No state income tax. |
| **Florida** | ~23.4M | No state income tax. |
| **Washington** | ~8.0M | No income tax on wages (Capital Gains tax exists). |
| **Tennessee** | ~7.2M | No state income tax. |
| **Nevada** | ~3.3M | No state income tax. |
| **New Hampshire** | ~1.4M | Tax on interest/dividends only (phasing out). |
| **South Dakota** | ~0.9M | No state income tax. |
| **Alaska** | ~0.7M | No state income tax. |
| **Wyoming** | ~0.6M | No state income tax. |

---

## Complexity Definitions

*   **Very Low**: Flat rate, few credits, minimal adjustments.
*   **Low**: Flat rate or simple brackets, standard deductions, standard credits.
*   **Medium**: Graduated brackets, state-specific additions/subtractions, local tax interplay.
*   **High**: Complex brackets, recapture rules, alternative minimum taxes, extensive state-specific credits, non-resident complexity (e.g., NY/NJ interaction).
