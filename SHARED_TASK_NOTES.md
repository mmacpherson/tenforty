# Iteration Notes

## Current Status
Completed: All states (NM, NE, WV, HI, ME) state income tax support for 2024 and 2025

## TASK STATUS: COMPLETE

Summary of completed work:
- Added Maine (ME) state income tax support for 2024 and 2025
- All 5 states (NM, NE, WV, HI, ME) now have full graph backend support

## Maine State Support (Iteration 5)
- Created Haskell form specs: TablesME2024.hs, TablesME2025.hs, MEME1040_2024.hs, MEME1040_2025.hs
- Registered in build system (tenforty-spec.cabal, app/Main.hs)
- Compiled Haskell specs to JSON and synced to Python package
- Added Python integration (OTSState.ME, STATE_TO_FORM, StateGraphConfig)
- Added comprehensive test coverage (5 silver standard, monotonicity, 3 range tests)
- All verification commands pass

### Technical Details - Maine
- AGI-import state with 3 progressive brackets (5.8%, 6.75%, 7.15%)
- 2024: brackets $26,050/$61,600, personal exemption $5,000, std deduction = federal
- 2025: brackets $26,800/$63,450 (COLA 1.274/1.269), personal exemption $5,150 (COLA 1.25), std deduction = federal
- Personal exemptions: accepted as total dollar amount input (num_exemptions cannot map to dollar amounts due to natural_to_node limitation)
- Standard deductions equal federal amounts for both years

### Implementation Notes - Maine
- Form structure: ME Form 1040ME imports federal AGI and applies ME-specific additions/subtractions
- Deductions: larger of itemized or standard (federal amount)
- Like HI and other states, test scenarios do not provide exemptions (defaults to 0) to avoid mapping complexity

## Hawaii State Support (Iteration 4)
- Created Haskell form specs: TablesHI2024.hs, TablesHI2025.hs, HIHIN11_2024.hs, HIHIN11_2025.hs
- Registered in build system (tenforty-spec.cabal, app/Main.hs)
- Compiled Haskell specs to JSON and synced to Python package
- Added Python integration (OTSState.HI, STATE_TO_FORM, StateGraphConfig)
- Added comprehensive test coverage (5 silver standard, monotonicity, 3 range tests)
- All verification commands pass

### Technical Details - Hawaii
- AGI-import state with 12 progressive brackets (1.4%-11%)
- 2024: 12 brackets with standard structure
- 2025: Widened brackets under GAP II (Green Affordability Plan II, Act 46 SLH 2024)
- Personal exemptions: $1,144 per exemption (total dollar amount input)
- Standard deductions: $4,400 (Single/MFS), $8,800 (MFJ), $6,424 (HoH)

### Key Implementation Fix
- State refund subtraction uses keyInput (not importForm from us_schedule_1)
- us_schedule_1:L1 is an input node, not an output, so cannot be imported
- Most states accept state refund as direct input
