{-# LANGUAGE OverloadedStrings #-}

module USScheduleEIC_2025 (
    usScheduleEIC_2025,
) where

import Tables2025
import TenForty

usScheduleEIC_2025 :: Either FormError Form
usScheduleEIC_2025 = form "us_schedule_eic" 2025 $ do
    -- Schedule EIC: Earned Income Credit
    -- This is a simplified implementation that handles the core EITC calculation
    -- based on IRS Direct File logic (eitc.xml)

    -- Inputs
    numQC <- keyInput "NUM_QC" "num_qualifying_children" "Number of qualifying children (0-3)"
    earnedIncome <- keyInput "EARNED_INCOME" "earned_income" "Earned income (wages, self-employment)"
    investmentIncome <- keyInput "INVESTMENT_INCOME" "investment_income" "Investment income"

    -- Get AGI for phase-out calculation
    agi <- interior "AGI" "agi" $ importForm "us_1040" "L11"

    -- Phase-out thresholds by status (varies by 0 vs 1+ qualifying children)
    phaseOutThreshold0QC <-
        interior "POT0" "phase_out_threshold_0qc" $
            byStatusE (fmap lit eitcPhaseOutThreshold0QC2025)
    phaseOutThreshold1PlusQC <-
        interior "POT1" "phase_out_threshold_1plus_qc" $
            byStatusE (fmap lit eitcPhaseOutThreshold1PlusQC2025)

    -- Calculate credit for 0 qualifying children
    -- Phase-in: credit grows from $0 at $0 income to max at phase-in-ends
    -- Plateau: credit stays at max until phase-out threshold
    -- Phase-out: credit decreases after threshold
    let zero = dollars 0
        invLimit = lit eitcInvestmentIncomeLimit2025

    -- 0 QC calculation
    phaseInAmt0 <-
        interior "PIA0" "phase_in_amount_0qc" $
            smallerOf earnedIncome (lit eitcPhaseInEnds0QC2025) .*. lit eitcPhaseInRate0QC2025

    excessOver0 <-
        interior "EXC0" "excess_over_threshold_0qc" $
            greaterOf zero (agi .-. phaseOutThreshold0QC)

    phaseOutAmt0 <-
        interior "POA0" "phase_out_amount_0qc" $
            excessOver0 .*. lit eitcPhaseOutRate0QC2025

    credit0QC <-
        interior "C0" "credit_0qc" $
            greaterOf zero (smallerOf (lit eitcMaxCredit0QC2025) phaseInAmt0 .-. phaseOutAmt0)

    -- 1 QC calculation
    phaseInAmt1 <-
        interior "PIA1" "phase_in_amount_1qc" $
            smallerOf earnedIncome (lit eitcPhaseInEnds1QC2025) .*. lit eitcPhaseInRate1QC2025

    excessOver1 <-
        interior "EXC1" "excess_over_threshold_1qc" $
            greaterOf zero (agi .-. phaseOutThreshold1PlusQC)

    phaseOutAmt1 <-
        interior "POA1" "phase_out_amount_1qc" $
            excessOver1 .*. lit eitcPhaseOutRate1QC2025

    credit1QC <-
        interior "C1" "credit_1qc" $
            greaterOf zero (smallerOf (lit eitcMaxCredit1QC2025) phaseInAmt1 .-. phaseOutAmt1)

    -- 2 QC calculation
    phaseInAmt2 <-
        interior "PIA2" "phase_in_amount_2qc" $
            smallerOf earnedIncome (lit eitcPhaseInEnds2PlusQC2025) .*. lit eitcPhaseInRate2QC2025

    phaseOutAmt2 <-
        interior "POA2" "phase_out_amount_2qc" $
            excessOver1 .*. lit eitcPhaseOutRate2PlusQC2025

    credit2QC <-
        interior "C2" "credit_2qc" $
            greaterOf zero (smallerOf (lit eitcMaxCredit2QC2025) phaseInAmt2 .-. phaseOutAmt2)

    -- 3+ QC calculation
    phaseInAmt3 <-
        interior "PIA3" "phase_in_amount_3qc" $
            smallerOf earnedIncome (lit eitcPhaseInEnds2PlusQC2025) .*. lit eitcPhaseInRate3PlusQC2025

    phaseOutAmt3 <-
        interior "POA3" "phase_out_amount_3qc" $
            excessOver1 .*. lit eitcPhaseOutRate2PlusQC2025

    credit3PlusQC <-
        interior "C3" "credit_3plus_qc" $
            greaterOf zero (smallerOf (lit eitcMaxCredit3PlusQC2025) phaseInAmt3 .-. phaseOutAmt3)

    -- Select credit based on number of qualifying children
    -- Using nested ifGte: if numQC >= 3, use 3+; else if >= 2, use 2; etc.
    creditByQC <-
        interior "CREDIT_BY_QC" "credit_by_qc" $
            ifGte numQC (dollars 3) credit3PlusQC $
                ifGte numQC (dollars 2) credit2QC $
                    ifGte numQC (dollars 1) credit1QC credit0QC

    -- Check investment income limit - if over limit, no EITC
    invExcess <-
        interior "INV_EXCESS" "investment_excess" $
            investmentIncome .-. invLimit

    -- Final EITC: zero if investment income exceeds limit, otherwise creditByQC
    _eitc <-
        keyOutput "EITC" "earned_income_credit" "Earned income credit (to Form 1040, line 27)" $
            ifPos invExcess zero creditByQC

    outputs ["EITC", "C0", "C1", "C2", "C3", "CREDIT_BY_QC"]
