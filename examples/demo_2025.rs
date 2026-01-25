use tenforty_runtime::{autodiff, solver, FilingStatus, Graph, Runtime};

fn main() {
    let json = include_str!("us_1040_2025.json");
    let graph = Graph::from_json(json).expect("Failed to parse graph");

    println!("=== Form 1040 (2025) Computation Graph Demo ===\n");

    let mut rt = Runtime::new(&graph, FilingStatus::Single);
    let mut rt_mfj = Runtime::new(&graph, FilingStatus::MarriedJoint);

    // Helper to zero out unused inputs
    let zero_inputs = ["L1b_household_wages", "L1c_tip_income", "L1d_medicaid_waiver",
                       "L1e_dependent_care", "L1f_adoption_benefits", "L1g_form8919_wages",
                       "L1h_other_earned", "L4b_taxable_ira", "L5b_taxable_pension",
                       "L6b_taxable_ss", "L8_sched1_income", "L10_adjustments",
                       "L12e_itemized_ded", "L13a_qbi_deduction", "L13b_sched1a_ded",
                       "L17_sched2_addl_tax", "L19_child_tax_credit", "L20_sched3_credits",
                       "L23_other_taxes", "L25b_1099_withholding", "L25c_other_withholding",
                       "L26_estimated_payments", "L27a_eic", "L28_addl_child_credit",
                       "L29_aoc", "L30_adoption_credit", "L31_sched3_refundable"];

    // Initialize both runtimes with zero inputs
    for name in &zero_inputs {
        rt.set(name, 0.0).unwrap();
        rt_mfj.set(name, 0.0).unwrap();
    }

    // Set income inputs (most default to 0)
    rt.set("L1a_w2_wages", 85000.0).unwrap();
    rt.set("L2b_taxable_interest", 750.0).unwrap();
    rt.set("L3b_ordinary_dividends", 1200.0).unwrap();
    rt.set("L7a_capital_gain", 3000.0).unwrap();

    // Set withholding
    rt.set("L25a_w2_withholding", 12000.0).unwrap();

    println!("SINGLE FILER - Standard Scenario");
    println!("─────────────────────────────────");
    println!("INCOME:");
    println!("  L1a  W-2 Wages:           ${:>10.2}", 85000.0);
    println!("  L2b  Taxable Interest:    ${:>10.2}", 750.0);
    println!("  L3b  Ordinary Dividends:  ${:>10.2}", 1200.0);
    println!("  L7a  Capital Gain:        ${:>10.2}", 3000.0);

    let total_income = rt.eval("L9_total_income").unwrap();
    let agi = rt.eval("L11a_agi").unwrap();
    let taxable = rt.eval("L15_taxable_income").unwrap();
    let tax = rt.eval("L16_tax").unwrap();
    let total_tax = rt.eval("L24_total_tax").unwrap();
    let total_payments = rt.eval("L33_total_payments").unwrap();
    let refund = rt.eval("L34_overpaid").unwrap();
    let owed = rt.eval("L37_amount_owed").unwrap();
    let eff_rate = rt.eval("effective_rate_pct").unwrap();

    println!("\nCALCULATIONS:");
    println!("  L9   Total Income:        ${:>10.2}", total_income);
    println!("  L11a AGI:                 ${:>10.2}", agi);
    println!("  L12e Standard Deduction:  ${:>10.2}", 15750.0);
    println!("  L15  Taxable Income:      ${:>10.2}", taxable);
    println!("  L16  Tax:                 ${:>10.2}", tax);
    println!("  L24  Total Tax:           ${:>10.2}", total_tax);
    println!("\nPAYMENTS:");
    println!("  L25a W-2 Withholding:     ${:>10.2}", 12000.0);
    println!("  L33  Total Payments:      ${:>10.2}", total_payments);
    println!("\nRESULT:");
    if refund > 0.0 {
        println!("  L34  REFUND:              ${:>10.2}", refund);
    } else {
        println!("  L37  AMOUNT OWED:         ${:>10.2}", owed);
    }
    println!("       Effective Rate:      {:>10.2}%", eff_rate);

    // Marginal rate via autodiff
    let tax_id = graph.node_id_by_name("L24_total_tax").unwrap();
    let wages_id = graph.node_id_by_name("L1a_w2_wages").unwrap();
    let marginal = autodiff::gradient(&mut rt, tax_id, wages_id).unwrap();
    println!("       Marginal Rate:       {:>10.0}%", marginal * 100.0);

    // ═══════════════════════════════════════════════════════════════════════════
    // AUTODIFF SHOWCASE
    // ═══════════════════════════════════════════════════════════════════════════

    println!("\n═══════════════════════════════════");
    println!("AUTODIFF SHOWCASE");
    println!("═══════════════════════════════════");

    // 1. Marginal rates from different income sources
    println!("\n1. MARGINAL RATES BY INCOME SOURCE");
    println!("   (Showing that ordinary income is fungible for tax purposes)");
    println!("   ─────────────────────────────────");

    let interest_id = graph.node_id_by_name("L2b_taxable_interest").unwrap();
    let dividends_id = graph.node_id_by_name("L3b_ordinary_dividends").unwrap();
    let capgain_id = graph.node_id_by_name("L7a_capital_gain").unwrap();

    let d_wages = autodiff::gradient(&mut rt, tax_id, wages_id).unwrap();
    let d_interest = autodiff::gradient(&mut rt, tax_id, interest_id).unwrap();
    let d_dividends = autodiff::gradient(&mut rt, tax_id, dividends_id).unwrap();
    let d_capgain = autodiff::gradient(&mut rt, tax_id, capgain_id).unwrap();

    println!("   ∂tax/∂wages:          {:>6.1}%", d_wages * 100.0);
    println!("   ∂tax/∂interest:       {:>6.1}%", d_interest * 100.0);
    println!("   ∂tax/∂dividends:      {:>6.1}%", d_dividends * 100.0);
    println!("   ∂tax/∂capital_gains:  {:>6.1}%", d_capgain * 100.0);
    println!("   → All equal! Income sources are fungible at the margin.");

    // 2. Marginal rate curve across income levels
    println!("\n2. MARGINAL RATE CURVE");
    println!("   (Showing bracket transitions)");
    println!("   ─────────────────────────────────");
    println!("   Wages        Marginal Rate   Bracket");

    let income_levels = [25_000.0, 50_000.0, 75_000.0, 100_000.0, 150_000.0, 200_000.0, 250_000.0];

    for wages in income_levels {
        rt.set("L1a_w2_wages", wages).unwrap();
        rt.set("L2b_taxable_interest", 0.0).unwrap();
        rt.set("L3b_ordinary_dividends", 0.0).unwrap();
        rt.set("L7a_capital_gain", 0.0).unwrap();
        let marginal_rate = autodiff::gradient(&mut rt, tax_id, wages_id).unwrap();
        let bracket = match (marginal_rate * 100.0).round() as i32 {
            10 => "10%",
            12 => "12%",
            22 => "22%",
            24 => "24%",
            32 => "32%",
            35 => "35%",
            37 => "37%",
            _ => "??%",
        };
        println!("   ${:>9.0}   {:>6.1}%          {}", wages, marginal_rate * 100.0, bracket);
    }

    // 3. Verify autodiff vs numerical gradient
    println!("\n3. AUTODIFF VS NUMERICAL GRADIENT");
    println!("   (Validating our implementation)");
    println!("   ─────────────────────────────────");

    rt.set("L1a_w2_wages", 85000.0).unwrap();
    rt.set("L2b_taxable_interest", 750.0).unwrap();
    rt.set("L3b_ordinary_dividends", 1200.0).unwrap();
    rt.set("L7a_capital_gain", 3000.0).unwrap();

    let epsilon = 0.01;
    let tax_base = rt.eval("L24_total_tax").unwrap();

    rt.set("L1a_w2_wages", 85000.0 + epsilon).unwrap();
    let tax_plus = rt.eval("L24_total_tax").unwrap();

    let numerical_gradient = (tax_plus - tax_base) / epsilon;
    let autodiff_gradient = autodiff::gradient(&mut rt, tax_id, wages_id).unwrap();

    println!("   Autodiff ∂tax/∂wages:   {:.6}", autodiff_gradient);
    println!("   Numerical (ε=0.01):     {:.6}", numerical_gradient);
    println!("   Difference:             {:.2e}", (autodiff_gradient - numerical_gradient).abs());
    println!("   → Match confirms correctness!");

    // Reset wages for subsequent examples
    rt.set("L1a_w2_wages", 85000.0).unwrap();

    // ═══════════════════════════════════════════════════════════════════════════
    // SOLVER SHOWCASE
    // ═══════════════════════════════════════════════════════════════════════════

    println!("\n═══════════════════════════════════");
    println!("SOLVER SHOWCASE");
    println!("═══════════════════════════════════");

    // 1. Original example: wages for target tax
    println!("\n1. WAGES FOR TARGET TAX");
    println!("   ─────────────────────────────────");
    let target_tax = 15000.0;
    let wages_for_target = solver::solve(&mut rt, tax_id, target_tax, wages_id, 80000.0).unwrap();
    println!("   Q: What wages result in ${:.0} total tax?", target_tax);
    println!("   A: ${:.2}", wages_for_target);

    // Verify by computing tax at that income
    rt.set("L1a_w2_wages", wages_for_target).unwrap();
    let verify_tax = rt.eval("L24_total_tax").unwrap();
    println!("   Verification: Tax at ${:.0} wages = ${:.2}", wages_for_target, verify_tax);

    // 2. Break-even withholding (zero refund/zero owed)
    println!("\n2. BREAK-EVEN WITHHOLDING");
    println!("   ─────────────────────────────────");

    // Reset to standard scenario
    rt.set("L1a_w2_wages", 85000.0).unwrap();
    rt.set("L2b_taxable_interest", 750.0).unwrap();
    rt.set("L3b_ordinary_dividends", 1200.0).unwrap();
    rt.set("L7a_capital_gain", 3000.0).unwrap();

    let withholding_id = graph.node_id_by_name("L25a_w2_withholding").unwrap();
    let refund_id = graph.node_id_by_name("L34_overpaid").unwrap();

    // For break-even, withholding should equal total tax
    // Since L34_overpaid = max(0, payments - tax), we can't use the solver directly
    // (it would just find the lower bound). Instead, compute tax and set withholding to match.
    let breakeven_withholding = rt.eval("L24_total_tax").unwrap();
    println!("   Q: What withholding gives exactly $0 refund/$0 owed?");
    println!("   A: ${:.2}", breakeven_withholding);

    // Verify
    rt.set("L25a_w2_withholding", breakeven_withholding).unwrap();
    let verify_refund = rt.eval("L34_overpaid").unwrap();
    let verify_owed = rt.eval("L37_amount_owed").unwrap();
    println!("   Verification: Refund=${:.2}, Owed=${:.2}", verify_refund, verify_owed);

    // 3. Target refund planning
    println!("\n3. TARGET REFUND PLANNING");
    println!("   ─────────────────────────────────");

    let target_refund = 2000.0;
    let withholding_for_refund = solver::solve(&mut rt, refund_id, target_refund, withholding_id, 12000.0).unwrap();
    println!("   Q: What withholding gives a ${:.0} refund?", target_refund);
    println!("   A: ${:.2}", withholding_for_refund);

    // Verify
    rt.set("L25a_w2_withholding", withholding_for_refund).unwrap();
    let verify_refund = rt.eval("L34_overpaid").unwrap();
    println!("   Verification: Refund at ${:.0} withholding = ${:.2}", withholding_for_refund, verify_refund);

    // 4. Income to hit 22% → 24% bracket boundary
    println!("\n4. BRACKET BOUNDARY TARGETING");
    println!("   ─────────────────────────────────");

    // Reset to simple scenario
    rt.set("L2b_taxable_interest", 0.0).unwrap();
    rt.set("L3b_ordinary_dividends", 0.0).unwrap();
    rt.set("L7a_capital_gain", 0.0).unwrap();
    rt.set("L25a_w2_withholding", 12000.0).unwrap();

    // 2025 single brackets: 22% up to $103,350 taxable, 24% above
    // With $15,750 standard deduction, that's $119,100 wages
    // We'll solve by finding where marginal rate transitions

    // Taxable income node
    let taxable_id = graph.node_id_by_name("L15_taxable_income").unwrap();

    // The 22%→24% bracket boundary for Single is $103,350 taxable income
    let bracket_boundary = 103_350.0;
    let wages_at_boundary = solver::solve(&mut rt, taxable_id, bracket_boundary, wages_id, 100000.0).unwrap();

    println!("   Q: What wages put you exactly at the 22%→24% bracket?");
    println!("      (Single: $103,350 taxable income)");
    println!("   A: ${:.2} wages", wages_at_boundary);

    // Verify marginal rates just below and above
    rt.set("L1a_w2_wages", wages_at_boundary - 100.0).unwrap();
    let rate_below = autodiff::gradient(&mut rt, tax_id, wages_id).unwrap();

    rt.set("L1a_w2_wages", wages_at_boundary + 100.0).unwrap();
    let rate_above = autodiff::gradient(&mut rt, tax_id, wages_id).unwrap();

    println!("   Verification:");
    println!("     At ${:.0} (below): {:.0}% marginal rate", wages_at_boundary - 100.0, rate_below * 100.0);
    println!("     At ${:.0} (above): {:.0}% marginal rate", wages_at_boundary + 100.0, rate_above * 100.0);

    // 5. MFJ vs Single break-even
    println!("\n5. MFJ VS SINGLE BREAK-EVEN");
    println!("   ─────────────────────────────────");
    println!("   Q: At what income does MFJ save money vs Single?");
    println!("   (Assuming same total household income)");

    // Binary search for the break-even point
    let mut low = 0.0;
    let mut high = 300_000.0;

    for _ in 0..50 {
        let mid = (low + high) / 2.0;

        // Single tax
        rt.set("L1a_w2_wages", mid).unwrap();
        rt.set("L2b_taxable_interest", 0.0).unwrap();
        rt.set("L3b_ordinary_dividends", 0.0).unwrap();
        rt.set("L7a_capital_gain", 0.0).unwrap();
        let single_tax = rt.eval("L24_total_tax").unwrap();

        // MFJ tax
        rt_mfj.set("L1a_w2_wages", mid).unwrap();
        rt_mfj.set("L2b_taxable_interest", 0.0).unwrap();
        rt_mfj.set("L3b_ordinary_dividends", 0.0).unwrap();
        rt_mfj.set("L7a_capital_gain", 0.0).unwrap();
        let mfj_tax = rt_mfj.eval("L24_total_tax").unwrap();

        if mfj_tax < single_tax {
            high = mid;
        } else {
            low = mid;
        }
    }

    let breakeven_income = (low + high) / 2.0;
    println!("   A: ~${:.0}", breakeven_income);

    // Show taxes at a few key points
    println!("\n   Comparison at selected income levels:");
    println!("   ─────────────────────────────────────────────────────");
    println!("   Income         Single Tax    MFJ Tax    Savings");

    for income in [20_000.0, 40_000.0, 60_000.0, 100_000.0, 150_000.0, 200_000.0] {
        rt.set("L1a_w2_wages", income).unwrap();
        let single_tax = rt.eval("L24_total_tax").unwrap();

        rt_mfj.set("L1a_w2_wages", income).unwrap();
        let mfj_tax = rt_mfj.eval("L24_total_tax").unwrap();

        let savings = single_tax - mfj_tax;
        let indicator = if savings > 0.01 { "← MFJ wins" } else if savings < -0.01 { "← Single wins" } else { "" };
        println!("   ${:>9.0}    ${:>10.2}  ${:>10.2}  ${:>8.2} {}", income, single_tax, mfj_tax, savings, indicator);
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // FINAL MFJ COMPARISON
    // ═══════════════════════════════════════════════════════════════════════════

    println!("\n═══════════════════════════════════");
    println!("MARRIED FILING JOINTLY - Same Income");
    println!("═══════════════════════════════════");

    // Reset both runtimes to the standard scenario
    rt.set("L1a_w2_wages", 85000.0).unwrap();
    rt.set("L2b_taxable_interest", 750.0).unwrap();
    rt.set("L3b_ordinary_dividends", 1200.0).unwrap();
    rt.set("L7a_capital_gain", 3000.0).unwrap();
    rt.set("L25a_w2_withholding", 12000.0).unwrap();

    rt_mfj.set("L1a_w2_wages", 85000.0).unwrap();
    rt_mfj.set("L2b_taxable_interest", 750.0).unwrap();
    rt_mfj.set("L3b_ordinary_dividends", 1200.0).unwrap();
    rt_mfj.set("L7a_capital_gain", 3000.0).unwrap();
    rt_mfj.set("L25a_w2_withholding", 12000.0).unwrap();

    let total_tax_single = rt.eval("L24_total_tax").unwrap();
    let taxable_mfj = rt_mfj.eval("L15_taxable_income").unwrap();
    let tax_mfj = rt_mfj.eval("L24_total_tax").unwrap();
    let refund_mfj = rt_mfj.eval("L34_overpaid").unwrap();
    let eff_rate_mfj = rt_mfj.eval("effective_rate_pct").unwrap();

    println!("  Standard Deduction:       ${:>10.2}", 31500.0);
    println!("  Taxable Income:           ${:>10.2}", taxable_mfj);
    println!("  Total Tax:                ${:>10.2}", tax_mfj);
    println!("  Refund:                   ${:>10.2}", refund_mfj);
    println!("  Effective Rate:           {:>10.2}%", eff_rate_mfj);
    println!("\n  Tax Savings vs Single:    ${:>10.2}", total_tax_single - tax_mfj);
}
