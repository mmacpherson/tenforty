use tenforty_runtime::{autodiff, solver, viz, FilingStatus, Graph, Runtime};

fn main() {
    let json = include_str!("us_1040_simple.json");
    let graph = Graph::from_json(json).expect("Failed to parse graph");

    println!("=== Tax Computation Graph Demo ===\n");

    let mut runtime = Runtime::new(&graph, FilingStatus::Single);

    runtime.set("wages", 75000.0).unwrap();
    runtime.set("interest", 500.0).unwrap();
    runtime.set("dividends", 1000.0).unwrap();

    let tax = runtime.eval("federal_tax").unwrap();
    let effective_rate = runtime.eval("effective_rate_pct").unwrap();
    let taxable = runtime.eval("taxable_income").unwrap();

    println!("Single filer:");
    println!("  Wages: $75,000");
    println!("  Interest: $500");
    println!("  Dividends: $1,000");
    println!("  Total Income: $76,500");
    println!("  Taxable Income: ${:.0}", taxable);
    println!("  Federal Tax: ${:.2}", tax);
    println!("  Effective Rate: {:.2}%", effective_rate);

    let output_id = graph.node_id_by_name("federal_tax").unwrap();
    let input_id = graph.node_id_by_name("wages").unwrap();
    let marginal = autodiff::gradient(&mut runtime, output_id, input_id).unwrap();
    println!("  Marginal Rate: {:.0}%", marginal * 100.0);

    println!("\n--- Inverse Solver ---");
    let target_tax = 10000.0;
    let wages_for_target = solver::solve(&mut runtime, output_id, target_tax, input_id, 50000.0).unwrap();
    println!("  To pay ${:.0} in tax: wages = ${:.0}", target_tax, wages_for_target);

    println!("\n--- Married Filing Jointly ---");
    let mut runtime_mfj = Runtime::new(&graph, FilingStatus::MarriedJoint);
    runtime_mfj.set("wages", 150000.0).unwrap();
    runtime_mfj.set("interest", 1000.0).unwrap();
    runtime_mfj.set("dividends", 2000.0).unwrap();

    let tax_mfj = runtime_mfj.eval("federal_tax").unwrap();
    let rate_mfj = runtime_mfj.eval("effective_rate_pct").unwrap();
    println!("  Wages: $150,000");
    println!("  Federal Tax: ${:.2}", tax_mfj);
    println!("  Effective Rate: {:.2}%", rate_mfj);

    println!("\n--- Graph Visualization ---");
    let dot = viz::to_dot(&graph);
    println!("DOT output (first 500 chars):");
    println!("{}", &dot[..dot.len().min(500)]);
    println!("...");
}
