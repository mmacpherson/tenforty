//! Newton's safeguarded bisection fallback on a stall inside a known bracket.

use graphlib::graph::TaxTableQuantizeMode;
use graphlib::solver::{solve, solve_multi_output, SolveError};
use graphlib::{FilingStatus, Graph, Node, Op, Runtime};
use std::collections::HashMap;
use std::path::Path;

fn graph(ops: Vec<Op>) -> Graph {
    let output = ops.len() as u32 - 1;
    Graph {
        meta: None,
        imports: vec![],
        nodes: ops
            .into_iter()
            .enumerate()
            .map(|(id, op)| {
                (
                    id as u32,
                    Node {
                        id: id as u32,
                        op,
                        name: Some(format!("node_{id}")),
                    },
                )
            })
            .collect(),
        tables: HashMap::new(),
        inputs: vec![0],
        outputs: vec![output],
        invariants: vec![],
    }
}

fn resolved_graph(year: u32) -> Graph {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(format!("../../src/tenforty/forms/us_tax_graph_{year}.json"));
    Graph::from_json(&std::fs::read_to_string(path).unwrap()).unwrap()
}

/// Joint filers, $1 and $2 of tax: a $50,000 start overshoots below the
/// standard deduction, where taxable income is clamped at zero and the planning
/// gradient vanishes. The first Newton step left a bracket, so the solver bisects
/// it and lands in the 2024/2025 Tax Table rows $5-15 ($1) and $15-25 ($2).
#[test]
fn joint_small_targets_bisect_the_newton_bracket_into_the_table_row() {
    for year in [2024, 2025] {
        let graph = resolved_graph(year);
        let id = |name: &str| graph.node_id_by_name(name).unwrap();
        let wages = [
            id("us_1040_L1a_wages"),
            id("us_form_8959_L1_medicare_wages"),
        ];
        let total_tax = id("us_1040_L24_total_tax");
        for (target, row) in [(1.0, 5.0..15.0), (2.0, 15.0..25.0)] {
            let mut runtime = Runtime::new(&graph, FilingStatus::MarriedJoint);
            let income = solve_multi_output(&mut runtime, &[total_tax], target, &wages, 50_000.0)
                .unwrap_or_else(|err| panic!("{year} ${target}: {err}"));
            for &wage in &wages {
                runtime.set_by_id(wage, income);
            }
            assert_eq!(runtime.eval_node(total_tax).unwrap(), target);
            let taxable_income = runtime.eval("us_1040_L15_taxable_income").unwrap();
            assert!(
                row.contains(&taxable_income),
                "{year} ${target}: taxable income {taxable_income}"
            );
        }
    }
}

/// An integer staircase cannot reach 2.5: Newton repeats an iterate inside the
/// bracket [2.5, 3], bisection collapses without an exact residual, and the
/// Newton stall is reported unchanged.
#[test]
fn unattainable_plateau_target_inside_a_bracket_still_errors() {
    let graph = graph(vec![
        Op::Input,
        Op::TaxTableQuantize {
            arg: 0,
            step: 1.0,
            output_offset: 0.0,
            mode: TaxTableQuantizeMode::Floor,
        },
    ]);
    let mut runtime = Runtime::new(&graph, FilingStatus::Single);
    assert!(matches!(
        solve(&mut runtime, 1, 2.5, 0, 0.0),
        Err(SolveError::RepeatedIterate)
    ));
}

/// max(x - 10, 0) from x = 0 toward 5: zero gradient with only a below-target
/// iterate, so there is no bracket and the Newton error stands.
#[test]
fn zero_gradient_without_a_bracket_still_errors() {
    let graph = graph(vec![
        Op::Input,
        Op::Literal { value: 10.0 },
        Op::Sub { left: 0, right: 1 },
        Op::Literal { value: 0.0 },
        Op::Max { left: 2, right: 3 },
    ]);
    let mut runtime = Runtime::new(&graph, FilingStatus::Single);
    assert!(matches!(
        solve(&mut runtime, 4, 5.0, 0, 0.0),
        Err(SolveError::ZeroGradient)
    ));
}
