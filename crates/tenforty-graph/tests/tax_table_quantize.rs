use graphlib::autodiff::{adjoints, gradient_sum};
use graphlib::graph::{Bracket, BracketTable, ByStatus, TaxTableQuantizeMode};
use graphlib::solver::{solve, solve_bisection, SolveError};
use graphlib::{FilingStatus, Graph, Node, Op, Runtime};
use std::collections::HashMap;

fn graph(ops: Vec<Op>) -> Graph {
    let output = ops.len() as u32 - 1;
    Graph {
        meta: None,
        imports: vec![],
        nodes: ops
            .into_iter()
            .enumerate()
            .map(|(id, op)| {
                let name = if id == 0 {
                    "x".to_owned()
                } else {
                    format!("node_{id}")
                };
                (
                    id as u32,
                    Node {
                        id: id as u32,
                        op,
                        name: Some(name),
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

fn quantize(arg: u32, step: f64, output_offset: f64, mode: TaxTableQuantizeMode) -> Op {
    Op::TaxTableQuantize {
        arg,
        step,
        output_offset,
        mode,
    }
}

fn band(arg: u32) -> Op {
    quantize(arg, 1.0, 0.0, TaxTableQuantizeMode::Floor)
}

#[test]
fn exact_values_half_up_and_identity_planning_gradient() {
    for (mode, step, offset, values) in [
        (
            TaxTableQuantizeMode::Floor,
            50.0,
            25.0,
            vec![(0.0, 25.0), (49.999, 25.0), (50.0, 75.0), (-0.001, -25.0)],
        ),
        (
            TaxTableQuantizeMode::Round,
            1.0,
            0.0,
            vec![(1.499, 1.0), (1.5, 2.0), (-1.5, -1.0), (-1.501, -2.0)],
        ),
    ] {
        let g = graph(vec![Op::Input, quantize(0, step, offset, mode)]);
        let json = g.to_json().unwrap();
        assert!(json.contains("tax_table_quantize"));
        let g = Graph::from_json(&json).unwrap();
        let mut rt = Runtime::new(&g, FilingStatus::Single);
        for (input, expected) in values {
            rt.set_by_id(0, input);
            assert_eq!(rt.eval_node(1).unwrap(), expected);
            assert_eq!(gradient_sum(&mut rt, 1, &[0]).unwrap(), 1.0);
            assert_eq!(adjoints(&mut rt, 1).unwrap()[&0], 1.0);
            assert_eq!(rt.eval_node(1).unwrap(), expected);
        }
        assert!(graphlib::viz::to_dot(&g).contains("TaxTableQuantize"));
    }
}

#[test]
fn invalid_parameters_fail_at_json_and_direct_runtime_boundaries() {
    for (step, offset) in [
        (0.0, 0.0),
        (-1.0, 0.0),
        (0.01, 0.0),
        (1e-300, 0.0),
        (f64::NAN, 0.0),
        (f64::INFINITY, 0.0),
        (1.0, f64::INFINITY),
    ] {
        let g = graph(vec![
            Op::Input,
            quantize(0, step, offset, TaxTableQuantizeMode::Floor),
        ]);
        assert!(g.topological_order().is_err());
        assert!(g
            .reachable_topological_order(&[1], FilingStatus::Single)
            .is_err());
        let mut rt = Runtime::new(&g, FilingStatus::Single);
        rt.set_by_id(0, 1.0);
        assert!(rt.eval_node(1).is_err());
        if step.is_finite() && offset.is_finite() {
            assert!(Graph::from_json(&g.to_json().unwrap()).is_err());
        }
    }
    let invalid_mode = r#"{"nodes":{"0":{"id":0,"op":{"type":"input"}},"1":{"id":1,"op":{"type":"tax_table_quantize","arg":0,"step":1,"output_offset":0,"mode":"truncate"}}},"outputs":[1]}"#;
    assert!(Graph::from_json(invalid_mode).is_err());
}

#[test]
fn ordinary_floor_keeps_zero_slope_in_mixed_table_output() {
    let g = graph(vec![
        Op::Input,
        band(0),
        Op::Floor { arg: 0 },
        Op::Add { left: 1, right: 2 },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 1.2);
    assert_eq!(gradient_sum(&mut rt, 2, &[0]).unwrap(), 0.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[0]).unwrap(), 1.0);
}

#[test]
fn table_planning_partials_seed_requested_intermediate_nodes() {
    let g = graph(vec![
        Op::Input,
        Op::Literal { value: 2.0 },
        Op::Mul { left: 0, right: 1 },
        band(2),
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 1.2);
    assert_eq!(gradient_sum(&mut rt, 3, &[2]).unwrap(), 1.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[0, 2]).unwrap(), 3.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[3]).unwrap(), 1.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[1]).unwrap(), 1.2);
}

#[test]
fn composed_clamp_and_branch_ties_follow_incoming_direction() {
    let g = graph(vec![
        Op::Input,
        band(0),
        Op::Neg { arg: 1 },
        Op::Clamp {
            arg: 2,
            min: 0.0,
            max: 10.0,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[0]).unwrap(), 0.0);
    rt.set_by_id(0, -10.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[0]).unwrap(), -1.0);

    let g = graph(vec![
        Op::Input,
        band(0),
        Op::Literal { value: 2.0 },
        Op::Mul { left: 1, right: 2 },
        Op::Literal { value: 3.0 },
        Op::Mul { left: 1, right: 4 },
        Op::IfPositive {
            cond: 1,
            then: 3,
            otherwise: 5,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(gradient_sum(&mut rt, 6, &[0]).unwrap(), 2.0);
    assert_eq!(rt.eval_node(6).unwrap(), 0.0);
}

#[test]
fn grouped_inputs_and_cancellation_compose_without_numeric_probes() {
    let g = graph(vec![
        Op::Input,
        Op::Input,
        Op::Add { left: 0, right: 1 },
        band(2),
        Op::Sub { left: 3, right: 3 },
        Op::IfPositive {
            cond: 4,
            then: 3,
            otherwise: 0,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.2);
    rt.set_by_id(1, 0.3);
    assert_eq!(gradient_sum(&mut rt, 3, &[0, 1]).unwrap(), 2.0);
    assert_eq!(gradient_sum(&mut rt, 4, &[0, 1]).unwrap(), 0.0);
    assert_eq!(gradient_sum(&mut rt, 5, &[0, 1]).unwrap(), 1.0);
}

#[test]
fn mixed_ordinary_step_uses_one_rule_across_equivalent_topologies() {
    let g = graph(vec![
        Op::Input,
        band(0),
        Op::Literal { value: 50.0 },
        Op::Literal { value: 0.0 },
        Op::IfPositive {
            cond: 0,
            then: 2,
            otherwise: 3,
        },
        Op::Add { left: 1, right: 4 },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    let ordinary = gradient_sum(&mut rt, 4, &[0]).unwrap();
    assert_eq!(ordinary, 500_000.0);
    assert_eq!(gradient_sum(&mut rt, 5, &[0]).unwrap(), 1.0);
    let rewritten = graph(vec![
        Op::Input,
        band(0),
        Op::Literal { value: 50.0 },
        Op::Add { left: 1, right: 2 },
        Op::IfPositive {
            cond: 0,
            then: 3,
            otherwise: 1,
        },
    ]);
    let mut rt = Runtime::new(&rewritten, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(gradient_sum(&mut rt, 4, &[0]).unwrap(), 1.0);
}

#[test]
fn quantized_conditional_plateau_keeps_legal_values_and_branch() {
    let mut g = graph(vec![
        Op::Input,
        quantize(0, 50.0, 0.0, TaxTableQuantizeMode::Floor),
        Op::Literal { value: 50.0 },
        Op::Sub { left: 1, right: 2 },
        Op::Literal { value: 10.0 },
        Op::Mul { left: 0, right: 4 },
        Op::Literal { value: 0.1 },
        Op::Mul { left: 0, right: 6 },
        Op::IfPositive {
            cond: 3,
            then: 5,
            otherwise: 7,
        },
        Op::BracketTax {
            table: "rates".into(),
            income: 8,
        },
    ]);
    g.tables.insert(
        "rates".into(),
        BracketTable {
            brackets: ByStatus::uniform(vec![
                Bracket {
                    threshold: 100.0,
                    rate: 0.1,
                },
                Bracket {
                    threshold: f64::INFINITY,
                    rate: 0.5,
                },
            ]),
        },
    );
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    for x in [50.0, 60.0, 75.0, 99.0] {
        rt.set_by_id(0, x);
        let value = rt.eval_node(9).unwrap();
        assert!((value - x * 0.01).abs() < 1e-12);
        assert!((gradient_sum(&mut rt, 9, &[0]).unwrap() - 0.01).abs() < 1e-12);
        assert_eq!(rt.eval_node(9).unwrap(), value);
    }
}

#[test]
fn ordinary_conditional_right_limit_selects_downstream_rate_without_changing_value() {
    let mut g = graph(vec![
        Op::Input,
        band(0),
        Op::Literal { value: 200.0 },
        Op::Add { left: 1, right: 2 },
        Op::IfPositive {
            cond: 0,
            then: 3,
            otherwise: 1,
        },
        Op::BracketTax {
            table: "rates".into(),
            income: 4,
        },
    ]);
    g.tables.insert(
        "rates".into(),
        BracketTable {
            brackets: ByStatus::uniform(vec![
                Bracket {
                    threshold: 100.0,
                    rate: 0.1,
                },
                Bracket {
                    threshold: f64::INFINITY,
                    rate: 0.5,
                },
            ]),
        },
    );
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(rt.eval_node(5).unwrap(), 0.0);
    assert_eq!(gradient_sum(&mut rt, 5, &[0]).unwrap(), 0.5);
    assert_eq!(rt.eval_node(4).unwrap(), 0.0);
}

#[test]
fn point_switch_uses_one_right_limit_through_bracket_tax_and_max() {
    let mut g = graph(vec![
        Op::Input,
        Op::Literal { value: 100.0 },
        Op::Sub { left: 0, right: 1 },
        Op::Literal { value: 3.0 },
        Op::Mul { left: 0, right: 3 },
        Op::Literal { value: 1000.0 },
        Op::Add { left: 4, right: 5 },
        Op::IfPositive {
            cond: 2,
            then: 6,
            otherwise: 0,
        },
        band(0),
        Op::Literal { value: 0.0 },
        Op::Mul { left: 8, right: 9 },
        Op::Add { left: 7, right: 10 },
        Op::BracketTax {
            table: "rates".into(),
            income: 11,
        },
        Op::Literal { value: 500.0 },
        Op::Max {
            left: 11,
            right: 13,
        },
    ]);
    g.outputs = vec![12, 14];
    g.tables.insert(
        "rates".into(),
        BracketTable {
            brackets: ByStatus::uniform(vec![
                Bracket {
                    threshold: 500.0,
                    rate: 0.1,
                },
                Bracket {
                    threshold: f64::INFINITY,
                    rate: 0.5,
                },
            ]),
        },
    );
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 100.0);
    assert_eq!(rt.eval_node(12).unwrap(), 10.0);
    assert_eq!(rt.eval_node(14).unwrap(), 500.0);
    assert_eq!(gradient_sum(&mut rt, 12, &[0]).unwrap(), 1.5);
    assert_eq!(gradient_sum(&mut rt, 14, &[0]).unwrap(), 3.0);
    assert_eq!(rt.eval_node(12).unwrap(), 10.0);
    assert_eq!(rt.eval_node(14).unwrap(), 500.0);
}

#[test]
fn continuous_whole_band_ties_agree_across_equivalent_forms() {
    let mut g = graph(vec![
        Op::Input,
        quantize(0, 50.0, 0.0, TaxTableQuantizeMode::Floor),
        Op::Literal { value: 50.0 },
        Op::Sub { left: 1, right: 2 },
        Op::Literal { value: 0.0 },
        Op::Max { left: 3, right: 4 },
        Op::Max { left: 4, right: 3 },
        Op::Clamp {
            arg: 3,
            min: 0.0,
            max: 1e9,
        },
        Op::IfPositive {
            cond: 3,
            then: 3,
            otherwise: 4,
        },
        Op::Neg { arg: 3 },
        Op::Min { left: 9, right: 4 },
        Op::IfPositive {
            cond: 3,
            then: 9,
            otherwise: 4,
        },
        Op::Neg { arg: 10 },
        Op::BracketTax {
            table: "positive_part".into(),
            income: 3,
        },
        Op::Min { left: 3, right: 4 },
        Op::IfPositive {
            cond: 9,
            then: 3,
            otherwise: 4,
        },
    ]);
    g.tables.insert(
        "positive_part".into(),
        BracketTable {
            brackets: ByStatus::uniform(vec![
                Bracket {
                    threshold: 0.0,
                    rate: 0.0,
                },
                Bracket {
                    threshold: f64::INFINITY,
                    rate: 1.0,
                },
            ]),
        },
    );
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    for x in [50.0, 60.0, 75.0, 99.0] {
        rt.set_by_id(0, x);
        for output in [5, 6, 7, 8, 12, 13] {
            assert_eq!(rt.eval_node(output).unwrap(), 0.0);
            assert_eq!(gradient_sum(&mut rt, output, &[0]).unwrap(), 1.0);
            assert_eq!(rt.eval_node(output).unwrap(), 0.0);
        }
        for output in [10, 11] {
            assert_eq!(rt.eval_node(output).unwrap(), 0.0);
            assert_eq!(gradient_sum(&mut rt, output, &[0]).unwrap(), -1.0);
        }
        for output in [14, 15] {
            assert_eq!(rt.eval_node(output).unwrap(), 0.0);
            assert_eq!(gradient_sum(&mut rt, output, &[0]).unwrap(), 0.0);
            assert_eq!(rt.eval_node(output).unwrap(), 0.0);
        }
    }
}

#[test]
fn bisection_does_not_confuse_income_width_with_output_residual() {
    let g = graph(vec![
        Op::Input,
        Op::Literal { value: 10.0 },
        Op::Mul { left: 0, right: 1 },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    let target = 10.0 / 3.0;
    let x = solve_bisection(&mut rt, 2, target, 0, 0.0, 1.0, 1e-6, 100).unwrap();
    assert!((10.0 * x - target).abs() < 1e-6);
}

#[test]
fn downward_quantizer_edges_select_the_right_limit_without_changing_values() {
    for step in [1.0, 7.0, 25.0, 50.0] {
        for mode in [TaxTableQuantizeMode::Floor, TaxTableQuantizeMode::Round] {
            for offset in [0.0, 2.5] {
                let source = match mode {
                    TaxTableQuantizeMode::Floor => 2.0 * step,
                    TaxTableQuantizeMode::Round => 1.5 * step,
                };
                let g = graph(vec![
                    Op::Input,
                    Op::Literal { value: source },
                    Op::Sub { left: 1, right: 0 },
                    quantize(2, step, offset, mode),
                    Op::Literal {
                        value: step + offset,
                    },
                    Op::Sub { left: 4, right: 3 },
                    Op::Literal { value: 10.0 },
                    Op::Mul { left: 0, right: 6 },
                    Op::Literal { value: 0.1 },
                    Op::Mul { left: 0, right: 8 },
                    Op::IfPositive {
                        cond: 5,
                        then: 7,
                        otherwise: 9,
                    },
                ]);
                let mut rt = Runtime::new(&g, FilingStatus::Single);
                rt.set_by_id(0, step);
                let legal = rt.eval_node(10).unwrap();
                assert_eq!(legal, 0.1 * step);
                assert_eq!(rt.eval_node(3).unwrap(), step + offset);
                assert_eq!(gradient_sum(&mut rt, 10, &[0]).unwrap(), 10.0);
                assert_eq!(rt.eval_node(10).unwrap(), legal);
                assert_eq!(rt.eval_node(3).unwrap(), step + offset);
                let h = step * 1e-5;
                rt.set_by_id(0, step + h);
                let right = rt.eval_node(10).unwrap();
                rt.set_by_id(0, step + 2.0 * h);
                let farther_right = rt.eval_node(10).unwrap();
                assert!(((farther_right - right) / h - 10.0).abs() < 1e-6);
            }
        }
    }
}

#[test]
fn directional_continuity_requires_exact_floating_point_equality() {
    let g = graph(vec![
        Op::Input,
        quantize(0, 50.0, 0.0, TaxTableQuantizeMode::Floor),
        Op::Literal { value: 50.0 },
        Op::Sub { left: 1, right: 2 },
        Op::Literal { value: 0.1 },
        Op::Literal { value: 0.2 },
        Op::Add { left: 4, right: 5 },
        Op::Literal { value: 0.3 },
        Op::Sub { left: 6, right: 7 },
        Op::Add { left: 3, right: 8 },
        Op::Literal { value: 0.0 },
        Op::IfPositive {
            cond: 3,
            then: 9,
            otherwise: 10,
        },
        Op::IfPositive {
            cond: 3,
            then: 3,
            otherwise: 10,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 75.0);
    assert!(rt.eval_node(8).unwrap() > 0.0);
    assert_eq!(gradient_sum(&mut rt, 11, &[0]).unwrap(), 0.0);
    assert_eq!(gradient_sum(&mut rt, 12, &[0]).unwrap(), 1.0);
    assert_eq!(rt.eval_node(11).unwrap(), 0.0);
    assert_eq!(rt.eval_node(12).unwrap(), 0.0);
}

#[test]
fn a_right_limit_error_does_not_change_the_valid_reported_value() {
    let g = graph(vec![
        Op::Input,
        Op::Literal { value: 1.0 },
        Op::Sub { left: 1, right: 0 },
        Op::Floor { arg: 2 },
        Op::Div { left: 1, right: 3 },
        quantize(0, 50.0, 0.0, TaxTableQuantizeMode::Floor),
        Op::Literal { value: 0.0 },
        Op::Mul { left: 5, right: 6 },
        Op::Add { left: 4, right: 7 },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(rt.eval_node(8).unwrap(), 1.0);
    assert!(matches!(
        gradient_sum(&mut rt, 8, &[0]),
        Err(graphlib::eval::EvalError::DivisionByZero(4))
    ));
    assert_eq!(rt.eval_node(8).unwrap(), 1.0);
}

#[test]
fn whole_band_continuity_probe_does_not_raise_an_inactive_branch_error() {
    let g = graph(vec![
        Op::Input,
        quantize(0, 50.0, 0.0, TaxTableQuantizeMode::Floor),
        Op::Literal { value: 50.0 },
        Op::Sub { left: 1, right: 2 },
        Op::Literal { value: 1.0 },
        Op::Literal { value: 0.0 },
        Op::Div { left: 4, right: 5 },
        Op::IfPositive {
            cond: 3,
            then: 6,
            otherwise: 0,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 75.0);
    assert_eq!(rt.eval_node(7).unwrap(), 75.0);
    assert_eq!(gradient_sum(&mut rt, 7, &[0]).unwrap(), 1.0);
    assert_eq!(rt.eval_node(7).unwrap(), 75.0);
    rt.set_by_id(0, 100.0);
    assert!(rt.eval_node(7).is_err());
    assert!(gradient_sum(&mut rt, 7, &[0]).is_err());
}

#[test]
fn ordinary_floor_right_limit_matches_its_local_conditional_rewrite() {
    let g = graph(vec![
        Op::Input,
        Op::Neg { arg: 0 },
        Op::Floor { arg: 1 },
        Op::Mul { left: 2, right: 0 },
        band(0),
        Op::Literal { value: 0.0 },
        Op::Mul { left: 4, right: 5 },
        Op::Add { left: 3, right: 6 },
        Op::IfPositive {
            cond: 0,
            then: 1,
            otherwise: 5,
        },
        Op::Add { left: 8, right: 6 },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    for output in [7, 9] {
        assert_eq!(rt.eval_node(output).unwrap(), 0.0);
        assert_eq!(gradient_sum(&mut rt, output, &[0]).unwrap(), -1.0);
        assert_eq!(rt.eval_node(output).unwrap(), 0.0);
    }
}

#[test]
fn directional_pass_keeps_inactive_branch_lazy() {
    let g = graph(vec![
        Op::Input,
        band(0),
        Op::Literal { value: 1.0 },
        Op::Literal { value: 0.0 },
        Op::Div { left: 1, right: 3 },
        Op::IfPositive {
            cond: 2,
            then: 1,
            otherwise: 4,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.25);
    assert_eq!(gradient_sum(&mut rt, 5, &[0]).unwrap(), 1.0);
    assert_eq!(rt.eval_node(5).unwrap(), 0.0);
}

#[test]
fn bracket_and_phaseout_edges_use_incoming_table_direction() {
    let mut g = graph(vec![
        Op::Input,
        quantize(0, 50.0, 40.0, TaxTableQuantizeMode::Floor),
        Op::BracketTax {
            table: "rates".into(),
            income: 1,
        },
    ]);
    g.tables.insert(
        "rates".into(),
        BracketTable {
            brackets: ByStatus::uniform(vec![
                Bracket {
                    threshold: 40.0,
                    rate: 0.1,
                },
                Bracket {
                    threshold: f64::INFINITY,
                    rate: 0.2,
                },
            ]),
        },
    );
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(gradient_sum(&mut rt, 2, &[0]).unwrap(), 0.2);
    let g = graph(vec![
        Op::Input,
        band(0),
        Op::PhaseOut {
            base: 10.0,
            threshold: ByStatus::uniform(0.0),
            rate: 1.0,
            agi: 1,
        },
    ]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 0.0);
    assert_eq!(gradient_sum(&mut rt, 2, &[0]).unwrap(), -1.0);
    rt.set_by_id(0, 10.0);
    assert_eq!(gradient_sum(&mut rt, 2, &[0]).unwrap(), 0.0);
}

#[test]
fn midpoint_rate_and_final_rounding_compose() {
    let mut g = graph(vec![
        Op::Input,
        quantize(0, 50.0, 25.0, TaxTableQuantizeMode::Floor),
        Op::BracketTax {
            table: "rates".into(),
            income: 1,
        },
        quantize(2, 1.0, 0.0, TaxTableQuantizeMode::Round),
    ]);
    g.tables.insert(
        "rates".into(),
        BracketTable {
            brackets: ByStatus::uniform(vec![
                Bracket {
                    threshold: 40.0,
                    rate: 0.1,
                },
                Bracket {
                    threshold: f64::INFINITY,
                    rate: 0.2,
                },
            ]),
        },
    );
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    rt.set_by_id(0, 49.0);
    assert_eq!(rt.eval_node(3).unwrap(), 3.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[0]).unwrap(), 0.1);
    rt.set_by_id(0, 50.0);
    assert_eq!(rt.eval_node(3).unwrap(), 11.0);
    assert_eq!(gradient_sum(&mut rt, 3, &[0]).unwrap(), 0.2);
}

#[test]
fn unreachable_targets_fail_and_reachable_plateaus_are_deterministic() {
    let g = graph(vec![Op::Input, band(0)]);
    let mut rt = Runtime::new(&g, FilingStatus::Single);
    assert!(matches!(
        solve(&mut rt, 1, 0.5, 0, 0.25),
        Err(SolveError::RepeatedIterate)
    ));
    assert!(matches!(
        solve_bisection(&mut rt, 1, 0.5, 0, 0.0, 2.0, 1e-6, 100),
        Err(SolveError::UnattainableTarget)
    ));
    let first = solve(&mut rt, 1, 2.0, 0, 0.25).unwrap();
    assert_eq!(first, 2.25);
    assert_eq!(rt.eval_node(1).unwrap(), 2.0);
    assert_eq!(
        solve_bisection(&mut rt, 1, 2.0, 0, 0.0, 2.0, 1e-6, 100).unwrap(),
        2.0
    );
    assert_eq!(solve(&mut rt, 1, 2.0, 0, 0.25).unwrap(), first);
    let midpoint = solve_bisection(&mut rt, 1, 2.0, 0, 0.0, 4.0, 1e-6, 100).unwrap();
    assert_eq!(midpoint, 2.0);
    assert_eq!(rt.eval_node(1).unwrap(), 2.0);
}

#[cfg(feature = "jit")]
#[test]
fn scalar_and_simd_jit_match_interpreter() {
    use graphlib::jit::{JitBatchRuntime, JitCompiler, JitRuntime};
    for mode in [TaxTableQuantizeMode::Floor, TaxTableQuantizeMode::Round] {
        let g = graph(vec![Op::Input, quantize(0, 25.0, 12.5, mode)]);
        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&g, FilingStatus::Single).unwrap();
        let mut jit = JitRuntime::new(compiled, &g);
        let batch_compiled = compiler
            .compile_batch(&g, FilingStatus::Single, &g.outputs)
            .unwrap();
        let mut batch = JitBatchRuntime::new(batch_compiled, &g);
        let mut rt = Runtime::new(&g, FilingStatus::Single);
        for pair in [[0.0, 24.999], [25.0, 37.5], [-12.5, -12.501]] {
            batch.set_batch("x", &pair).unwrap();
            let actual = batch.eval_batch("node_1").unwrap();
            for (index, input) in pair.into_iter().enumerate() {
                rt.set_by_id(0, input);
                assert_eq!(actual[index], rt.eval_node(1).unwrap());
                jit.set("x", input).unwrap();
                assert_eq!(jit.eval("node_1").unwrap(), rt.eval_node(1).unwrap());
            }
        }
    }
}
