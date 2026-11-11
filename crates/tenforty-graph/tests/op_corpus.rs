use graphlib::autodiff::{adjoints, gradient_sum};
use graphlib::eval::EvalError;
use graphlib::{FilingStatus, Graph, Op, Runtime};
use serde::Deserialize;
use std::collections::{BTreeSet, HashMap};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Corpus {
    schema_version: u32,
    supported_ops: BTreeSet<String>,
    cases: Vec<Case>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Case {
    id: String,
    focus: String,
    evidence: String,
    graph: serde_json::Value,
    status: FilingStatus,
    sets: Vec<Assignment>,
    expected: Expected,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Assignment {
    name: String,
    value: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Expected {
    value: Option<String>,
    error: Option<ExpectedError>,
    #[serde(default)]
    gradients: Vec<Gradient>,
    #[serde(default)]
    uncached_after_value: Vec<u32>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ExpectedError {
    stage: String,
    kind: String,
    node: Option<u32>,
    name: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Gradient {
    inputs: Vec<u32>,
    planning: String,
    reverse: String,
    smooth_true: Option<String>,
    fd_truncation_bound: Option<String>,
}

fn corpus() -> Corpus {
    serde_json::from_str(include_str!("../../../tests/fixtures/op-corpus/v1.json")).unwrap()
}

fn rational_as_f64(value: &str) -> f64 {
    match value.split_once('/') {
        Some((n, d)) => {
            let denominator = d.parse::<f64>().unwrap();
            assert!(denominator > 0.0, "{value}");
            n.parse::<f64>().unwrap() / denominator
        }
        None => value.parse().unwrap(),
    }
}

// Exhaustive: adding a runtime variant requires updating this corpus consumer.
fn op_name(op: &Op) -> &'static str {
    match op {
        Op::Input => "input",
        Op::Import { .. } => "import",
        Op::Literal { .. } => "literal",
        Op::Add { .. } => "add",
        Op::Sub { .. } => "sub",
        Op::Mul { .. } => "mul",
        Op::Div { .. } => "div",
        Op::Neg { .. } => "neg",
        Op::Max { .. } => "max",
        Op::Min { .. } => "min",
        Op::Floor { .. } => "floor",
        Op::TaxTableQuantize { .. } => "tax_table_quantize",
        Op::BracketTax { .. } => "bracket_tax",
        Op::PhaseOut { .. } => "phase_out",
        Op::ByStatus { .. } => "by_status",
        Op::IfPositive { .. } => "if_positive",
    }
}

fn close(actual: f64, expected: f64, tolerance: f64, context: &str) {
    assert!(
        actual.is_finite() && expected.is_finite(),
        "{context}: nonfinite result"
    );
    assert!(
        (actual - expected).abs() <= tolerance,
        "{context}: {actual} != {expected} (error bound {tolerance})"
    );
}

fn arithmetic_bound(expected: f64) -> f64 {
    32.0 * f64::EPSILON * expected.abs().max(1.0)
}

fn assert_eval_error(error: EvalError, expected: &ExpectedError, case: &str) {
    let matches = match (&error, expected.kind.as_str()) {
        (EvalError::DivisionByZero(id), "division_by_zero")
        | (EvalError::NodeNotFound(id), "node_not_found") => Some(*id) == expected.node,
        (EvalError::TableNotFound(name), "table_not_found") => Some(name) == expected.name.as_ref(),
        (EvalError::NonFiniteInput { name, .. }, "nonfinite_input") => {
            Some(name) == expected.name.as_ref()
        }
        (EvalError::CycleDetected(path), "cycle") => !path.is_empty(),
        _ => false,
    };
    assert!(matches, "{case}: unexpected error {error}");
}

fn run_case(case: &Case) {
    let graph = Graph::from_json(&case.graph.to_string());
    if let Some(error) = case.expected.error.as_ref().filter(|e| e.stage == "load") {
        let text = graph.expect_err(&case.id).to_string();
        match error.kind.as_str() {
            "unknown_variant" => assert!(
                text.contains(&format!(
                    "unknown variant `{}`",
                    error.name.as_ref().unwrap()
                )),
                "{}: {text}",
                case.id
            ),
            "invalid_quantizer" => assert!(
                text.contains(&format!(
                    "Invalid TaxTableQuantize parameters at node {}",
                    error.node.unwrap()
                )),
                "{}: {text}",
                case.id
            ),
            _ => panic!("{}: unknown load-error contract", case.id),
        }
        return;
    }
    let graph = graph.unwrap_or_else(|error| panic!("{}: {error}", case.id));
    let output = *graph.outputs.first().unwrap();
    assert_eq!(graph.outputs.len(), 1, "{}", case.id);
    let mut runtime = Runtime::new(&graph, case.status);
    let mut inputs = HashMap::new();
    for assignment in &case.sets {
        let value = assignment.value.parse::<f64>().unwrap();
        let result = runtime.set(&assignment.name, value);
        if let Some(error) = case.expected.error.as_ref().filter(|e| e.stage == "set") {
            assert_eval_error(result.expect_err(&case.id), error, &case.id);
            return;
        }
        result.unwrap_or_else(|error| panic!("{}: {error}", case.id));
        inputs.insert(graph.node_id_by_name(&assignment.name).unwrap(), value);
    }
    let value = runtime.eval_node(output);
    if let Some(error) = &case.expected.error {
        assert_eq!(error.stage, "eval", "{}", case.id);
        assert_eval_error(value.expect_err(&case.id), error, &case.id);
        return;
    }
    let expected = rational_as_f64(case.expected.value.as_ref().unwrap());
    close(
        value.unwrap(),
        expected,
        arithmetic_bound(expected),
        &case.id,
    );
    for id in &case.expected.uncached_after_value {
        assert!(
            !runtime.get_all_values().contains_key(id),
            "{}: inactive node {id} cached",
            case.id
        );
    }
    for g in &case.expected.gradients {
        let reverse = adjoints(&mut runtime, output).unwrap();
        let actual_reverse: f64 = g
            .inputs
            .iter()
            .map(|id| reverse.get(id).copied().unwrap_or(0.0))
            .sum();
        let expected_reverse = rational_as_f64(&g.reverse);
        close(
            actual_reverse,
            expected_reverse,
            arithmetic_bound(expected_reverse),
            &format!("{} reverse", case.id),
        );
        let planning = gradient_sum(&mut runtime, output, &g.inputs).unwrap();
        // Ordinary active kinks currently use a forward quotient with h >= 1e-4.
        // The corpus README derives this absolute roundoff bound independently.
        close(
            planning,
            rational_as_f64(&g.planning),
            128.0 * f64::EPSILON * expected.abs().max(1.0) / 1e-4,
            &format!("{} public/planning", case.id),
        );
        if let Some(derivative) = &g.smooth_true {
            let h = 1.0 / 1024.0;
            let mut values = Vec::new();
            for direction in [-1.0, 1.0] {
                let mut probe = Runtime::new(&graph, case.status);
                for (&id, &value) in &inputs {
                    probe.set_by_id(id, value);
                }
                for &id in &g.inputs {
                    probe.set_by_id(id, inputs.get(&id).copied().unwrap_or(0.0) + direction * h);
                }
                values.push(probe.eval_node(output).unwrap());
            }
            close(
                (values[1] - values[0]) / (2.0 * h),
                rational_as_f64(derivative),
                128.0 * f64::EPSILON * expected.abs().max(1.0) / h
                    + g.fd_truncation_bound
                        .as_deref()
                        .map(rational_as_f64)
                        .unwrap_or(0.0),
                &format!("{} smooth true", case.id),
            );
        }
    }
}

#[test]
fn every_case_conforms_to_the_shared_data() {
    let corpus = corpus();
    assert_eq!(corpus.schema_version, 1);
    let mut ids = BTreeSet::new();
    for case in &corpus.cases {
        assert!(ids.insert(&case.id), "duplicate case {}", case.id);
        assert!(!case.evidence.trim().is_empty(), "{}", case.id);
        assert_ne!(
            case.expected.value.is_some(),
            case.expected.error.is_some(),
            "{}",
            case.id
        );
        run_case(case);
    }
}

#[test]
fn corpus_covers_every_supported_operation_and_status() {
    let corpus = corpus();
    let supported: BTreeSet<_> = [
        "input",
        "import",
        "literal",
        "add",
        "sub",
        "mul",
        "div",
        "neg",
        "max",
        "min",
        "floor",
        "tax_table_quantize",
        "bracket_tax",
        "phase_out",
        "by_status",
        "if_positive",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    assert_eq!(corpus.supported_ops, supported);
    let mut covered = BTreeSet::new();
    let mut statuses = BTreeSet::new();
    for case in &corpus.cases {
        if case.expected.error.is_some() {
            continue;
        }
        let graph = Graph::from_json(&case.graph.to_string()).unwrap();
        let focus = op_name(&graph.nodes[&graph.outputs[0]].op);
        assert_eq!(focus, case.focus, "{}", case.id);
        covered.insert(focus.to_string());
        if focus == "by_status" {
            statuses.insert(format!("{:?}", case.status));
        }
    }
    assert_eq!(covered, supported);
    assert_eq!(statuses.len(), 5);
}
