use graphlib::autodiff::{adjoints, gradient_sum};
use graphlib::eval::EvalError;
use graphlib::{FilingStatus, Graph, Op, Runtime};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Corpus {
    schema_version: u32,
    supported_ops: BTreeSet<String>,
    capabilities: Capabilities,
    coverage: BTreeMap<String, BTreeMap<String, Vec<String>>>,
    status_coverage: BTreeMap<String, BTreeMap<String, BTreeMap<String, Vec<String>>>>,
    cases: Vec<Case>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Capabilities {
    executed: BTreeSet<String>,
    declared_only: BTreeSet<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Case {
    id: String,
    focus: String,
    evidence: String,
    graph_encoding: GraphEncoding,
    graph: serde_json::Value,
    status: FilingStatus,
    sets: Vec<Assignment>,
    expected: Expected,
    migration: Option<Migration>,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum GraphEncoding {
    ExactScalars,
    RawJson,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Migration {
    kind: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Assignment {
    name: String,
    value: Option<Scalar>,
    nonfinite: Option<String>,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct Scalar {
    n: String,
    d: String,
}

impl Scalar {
    fn as_f64(&self) -> f64 {
        let numerator = self.n.strip_prefix('-').unwrap_or(&self.n);
        assert!(
            !numerator.is_empty() && numerator.bytes().all(|c| c.is_ascii_digit()),
            "invalid numerator {}",
            self.n
        );
        assert!(
            !self.d.is_empty()
                && self.d.bytes().all(|c| c.is_ascii_digit())
                && self.d.bytes().any(|c| c != b'0'),
            "invalid positive denominator {}",
            self.d
        );
        let n: f64 = self.n.parse().unwrap();
        let d: f64 = self.d.parse().unwrap();
        assert!(
            n.is_finite() && d.is_finite() && d > 0.0,
            "legacy scalar conversion overflow"
        );
        let result = n / d;
        assert!(result.is_finite());
        result
    }
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
enum Category {
    Semantic,
    Legacy,
    ExactOnly,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Quantity {
    #[serde(default)]
    comparison: Comparison,
    category: Category,
    value: Scalar,
    legacy_f64: Option<Scalar>,
}

#[derive(Default, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Comparison {
    #[default]
    Roundoff,
    ExactBits,
}

impl Quantity {
    fn check(&self, actual: f64, tolerance: f64, context: &str) {
        assert_ne!(
            self.category,
            Category::ExactOnly,
            "{context}: unsupported exact-only query executed"
        );
        let semantic = self.value.as_f64();
        if let Some(legacy) = &self.legacy_f64 {
            let legacy = legacy.as_f64();
            assert_ne!(
                legacy.to_bits(),
                semantic.to_bits(),
                "{context}: migration witness must diverge"
            );
            assert_eq!(actual.to_bits(), legacy.to_bits(), "{context}: historical f64 departure changed; review and remove its migration marker deliberately");
        } else if matches!(self.comparison, Comparison::ExactBits) {
            assert_eq!(
                actual.to_bits(),
                semantic.to_bits(),
                "{context}: exact-bit contract differs"
            );
        } else {
            close(actual, semantic, tolerance, context);
        }
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Expected {
    value: Option<Quantity>,
    error: Option<ExpectedError>,
    #[serde(default)]
    gradients: Vec<Gradient>,
    #[serde(default)]
    uncached_after_value: Vec<u32>,
    cache_category: Option<Category>,
    #[serde(default)]
    directions: Vec<Direction>,
    #[serde(default)]
    queries: Vec<Query>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ExpectedError {
    category: Category,
    stage: String,
    kind: String,
    node: Option<u32>,
    name: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Gradient {
    inputs: Vec<u32>,
    planning: Quantity,
    planning_via: PlanningVia,
    reverse: Quantity,
    smooth_true: Option<Quantity>,
    fd_truncation_bound: Option<Scalar>,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum PlanningVia {
    Symbolic,
    NumericalFallback,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Seed {
    input: u32,
    delta: Scalar,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Direction {
    category: Category,
    seeds: Vec<Seed>,
    r#true: Jet,
    planning: Jet,
    classification: Classification,
    requires: BTreeSet<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum Classification {
    Smooth,
    ContinuousTie,
    Jump,
    Deferred,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Jet {
    limit: Option<Quantity>,
    tangent: Option<Quantity>,
    error: Option<ExpectedError>,
    deferred: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Query {
    query: String,
    requires: String,
    budget: Option<u64>,
    expected: QueryResult,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct QueryResult {
    category: Category,
    outcome: String,
    error_kind: String,
}

fn corpus() -> Corpus {
    serde_json::from_str(include_str!("../../../tests/fixtures/op-corpus/v1.json")).unwrap()
}

fn legacy_graph(value: &serde_json::Value) -> serde_json::Value {
    match value {
        serde_json::Value::Object(map) if map.contains_key("n") || map.contains_key("d") => {
            let scalar: Scalar = serde_json::from_value(value.clone()).unwrap();
            serde_json::json!(scalar.as_f64())
        }
        serde_json::Value::Object(map) => serde_json::Value::Object(
            map.iter()
                .map(|(k, v)| (k.clone(), legacy_graph(v)))
                .collect(),
        ),
        serde_json::Value::Array(values) => {
            serde_json::Value::Array(values.iter().map(legacy_graph).collect())
        }
        value => value.clone(),
    }
}

fn graph_for(case: &Case) -> Result<Graph, serde_json::Error> {
    let graph = match case.graph_encoding {
        GraphEncoding::ExactScalars => legacy_graph(&case.graph),
        GraphEncoding::RawJson => {
            assert!(
                case.expected
                    .error
                    .as_ref()
                    .is_some_and(|e| e.stage == "load"),
                "{}: raw schema payload must test load rejection",
                case.id
            );
            case.graph.clone()
        }
    };
    Graph::from_json(&graph.to_string())
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
    let graph = graph_for(case);
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
        let value = match (&assignment.value, &assignment.nonfinite) {
            (Some(value), None) => value.as_f64(),
            (None, Some(token)) => match token.as_str() {
                "NaN" => f64::NAN,
                "inf" => f64::INFINITY,
                "-inf" => f64::NEG_INFINITY,
                _ => panic!("invalid nonfinite setter token"),
            },
            _ => panic!("assignment must have one exact value or explicit nonfinite token"),
        };
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
    let expected_value = case.expected.value.as_ref().unwrap();
    let expected = expected_value.value.as_f64();
    expected_value.check(value.unwrap(), arithmetic_bound(expected), &case.id);
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
        g.reverse.check(
            actual_reverse,
            arithmetic_bound(g.reverse.value.as_f64()),
            &format!("{} reverse", case.id),
        );
        let planning = gradient_sum(&mut runtime, output, &g.inputs).unwrap();
        let tolerance = match g.planning_via {
            PlanningVia::Symbolic => arithmetic_bound(g.planning.value.as_f64()),
            PlanningVia::NumericalFallback => 128.0 * f64::EPSILON * expected.abs().max(1.0) / 1e-4,
        };
        g.planning
            .check(planning, tolerance, &format!("{} public/planning", case.id));
        if let Some(derivative) = &g.smooth_true {
            let h = 1.0 / 1024.0;
            let mut values = Vec::new();
            for direction in [-1.0, 1.0] {
                let mut probe = Runtime::new(&graph, case.status);
                for (&id, &value) in &inputs {
                    probe.set_by_id(id, value);
                }
                let mut multiplicities = BTreeMap::<u32, usize>::new();
                for &id in &g.inputs {
                    *multiplicities.entry(id).or_default() += 1;
                }
                for (id, count) in multiplicities {
                    probe.set_by_id(
                        id,
                        inputs.get(&id).copied().unwrap_or(0.0) + direction * h * count as f64,
                    );
                }
                values.push(probe.eval_node(output).unwrap());
            }
            close(
                (values[1] - values[0]) / (2.0 * h),
                derivative.value.as_f64(),
                128.0 * f64::EPSILON * expected.abs().max(1.0) / h
                    + g.fd_truncation_bound
                        .as_ref()
                        .map(Scalar::as_f64)
                        .unwrap_or(0.0),
                &format!("{} smooth true", case.id),
            );
        }
    }
}

fn validate_jet(jet: &Jet) {
    let numeric = jet.limit.is_some() && jet.tangent.is_some();
    assert_eq!(
        usize::from(numeric)
            + usize::from(jet.error.is_some())
            + usize::from(jet.deferred.is_some()),
        1
    );
    assert_eq!(jet.limit.is_some(), jet.tangent.is_some());
    for q in [&jet.limit, &jet.tangent].into_iter().flatten() {
        assert_eq!(q.category, Category::ExactOnly);
        q.value.as_f64();
        assert!(q.legacy_f64.is_none());
    }
    if let Some(error) = &jet.error {
        assert_eq!(error.category, Category::Semantic);
        assert_eq!(error.stage, "directional");
        assert_eq!(error.kind, "division_by_zero");
    }
    if let Some(reason) = &jet.deferred {
        assert!(
            ["public_jump_derivative_unsettled", "inactive_domain_error"]
                .contains(&reason.as_str())
        );
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
        if let Some(e) = &case.expected.error {
            assert_eq!(e.category, Category::Semantic);
        }
        if let Some(m) = &case.migration {
            match m.kind.as_str() {
                "exact_money" => {
                    assert!(case.expected.value.as_ref().unwrap().legacy_f64.is_some())
                }
                "directional_fallback" => assert!(case.expected.gradients.iter().any(|g| g
                    .planning
                    .legacy_f64
                    .is_some()
                    && matches!(g.planning_via, PlanningVia::NumericalFallback))),
                _ => panic!("unknown migration family"),
            }
        }
        run_case(case);
    }
}

#[test]
fn declared_directional_and_error_queries_are_explicit_capability_obligations() {
    let corpus = corpus();
    assert_eq!(
        corpus.capabilities.executed,
        [
            "value",
            "reverse",
            "public_planning",
            "smooth_fd",
            "value_cache"
        ]
        .map(String::from)
        .into_iter()
        .collect()
    );
    assert_eq!(
        corpus.capabilities.declared_only,
        [
            "true_directional",
            "planning_directional",
            "boundary_classification",
            "resolver_trace",
            "resource_budget"
        ]
        .map(String::from)
        .into_iter()
        .collect()
    );
    let mut classifications = BTreeSet::new();
    let mut directions = 0;
    let mut errors = 0;
    for case in &corpus.cases {
        assert_eq!(
            case.expected.cache_category.is_some(),
            !case.expected.uncached_after_value.is_empty()
        );
        if let Some(category) = case.expected.cache_category {
            assert_eq!(category, Category::Semantic);
        }
        for d in &case.expected.directions {
            assert_eq!(d.category, Category::ExactOnly);
            assert_eq!(
                d.requires,
                [
                    "true_directional",
                    "planning_directional",
                    "boundary_classification"
                ]
                .map(String::from)
                .into_iter()
                .collect()
            );
            assert!(!d.seeds.is_empty());
            let nodes = &case.graph["nodes"];
            for seed in &d.seeds {
                assert!(nodes.get(seed.input.to_string()).is_some());
                assert_ne!(seed.delta.as_f64(), 0.0);
            }
            validate_jet(&d.r#true);
            validate_jet(&d.planning);
            classifications.insert(match d.classification {
                Classification::Smooth => "smooth",
                Classification::ContinuousTie => "continuous_tie",
                Classification::Jump => "jump",
                Classification::Deferred => "deferred",
            });
            directions += 1;
        }
        for q in &case.expected.queries {
            assert_eq!(q.query, "speculative_planning");
            match q.requires.as_str() {
                "resolver_trace" => {
                    assert_eq!(q.expected.category, Category::Legacy);
                    assert_eq!(q.expected.outcome, "reject_domain_error");
                    assert_eq!(q.expected.error_kind, "table_not_found");
                    assert!(q.budget.is_none());
                }
                "resource_budget" => {
                    assert_eq!(q.expected.category, Category::ExactOnly);
                    assert_eq!(q.expected.outcome, "propagate_error");
                    assert_eq!(q.expected.error_kind, "resource_limit");
                    assert_eq!(q.budget, Some(0));
                }
                _ => panic!("unknown query capability"),
            }
            errors += 1;
        }
    }
    assert_eq!(
        classifications,
        ["smooth", "continuous_tie", "jump", "deferred"]
            .into_iter()
            .collect()
    );
    assert!(directions >= 50);
    assert_eq!(errors, 2);
}

#[test]
fn corpus_covers_every_supported_operation_contract_and_status() {
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
        "if_positive",
        "bracket_tax",
        "phase_out",
        "by_status",
    ]
    .map(String::from)
    .into_iter()
    .collect();
    assert_eq!(corpus.supported_ops, supported);
    let contracts = [
        "value",
        "error",
        "reverse",
        "public_planning",
        "smooth_fd",
        "true_directional",
        "planning_directional",
        "boundary_classification",
        "resolver_trace",
        "resource_budget",
    ];
    let mut coverage: BTreeMap<String, BTreeMap<String, Vec<String>>> = supported
        .iter()
        .map(|op| {
            (
                op.clone(),
                contracts
                    .map(|k| (k.to_string(), Vec::new()))
                    .into_iter()
                    .collect(),
            )
        })
        .collect();
    let mut statuses = BTreeMap::<String, BTreeSet<String>>::new();
    for case in &corpus.cases {
        let Some(row) = coverage.get_mut(&case.focus) else {
            continue;
        };
        let e = &case.expected;
        if e.error.is_none() {
            let graph = graph_for(case).unwrap();
            assert_eq!(
                op_name(&graph.nodes[&graph.outputs[0]].op),
                case.focus,
                "{}",
                case.id
            );
            statuses
                .entry(case.focus.clone())
                .or_default()
                .insert(format!("{:?}", case.status));
        }
        for (k, has) in [
            ("value", e.value.is_some()),
            ("error", e.error.is_some()),
            ("reverse", !e.gradients.is_empty()),
            ("public_planning", !e.gradients.is_empty()),
            (
                "smooth_fd",
                e.gradients.iter().any(|g| g.smooth_true.is_some()),
            ),
            ("true_directional", !e.directions.is_empty()),
            ("planning_directional", !e.directions.is_empty()),
            ("boundary_classification", !e.directions.is_empty()),
        ] {
            if has {
                row.get_mut(k).unwrap().push(case.id.clone());
            }
        }
        for q in &e.queries {
            row.get_mut(&q.requires).unwrap().push(case.id.clone());
        }
    }
    assert_eq!(coverage, corpus.coverage);
    for (op, row) in &coverage {
        for required in [
            "value",
            "reverse",
            "public_planning",
            "true_directional",
            "planning_directional",
            "boundary_classification",
        ] {
            assert!(
                !row[required].is_empty(),
                "{op}: absent {required} coverage"
            );
        }
    }
    for op in ["by_status", "bracket_tax", "phase_out"] {
        assert_eq!(statuses[op].len(), 5);
        let status_matrix = &corpus.status_coverage[op];
        assert_eq!(status_matrix.len(), 5);
        for status in [
            "single",
            "married_joint",
            "married_separate",
            "head_of_household",
            "qualifying_widow",
        ] {
            for (contract, ids) in &coverage[op] {
                let expected: Vec<_> = ids
                    .iter()
                    .filter(|id| {
                        let case = corpus.cases.iter().find(|case| &case.id == *id).unwrap();
                        serde_json::to_value(case.status).unwrap() == status
                    })
                    .cloned()
                    .collect();
                assert_eq!(status_matrix[status][contract], expected);
            }
            for contract in [
                "value",
                "reverse",
                "public_planning",
                "smooth_fd",
                "true_directional",
                "planning_directional",
                "boundary_classification",
            ] {
                assert!(
                    !status_matrix[status][contract].is_empty(),
                    "{op}/{status}/{contract}"
                );
            }
        }
    }
}

#[test]
fn exact_scalar_parser_rejects_malformed_and_nonfinite_rationals() {
    for (n, d) in [
        ("NaN", "1"),
        ("inf", "1"),
        ("1/2", "1"),
        ("", "1"),
        ("1", "0"),
        ("1", "-2"),
        ("1", "NaN"),
        ("1", "1.5"),
    ] {
        assert!(
            std::panic::catch_unwind(|| Scalar {
                n: n.into(),
                d: d.into()
            }
            .as_f64())
            .is_err(),
            "{n}/{d}"
        );
    }
    assert!(serde_json::from_str::<Scalar>(r#"{"n":"1","d":"2","extra":0}"#).is_err());
    assert_eq!(
        Scalar {
            n: "-1".into(),
            d: "3".into()
        }
        .as_f64(),
        -1.0 / 3.0
    );
}

#[test]
fn migration_expectations_fail_when_legacy_departures_are_disallowed() {
    let mut data: serde_json::Value =
        serde_json::from_str(include_str!("../../../tests/fixtures/op-corpus/v1.json")).unwrap();
    let mut count = 0;
    for case in data["cases"].as_array_mut().unwrap() {
        if case.get("migration").is_none() {
            continue;
        }
        if case["migration"]["kind"] == "exact_money" {
            case["expected"]["value"]
                .as_object_mut()
                .unwrap()
                .remove("legacy_f64");
        } else {
            case["expected"]["gradients"][0]["planning"]
                .as_object_mut()
                .unwrap()
                .remove("legacy_f64");
        }
        let case: Case = serde_json::from_value(case.clone()).unwrap();
        assert!(
            std::panic::catch_unwind(|| run_case(&case)).is_err(),
            "{}: f64 silently passed the exact contract",
            case.id
        );
        count += 1;
    }
    assert_eq!(count, 4);
}
