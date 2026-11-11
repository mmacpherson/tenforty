//! Graph JSON schema version 2: exact scalars (tenforty-yxun.3).
//!
//! Every value scalar is `{"n": "<signed integer>", "d": "<positive integer>"}`.
//! The loader converts each to the nearest f64 (round half to even), so no
//! computed figure changes in this stage. Expected values here come from Rust's
//! own correctly rounded decimal parser or from exact binary arithmetic, never
//! from the runtime.

use graphlib::{FilingStatus, Graph, Runtime};

fn graph_json(literal: &str) -> String {
    format!(
        r#"{{
            "meta": {{"schema_version": 2}},
            "nodes": {{
                "0": {{"id": 0, "name": "x", "op": {{"type": "literal", "value": {literal}}}}}
            }},
            "inputs": [],
            "outputs": [0]
        }}"#
    )
}

fn load_literal(n: &str, d: &str) -> f64 {
    let json = graph_json(&format!(r#"{{"n": "{n}", "d": "{d}"}}"#));
    let graph = Graph::from_json(&json).unwrap();
    let mut runtime = Runtime::new(&graph, FilingStatus::Single);
    runtime.eval_node(0).unwrap()
}

fn load_error(json: &str) -> String {
    Graph::from_json(json).unwrap_err().to_string()
}

#[test]
fn exact_scalar_converts_to_the_nearest_f64() {
    assert_eq!(load_literal("11", "250").to_bits(), 0.044f64.to_bits());
    assert_eq!(load_literal("-1", "2").to_bits(), (-0.5f64).to_bits());
    assert_eq!(load_literal("0", "1").to_bits(), 0.0f64.to_bits());
    assert_eq!(load_literal("1", "3").to_bits(), (1.0f64 / 3.0).to_bits());
    assert_eq!(
        load_literal("1000000000000", "1").to_bits(),
        1e12f64.to_bits()
    );
    let big = "123456789012345678901234567890123456789";
    assert_eq!(
        load_literal(big, "1").to_bits(),
        big.parse::<f64>().unwrap().to_bits()
    );
    // Exact dyadics are exact in f64.
    assert_eq!(load_literal("3", "1024"), 3.0 / 1024.0);
    assert_eq!(
        load_literal("-7", "4503599627370496"),
        -7.0 * 2f64.powi(-52)
    );
}

#[test]
fn decimal_scalars_match_the_correctly_rounded_decimal_parser() {
    for numerator in [1i64, 3, 7, 11, 44, 47, 495, 875, 12345, 9_999_999, -2875] {
        for exponent in 0..=18u32 {
            let denominator = 10u128.pow(exponent);
            let expected: f64 = format!("{numerator}e-{exponent}").parse().unwrap();
            let actual = load_literal(&numerator.to_string(), &denominator.to_string());
            assert_eq!(
                actual.to_bits(),
                expected.to_bits(),
                "{numerator}/{denominator}"
            );
        }
    }
}

#[test]
fn halfway_cases_round_to_even() {
    // 2^53 + 1 is exactly halfway between 2^53 and 2^53 + 2: ties to even 2^53.
    assert_eq!(load_literal("9007199254740993", "1"), 9007199254740992.0);
    // 2^53 + 3 is halfway between 2^53 + 2 and 2^53 + 4: ties to even 2^53 + 4.
    assert_eq!(load_literal("9007199254740995", "1"), 9007199254740996.0);
    // Just above halfway rounds up.
    assert_eq!(load_literal("18014398509481987", "2"), 9007199254740994.0);
}

#[test]
fn the_old_numeric_format_is_rejected_with_a_clear_error() {
    let message = load_error(&graph_json("0.044"));
    assert!(message.contains("exact scalar"), "{message}");
    assert!(message.contains("schema version 2"), "{message}");
}

#[test]
fn malformed_exact_scalars_are_rejected() {
    for scalar in [
        r#"{"n": "1", "d": "0"}"#,
        r#"{"n": "1", "d": "-2"}"#,
        r#"{"n": "1.5", "d": "1"}"#,
        r#"{"n": "", "d": "1"}"#,
        r#"{"n": "+1", "d": "1"}"#,
        r#"{"n": " 1", "d": "1"}"#,
        r#"{"n": "1e3", "d": "1"}"#,
        r#"{"n": "NaN", "d": "1"}"#,
        r#"{"n": 1, "d": "1"}"#,
        r#"{"n": "1"}"#,
        r#"{"n": "1", "d": "1", "x": "0"}"#,
    ] {
        assert!(
            Graph::from_json(&graph_json(scalar)).is_err(),
            "accepted {scalar}"
        );
    }
}

#[test]
fn the_schema_version_is_required() {
    let without_meta =
        graph_json(r#"{"n": "1", "d": "1"}"#).replace(r#""meta": {"schema_version": 2},"#, "");
    assert!(load_error(&without_meta).contains("schema version"));
    let old_version = graph_json(r#"{"n": "1", "d": "1"}"#)
        .replace(r#""schema_version": 2"#, r#""schema_version": 1"#);
    assert!(load_error(&old_version).contains("schema version"));
}

fn bracket_graph(top_threshold: &str, extra: &str) -> String {
    format!(
        r#"{{
            "meta": {{"schema_version": 2}},
            "nodes": {{
                "0": {{"id": 0, "name": "income", "op": {{"type": "input"}}}},
                "1": {{"id": 1, "name": "tax", "op": {{"type": "bracket_tax", "table": "t", "income": 0}}}}
            }},
            "tables": {{"t": {{"brackets": {{
                "single": [{extra}{{"threshold": {{"n": "10", "d": "1"}}, "rate": {{"n": "1", "d": "10"}}}},
                           {{"threshold": {top_threshold}, "rate": {{"n": "1", "d": "5"}}}}],
                "married_joint": [{{"threshold": null, "rate": {{"n": "0", "d": "1"}}}}],
                "married_separate": [{{"threshold": null, "rate": {{"n": "0", "d": "1"}}}}],
                "head_of_household": [{{"threshold": null, "rate": {{"n": "0", "d": "1"}}}}],
                "qualifying_widow": [{{"threshold": null, "rate": {{"n": "0", "d": "1"}}}}]
            }}}}}},
            "inputs": [0],
            "outputs": [1]
        }}"#
    )
}

#[test]
fn an_unbounded_top_bracket_is_explicit_and_only_last() {
    let graph = Graph::from_json(&bracket_graph("null", "")).unwrap();
    let mut runtime = Runtime::new(&graph, FilingStatus::Single);
    runtime.set_by_id(0, 1e15);
    let expected: f64 = 10.0 * 0.1 + (1e15 - 10.0) * 0.2;
    assert_eq!(runtime.eval_node(1).unwrap().to_bits(), expected.to_bits());

    let interior_null = r#"{"threshold": null, "rate": {"n": "0", "d": "1"}},"#;
    assert!(Graph::from_json(&bracket_graph("null", interior_null)).is_err());
}

#[test]
fn serialization_round_trips_every_f64_exactly() {
    let sum = 0.1f64 + 0.2;
    for value in [0.044f64, sum, -0.5, 1e12, 5e-324, f64::MAX] {
        let graph = Graph::from_json(&graph_json(r#"{"n": "0", "d": "1"}"#)).unwrap();
        let mut graph = graph;
        if let Some(node) = graph.nodes.get_mut(&0) {
            node.op = graphlib::Op::Literal { value };
        }
        let reloaded = Graph::from_json(&graph.to_json().unwrap()).unwrap();
        let mut runtime = Runtime::new(&reloaded, FilingStatus::Single);
        assert_eq!(runtime.eval_node(0).unwrap().to_bits(), value.to_bits());
    }
}

fn assert_nearest_ties_to_even(n: &str, d: &str) {
    use num_bigint::BigInt;
    use num_rational::BigRational;
    use num_traits::Signed;
    let x = graphlib::exact::scalar_to_f64(n, d).unwrap();
    let exact = BigRational::new(n.parse::<BigInt>().unwrap(), d.parse::<BigInt>().unwrap());
    let distance = |v: f64| (BigRational::from_float(v).unwrap() - &exact).abs();
    let here = distance(x);
    for neighbour in [x.next_down(), x.next_up()] {
        if !neighbour.is_finite() {
            continue;
        }
        let there = distance(neighbour);
        assert!(here <= there, "{n}/{d} -> {x:e} is not nearest");
        if here == there {
            assert_eq!(x.to_bits() & 1, 0, "{n}/{d}: a tie did not go to even");
        }
    }
}

proptest::proptest! {
    #[test]
    fn every_exact_scalar_converts_to_the_nearest_double_ties_to_even(
        n in proptest::num::i128::ANY,
        d in 1u128..,
    ) {
        assert_nearest_ties_to_even(&n.to_string(), &d.to_string());
    }

    #[test]
    fn small_ratios_with_frequent_near_ties_convert_correctly(
        n in -(1i64 << 55)..(1i64 << 55),
        shift in 0u32..64,
    ) {
        assert_nearest_ties_to_even(&n.to_string(), &(1u128 << shift).to_string());
    }
}
