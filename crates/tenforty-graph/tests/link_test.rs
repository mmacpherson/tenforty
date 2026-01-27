use graphlib::{FilingStatus, Graph, GraphSet, Runtime};

fn make_income_graph() -> Graph {
    let json = r#"{
        "meta": {"form_id": "schedule_d", "year": 2024},
        "nodes": {
            "0": {"id": 0, "op": {"type": "input"}, "name": "short_term_gain"},
            "1": {"id": 1, "op": {"type": "input"}, "name": "long_term_gain"},
            "2": {"id": 2, "op": {"type": "add", "left": 0, "right": 1}, "name": "L16_net_gain"}
        },
        "tables": {},
        "inputs": [0, 1],
        "outputs": [2]
    }"#;
    Graph::from_json(json).unwrap()
}

fn make_1040_with_import() -> Graph {
    let json = r#"{
        "meta": {"form_id": "us_1040", "year": 2024},
        "imports": [
            {"form": "us_schedule_d", "line": "L16_net_gain", "year": 2024}
        ],
        "nodes": {
            "0": {"id": 0, "op": {"type": "input"}, "name": "L1a_w2_wages"},
            "1": {"id": 1, "op": {"type": "import", "form": "us_schedule_d", "line": "L16_net_gain", "year": 2024}, "name": "L7_capital_gain"},
            "2": {"id": 2, "op": {"type": "add", "left": 0, "right": 1}, "name": "L9_total_income"},
            "3": {"id": 3, "op": {"type": "literal", "value": 14600}, "name": "std_deduction"},
            "4": {"id": 4, "op": {"type": "sub", "left": 2, "right": 3}, "name": "taxable_income"},
            "5": {"id": 5, "op": {"type": "literal", "value": 0}, "name": "zero"},
            "6": {"id": 6, "op": {"type": "max", "left": 4, "right": 5}, "name": "L15_taxable_income"}
        },
        "tables": {},
        "inputs": [0, 1],
        "outputs": [6]
    }"#;
    Graph::from_json(json).unwrap()
}

#[test]
fn test_link_schedule_d_to_1040() {
    let schedule_d = make_income_graph();
    let us_1040 = make_1040_with_import();

    let gs = GraphSet::new()
        .add("us_schedule_d", schedule_d)
        .add("us_1040", us_1040);

    // Before linking: check unresolved imports
    let unresolved = gs.unresolved_imports();
    assert!(unresolved.is_empty(), "All imports should be resolved");

    // Link graphs
    let linked = gs.link().expect("Linking should succeed");

    // Verify structure
    assert!(linked
        .node_id_by_name("us_schedule_d_short_term_gain")
        .is_some());
    assert!(linked
        .node_id_by_name("us_schedule_d_long_term_gain")
        .is_some());
    assert!(linked
        .node_id_by_name("us_schedule_d_L16_net_gain")
        .is_some());
    assert!(linked.node_id_by_name("us_1040_L1a_w2_wages").is_some());
    assert!(linked
        .node_id_by_name("us_1040_L15_taxable_income")
        .is_some());

    // Evaluate
    let mut rt = Runtime::new(&linked, FilingStatus::Single);
    rt.set("us_schedule_d_short_term_gain", 5000.0).unwrap();
    rt.set("us_schedule_d_long_term_gain", 15000.0).unwrap();
    rt.set("us_1040_L1a_w2_wages", 100000.0).unwrap();

    let taxable = rt.eval("us_1040_L15_taxable_income").unwrap();
    // wages + (short + long gains) - standard deduction = 100000 + 20000 - 14600 = 105400
    assert!((taxable - 105400.0).abs() < 0.01);
}

#[test]
fn test_unresolved_import_becomes_input() {
    // Create a graph that imports from a non-existent form
    let json = r#"{
        "meta": {"form_id": "state_form", "year": 2024},
        "imports": [
            {"form": "us_1040", "line": "L15_taxable_income", "year": 2024}
        ],
        "nodes": {
            "0": {"id": 0, "op": {"type": "import", "form": "us_1040", "line": "L15_taxable_income", "year": 2024}, "name": "federal_taxable"},
            "1": {"id": 1, "op": {"type": "literal", "value": 0.05}, "name": "state_rate"},
            "2": {"id": 2, "op": {"type": "mul", "left": 0, "right": 1}, "name": "state_tax"}
        },
        "tables": {},
        "inputs": [0],
        "outputs": [2]
    }"#;
    let state_form = Graph::from_json(json).unwrap();

    let gs = GraphSet::new().add("state_form", state_form);

    // Check that we have an unresolved import
    let unresolved = gs.unresolved_imports();
    assert_eq!(unresolved.len(), 1);
    assert_eq!(unresolved[0].form, "us_1040");
    assert_eq!(unresolved[0].line, "L15_taxable_income");

    // Link anyway - unresolved import becomes an Input node
    let linked = gs
        .link()
        .expect("Linking should succeed with unresolved imports");

    // The unresolved import should be an input node with a descriptive name
    let input_names: Vec<_> = linked
        .inputs
        .iter()
        .filter_map(|id| linked.nodes.get(id).and_then(|n| n.name.clone()))
        .collect();

    assert!(input_names
        .iter()
        .any(|n| n.contains("import__state_form__us_1040__L15_taxable_income")));
}

#[test]
fn test_chained_imports() {
    // Form A: base calculation
    let form_a_json = r#"{
        "meta": {"form_id": "form_a", "year": 2024},
        "nodes": {
            "0": {"id": 0, "op": {"type": "input"}, "name": "base_value"},
            "1": {"id": 1, "op": {"type": "literal", "value": 2}, "name": "multiplier"},
            "2": {"id": 2, "op": {"type": "mul", "left": 0, "right": 1}, "name": "doubled"}
        },
        "tables": {},
        "inputs": [0],
        "outputs": [2]
    }"#;

    // Form B: imports from A, adds 100
    let form_b_json = r#"{
        "meta": {"form_id": "form_b", "year": 2024},
        "imports": [
            {"form": "form_a", "line": "doubled", "year": 2024}
        ],
        "nodes": {
            "0": {"id": 0, "op": {"type": "import", "form": "form_a", "line": "doubled", "year": 2024}, "name": "from_a"},
            "1": {"id": 1, "op": {"type": "literal", "value": 100}, "name": "addition"},
            "2": {"id": 2, "op": {"type": "add", "left": 0, "right": 1}, "name": "result"}
        },
        "tables": {},
        "inputs": [0],
        "outputs": [2]
    }"#;

    // Form C: imports from B, multiplies by 3
    let form_c_json = r#"{
        "meta": {"form_id": "form_c", "year": 2024},
        "imports": [
            {"form": "form_b", "line": "result", "year": 2024}
        ],
        "nodes": {
            "0": {"id": 0, "op": {"type": "import", "form": "form_b", "line": "result", "year": 2024}, "name": "from_b"},
            "1": {"id": 1, "op": {"type": "literal", "value": 3}, "name": "multiplier"},
            "2": {"id": 2, "op": {"type": "mul", "left": 0, "right": 1}, "name": "final_result"}
        },
        "tables": {},
        "inputs": [0],
        "outputs": [2]
    }"#;

    let form_a = Graph::from_json(form_a_json).unwrap();
    let form_b = Graph::from_json(form_b_json).unwrap();
    let form_c = Graph::from_json(form_c_json).unwrap();

    let gs = GraphSet::new()
        .add("form_a", form_a)
        .add("form_b", form_b)
        .add("form_c", form_c);

    assert!(gs.unresolved_imports().is_empty());

    let linked = gs.link().unwrap();

    let mut rt = Runtime::new(&linked, FilingStatus::Single);
    rt.set("form_a_base_value", 50.0).unwrap();

    // form_a: 50 * 2 = 100
    // form_b: 100 + 100 = 200
    // form_c: 200 * 3 = 600
    let final_result = rt.eval("form_c_final_result").unwrap();
    assert!((final_result - 600.0).abs() < 0.01);
}

#[test]
fn test_link_with_bracket_tables() {
    let form_json = r#"{
        "meta": {"form_id": "tax_form", "year": 2024},
        "nodes": {
            "0": {"id": 0, "op": {"type": "input"}, "name": "income"},
            "1": {"id": 1, "op": {"type": "bracket_tax", "table": "rates", "income": 0}, "name": "tax"}
        },
        "tables": {
            "rates": {
                "brackets": {
                    "single": [
                        {"threshold": 11600, "rate": 0.10},
                        {"threshold": 47150, "rate": 0.12},
                        {"threshold": 100525, "rate": 0.22}
                    ],
                    "married_joint": [
                        {"threshold": 23200, "rate": 0.10},
                        {"threshold": 94300, "rate": 0.12}
                    ],
                    "married_separate": [{"threshold": 11600, "rate": 0.10}],
                    "head_of_household": [{"threshold": 16550, "rate": 0.10}],
                    "qualifying_widow": [{"threshold": 23200, "rate": 0.10}]
                }
            }
        },
        "inputs": [0],
        "outputs": [1]
    }"#;

    let form = Graph::from_json(form_json).unwrap();

    let gs = GraphSet::new().add("tax_form", form);

    let linked = gs.link().unwrap();

    // Table should be prefixed
    assert!(linked.tables.contains_key("tax_form__rates"));

    let mut rt = Runtime::new(&linked, FilingStatus::Single);
    rt.set("tax_form_income", 50000.0).unwrap();

    let tax = rt.eval("tax_form_tax").unwrap();
    // 10% on first $11,600 = $1,160
    // 12% on next $35,550 ($11,600 to $47,150) = $4,266
    // 22% on remaining $2,850 ($47,150 to $50,000) = $627
    // Total = $6,053
    // 10% on first $11,600 = $1,160
    // 12% on next $35,550 ($11,600 to $47,150) = $4,266
    // 22% on remaining $2,850 ($47,150 to $50,000) = $627
    // Total = $6,053
    assert!((tax - 6053.0).abs() < 1.0);
}
