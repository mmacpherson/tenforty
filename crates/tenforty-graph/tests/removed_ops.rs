use graphlib::Graph;
use serde_json::json;

fn assert_removed_op_rejected(op: serde_json::Value) {
    let kind = op["type"].as_str().unwrap();
    let graph = json!({
        "nodes": {
            "0": {"id": 0, "op": {"type": "input"}},
            "1": {"id": 1, "op": op}
        },
        "inputs": [0],
        "outputs": [1]
    });
    let error = Graph::from_json(&graph.to_string())
        .expect_err("removed operation must be rejected when loading the graph");
    assert!(
        error
            .to_string()
            .contains(&format!("unknown variant `{kind}`")),
        "unexpected load error: {error}"
    );
}

#[test]
fn abs_is_rejected_at_load() {
    assert_removed_op_rejected(json!({"type": "abs", "arg": 0}));
}

#[test]
fn clamp_is_rejected_at_load() {
    assert_removed_op_rejected(json!({"type": "clamp", "arg": 0, "min": 0.0, "max": 1.0}));
}
