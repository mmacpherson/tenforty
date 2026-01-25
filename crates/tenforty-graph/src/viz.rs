use crate::graph::{Graph, Op};
use std::fmt::Write;

/// Export graph to DOT format for visualization with Graphviz.
pub fn to_dot(graph: &Graph) -> String {
    let mut out = String::new();
    writeln!(out, "digraph TaxGraph {{").unwrap();
    writeln!(out, "    rankdir=TB;").unwrap();
    writeln!(out, "    node [shape=box, fontname=\"monospace\"];").unwrap();
    writeln!(out).unwrap();

    for (id, node) in &graph.nodes {
        let label = node_label(node);
        let shape = match &node.op {
            Op::Input | Op::Import { .. } => "ellipse",
            Op::Literal { .. } => "diamond",
            _ => "box",
        };
        let color = match &node.op {
            Op::Input => "lightblue",
            Op::Import { .. } => "lightcoral",
            Op::Literal { .. } => "lightyellow",
            Op::BracketTax { .. } | Op::PhaseOut { .. } => "lightgreen",
            _ => "white",
        };
        writeln!(
            out,
            "    n{} [label=\"{}\", shape={}, fillcolor=\"{}\", style=filled];",
            id, label, shape, color
        )
        .unwrap();
    }

    writeln!(out).unwrap();

    for (id, node) in &graph.nodes {
        for dep in node.op.dependencies() {
            writeln!(out, "    n{} -> n{};", dep, id).unwrap();
        }
    }

    writeln!(out, "}}").unwrap();
    out
}

fn node_label(node: &crate::graph::Node) -> String {
    let name_part = node
        .name
        .as_ref()
        .map(|n| format!("{}\\n", n))
        .unwrap_or_default();

    let op_part = match &node.op {
        Op::Input => "INPUT".to_string(),
        Op::Literal { value } => format!("{}", value),
        Op::Add { .. } => "+".to_string(),
        Op::Sub { .. } => "-".to_string(),
        Op::Mul { .. } => "×".to_string(),
        Op::Div { .. } => "÷".to_string(),
        Op::Max { .. } => "max".to_string(),
        Op::Min { .. } => "min".to_string(),
        Op::Floor { .. } => "floor".to_string(),
        Op::Neg { .. } => "neg".to_string(),
        Op::Abs { .. } => "abs".to_string(),
        Op::Clamp { min, max, .. } => format!("clamp[{},{}]", min, max),
        Op::BracketTax { table, .. } => format!("bracket_tax\\n({})", table),
        Op::PhaseOut { base, rate, .. } => format!("phase_out\\nbase={}, rate={}", base, rate),
        Op::ByStatus { .. } => "by_status".to_string(),
        Op::IfPositive { .. } => "if_positive".to_string(),
        Op::Import { form, line, .. } => format!("import\\n{}:{}", form, line),
    };

    format!("{}{}", name_part, op_part)
}

/// Export graph structure as simplified JSON for debugging.
pub fn to_debug_json(graph: &Graph) -> serde_json::Value {
    let nodes: Vec<serde_json::Value> = graph
        .nodes
        .iter()
        .map(|(id, node)| {
            serde_json::json!({
                "id": id,
                "name": node.name,
                "op": op_type_name(&node.op),
                "deps": node.op.dependencies(),
            })
        })
        .collect();

    serde_json::json!({
        "nodes": nodes,
        "inputs": graph.inputs,
        "outputs": graph.outputs,
    })
}

fn op_type_name(op: &Op) -> &'static str {
    match op {
        Op::Input => "input",
        Op::Literal { .. } => "literal",
        Op::Add { .. } => "add",
        Op::Sub { .. } => "sub",
        Op::Mul { .. } => "mul",
        Op::Div { .. } => "div",
        Op::Max { .. } => "max",
        Op::Min { .. } => "min",
        Op::Floor { .. } => "floor",
        Op::Neg { .. } => "neg",
        Op::Abs { .. } => "abs",
        Op::Clamp { .. } => "clamp",
        Op::BracketTax { .. } => "bracket_tax",
        Op::PhaseOut { .. } => "phase_out",
        Op::ByStatus { .. } => "by_status",
        Op::IfPositive { .. } => "if_positive",
        Op::Import { .. } => "import",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Node;
    use std::collections::HashMap;

    #[test]
    fn test_to_dot() {
        let mut nodes = HashMap::new();
        nodes.insert(
            0,
            Node {
                id: 0,
                op: Op::Input,
                name: Some("x".to_string()),
            },
        );
        nodes.insert(
            1,
            Node {
                id: 1,
                op: Op::Literal { value: 2.0 },
                name: None,
            },
        );
        nodes.insert(
            2,
            Node {
                id: 2,
                op: Op::Mul { left: 0, right: 1 },
                name: Some("y".to_string()),
            },
        );

        let graph = Graph {
            meta: None,
            nodes,
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![2],
            invariants: vec![],
        };

        let dot = to_dot(&graph);
        assert!(dot.contains("digraph"));
        assert!(dot.contains("n0"));
        assert!(dot.contains("n0 -> n2"));
    }
}
