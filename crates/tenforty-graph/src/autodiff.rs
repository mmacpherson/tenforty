use crate::eval::{EvalError, Runtime};
use crate::graph::{NodeId, Op};
use crate::primitives;
use std::collections::HashMap;

/// Compute the gradient of an output node with respect to an input node.
/// Uses reverse-mode automatic differentiation (backpropagation).
pub fn gradient(
    runtime: &mut Runtime,
    output: NodeId,
    input: NodeId,
) -> Result<f64, EvalError> {
    runtime.eval_node(output)?;

    let order = runtime.graph().topological_order();
    let values = runtime.get_all_values();

    let mut adjoints: HashMap<NodeId, f64> = HashMap::new();
    adjoints.insert(output, 1.0);

    for &node_id in order.iter().rev() {
        let adj = *adjoints.get(&node_id).unwrap_or(&0.0);
        if adj == 0.0 {
            continue;
        }

        let node = match runtime.graph().nodes.get(&node_id) {
            Some(n) => n,
            None => continue,
        };

        backprop(
            &node.op,
            node_id,
            adj,
            values,
            runtime,
            &mut adjoints,
        )?;
    }

    Ok(*adjoints.get(&input).unwrap_or(&0.0))
}

fn backprop(
    op: &Op,
    _node_id: NodeId,
    adj: f64,
    values: &HashMap<NodeId, f64>,
    runtime: &Runtime,
    adjoints: &mut HashMap<NodeId, f64>,
) -> Result<(), EvalError> {
    match op {
        Op::Input | Op::Literal { .. } | Op::Import { .. } => {}

        Op::Add { left, right } => {
            *adjoints.entry(*left).or_insert(0.0) += adj;
            *adjoints.entry(*right).or_insert(0.0) += adj;
        }

        Op::Sub { left, right } => {
            *adjoints.entry(*left).or_insert(0.0) += adj;
            *adjoints.entry(*right).or_insert(0.0) -= adj;
        }

        Op::Mul { left, right } => {
            let l = values.get(left).copied().unwrap_or(0.0);
            let r = values.get(right).copied().unwrap_or(0.0);
            *adjoints.entry(*left).or_insert(0.0) += adj * r;
            *adjoints.entry(*right).or_insert(0.0) += adj * l;
        }

        Op::Div { left, right } => {
            let l = values.get(left).copied().unwrap_or(0.0);
            let r = values.get(right).copied().unwrap_or(0.0);
            if r != 0.0 {
                *adjoints.entry(*left).or_insert(0.0) += adj / r;
                *adjoints.entry(*right).or_insert(0.0) -= adj * l / (r * r);
            }
        }

        Op::Max { left, right } => {
            let l = values.get(left).copied().unwrap_or(0.0);
            let r = values.get(right).copied().unwrap_or(0.0);
            if l >= r {
                *adjoints.entry(*left).or_insert(0.0) += adj;
            } else {
                *adjoints.entry(*right).or_insert(0.0) += adj;
            }
        }

        Op::Min { left, right } => {
            let l = values.get(left).copied().unwrap_or(0.0);
            let r = values.get(right).copied().unwrap_or(0.0);
            if l <= r {
                *adjoints.entry(*left).or_insert(0.0) += adj;
            } else {
                *adjoints.entry(*right).or_insert(0.0) += adj;
            }
        }

        Op::Floor { arg } => {
            *adjoints.entry(*arg).or_insert(0.0) += 0.0;
        }

        Op::Neg { arg } => {
            *adjoints.entry(*arg).or_insert(0.0) -= adj;
        }

        Op::Abs { arg } => {
            let v = values.get(arg).copied().unwrap_or(0.0);
            let sign = if v >= 0.0 { 1.0 } else { -1.0 };
            *adjoints.entry(*arg).or_insert(0.0) += adj * sign;
        }

        Op::Clamp { arg, min, max } => {
            let v = values.get(arg).copied().unwrap_or(0.0);
            if v > *min && v < *max {
                *adjoints.entry(*arg).or_insert(0.0) += adj;
            }
        }

        Op::BracketTax { table, income } => {
            let table = match runtime.graph().tables.get(table) {
                Some(t) => t,
                None => return Ok(()),
            };
            let brackets = table.brackets.get(runtime.filing_status());
            let inc = values.get(income).copied().unwrap_or(0.0);
            let rate = primitives::marginal_rate(brackets, inc);
            *adjoints.entry(*income).or_insert(0.0) += adj * rate;
        }

        Op::PhaseOut {
            base,
            threshold,
            rate,
            agi,
        } => {
            let thresh = *threshold.get(runtime.filing_status());
            let agi_val = values.get(agi).copied().unwrap_or(0.0);
            let grad = primitives::phase_out_gradient(*base, thresh, *rate, agi_val);
            *adjoints.entry(*agi).or_insert(0.0) += adj * grad;
        }

        Op::ByStatus { values: by_status } => {
            let node_id = *by_status.get(runtime.filing_status());
            *adjoints.entry(node_id).or_insert(0.0) += adj;
        }

        Op::IfPositive { cond, then, otherwise } => {
            let c = values.get(cond).copied().unwrap_or(0.0);
            if c > 0.0 {
                *adjoints.entry(*then).or_insert(0.0) += adj;
            } else {
                *adjoints.entry(*otherwise).or_insert(0.0) += adj;
            }
        }
    }

    Ok(())
}

/// Compute numerical gradient for verification.
pub fn numerical_gradient(
    runtime: &mut Runtime,
    output: NodeId,
    input: NodeId,
    epsilon: f64,
) -> Result<f64, EvalError> {
    let original = runtime.get_all_values().get(&input).copied().unwrap_or(0.0);

    runtime.set_by_id(input, original + epsilon);
    let f_plus = runtime.eval_node(output)?;

    runtime.set_by_id(input, original - epsilon);
    let f_minus = runtime.eval_node(output)?;

    runtime.set_by_id(input, original);
    runtime.eval_node(output)?;

    Ok((f_plus - f_minus) / (2.0 * epsilon))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{Bracket, BracketTable, ByStatus, FilingStatus, Graph, Node};
    use std::collections::HashMap;

    fn simple_arithmetic_graph() -> Graph {
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
                name: Some("two".to_string()),
            },
        );
        nodes.insert(
            2,
            Node {
                id: 2,
                op: Op::Mul { left: 0, right: 1 },
                name: Some("double".to_string()),
            },
        );

        Graph {
            meta: None,
            nodes,
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![2],
            invariants: vec![],
        }
    }

    fn tax_graph() -> Graph {
        let mut nodes = HashMap::new();
        nodes.insert(
            0,
            Node {
                id: 0,
                op: Op::Input,
                name: Some("income".to_string()),
            },
        );
        nodes.insert(
            1,
            Node {
                id: 1,
                op: Op::BracketTax {
                    table: "federal".to_string(),
                    income: 0,
                },
                name: Some("tax".to_string()),
            },
        );

        let mut tables = HashMap::new();
        tables.insert(
            "federal".to_string(),
            BracketTable {
                brackets: ByStatus::uniform(vec![
                    Bracket { threshold: 10000.0, rate: 0.10 },
                    Bracket { threshold: 40000.0, rate: 0.20 },
                    Bracket { threshold: f64::INFINITY, rate: 0.30 },
                ]),
            },
        );

        Graph {
            meta: None,
            nodes,
            tables,
            inputs: vec![0],
            outputs: vec![1],
            invariants: vec![],
        }
    }

    #[test]
    fn test_gradient_multiply() {
        let graph = simple_arithmetic_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 5.0);

        let grad = gradient(&mut runtime, 2, 0).unwrap();
        assert_eq!(grad, 2.0);
    }

    #[test]
    fn test_gradient_vs_numerical() {
        let graph = simple_arithmetic_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 5.0);

        let analytical = gradient(&mut runtime, 2, 0).unwrap();
        let numerical = numerical_gradient(&mut runtime, 2, 0, 1e-6).unwrap();
        assert!((analytical - numerical).abs() < 1e-4);
    }

    #[test]
    fn test_gradient_bracket_tax() {
        let graph = tax_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        runtime.set_by_id(0, 5000.0);
        let grad = gradient(&mut runtime, 1, 0).unwrap();
        assert_eq!(grad, 0.10);

        runtime.set_by_id(0, 25000.0);
        let grad = gradient(&mut runtime, 1, 0).unwrap();
        assert_eq!(grad, 0.20);

        runtime.set_by_id(0, 50000.0);
        let grad = gradient(&mut runtime, 1, 0).unwrap();
        assert_eq!(grad, 0.30);
    }
}
