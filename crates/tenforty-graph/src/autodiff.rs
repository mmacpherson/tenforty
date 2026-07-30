use crate::eval::{EvalError, Runtime};
use crate::graph::{NodeId, Op};
use crate::primitives;
use std::collections::{HashMap, HashSet};

/// Compute the adjoint of every node with respect to an output node.
///
/// One reverse-mode pass produces the partial derivative of `output` with
/// respect to every node on its active dependency path, so callers wanting
/// several partials should take this map rather than calling [`gradient`]
/// repeatedly.
pub fn adjoints(runtime: &mut Runtime, output: NodeId) -> Result<HashMap<NodeId, f64>, EvalError> {
    let (adjoints, _) = adjoints_with_order(runtime, output)?;
    Ok(adjoints)
}

fn adjoints_with_order(
    runtime: &mut Runtime,
    output: NodeId,
) -> Result<(HashMap<NodeId, f64>, Vec<NodeId>), EvalError> {
    runtime.eval_node(output)?;

    let order = runtime
        .graph()
        .reachable_topological_order(&[output], runtime.filing_status())?;
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

        backprop(&node.op, node_id, adj, values, runtime, &mut adjoints)?;
    }

    Ok((adjoints, order))
}

/// Compute the gradient of an output node with respect to an input node.
/// Uses reverse-mode automatic differentiation in smooth regions and the
/// composed function's right-hand numerical derivative at an active kink.
pub fn gradient(runtime: &mut Runtime, output: NodeId, input: NodeId) -> Result<f64, EvalError> {
    gradient_sum(runtime, output, &[input])
}

/// Compute the total derivative of an output with respect to a quantity that
/// is written into several input nodes at once.
///
/// One natural input often feeds more than one node — wage income reaches both
/// the 1040 wage line and Form 8959's Medicare wages. Setting such a quantity
/// assigns the same value to every one of those nodes, so by the chain rule
/// its total derivative is the sum of the individual partials. Taking only the
/// first node silently omits whatever the others contribute.
///
/// Costs a single backward pass regardless of how many inputs are named.
/// Nodes absent from the graph contribute nothing.
pub fn gradient_sum(
    runtime: &mut Runtime,
    output: NodeId,
    inputs: &[NodeId],
) -> Result<f64, EvalError> {
    Ok(gradient_slices(runtime, output, &[inputs])?[0])
}

/// Compute grouped derivatives of one output in a single reverse pass.
///
/// Each inner slice names every graph node written by one natural input. In
/// smooth regions all groups are read from the same adjoint map. A group whose
/// requested path reaches an active piecewise boundary instead receives the
/// same composed right-hand directional derivative as [`gradient_sum`].
fn gradient_slices(
    runtime: &mut Runtime,
    output: NodeId,
    input_groups: &[&[NodeId]],
) -> Result<Vec<f64>, EvalError> {
    let (adjoints, order) = adjoints_with_order(runtime, output)?;
    let mut gradients = Vec::with_capacity(input_groups.len());

    for inputs in input_groups {
        let reverse_gradient = inputs
            .iter()
            .map(|input| adjoints.get(input).copied().unwrap_or(0.0))
            .sum();
        let gradient = if has_active_kink(runtime, &adjoints, inputs, &order) {
            forward_gradient(runtime, output, inputs)?
        } else {
            reverse_gradient
        };
        gradients.push(gradient);
    }

    Ok(gradients)
}

/// Compute grouped derivatives of one output.
///
/// This is the vector form of [`gradient_sum`]: one reverse traversal supplies
/// every smooth group, rather than repeating the traversal for each natural
/// input.
pub fn gradient_sums(
    runtime: &mut Runtime,
    output: NodeId,
    input_groups: &[Vec<NodeId>],
) -> Result<Vec<f64>, EvalError> {
    let input_slices: Vec<_> = input_groups.iter().map(Vec::as_slice).collect();
    gradient_slices(runtime, output, &input_slices)
}

fn has_active_kink(
    runtime: &Runtime,
    adjoints: &HashMap<NodeId, f64>,
    inputs: &[NodeId],
    order: &[NodeId],
) -> bool {
    let active_nodes: HashSet<NodeId> = order.iter().copied().collect();
    let mut reachable: HashSet<NodeId> = inputs
        .iter()
        .copied()
        .filter(|input| active_nodes.contains(input))
        .collect();
    let values = runtime.get_all_values();

    for &node_id in order {
        let Some(node) = runtime.graph().nodes.get(&node_id) else {
            continue;
        };
        if node
            .op
            .dependencies()
            .iter()
            .any(|dependency| reachable.contains(dependency))
        {
            reachable.insert(node_id);
        }

        if !reachable.contains(&node_id) || adjoints.get(&node_id).copied().unwrap_or(0.0) == 0.0 {
            continue;
        }

        let is_tie = match &node.op {
            Op::Max { left, right } | Op::Min { left, right } => {
                values.get(left).copied().unwrap_or(0.0)
                    == values.get(right).copied().unwrap_or(0.0)
            }
            Op::Abs { arg } => values.get(arg).copied().unwrap_or(0.0) == 0.0,
            Op::Clamp { arg, min, max } => {
                let value = values.get(arg).copied().unwrap_or(0.0);
                value == *min || value == *max
            }
            Op::BracketTax { table, income } => {
                let income = values.get(income).copied().unwrap_or(0.0);
                runtime.graph().tables.get(table).is_some_and(|table| {
                    table
                        .brackets
                        .get(runtime.filing_status())
                        .iter()
                        .any(|bracket| bracket.threshold.is_finite() && income == bracket.threshold)
                })
            }
            Op::PhaseOut {
                base,
                threshold,
                rate,
                agi,
            } => {
                let agi = values.get(agi).copied().unwrap_or(0.0);
                let threshold = *threshold.get(runtime.filing_status());
                agi == threshold || (*rate != 0.0 && agi == threshold + base / rate)
            }
            Op::IfPositive { cond, .. } => {
                reachable.contains(cond) && values.get(cond).copied().unwrap_or(0.0) == 0.0
            }
            _ => false,
        };
        if is_tie {
            return true;
        }
    }

    false
}

fn forward_gradient(
    runtime: &mut Runtime,
    output: NodeId,
    inputs: &[NodeId],
) -> Result<f64, EvalError> {
    let originals: Vec<_> = inputs
        .iter()
        .map(|input| (*input, original_value(runtime, *input)))
        .collect();
    let scale = originals
        .iter()
        .map(|(_, value)| value.abs())
        .fold(1.0, f64::max);
    let epsilon = (scale * f64::EPSILON.sqrt()).max(1e-4);

    for (input, original) in &originals {
        runtime.set_by_id(*input, original + epsilon);
    }
    let f_plus = runtime.eval_node(output);

    for (input, original) in &originals {
        runtime.set_by_id(*input, *original);
    }
    let f_original = runtime.eval_node(output);

    let f_plus = f_plus?;
    let f_original = f_original?;
    Ok((f_plus - f_original) / epsilon)
}

fn original_value(runtime: &Runtime, node_id: NodeId) -> f64 {
    runtime
        .input_value(node_id)
        .or_else(|| runtime.get_all_values().get(&node_id).copied())
        .unwrap_or(0.0)
}

/// Compute the derivative of the sum of several outputs with respect to a
/// quantity written into several input nodes.
pub fn gradient_sum_outputs(
    runtime: &mut Runtime,
    outputs: &[NodeId],
    inputs: &[NodeId],
) -> Result<f64, EvalError> {
    Ok(gradient_slices_outputs(runtime, outputs, &[inputs])?[0])
}

fn gradient_slices_outputs(
    runtime: &mut Runtime,
    outputs: &[NodeId],
    input_groups: &[&[NodeId]],
) -> Result<Vec<f64>, EvalError> {
    let mut gradients = vec![0.0; input_groups.len()];
    for output in outputs {
        for (total, partial) in
            gradients
                .iter_mut()
                .zip(gradient_slices(runtime, *output, input_groups)?)
        {
            *total += partial;
        }
    }
    Ok(gradients)
}

/// Compute grouped derivatives of a sum of outputs.
///
/// Costs one reverse traversal per output in smooth regions, independent of
/// the number of natural-input groups.
pub fn gradient_sums_outputs(
    runtime: &mut Runtime,
    outputs: &[NodeId],
    input_groups: &[Vec<NodeId>],
) -> Result<Vec<f64>, EvalError> {
    let input_slices: Vec<_> = input_groups.iter().map(Vec::as_slice).collect();
    gradient_slices_outputs(runtime, outputs, &input_slices)
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
            if v >= *min && v <= *max {
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

        Op::IfPositive {
            cond,
            then,
            otherwise,
        } => {
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
    runtime.eval_node(output)?;
    let original = original_value(runtime, input);

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
            imports: vec![],
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![2],
            invariants: vec![],
        }
    }

    fn coincident_max_graph() -> Graph {
        let nodes = [
            (0, Op::Input, Some("x")),
            (1, Op::Literal { value: 0.0 }, Some("zero")),
            (2, Op::Neg { arg: 0 }, Some("negative_x")),
            (3, Op::Max { left: 1, right: 0 }, Some("positive_x")),
            (
                4,
                Op::Max { left: 1, right: 2 },
                Some("positive_negative_x"),
            ),
            (5, Op::Literal { value: 11.0 }, Some("other_income")),
            (
                6,
                Op::Sub { left: 5, right: 4 },
                Some("income_above_remaining_threshold"),
            ),
            (
                7,
                Op::Max { left: 1, right: 6 },
                Some("positive_other_income"),
            ),
            (8, Op::Add { left: 3, right: 7 }, Some("combined")),
        ]
        .into_iter()
        .map(|(id, op, name)| {
            (
                id,
                Node {
                    id,
                    op,
                    name: name.map(str::to_string),
                },
            )
        })
        .collect();

        Graph {
            meta: None,
            nodes,
            imports: vec![],
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![8],
            invariants: vec![],
        }
    }

    fn limited_max_graph() -> Graph {
        let nodes = [
            (0, Op::Input, Some("x")),
            (1, Op::Literal { value: 0.0 }, Some("zero")),
            (2, Op::Max { left: 1, right: 0 }, Some("positive_x")),
            (3, Op::Literal { value: 10.0 }, Some("threshold")),
            (4, Op::Sub { left: 0, right: 3 }, Some("above_threshold")),
            (
                5,
                Op::Max { left: 1, right: 4 },
                Some("positive_above_threshold"),
            ),
            (6, Op::Min { left: 2, right: 5 }, Some("limited_amount")),
        ]
        .into_iter()
        .map(|(id, op, name)| {
            (
                id,
                Node {
                    id,
                    op,
                    name: name.map(str::to_string),
                },
            )
        })
        .collect();

        Graph {
            meta: None,
            nodes,
            imports: vec![],
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![6],
            invariants: vec![],
        }
    }

    fn disjoint_fanout_graph() -> Graph {
        let nodes = [
            (0, Op::Input, Some("federal_input")),
            (1, Op::Input, Some("state_input")),
            (2, Op::Literal { value: 0.0 }, Some("zero")),
            (3, Op::Max { left: 2, right: 0 }, Some("federal_output")),
            (4, Op::Literal { value: 10.0 }, Some("state_deduction")),
            (
                5,
                Op::Sub { left: 1, right: 4 },
                Some("state_taxable_income"),
            ),
            (6, Op::Max { left: 2, right: 5 }, Some("state_output")),
        ]
        .into_iter()
        .map(|(id, op, name)| {
            (
                id,
                Node {
                    id,
                    op,
                    name: name.map(str::to_string),
                },
            )
        })
        .collect();

        Graph {
            meta: None,
            nodes,
            imports: vec![],
            tables: HashMap::new(),
            inputs: vec![0, 1],
            outputs: vec![3, 6],
            invariants: vec![],
        }
    }

    fn minimum_graph() -> Graph {
        let nodes = [
            (0, Op::Input, Some("x")),
            (1, Op::Literal { value: 0.0 }, Some("zero")),
            (2, Op::Min { left: 0, right: 1 }, Some("nonpositive_x")),
        ]
        .into_iter()
        .map(|(id, op, name)| {
            (
                id,
                Node {
                    id,
                    op,
                    name: name.map(str::to_string),
                },
            )
        })
        .collect();

        Graph {
            meta: None,
            nodes,
            imports: vec![],
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
                    Bracket {
                        threshold: 10000.0,
                        rate: 0.10,
                    },
                    Bracket {
                        threshold: 40000.0,
                        rate: 0.20,
                    },
                    Bracket {
                        threshold: f64::INFINITY,
                        rate: 0.30,
                    },
                ]),
            },
        );

        Graph {
            meta: None,
            nodes,
            imports: vec![],
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
    fn test_gradient_sum_outputs() {
        let graph = simple_arithmetic_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 5.0);

        let grad = gradient_sum_outputs(&mut runtime, &[0, 2], &[0]).unwrap();
        assert_eq!(grad, 3.0);
    }

    #[test]
    fn test_gradient_sum_outputs_preserves_inputs_outside_each_output_path() {
        let graph = disjoint_fanout_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);
        runtime.set_by_id(1, 50.0);

        let grad = gradient_sum_outputs(&mut runtime, &[3, 6], &[0, 1]).unwrap();

        assert_eq!(grad, 2.0);
        assert_eq!(runtime.input_value(0), Some(0.0));
        assert_eq!(runtime.input_value(1), Some(50.0));
        assert_eq!(runtime.eval_node(6).unwrap(), 40.0);
    }

    #[test]
    fn test_grouped_gradients_preserve_inputs_and_kink_semantics() {
        let graph = disjoint_fanout_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);
        runtime.set_by_id(1, 50.0);

        let gradients =
            gradient_sums_outputs(&mut runtime, &[3, 6], &[vec![0, 1], vec![0], vec![1]]).unwrap();

        assert_eq!(gradients, vec![2.0, 1.0, 1.0]);
        assert_eq!(runtime.input_value(0), Some(0.0));
        assert_eq!(runtime.input_value(1), Some(50.0));
        assert_eq!(runtime.eval_node(6).unwrap(), 40.0);
    }

    #[test]
    fn test_grouped_gradient_matches_scalar_at_coincident_max_ties() {
        let graph = coincident_max_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);

        let gradients = gradient_sums(&mut runtime, 8, &[vec![0]]).unwrap();

        assert!((gradients[0] - 1.0).abs() < 1e-8);
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
    fn test_numerical_gradient_restores_an_uncached_input() {
        let graph = simple_arithmetic_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 5.0);

        let numerical = numerical_gradient(&mut runtime, 2, 0, 1e-6).unwrap();

        assert!((numerical - 2.0).abs() < 1e-6);
        assert_eq!(runtime.input_value(0), Some(5.0));
        assert_eq!(runtime.eval_node(2).unwrap(), 10.0);
    }

    #[test]
    fn test_gradient_uses_composed_right_derivative_at_a_max_kink() {
        let graph = coincident_max_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);

        let grad = gradient(&mut runtime, 3, 0).unwrap();
        assert_eq!(grad, 1.0);
    }

    #[test]
    fn test_gradient_uses_composed_right_derivative_at_a_min_kink() {
        let graph = minimum_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);

        let grad = gradient(&mut runtime, 2, 0).unwrap();
        assert_eq!(grad, 0.0);
    }

    #[test]
    fn test_gradient_survives_coincident_max_ties() {
        let graph = coincident_max_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);

        let analytical = gradient(&mut runtime, 8, 0).unwrap();
        let numerical = numerical_gradient(&mut runtime, 8, 0, 1e-6).unwrap();
        assert!((analytical - 1.0).abs() < 1e-8);
        assert!((analytical - numerical).abs() < 1e-6);
    }

    #[test]
    fn test_gradient_does_not_leak_through_limited_max_tie() {
        let graph = limited_max_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);
        runtime.set_by_id(0, 0.0);

        let analytical = gradient(&mut runtime, 6, 0).unwrap();
        let numerical = numerical_gradient(&mut runtime, 6, 0, 1e-6).unwrap();
        assert_eq!(analytical, 0.0);
        assert_eq!(analytical, numerical);
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

    /// A graph where the input fans out to two branches that recombine, so the
    /// gradient must sum both paths. `income` feeds the bracket tax (node 1) and
    /// a flat 3.8% surtax (node 3); the output (node 4) adds them. A backprop
    /// that follows only one edge out of the input gets the surtax or the bracket
    /// rate but not their sum — exactly the fan-out/chain-rule bug the numerical
    /// oracle catches.
    fn fanout_graph() -> Graph {
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
        nodes.insert(
            2,
            Node {
                id: 2,
                op: Op::Literal { value: 0.038 },
                name: Some("surtax_rate".to_string()),
            },
        );
        nodes.insert(
            3,
            Node {
                id: 3,
                op: Op::Mul { left: 0, right: 2 },
                name: Some("surtax".to_string()),
            },
        );
        nodes.insert(
            4,
            Node {
                id: 4,
                op: Op::Add { left: 1, right: 3 },
                name: Some("total".to_string()),
            },
        );

        let mut tables = HashMap::new();
        tables.insert(
            "federal".to_string(),
            BracketTable {
                brackets: ByStatus::uniform(vec![
                    Bracket {
                        threshold: 10000.0,
                        rate: 0.10,
                    },
                    Bracket {
                        threshold: 40000.0,
                        rate: 0.20,
                    },
                    Bracket {
                        threshold: f64::INFINITY,
                        rate: 0.30,
                    },
                ]),
            },
        );

        Graph {
            meta: None,
            nodes,
            imports: vec![],
            tables,
            inputs: vec![0],
            outputs: vec![4],
            invariants: vec![],
        }
    }

    proptest::proptest! {
        /// Autodiff agrees with a central finite difference at any income away
        /// from a bracket edge. Bracket tax is piecewise linear, so within a
        /// bracket the analytical gradient is the exact slope and the central
        /// difference recovers it; a +/-$50 guard keeps the +/-$1 difference from
        /// straddling the 10k/40k kinks. This is the property version of the
        /// fixed-point `test_gradient_vs_numerical`, over the fan-out graph.
        #[test]
        fn autodiff_matches_numerical_on_fanout(income in 1.0f64..100_000.0) {
            proptest::prop_assume!(
                (income - 10_000.0).abs() > 50.0 && (income - 40_000.0).abs() > 50.0
            );
            let graph = fanout_graph();
            let mut runtime = Runtime::new(&graph, FilingStatus::Single);
            runtime.set_by_id(0, income);

            let analytical = gradient(&mut runtime, 4, 0).unwrap();
            let numerical = numerical_gradient(&mut runtime, 4, 0, 1.0).unwrap();
            proptest::prop_assert!(
                (analytical - numerical).abs() < 1e-6,
                "income={income}: analytical={analytical} numerical={numerical}"
            );
        }
    }
}
