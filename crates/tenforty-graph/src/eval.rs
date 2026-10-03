use crate::graph::{FilingStatus, Graph, GraphError, NodeId, Op};
use crate::primitives;
#[cfg(feature = "parallel")]
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum EvalError {
    #[error("Input '{0}' not set")]
    InputNotSet(String),
    #[error("Input '{name}' must be a finite number, got {value}")]
    NonFiniteInput { name: String, value: f64 },
    #[error("Node {0} not found")]
    NodeNotFound(NodeId),
    #[error("Node '{0}' not found")]
    NodeNameNotFound(String),
    #[error("Table '{0}' not found")]
    TableNotFound(String),
    #[error("Division by zero at node {0}")]
    DivisionByZero(NodeId),
    #[error("Cycle detected: {0:?}")]
    CycleDetected(Vec<String>),
    #[error(transparent)]
    Graph(#[from] GraphError),
}

pub(crate) fn ensure_finite(name: &str, value: f64) -> Result<(), EvalError> {
    if value.is_finite() {
        Ok(())
    } else {
        Err(EvalError::NonFiniteInput {
            name: name.to_string(),
            value,
        })
    }
}

pub struct Runtime<'g> {
    graph: &'g Graph,
    filing_status: FilingStatus,
    inputs: HashMap<NodeId, f64>,
    cache: HashMap<NodeId, f64>,
    visiting: HashSet<NodeId>,
    stack: Vec<NodeId>,
}

impl<'g> Runtime<'g> {
    pub fn new(graph: &'g Graph, filing_status: FilingStatus) -> Self {
        Runtime {
            graph,
            filing_status,
            inputs: HashMap::new(),
            cache: HashMap::new(),
            visiting: HashSet::new(),
            stack: Vec::new(),
        }
    }

    pub fn set(&mut self, name: &str, value: f64) -> Result<(), EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or_else(|| EvalError::InputNotSet(name.to_string()))?;
        ensure_finite(name, value)?;
        self.inputs.insert(node_id, value);
        self.cache.clear();
        Ok(())
    }

    pub fn set_by_id(&mut self, node_id: NodeId, value: f64) {
        self.inputs.insert(node_id, value);
        self.cache.clear();
    }

    pub fn eval(&mut self, name: &str) -> Result<f64, EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or_else(|| EvalError::NodeNameNotFound(name.to_string()))?;
        self.visiting.clear();
        self.stack.clear();
        self.eval_node(node_id)
    }

    pub fn eval_node(&mut self, node_id: NodeId) -> Result<f64, EvalError> {
        if let Some(&cached) = self.cache.get(&node_id) {
            return Ok(cached);
        }

        if self.visiting.contains(&node_id) {
            let start = self.stack.iter().position(|&id| id == node_id).unwrap_or(0);
            let mut cycle_ids: Vec<NodeId> = self.stack[start..].to_vec();
            cycle_ids.push(node_id);
            let cycle = cycle_ids
                .into_iter()
                .map(|id| {
                    self.graph
                        .nodes
                        .get(&id)
                        .and_then(|n| n.name.clone())
                        .unwrap_or_else(|| format!("node_{id}"))
                })
                .collect();
            return Err(EvalError::CycleDetected(cycle));
        }

        self.visiting.insert(node_id);
        self.stack.push(node_id);

        let node = self
            .graph
            .nodes
            .get(&node_id)
            .ok_or(EvalError::NodeNotFound(node_id))?;

        let result = self.eval_op(&node.op, node_id);
        self.stack.pop();
        self.visiting.remove(&node_id);

        let value = result?;
        self.cache.insert(node_id, value);
        Ok(value)
    }

    fn eval_op(&mut self, op: &Op, node_id: NodeId) -> Result<f64, EvalError> {
        op.validate(node_id)?;
        match op {
            // An input that was never set reads as 0. The resolved per-year
            // graph carries every state's inputs (~800); a given return only
            // provides a handful, and the rest are genuinely zero. Erroring
            // here would force callers to zero all inputs up front, which costs
            // ~800 writes per eval for no benefit. Mapping typos are still
            // caught at `set()`, where an unknown input *name* is rejected.
            Op::Input | Op::Import { .. } => Ok(self.inputs.get(&node_id).copied().unwrap_or(0.0)),

            Op::Literal { value } => Ok(*value),

            Op::Add { left, right } => {
                let l = self.eval_node(*left)?;
                let r = self.eval_node(*right)?;
                Ok(l + r)
            }

            Op::Sub { left, right } => {
                let l = self.eval_node(*left)?;
                let r = self.eval_node(*right)?;
                Ok(l - r)
            }

            Op::Mul { left, right } => {
                let l = self.eval_node(*left)?;
                let r = self.eval_node(*right)?;
                Ok(l * r)
            }

            Op::Div { left, right } => {
                let l = self.eval_node(*left)?;
                let r = self.eval_node(*right)?;
                if r == 0.0 {
                    return Err(EvalError::DivisionByZero(node_id));
                }
                Ok(l / r)
            }

            Op::Max { left, right } => {
                let l = self.eval_node(*left)?;
                let r = self.eval_node(*right)?;
                Ok(l.max(r))
            }

            Op::Min { left, right } => {
                let l = self.eval_node(*left)?;
                let r = self.eval_node(*right)?;
                Ok(l.min(r))
            }

            Op::Floor { arg } => {
                let v = self.eval_node(*arg)?;
                Ok(v.floor())
            }
            Op::TaxTableQuantize {
                arg,
                step,
                output_offset,
                mode,
            } => {
                let value = self.eval_node(*arg)?;
                Ok(primitives::tax_table_quantize(
                    value,
                    *step,
                    *output_offset,
                    *mode,
                ))
            }

            Op::Neg { arg } => {
                let v = self.eval_node(*arg)?;
                Ok(-v)
            }

            Op::Abs { arg } => {
                let v = self.eval_node(*arg)?;
                Ok(v.abs())
            }

            Op::Clamp { arg, min, max } => {
                let v = self.eval_node(*arg)?;
                Ok(v.clamp(*min, *max))
            }

            Op::BracketTax { table, income } => {
                let table = self
                    .graph
                    .tables
                    .get(table)
                    .ok_or_else(|| EvalError::TableNotFound(table.clone()))?;
                let brackets = table.brackets.get(self.filing_status);
                let inc = self.eval_node(*income)?;
                Ok(primitives::bracket_tax(brackets, inc))
            }

            Op::PhaseOut {
                base,
                threshold,
                rate,
                agi,
            } => {
                let thresh = *threshold.get(self.filing_status);
                let agi_val = self.eval_node(*agi)?;
                Ok(primitives::phase_out(*base, thresh, *rate, agi_val))
            }

            Op::ByStatus { values } => {
                let node_id = *values.get(self.filing_status);
                self.eval_node(node_id)
            }

            Op::IfPositive {
                cond,
                then,
                otherwise,
            } => {
                let c = self.eval_node(*cond)?;
                if c > 0.0 {
                    self.eval_node(*then)
                } else {
                    self.eval_node(*otherwise)
                }
            }
        }
    }

    pub fn get_all_values(&self) -> &HashMap<NodeId, f64> {
        &self.cache
    }

    pub fn input_value(&self, node_id: NodeId) -> Option<f64> {
        self.inputs.get(&node_id).copied()
    }

    pub fn filing_status(&self) -> FilingStatus {
        self.filing_status
    }

    pub fn graph(&self) -> &Graph {
        self.graph
    }
}

/// A scenario for batch evaluation
#[derive(Debug, Clone)]
pub struct Scenario {
    pub filing_status: FilingStatus,
    pub inputs: HashMap<NodeId, f64>,
}

/// Results from evaluating a scenario
#[derive(Debug, Clone)]
pub struct ScenarioResult {
    pub outputs: HashMap<NodeId, f64>,
}

/// Evaluate multiple scenarios in parallel using Rayon (when available).
/// Each scenario gets its own Runtime instance; the Graph is shared.
pub fn eval_batch(
    graph: &Arc<Graph>,
    scenarios: &[Scenario],
    output_nodes: &[NodeId],
) -> Vec<Result<ScenarioResult, EvalError>> {
    #[cfg(feature = "parallel")]
    let iter = scenarios.par_iter();
    #[cfg(not(feature = "parallel"))]
    let iter = scenarios.iter();

    iter.map(|scenario| {
        let mut rt = Runtime::new(graph, scenario.filing_status);

        // Set all inputs for this scenario
        for (&node_id, &value) in &scenario.inputs {
            rt.set_by_id(node_id, value);
        }

        // Evaluate requested outputs
        let mut outputs = HashMap::new();
        for &output_id in output_nodes {
            let value = rt.eval_node(output_id)?;
            outputs.insert(output_id, value);
        }

        Ok(ScenarioResult { outputs })
    })
    .collect()
}

/// An evaluation error raised by one row of a batch, tagged with that row's
/// index so a caller can tell which scenario failed.
#[derive(Debug, Error)]
#[error("batch row {row}: {source}")]
pub struct BatchRowError {
    pub row: usize,
    #[source]
    pub source: EvalError,
}

/// Evaluate one scenario by name, with the same semantics as a single
/// `Runtime`: an unknown input name, a missing output, or any evaluation error
/// is returned rather than read as 0.
pub fn eval_named_scenario<'a>(
    graph: &Graph,
    filing_status: FilingStatus,
    named_inputs: &HashMap<String, f64>,
    output_names: impl ExactSizeIterator<Item = &'a str>,
) -> Result<HashMap<String, f64>, EvalError> {
    let mut rt = Runtime::new(graph, filing_status);
    for (name, &value) in named_inputs {
        rt.set(name, value)?;
    }

    let mut results = HashMap::with_capacity(output_names.len());
    for name in output_names {
        let value = rt.eval(name)?;
        results.insert(name.to_string(), value);
    }
    Ok(results)
}

/// Convenience version that takes named inputs/outputs
pub fn eval_batch_named(
    graph: &Arc<Graph>,
    scenarios: &[(FilingStatus, HashMap<String, f64>)],
    output_names: &[&str],
) -> Vec<Result<HashMap<String, f64>, EvalError>> {
    #[cfg(feature = "parallel")]
    let iter = scenarios.par_iter();
    #[cfg(not(feature = "parallel"))]
    let iter = scenarios.iter();

    iter.map(|(status, named_inputs)| {
        eval_named_scenario(graph, *status, named_inputs, output_names.iter().copied())
    })
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Node;

    fn simple_graph() -> Graph {
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
                op: Op::Literal { value: 12950.0 },
                name: Some("std_ded".to_string()),
            },
        );
        nodes.insert(
            2,
            Node {
                id: 2,
                op: Op::Sub { left: 0, right: 1 },
                name: Some("taxable".to_string()),
            },
        );
        nodes.insert(
            3,
            Node {
                id: 3,
                op: Op::Literal { value: 0.0 },
                name: Some("zero".to_string()),
            },
        );
        nodes.insert(
            4,
            Node {
                id: 4,
                op: Op::Max { left: 2, right: 3 },
                name: Some("taxable_floor".to_string()),
            },
        );

        Graph {
            meta: None,
            nodes,
            imports: vec![],
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![4],
            invariants: vec![],
        }
    }

    #[test]
    fn test_basic_eval() {
        let graph = simple_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        runtime.set("income", 50000.0).unwrap();
        let taxable = runtime.eval("taxable_floor").unwrap();
        assert_eq!(taxable, 50000.0 - 12950.0);
    }

    #[test]
    fn test_floor_at_zero() {
        let graph = simple_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        runtime.set("income", 5000.0).unwrap();
        let taxable = runtime.eval("taxable_floor").unwrap();
        assert_eq!(taxable, 0.0);
    }

    #[test]
    fn test_set_rejects_non_finite_inputs() {
        let graph = simple_graph();
        for bad in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            let mut runtime = Runtime::new(&graph, FilingStatus::Single);
            let err = runtime.set("income", bad).unwrap_err();
            assert!(
                matches!(&err, EvalError::NonFiniteInput { name, .. } if name == "income"),
                "unexpected error for {bad}: {err:?}"
            );
            assert!(err.to_string().contains("income"));
            assert!(err.to_string().contains("finite"));
        }
    }

    #[test]
    fn test_rejected_set_keeps_previous_value() {
        let graph = simple_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        runtime.set("income", 50000.0).unwrap();
        assert!(runtime.set("income", f64::NAN).is_err());
        let taxable = runtime.eval("taxable_floor").unwrap();
        assert_eq!(taxable, 50000.0 - 12950.0);
    }

    #[test]
    fn test_set_reports_unknown_name_before_finiteness() {
        let graph = simple_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        let err = runtime.set("no_such_input", f64::NAN).unwrap_err();
        assert!(matches!(err, EvalError::InputNotSet(_)));
    }

    #[test]
    fn test_eval_batch_named_rejects_non_finite_scenario() {
        let graph = Arc::new(simple_graph());
        let scenarios = vec![
            (
                FilingStatus::Single,
                HashMap::from([("income".to_string(), 50000.0)]),
            ),
            (
                FilingStatus::Single,
                HashMap::from([("income".to_string(), f64::NAN)]),
            ),
        ];

        let results = eval_batch_named(&graph, &scenarios, &["taxable_floor"]);
        assert!(results[0].is_ok());
        assert!(matches!(results[1], Err(EvalError::NonFiniteInput { .. })));
    }

    fn ratio_graph() -> Graph {
        let mut nodes = HashMap::new();
        for (id, name) in [(0, "numerator"), (1, "denominator")] {
            nodes.insert(
                id,
                Node {
                    id,
                    op: Op::Input,
                    name: Some(name.to_string()),
                },
            );
        }
        nodes.insert(
            2,
            Node {
                id: 2,
                op: Op::Div { left: 0, right: 1 },
                name: Some("ratio".to_string()),
            },
        );
        nodes.insert(
            3,
            Node {
                id: 3,
                op: Op::BracketTax {
                    table: "absent_table".to_string(),
                    income: 0,
                },
                name: Some("tabled".to_string()),
            },
        );

        Graph {
            meta: None,
            nodes,
            imports: vec![],
            tables: HashMap::new(),
            inputs: vec![0, 1],
            outputs: vec![2],
            invariants: vec![],
        }
    }

    fn ratio_row(numerator: f64, denominator: f64) -> (FilingStatus, HashMap<String, f64>) {
        (
            FilingStatus::Single,
            HashMap::from([
                ("numerator".to_string(), numerator),
                ("denominator".to_string(), denominator),
            ]),
        )
    }

    #[test]
    fn batch_named_propagates_division_by_zero_on_the_failing_row_only() {
        let graph = Arc::new(ratio_graph());
        let rows = [
            ratio_row(10.0, 2.0),
            ratio_row(10.0, 0.0),
            ratio_row(9.0, 3.0),
        ];

        let results = eval_batch_named(&graph, &rows, &["ratio"]);

        assert_eq!(results[0].as_ref().unwrap()["ratio"], 5.0);
        assert!(matches!(results[1], Err(EvalError::DivisionByZero(2))));
        assert_eq!(results[2].as_ref().unwrap()["ratio"], 3.0);
    }

    #[test]
    fn batch_named_matches_single_runtime_errors() {
        let graph = Arc::new(ratio_graph());
        let mut single = Runtime::new(&graph, FilingStatus::Single);
        single.set("numerator", 10.0).unwrap();
        single.set("denominator", 0.0).unwrap();
        let single_err = single.eval("ratio").unwrap_err();

        let batch = eval_batch_named(&graph, &[ratio_row(10.0, 0.0)], &["ratio"]);
        let batch_err = batch.into_iter().next().unwrap().unwrap_err();

        assert_eq!(batch_err.to_string(), single_err.to_string());
    }

    #[test]
    fn batch_named_rejects_unknown_input_name() {
        let graph = Arc::new(ratio_graph());
        let (status, mut inputs) = ratio_row(10.0, 2.0);
        inputs.insert("numeratr".to_string(), 1.0);

        let results = eval_batch_named(&graph, &[(status, inputs)], &["ratio"]);

        assert!(matches!(&results[0], Err(EvalError::InputNotSet(name)) if name == "numeratr"));
    }

    #[test]
    fn batch_named_rejects_unknown_output_name() {
        let graph = Arc::new(ratio_graph());

        let results = eval_batch_named(&graph, &[ratio_row(10.0, 2.0)], &["ratio", "nope"]);

        assert!(matches!(&results[0], Err(EvalError::NodeNameNotFound(name)) if name == "nope"));
    }

    #[test]
    fn batch_named_propagates_missing_table() {
        let graph = Arc::new(ratio_graph());

        let results = eval_batch_named(&graph, &[ratio_row(10.0, 2.0)], &["tabled"]);

        assert!(
            matches!(&results[0], Err(EvalError::TableNotFound(name)) if name == "absent_table")
        );
    }

    #[test]
    fn batch_row_error_names_row_and_cause() {
        let err = BatchRowError {
            row: 7,
            source: EvalError::DivisionByZero(2),
        };

        assert_eq!(err.to_string(), "batch row 7: Division by zero at node 2");
    }
}
