use crate::graph::{FilingStatus, Graph, NodeId, Op};
use crate::primitives;
#[cfg(feature = "parallel")]
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Input '{0}' not set")]
    InputNotSet(String),
    #[error("Node {0} not found")]
    NodeNotFound(NodeId),
    #[error("Table '{0}' not found")]
    TableNotFound(String),
    #[error("Division by zero at node {0}")]
    DivisionByZero(NodeId),
}

pub struct Runtime<'g> {
    graph: &'g Graph,
    filing_status: FilingStatus,
    inputs: HashMap<NodeId, f64>,
    cache: HashMap<NodeId, f64>,
}

impl<'g> Runtime<'g> {
    pub fn new(graph: &'g Graph, filing_status: FilingStatus) -> Self {
        Runtime {
            graph,
            filing_status,
            inputs: HashMap::new(),
            cache: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: &str, value: f64) -> Result<(), EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or_else(|| EvalError::InputNotSet(name.to_string()))?;
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
            .ok_or(EvalError::NodeNotFound(0))?;
        self.eval_node(node_id)
    }

    pub fn eval_node(&mut self, node_id: NodeId) -> Result<f64, EvalError> {
        if let Some(&cached) = self.cache.get(&node_id) {
            return Ok(cached);
        }

        let node = self
            .graph
            .nodes
            .get(&node_id)
            .ok_or(EvalError::NodeNotFound(node_id))?;

        let result = self.eval_op(&node.op, node_id)?;
        self.cache.insert(node_id, result);
        Ok(result)
    }

    fn eval_op(&mut self, op: &Op, node_id: NodeId) -> Result<f64, EvalError> {
        match op {
            Op::Input | Op::Import { .. } => self
                .inputs
                .get(&node_id)
                .copied()
                .ok_or_else(|| EvalError::InputNotSet(format!("node_{}", node_id))),

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

/// Convenience version that takes named inputs/outputs
pub fn eval_batch_named(
    graph: &Arc<Graph>,
    scenarios: &[(FilingStatus, HashMap<String, f64>)],
    output_names: &[&str],
) -> Vec<Result<HashMap<String, f64>, EvalError>> {
    // Resolve output names to IDs once (for validation)
    let _output_ids: Vec<NodeId> = output_names
        .iter()
        .filter_map(|name| graph.node_id_by_name(name))
        .collect();

    #[cfg(feature = "parallel")]
    let iter = scenarios.par_iter();
    #[cfg(not(feature = "parallel"))]
    let iter = scenarios.iter();

    iter.map(|(status, named_inputs)| {
        let mut rt = Runtime::new(graph, *status);

        // Set inputs by name
        for (name, &value) in named_inputs {
            rt.set(name, value)?;
        }

        // Evaluate outputs
        let mut results = HashMap::new();
        for &name in output_names {
            if let Ok(value) = rt.eval(name) {
                results.insert(name.to_string(), value);
            }
        }

        Ok(results)
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
}
