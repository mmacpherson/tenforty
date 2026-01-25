use crate::autodiff::gradient;
use crate::eval::{EvalError, Runtime};
use crate::graph::NodeId;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SolveError {
    #[error("Evaluation error: {0}")]
    Eval(#[from] EvalError),
    #[error("Failed to converge after {0} iterations")]
    NoConvergence(u32),
    #[error("Zero gradient encountered")]
    ZeroGradient,
}

pub struct SolverConfig {
    pub max_iterations: u32,
    pub tolerance: f64,
    pub min_step: f64,
    pub max_step: f64,
    pub lower_bound: Option<f64>,
    pub upper_bound: Option<f64>,
}

impl Default for SolverConfig {
    fn default() -> Self {
        SolverConfig {
            max_iterations: 100,
            tolerance: 1e-6,
            min_step: 1e-10,
            max_step: 1e10,
            lower_bound: Some(0.0), // Tax inputs are non-negative
            upper_bound: None,
        }
    }
}

/// Find the input value that produces a target output.
/// Uses Newton's method with autodiff gradients.
pub fn solve(
    runtime: &mut Runtime,
    output: NodeId,
    target: f64,
    input: NodeId,
    initial_guess: f64,
) -> Result<f64, SolveError> {
    solve_with_config(runtime, output, target, input, initial_guess, &SolverConfig::default())
}

pub fn solve_with_config(
    runtime: &mut Runtime,
    output: NodeId,
    target: f64,
    input: NodeId,
    initial_guess: f64,
    config: &SolverConfig,
) -> Result<f64, SolveError> {
    let mut x = initial_guess;

    for _ in 0..config.max_iterations {
        runtime.set_by_id(input, x);
        let y = runtime.eval_node(output)?;
        let error = y - target;

        if error.abs() < config.tolerance {
            return Ok(x);
        }

        let grad = gradient(runtime, output, input)?;

        if grad.abs() < config.min_step {
            return Err(SolveError::ZeroGradient);
        }

        let step = error / grad;
        let clamped_step = step.clamp(-config.max_step, config.max_step);
        x -= clamped_step;

        // Clamp to bounds
        if let Some(lb) = config.lower_bound {
            x = x.max(lb);
        }
        if let Some(ub) = config.upper_bound {
            x = x.min(ub);
        }
    }

    Err(SolveError::NoConvergence(config.max_iterations))
}

/// Binary search fallback for non-smooth regions.
/// Requires knowing bounds where the output crosses the target.
#[allow(clippy::too_many_arguments)]
pub fn solve_bisection(
    runtime: &mut Runtime,
    output: NodeId,
    target: f64,
    input: NodeId,
    mut low: f64,
    mut high: f64,
    tolerance: f64,
    max_iterations: u32,
) -> Result<f64, SolveError> {
    for _ in 0..max_iterations {
        let mid = (low + high) / 2.0;

        if (high - low) < tolerance {
            return Ok(mid);
        }

        runtime.set_by_id(input, mid);
        let y = runtime.eval_node(output)?;

        runtime.set_by_id(input, low);
        let y_low = runtime.eval_node(output)?;

        if (y - target).signum() == (y_low - target).signum() {
            low = mid;
        } else {
            high = mid;
        }
    }

    Err(SolveError::NoConvergence(max_iterations))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{Bracket, BracketTable, ByStatus, FilingStatus, Graph, Node, Op};
    use std::collections::HashMap;

    fn linear_graph() -> Graph {
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
        nodes.insert(
            3,
            Node {
                id: 3,
                op: Op::Literal { value: 10.0 },
                name: None,
            },
        );
        nodes.insert(
            4,
            Node {
                id: 4,
                op: Op::Add { left: 2, right: 3 },
                name: Some("output".to_string()),
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
    fn test_solve_linear() {
        let graph = linear_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        let x = solve(&mut runtime, 4, 100.0, 0, 0.0).unwrap();
        assert!((x - 45.0).abs() < 1e-6);
    }

    #[test]
    fn test_solve_tax() {
        let graph = tax_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        let tax_at_25k = 10000.0 * 0.10 + 15000.0 * 0.20;
        let income = solve(&mut runtime, 1, tax_at_25k, 0, 20000.0).unwrap();
        assert!((income - 25000.0).abs() < 1.0);
    }

    #[test]
    fn test_solve_bisection() {
        let graph = linear_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        let x = solve_bisection(&mut runtime, 4, 100.0, 0, 0.0, 100.0, 1e-6, 100).unwrap();
        assert!((x - 45.0).abs() < 1e-5);
    }

    #[test]
    fn test_solve_with_lower_bound() {
        let graph = linear_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        // y = 2x + 10, solve for target = 5 would give x = -2.5
        // but with lower bound of 0, should get x = 0
        let config = SolverConfig {
            lower_bound: Some(0.0),
            ..Default::default()
        };
        let result = solve_with_config(&mut runtime, 4, 5.0, 0, 10.0, &config);
        // Should not converge since target is unreachable with x >= 0
        assert!(result.is_err());
    }

    #[test]
    fn test_solve_respects_non_negativity() {
        let graph = tax_graph();
        let mut runtime = Runtime::new(&graph, FilingStatus::Single);

        // With default config (lower_bound = 0), solution should be non-negative
        let income = solve(&mut runtime, 1, 1000.0, 0, 50000.0).unwrap();
        assert!(income >= 0.0);
    }
}
