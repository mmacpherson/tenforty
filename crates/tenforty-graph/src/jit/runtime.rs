use crate::eval::{EvalError, Runtime};
use crate::graph::{FilingStatus, Graph, NodeId};
use crate::solver::SolveError;
use std::collections::HashMap;

use super::compiler::{CompiledBatchGraph, CompiledGraph};
use super::lower::BATCH_SIZE;

pub struct JitRuntime<'g> {
    compiled: CompiledGraph,
    graph: &'g Graph,
    slots: Vec<f64>,
    input_values: HashMap<NodeId, f64>,
    dirty: bool,
}

impl<'g> JitRuntime<'g> {
    pub fn new(compiled: CompiledGraph, graph: &'g Graph) -> Self {
        let num_slots = compiled.num_slots();
        JitRuntime {
            compiled,
            graph,
            slots: vec![0.0; num_slots],
            input_values: HashMap::new(),
            dirty: true,
        }
    }

    pub fn set(&mut self, name: &str, value: f64) -> Result<(), EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or_else(|| EvalError::InputNotSet(name.to_string()))?;
        self.set_by_id(node_id, value);
        Ok(())
    }

    pub fn set_by_id(&mut self, node_id: NodeId, value: f64) {
        self.input_values.insert(node_id, value);
        self.dirty = true;
    }

    fn ensure_evaluated(&mut self) {
        if !self.dirty {
            return;
        }

        self.slots.fill(0.0);

        for (&node_id, &value) in &self.input_values {
            if let Some(slot) = self.compiled.input_offset(node_id) {
                self.slots[slot] = value;
            }
        }

        unsafe {
            self.compiled
                .call(self.slots.as_ptr(), self.slots.as_mut_ptr());
        }

        self.dirty = false;
    }

    pub fn eval(&mut self, name: &str) -> Result<f64, EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or(EvalError::NodeNotFound(0))?;
        self.eval_node(node_id)
    }

    pub fn eval_node(&mut self, node_id: NodeId) -> Result<f64, EvalError> {
        self.ensure_evaluated();

        if let Some(slot) = self.compiled.output_offset(node_id) {
            return Ok(self.slots[slot]);
        }

        Err(EvalError::NodeNotFound(node_id))
    }

    pub fn gradient(&mut self, output_name: &str, input_name: &str) -> Result<f64, EvalError> {
        let output_id = self
            .graph
            .node_id_by_name(output_name)
            .ok_or(EvalError::NodeNotFound(0))?;
        let input_id = self
            .graph
            .node_id_by_name(input_name)
            .ok_or_else(|| EvalError::InputNotSet(input_name.to_string()))?;

        let mut interp = Runtime::new(self.graph, self.compiled.filing_status());
        for (&node_id, &value) in &self.input_values {
            interp.set_by_id(node_id, value);
        }
        crate::autodiff::gradient(&mut interp, output_id, input_id)
    }

    pub fn solve(
        &mut self,
        output_name: &str,
        target: f64,
        input_name: &str,
        initial_guess: f64,
    ) -> Result<f64, SolveError> {
        let output_id = self
            .graph
            .node_id_by_name(output_name)
            .ok_or(EvalError::NodeNotFound(0))?;
        let input_id = self
            .graph
            .node_id_by_name(input_name)
            .ok_or_else(|| EvalError::InputNotSet(input_name.to_string()))?;

        let mut interp = Runtime::new(self.graph, self.compiled.filing_status());
        for (&node_id, &value) in &self.input_values {
            interp.set_by_id(node_id, value);
        }
        crate::solver::solve(&mut interp, output_id, target, input_id, initial_guess)
    }

    pub fn filing_status(&self) -> FilingStatus {
        self.compiled.filing_status()
    }

    pub fn graph(&self) -> &Graph {
        self.graph
    }
}

pub struct JitBatchRuntime<'g> {
    compiled: CompiledBatchGraph,
    graph: &'g Graph,
    input_buffer: Vec<f64>,
    output_buffer: Vec<f64>,
    input_values: HashMap<NodeId, [f64; BATCH_SIZE]>,
    dirty: bool,
}

impl<'g> JitBatchRuntime<'g> {
    pub fn new(compiled: CompiledBatchGraph, graph: &'g Graph) -> Self {
        let input_buffer = vec![0.0; compiled.num_inputs() * BATCH_SIZE];
        let output_buffer = vec![0.0; compiled.num_outputs() * BATCH_SIZE];
        JitBatchRuntime {
            compiled,
            graph,
            input_buffer,
            output_buffer,
            input_values: HashMap::new(),
            dirty: true,
        }
    }

    pub fn set_batch(&mut self, name: &str, values: &[f64; BATCH_SIZE]) -> Result<(), EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or_else(|| EvalError::InputNotSet(name.to_string()))?;
        self.set_batch_by_id(node_id, *values);
        Ok(())
    }

    pub fn set_batch_by_id(&mut self, node_id: NodeId, values: [f64; BATCH_SIZE]) {
        self.input_values.insert(node_id, values);
        self.dirty = true;
    }

    fn ensure_evaluated(&mut self) {
        if !self.dirty {
            return;
        }

        self.input_buffer.fill(0.0);

        for (&node_id, &values) in &self.input_values {
            if let Some(slot) = self.compiled.input_offset(node_id) {
                let offset = slot * BATCH_SIZE;
                self.input_buffer[offset..offset + BATCH_SIZE].copy_from_slice(&values);
            }
        }

        unsafe {
            self.compiled
                .call(self.input_buffer.as_ptr(), self.output_buffer.as_mut_ptr());
        }

        self.dirty = false;
    }

    pub fn eval_batch(&mut self, name: &str) -> Result<[f64; BATCH_SIZE], EvalError> {
        let node_id = self
            .graph
            .node_id_by_name(name)
            .ok_or(EvalError::NodeNotFound(0))?;
        self.eval_batch_node(node_id)
    }

    pub fn eval_batch_node(&mut self, node_id: NodeId) -> Result<[f64; BATCH_SIZE], EvalError> {
        self.ensure_evaluated();

        if let Some(slot) = self.compiled.output_offset(node_id) {
            let offset = slot * BATCH_SIZE;
            let mut result = [0.0; BATCH_SIZE];
            result.copy_from_slice(&self.output_buffer[offset..offset + BATCH_SIZE]);
            return Ok(result);
        }

        Err(EvalError::NodeNotFound(node_id))
    }

    pub fn filing_status(&self) -> FilingStatus {
        self.compiled.filing_status()
    }

    pub fn graph(&self) -> &Graph {
        self.graph
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{Bracket, BracketTable, ByStatus, Graph, Node, Op};
    use crate::jit::{JitCompiler, BATCH_SIZE};
    use std::collections::HashMap;

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
    fn test_jit_basic_eval() {
        let graph = simple_graph();
        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
        let mut jit_rt = JitRuntime::new(compiled, &graph);

        jit_rt.set("income", 50000.0).unwrap();
        let taxable = jit_rt.eval("taxable_floor").unwrap();
        assert!((taxable - (50000.0 - 12950.0)).abs() < 1e-10);
    }

    #[test]
    fn test_jit_floor_at_zero() {
        let graph = simple_graph();
        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
        let mut jit_rt = JitRuntime::new(compiled, &graph);

        jit_rt.set("income", 5000.0).unwrap();
        let taxable = jit_rt.eval("taxable_floor").unwrap();
        assert!((taxable - 0.0).abs() < 1e-10);
    }

    #[test]
    fn test_jit_matches_interpreter() {
        let graph = simple_graph();

        let mut interp = Runtime::new(&graph, FilingStatus::Single);
        interp.set("income", 75000.0).unwrap();
        let interp_result = interp.eval("taxable_floor").unwrap();

        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
        let mut jit_rt = JitRuntime::new(compiled, &graph);
        jit_rt.set("income", 75000.0).unwrap();
        let jit_result = jit_rt.eval("taxable_floor").unwrap();

        assert!((interp_result - jit_result).abs() < 1e-10);
    }

    #[test]
    fn test_jit_bracket_tax() {
        let graph = tax_graph();

        let test_cases = [
            (5000.0, 500.0),
            (10000.0, 1000.0),
            (25000.0, 1000.0 + 15000.0 * 0.20),
            (50000.0, 1000.0 + 30000.0 * 0.20 + 10000.0 * 0.30),
        ];

        let compiler = JitCompiler::new().unwrap();

        for (income, expected_tax) in test_cases {
            let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
            let mut jit_rt = JitRuntime::new(compiled, &graph);
            jit_rt.set("income", income).unwrap();
            let jit_tax = jit_rt.eval("tax").unwrap();

            let mut interp = Runtime::new(&graph, FilingStatus::Single);
            interp.set("income", income).unwrap();
            let interp_tax = interp.eval("tax").unwrap();

            assert!(
                (jit_tax - interp_tax).abs() < 1e-10,
                "JIT/interpreter mismatch at income={}: jit={}, interp={}",
                income,
                jit_tax,
                interp_tax
            );
            assert!(
                (jit_tax - expected_tax).abs() < 1e-10,
                "Tax mismatch at income={}: got={}, expected={}",
                income,
                jit_tax,
                expected_tax
            );
        }
    }

    #[test]
    fn test_jit_gradient_via_hybrid() {
        let graph = tax_graph();

        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
        let mut jit_rt = JitRuntime::new(compiled, &graph);

        jit_rt.set("income", 25000.0).unwrap();
        let grad = jit_rt.gradient("tax", "income").unwrap();

        assert!(
            (grad - 0.20).abs() < 1e-10,
            "Expected marginal rate 0.20, got {}",
            grad
        );
    }

    #[test]
    fn test_jit_solve_via_hybrid() {
        let graph = tax_graph();

        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
        let mut jit_rt = JitRuntime::new(compiled, &graph);

        let target_tax = 1000.0 + 15000.0 * 0.20;
        let income = jit_rt.solve("tax", target_tax, "income", 20000.0).unwrap();

        assert!(
            (income - 25000.0).abs() < 1.0,
            "Expected income ~25000, got {}",
            income
        );
    }

    #[test]
    fn test_jit_with_demo_graph() {
        let json = include_str!("../../demo/graph_source.json");
        let graph = Graph::from_json(json).unwrap();

        let compiler = JitCompiler::new().unwrap();
        let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
        let mut jit_rt = JitRuntime::new(compiled, &graph);

        jit_rt.set("wages", 75000.0).unwrap();
        jit_rt.set("interest", 500.0).unwrap();
        jit_rt.set("dividends", 1000.0).unwrap();

        let jit_tax = jit_rt.eval("federal_tax").unwrap();

        let mut interp = Runtime::new(&graph, FilingStatus::Single);
        interp.set("wages", 75000.0).unwrap();
        interp.set("interest", 500.0).unwrap();
        interp.set("dividends", 1000.0).unwrap();

        let interp_tax = interp.eval("federal_tax").unwrap();

        assert!(
            (jit_tax - interp_tax).abs() < 1e-6,
            "JIT/interpreter mismatch: jit={}, interp={}",
            jit_tax,
            interp_tax
        );
    }

    #[test]
    fn test_simd_batch_matches_scalar() {
        let graph = tax_graph();
        let compiler = JitCompiler::new().unwrap();

        let incomes = [5000.0, 25000.0];
        let mut scalar_results = [0.0; BATCH_SIZE];

        for (i, &income) in incomes.iter().enumerate() {
            let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
            let mut jit_rt = JitRuntime::new(compiled, &graph);
            jit_rt.set("income", income).unwrap();
            scalar_results[i] = jit_rt.eval("tax").unwrap();
        }

        let batch_compiled = compiler
            .compile_batch(&graph, FilingStatus::Single)
            .unwrap();
        let mut batch_rt = super::JitBatchRuntime::new(batch_compiled, &graph);
        batch_rt.set_batch("income", &incomes).unwrap();
        let simd_results = batch_rt.eval_batch("tax").unwrap();

        for i in 0..BATCH_SIZE {
            assert!(
                (scalar_results[i] - simd_results[i]).abs() < 1e-10,
                "Mismatch at index {}: scalar={}, simd={}",
                i,
                scalar_results[i],
                simd_results[i]
            );
        }
    }

    #[test]
    fn test_simd_batch_with_demo_graph() {
        let json = include_str!("../../demo/graph_source.json");
        let graph = Graph::from_json(json).unwrap();
        let compiler = JitCompiler::new().unwrap();

        let wages = [50000.0, 100000.0];
        let interest = [100.0, 200.0];
        let dividends = [200.0, 0.0];

        let mut scalar_results = [0.0; BATCH_SIZE];
        for i in 0..BATCH_SIZE {
            let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
            let mut jit_rt = JitRuntime::new(compiled, &graph);
            jit_rt.set("wages", wages[i]).unwrap();
            jit_rt.set("interest", interest[i]).unwrap();
            jit_rt.set("dividends", dividends[i]).unwrap();
            scalar_results[i] = jit_rt.eval("federal_tax").unwrap();
        }

        let batch_compiled = compiler
            .compile_batch(&graph, FilingStatus::Single)
            .unwrap();
        let mut batch_rt = super::JitBatchRuntime::new(batch_compiled, &graph);
        batch_rt.set_batch("wages", &wages).unwrap();
        batch_rt.set_batch("interest", &interest).unwrap();
        batch_rt.set_batch("dividends", &dividends).unwrap();
        let simd_results = batch_rt.eval_batch("federal_tax").unwrap();

        for i in 0..BATCH_SIZE {
            assert!(
                (scalar_results[i] - simd_results[i]).abs() < 1e-6,
                "Mismatch at index {}: scalar={}, simd={}",
                i,
                scalar_results[i],
                simd_results[i]
            );
        }
    }

    #[test]
    fn test_simd_batch_edge_cases() {
        let graph = tax_graph();
        let compiler = JitCompiler::new().unwrap();

        let incomes = [0.0, -100.0];

        let batch_compiled = compiler
            .compile_batch(&graph, FilingStatus::Single)
            .unwrap();
        let mut batch_rt = super::JitBatchRuntime::new(batch_compiled, &graph);
        batch_rt.set_batch("income", &incomes).unwrap();
        let simd_results = batch_rt.eval_batch("tax").unwrap();

        for i in 0..BATCH_SIZE {
            let compiled = compiler.compile(&graph, FilingStatus::Single).unwrap();
            let mut jit_rt = JitRuntime::new(compiled, &graph);
            jit_rt.set("income", incomes[i]).unwrap();
            let scalar_result = jit_rt.eval("tax").unwrap();

            assert!(
                (scalar_result - simd_results[i]).abs() < 1e-6
                    || (scalar_result.is_nan() && simd_results[i].is_nan())
                    || (scalar_result.is_infinite() && simd_results[i].is_infinite()),
                "Mismatch at index {} (income={}): scalar={}, simd={}",
                i,
                incomes[i],
                scalar_result,
                simd_results[i]
            );
        }
    }
}
