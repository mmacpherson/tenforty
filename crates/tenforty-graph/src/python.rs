#[cfg(feature = "jit")]
use crate::jit::{JitCompiler, BATCH_SIZE};
use ouroboros::self_referencing;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyDict;
#[cfg(feature = "parallel")]
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;

use crate::eval::Runtime as RsRuntime;
use crate::graph::{FilingStatus as RsFilingStatus, Graph as RsGraph};
use crate::link::{GraphSet as RsGraphSet, UnresolvedImport as RsUnresolvedImport};
use crate::{autodiff, solver, viz};

#[pyclass]
#[derive(Clone)]
pub struct FilingStatus(RsFilingStatus);

#[pymethods]
impl FilingStatus {
    #[staticmethod]
    fn single() -> Self {
        FilingStatus(RsFilingStatus::Single)
    }

    #[staticmethod]
    fn married_joint() -> Self {
        FilingStatus(RsFilingStatus::MarriedJoint)
    }

    #[staticmethod]
    fn married_separate() -> Self {
        FilingStatus(RsFilingStatus::MarriedSeparate)
    }

    #[staticmethod]
    fn head_of_household() -> Self {
        FilingStatus(RsFilingStatus::HeadOfHousehold)
    }

    #[staticmethod]
    fn qualifying_widow() -> Self {
        FilingStatus(RsFilingStatus::QualifyingWidow)
    }

    #[staticmethod]
    fn from_str(s: &str) -> PyResult<Self> {
        match s.to_lowercase().replace(['-', ' '], "_").as_str() {
            "single" => Ok(FilingStatus(RsFilingStatus::Single)),
            "married_joint" | "married_filing_jointly" | "mfj" => {
                Ok(FilingStatus(RsFilingStatus::MarriedJoint))
            }
            "married_separate" | "married_filing_separately" | "mfs" => {
                Ok(FilingStatus(RsFilingStatus::MarriedSeparate))
            }
            "head_of_household" | "hoh" => Ok(FilingStatus(RsFilingStatus::HeadOfHousehold)),
            "qualifying_widow" | "qw" => Ok(FilingStatus(RsFilingStatus::QualifyingWidow)),
            _ => Err(PyValueError::new_err(format!(
                "Unknown filing status: {}",
                s
            ))),
        }
    }

    fn __str__(&self) -> String {
        self.__repr__()
    }

    fn __repr__(&self) -> String {
        match self.0 {
            RsFilingStatus::Single => "FilingStatus.single".to_string(),
            RsFilingStatus::MarriedJoint => "FilingStatus.married_joint".to_string(),
            RsFilingStatus::MarriedSeparate => "FilingStatus.married_separate".to_string(),
            RsFilingStatus::HeadOfHousehold => "FilingStatus.head_of_household".to_string(),
            RsFilingStatus::QualifyingWidow => "FilingStatus.qualifying_widow".to_string(),
        }
    }
}

fn parse_filing_status(s: &str) -> PyResult<RsFilingStatus> {
    match s.to_lowercase().replace(['-', ' '], "_").as_str() {
        "single" => Ok(RsFilingStatus::Single),
        "married_joint" | "married_filing_jointly" | "mfj" => Ok(RsFilingStatus::MarriedJoint),
        "married_separate" | "married_filing_separately" | "mfs" => {
            Ok(RsFilingStatus::MarriedSeparate)
        }
        "head_of_household" | "hoh" => Ok(RsFilingStatus::HeadOfHousehold),
        "qualifying_widow" | "qw" => Ok(RsFilingStatus::QualifyingWidow),
        _ => Err(PyValueError::new_err(format!(
            "Unknown filing status: {}",
            s
        ))),
    }
}

#[pyclass]
pub struct Graph {
    inner: Arc<RsGraph>,
}

impl Graph {
    fn eval_scenarios_interpreter(
        &self,
        scenarios: Vec<(usize, RsFilingStatus, HashMap<String, f64>)>,
        outputs: &[String],
    ) -> Vec<(
        usize,
        RsFilingStatus,
        HashMap<String, f64>,
        HashMap<String, f64>,
    )> {
        let graph = &self.inner;
        #[cfg(feature = "parallel")]
        let iter = scenarios.into_par_iter();
        #[cfg(not(feature = "parallel"))]
        let iter = scenarios.into_iter();

        iter.map(|(idx, status, input_vals)| {
            let mut rt = RsRuntime::new(graph, status);

            // Set all graph inputs to 0 first
            for input_id in &graph.inputs {
                rt.set_by_id(*input_id, 0.0);
            }

            // Set specified inputs
            for (name, &value) in &input_vals {
                let _ = rt.set(name, value);
            }

            // Evaluate outputs
            let mut output_vals = HashMap::new();
            for output in outputs {
                if let Ok(value) = rt.eval(output) {
                    output_vals.insert(output.clone(), value);
                }
            }
            (idx, status, input_vals, output_vals)
        })
        .collect()
    }
}

#[pymethods]
impl Graph {
    #[staticmethod]
    fn from_json(json: &str) -> PyResult<Self> {
        let graph = RsGraph::from_json(json)
            .map_err(|e| PyValueError::new_err(format!("Failed to parse graph: {}", e)))?;
        Ok(Graph {
            inner: Arc::new(graph),
        })
    }

    fn to_json(&self) -> PyResult<String> {
        self.inner
            .to_json()
            .map_err(|e| PyValueError::new_err(format!("Failed to serialize graph: {}", e)))
    }

    fn to_dot(&self) -> String {
        viz::to_dot(&self.inner)
    }

    fn node_names(&self) -> Vec<String> {
        self.inner
            .nodes
            .values()
            .filter_map(|n| n.name.clone())
            .collect()
    }

    fn input_names(&self) -> Vec<String> {
        self.inner
            .inputs
            .iter()
            .filter_map(|id| self.inner.nodes.get(id).and_then(|n| n.name.clone()))
            .collect()
    }

    fn output_names(&self) -> Vec<String> {
        self.inner
            .outputs
            .iter()
            .filter_map(|id| self.inner.nodes.get(id).and_then(|n| n.name.clone()))
            .collect()
    }

    fn imports(&self) -> Vec<(String, String, u16)> {
        self.inner
            .imports
            .iter()
            .map(|i| (i.form.clone(), i.line.clone(), i.year))
            .collect()
    }

    /// Evaluate scenarios defined by outer product of input ranges.
    ///
    /// Args:
    ///     inputs: Dict of input_name -> list of values to try
    ///     statuses: List of filing status strings
    ///     outputs: List of output node names to evaluate
    ///
    /// Returns:
    ///     Tuple of (status_col, input_cols, output_cols) where:
    ///       - status_col: list[str] of filing status strings
    ///       - input_cols: dict[str, list[float]] of input column data
    ///       - output_cols: dict[str, list[float]] of output column data
    fn eval_scenarios(
        &self,
        inputs: Bound<'_, PyDict>,
        statuses: Vec<String>,
        outputs: Vec<String>,
    ) -> PyResult<(
        Vec<String>,
        HashMap<String, Vec<f64>>,
        HashMap<String, Vec<f64>>,
    )> {
        // Extract inputs dict to HashMap
        let inputs: Vec<(String, Vec<f64>)> = inputs
            .iter()
            .map(|(k, v)| {
                let key: String = k.extract()?;
                let values: Vec<f64> = v.extract()?;
                Ok((key, values))
            })
            .collect::<PyResult<Vec<_>>>()?;

        // Parse filing statuses
        let parsed_statuses: Vec<RsFilingStatus> = statuses
            .iter()
            .map(|s| parse_filing_status(s))
            .collect::<PyResult<Vec<_>>>()?;

        // Build cartesian product of all inputs × statuses
        let input_names: Vec<&String> = inputs.iter().map(|(name, _)| name).collect();
        let input_values: Vec<&Vec<f64>> = inputs.iter().map(|(_, values)| values).collect();

        // Generate all combinations using itertools-style cartesian product
        let mut scenarios: Vec<(usize, RsFilingStatus, HashMap<String, f64>)> = Vec::new();
        let mut next_idx = 0usize;

        fn cartesian_recurse(
            input_names: &[&String],
            input_values: &[&Vec<f64>],
            statuses: &[RsFilingStatus],
            current: &mut HashMap<String, f64>,
            depth: usize,
            next_idx: &mut usize,
            scenarios: &mut Vec<(usize, RsFilingStatus, HashMap<String, f64>)>,
        ) {
            if depth == input_names.len() {
                for &status in statuses {
                    scenarios.push((*next_idx, status, current.clone()));
                    *next_idx += 1;
                }
            } else {
                for &val in input_values[depth] {
                    current.insert(input_names[depth].clone(), val);
                    cartesian_recurse(
                        input_names,
                        input_values,
                        statuses,
                        current,
                        depth + 1,
                        next_idx,
                        scenarios,
                    );
                }
            }
        }

        let mut current = HashMap::new();
        cartesian_recurse(
            &input_names,
            &input_values,
            &parsed_statuses,
            &mut current,
            0,
            &mut next_idx,
            &mut scenarios,
        );

        #[cfg(feature = "jit")]
        let graph = &self.inner;

        #[cfg(feature = "jit")]
        let results = {
            let mut results: Vec<
                Option<(RsFilingStatus, HashMap<String, f64>, HashMap<String, f64>)>,
            > = vec![None; scenarios.len()];
            let compiler = JitCompiler::new().ok();

            if let Some(compiler) = compiler {
                let mut status_to_index: HashMap<RsFilingStatus, usize> = HashMap::new();
                for (idx, status) in parsed_statuses.iter().enumerate() {
                    status_to_index.insert(*status, idx);
                }
                let mut blocks: Vec<Vec<(usize, RsFilingStatus, HashMap<String, f64>)>> =
                    vec![Vec::new(); parsed_statuses.len()];
                for scenario in &scenarios {
                    let status_index = status_to_index.get(&scenario.1).copied().unwrap_or(0);
                    blocks[status_index].push(scenario.clone());
                }

                for block in blocks {
                    if block.is_empty() {
                        continue;
                    }
                    let status = block[0].1;

                    // Compile for this status
                    if let Ok(compiled) = compiler.compile_batch(graph, status) {
                        // Pre-calculate input mappings
                        let input_mappings: Vec<(&String, Option<usize>)> = input_names
                            .iter()
                            .map(|name| {
                                if let Some(nid) = graph.node_id_by_name(name) {
                                    (*name, compiled.input_offset(nid))
                                } else {
                                    (*name, None)
                                }
                            })
                            .collect();

                        // Output mappings
                        let output_mappings: Vec<(&String, Option<usize>)> = outputs
                            .iter()
                            .map(|name| {
                                if let Some(nid) = graph.node_id_by_name(name) {
                                    (name, compiled.output_offset(nid))
                                } else {
                                    (name, None)
                                }
                            })
                            .collect();

                        // Process block in chunks
                        let process_chunk =
                            |chunk: &[(usize, RsFilingStatus, HashMap<String, f64>)]| {
                                let mut chunk_results = Vec::with_capacity(chunk.len());
                                let mut batch_inputs =
                                    vec![0.0; compiled.num_inputs() * BATCH_SIZE];
                                let mut batch_outputs =
                                    vec![0.0; compiled.num_outputs() * BATCH_SIZE];

                                // Fill inputs
                                for (lane, (_idx, _stat, scen_inputs)) in chunk.iter().enumerate() {
                                    for (name, slot_opt) in &input_mappings {
                                        if let Some(slot) = slot_opt {
                                            let val =
                                                scen_inputs.get(*name).copied().unwrap_or(0.0);
                                            batch_inputs[*slot * BATCH_SIZE + lane] = val;
                                        }
                                    }
                                }

                                // Call JIT
                                unsafe {
                                    // SAFETY: compiled.call expects valid pointers to contiguous
                                    // input/output buffers sized for num_inputs/num_outputs * BATCH_SIZE,
                                    // which we allocate above. The JIT does not retain these pointers.
                                    compiled
                                        .call(batch_inputs.as_ptr(), batch_outputs.as_mut_ptr());
                                }

                                // Read outputs
                                for (lane, (idx, stat, scen_inputs)) in chunk.iter().enumerate() {
                                    let mut output_vals = HashMap::new();
                                    for (name, slot_opt) in &output_mappings {
                                        let val = if let Some(slot) = slot_opt {
                                            batch_outputs[*slot * BATCH_SIZE + lane]
                                        } else {
                                            0.0
                                        };
                                        output_vals.insert((*name).clone(), val);
                                    }
                                    chunk_results.push((
                                        *idx,
                                        *stat,
                                        scen_inputs.clone(),
                                        output_vals,
                                    ));
                                }
                                chunk_results
                            };

                        #[cfg(feature = "parallel")]
                        let chunk_results: Vec<_> = block
                            .par_chunks(BATCH_SIZE)
                            .flat_map(process_chunk)
                            .collect();

                        #[cfg(not(feature = "parallel"))]
                        let chunk_results: Vec<_> =
                            block.chunks(BATCH_SIZE).flat_map(process_chunk).collect();

                        for (idx, stat, scen_inputs, output_vals) in chunk_results {
                            results[idx] = Some((stat, scen_inputs, output_vals));
                        }
                    } else {
                        // Compilation failed, fallback to interpreter for this block
                        let block_vec = block.to_vec();
                        let fallback = self.eval_scenarios_interpreter(block_vec, &outputs);
                        for (idx, stat, scen_inputs, output_vals) in fallback {
                            results[idx] = Some((stat, scen_inputs, output_vals));
                        }
                    }
                }
                Some(results)
            } else {
                None
            }
        };

        #[cfg(feature = "jit")]
        let results = results.unwrap_or_else(|| {
            let mut results: Vec<
                Option<(RsFilingStatus, HashMap<String, f64>, HashMap<String, f64>)>,
            > = vec![None; scenarios.len()];
            let fallback = self.eval_scenarios_interpreter(scenarios, &outputs);
            for (idx, stat, scen_inputs, output_vals) in fallback {
                results[idx] = Some((stat, scen_inputs, output_vals));
            }
            results
        });

        #[cfg(not(feature = "jit"))]
        let results = {
            let mut results: Vec<
                Option<(RsFilingStatus, HashMap<String, f64>, HashMap<String, f64>)>,
            > = vec![None; scenarios.len()];
            let fallback = self.eval_scenarios_interpreter(scenarios, &outputs);
            for (idx, stat, scen_inputs, output_vals) in fallback {
                results[idx] = Some((stat, scen_inputs, output_vals));
            }
            results
        };

        // Build column-oriented data
        let status_col: Vec<String> = results
            .iter()
            .map(|r| {
                let (s, _, _) = r.as_ref().expect("missing scenario result");
                match s {
                    RsFilingStatus::Single => "single".to_string(),
                    RsFilingStatus::MarriedJoint => "married_joint".to_string(),
                    RsFilingStatus::MarriedSeparate => "married_separate".to_string(),
                    RsFilingStatus::HeadOfHousehold => "head_of_household".to_string(),
                    RsFilingStatus::QualifyingWidow => "qualifying_widow".to_string(),
                }
            })
            .collect();

        // Input columns
        let mut input_cols: HashMap<String, Vec<f64>> = HashMap::new();
        for input_name in &input_names {
            let values: Vec<f64> = results
                .iter()
                .map(|r| {
                    let (_, inputs, _) = r.as_ref().expect("missing scenario result");
                    inputs.get(*input_name).copied().unwrap_or(0.0)
                })
                .collect();
            input_cols.insert((*input_name).clone(), values);
        }

        // Output columns
        let mut output_cols: HashMap<String, Vec<f64>> = HashMap::new();
        for output_name in &outputs {
            let values: Vec<f64> = results
                .iter()
                .map(|r| {
                    let (_, _, outputs) = r.as_ref().expect("missing scenario result");
                    outputs.get(output_name).copied().unwrap_or(0.0)
                })
                .collect();
            output_cols.insert(output_name.clone(), values);
        }

        Ok((status_col, input_cols, output_cols))
    }
}

#[self_referencing]
struct OwnedRuntime {
    graph: Arc<RsGraph>,
    #[borrows(graph)]
    #[covariant]
    runtime: RsRuntime<'this>,
}

#[pyclass]
pub struct Runtime {
    inner: OwnedRuntime,
}

#[pymethods]
impl Runtime {
    #[new]
    fn new(graph: &Graph, filing_status: &FilingStatus) -> Self {
        let graph_arc = Arc::clone(&graph.inner);
        let status = filing_status.0;
        Runtime {
            inner: OwnedRuntimeBuilder {
                graph: graph_arc,
                runtime_builder: |graph: &Arc<RsGraph>| RsRuntime::new(graph, status),
            }
            .build(),
        }
    }

    fn set(&mut self, name: &str, value: f64) -> PyResult<()> {
        self.inner.with_runtime_mut(|rt| {
            rt.set(name, value)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    fn eval(&mut self, name: &str) -> PyResult<f64> {
        self.inner.with_runtime_mut(|rt| {
            rt.eval(name)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    fn gradient(&mut self, output: &str, input: &str) -> PyResult<f64> {
        self.inner.with_runtime_mut(|rt| {
            let output_id = rt
                .graph()
                .node_id_by_name(output)
                .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", output)))?;
            let input_id = rt
                .graph()
                .node_id_by_name(input)
                .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", input)))?;

            autodiff::gradient(rt, output_id, input_id)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    #[pyo3(signature = (output, target, for_input, initial_guess=None))]
    fn solve(
        &mut self,
        output: &str,
        target: f64,
        for_input: &str,
        initial_guess: Option<f64>,
    ) -> PyResult<f64> {
        let guess = initial_guess.unwrap_or(target);
        self.inner.with_runtime_mut(|rt| {
            let graph = rt.graph();
            let output_id = graph
                .node_id_by_name(output)
                .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", output)))?;
            let input_id = graph
                .node_id_by_name(for_input)
                .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", for_input)))?;

            solver::solve(rt, output_id, target, input_id, guess)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }
}

#[pyclass]
pub struct UnresolvedImport {
    #[pyo3(get)]
    pub form: String,
    #[pyo3(get)]
    pub line: String,
    #[pyo3(get)]
    pub year: u16,
}

impl From<RsUnresolvedImport> for UnresolvedImport {
    fn from(u: RsUnresolvedImport) -> Self {
        UnresolvedImport {
            form: u.form,
            line: u.line,
            year: u.year,
        }
    }
}

#[pymethods]
impl UnresolvedImport {
    fn __repr__(&self) -> String {
        format!(
            "UnresolvedImport(form='{}', line='{}', year={})",
            self.form, self.line, self.year
        )
    }
}

#[pyclass]
#[derive(Default)]
pub struct GraphSet {
    inner: RsGraphSet,
}

#[pymethods]
impl GraphSet {
    #[new]
    fn new() -> Self {
        GraphSet {
            inner: RsGraphSet::new(),
        }
    }

    fn add(&mut self, form_id: &str, graph: &Graph) {
        self.inner.add_mut(form_id, (*graph.inner).clone());
    }

    fn forms(&self) -> Vec<String> {
        self.inner.forms().to_vec()
    }

    fn unresolved_imports(&self) -> Vec<UnresolvedImport> {
        self.inner
            .unresolved_imports()
            .into_iter()
            .map(UnresolvedImport::from)
            .collect()
    }

    fn link(&self) -> PyResult<Graph> {
        let linked = self
            .inner
            .link()
            .map_err(|e| PyValueError::new_err(format!("Link error: {}", e)))?;
        Ok(Graph {
            inner: Arc::new(linked),
        })
    }
}

#[pymodule]
fn graphlib(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<FilingStatus>()?;
    m.add_class::<Graph>()?;
    m.add_class::<Runtime>()?;
    m.add_class::<GraphSet>()?;
    m.add_class::<UnresolvedImport>()?;
    Ok(())
}
