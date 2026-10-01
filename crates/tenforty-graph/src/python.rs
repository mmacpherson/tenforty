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

use crate::eval::{eval_named_scenario, BatchRowError, Runtime as RsRuntime};
use crate::graph::{FilingStatus as RsFilingStatus, Graph as RsGraph};
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

type ScenarioRow = (usize, RsFilingStatus, HashMap<String, f64>);
type EvaluatedRow = (
    usize,
    RsFilingStatus,
    HashMap<String, f64>,
    HashMap<String, f64>,
);

fn batch_row_error_to_py(err: BatchRowError) -> PyErr {
    PyValueError::new_err(err.to_string())
}

type PlacedRow = Option<(RsFilingStatus, HashMap<String, f64>, HashMap<String, f64>)>;

fn place_by_row(evaluated: Vec<EvaluatedRow>, row_count: usize) -> Vec<PlacedRow> {
    let mut results: Vec<PlacedRow> = vec![None; row_count];
    for (idx, stat, scen_inputs, output_vals) in evaluated {
        results[idx] = Some((stat, scen_inputs, output_vals));
    }
    results
}

fn lowest_row_error(errors: impl IntoIterator<Item = BatchRowError>) -> Option<BatchRowError> {
    errors.into_iter().min_by_key(|err| err.row)
}

impl Graph {
    fn eval_scenarios_interpreter(
        &self,
        scenarios: Vec<ScenarioRow>,
        outputs: &[String],
    ) -> Result<Vec<EvaluatedRow>, BatchRowError> {
        let graph = &self.inner;
        #[cfg(feature = "parallel")]
        let iter = scenarios.into_par_iter();
        #[cfg(not(feature = "parallel"))]
        let iter = scenarios.into_iter();

        // Unset inputs default to 0 in eval, so only the provided ones need
        // setting — no per-scenario zero-fill over all ~800 graph inputs.
        let evaluated: Vec<Result<EvaluatedRow, BatchRowError>> = iter
            .map(|(idx, status, input_vals)| {
                eval_named_scenario(
                    graph,
                    status,
                    &input_vals,
                    outputs.iter().map(String::as_str),
                )
                .map(|output_vals| (idx, status, input_vals, output_vals))
                .map_err(|source| BatchRowError { row: idx, source })
            })
            .collect();

        let mut rows = Vec::with_capacity(evaluated.len());
        let mut errors = Vec::new();
        for result in evaluated {
            match result {
                Ok(row) => rows.push(row),
                Err(err) => errors.push(err),
            }
        }
        match lowest_row_error(errors) {
            Some(err) => Err(err),
            None => Ok(rows),
        }
    }

    fn evaluate_and_columnize(
        &self,
        scenarios: Vec<ScenarioRow>,
        input_names: &[&String],
        outputs: &[String],
    ) -> PyResult<(
        Vec<String>,
        HashMap<String, Vec<f64>>,
        HashMap<String, Vec<f64>>,
    )> {
        // Derive unique statuses from the scenarios themselves
        let mut unique_statuses: Vec<RsFilingStatus> = Vec::new();
        let mut seen: HashMap<RsFilingStatus, usize> = HashMap::new();
        for (_, status, _) in &scenarios {
            if !seen.contains_key(status) {
                seen.insert(*status, unique_statuses.len());
                unique_statuses.push(*status);
            }
        }

        #[cfg(feature = "jit")]
        let graph = &self.inner;

        // The JIT has no error channel: it reads an unknown name as 0, so it
        // only runs when every requested name resolves, and otherwise the
        // interpreter raises what `Runtime` would. A block whose slice reaches
        // a division, or that fails to compile (a missing table), falls back
        // to the interpreter the same way.
        #[cfg(feature = "jit")]
        let jit_names_resolve = input_names
            .iter()
            .map(|name| name.as_str())
            .chain(outputs.iter().map(String::as_str))
            .all(|name| graph.node_id_by_name(name).is_some());

        #[cfg(feature = "jit")]
        let results = {
            let mut results: Vec<PlacedRow> = vec![None; scenarios.len()];
            let compiler = if jit_names_resolve {
                JitCompiler::new().ok()
            } else {
                None
            };

            if let Some(compiler) = compiler {
                // Slice the JIT to just the requested outputs: compile only the
                // nodes those outputs depend on, not the whole ~3000-node graph.
                let output_ids: Vec<crate::graph::NodeId> = outputs
                    .iter()
                    .filter_map(|name| graph.node_id_by_name(name))
                    .collect();

                let mut blocks: Vec<Vec<ScenarioRow>> = vec![Vec::new(); unique_statuses.len()];
                for scenario in &scenarios {
                    let status_index = seen.get(&scenario.1).copied().unwrap_or(0);
                    blocks[status_index].push(scenario.clone());
                }

                let mut block_errors: Vec<BatchRowError> = Vec::new();
                for block in blocks {
                    if block.is_empty() {
                        continue;
                    }
                    let status = block[0].1;

                    if let Ok(Some(compiled)) =
                        compiler.compile_batch_strict(graph, status, &output_ids)
                    {
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
                        let process_chunk = |chunk: &[ScenarioRow]| {
                            let mut chunk_results = Vec::with_capacity(chunk.len());
                            let mut batch_inputs = vec![0.0; compiled.num_inputs() * BATCH_SIZE];
                            let mut batch_outputs = vec![0.0; compiled.num_outputs() * BATCH_SIZE];

                            // Fill inputs
                            for (lane, (_idx, _stat, scen_inputs)) in chunk.iter().enumerate() {
                                for (name, slot_opt) in &input_mappings {
                                    if let Some(slot) = slot_opt {
                                        let val = scen_inputs.get(*name).copied().unwrap_or(0.0);
                                        batch_inputs[*slot * BATCH_SIZE + lane] = val;
                                    }
                                }
                            }

                            // Call JIT
                            unsafe {
                                // SAFETY: compiled.call expects valid pointers to contiguous
                                // input/output buffers sized for num_inputs/num_outputs * BATCH_SIZE,
                                // which we allocate above. The JIT does not retain these pointers.
                                compiled.call(batch_inputs.as_ptr(), batch_outputs.as_mut_ptr());
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
                                chunk_results.push((*idx, *stat, scen_inputs.clone(), output_vals));
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
                        // Declined or failed to compile: interpret this block
                        match self.eval_scenarios_interpreter(block, outputs) {
                            Ok(fallback) => {
                                for (idx, stat, scen_inputs, output_vals) in fallback {
                                    results[idx] = Some((stat, scen_inputs, output_vals));
                                }
                            }
                            Err(err) => block_errors.push(err),
                        }
                    }
                }
                if let Some(err) = lowest_row_error(block_errors) {
                    return Err(batch_row_error_to_py(err));
                }
                Some(results)
            } else {
                None
            }
        };

        #[cfg(feature = "jit")]
        let results = match results {
            Some(results) => results,
            None => {
                let row_count = scenarios.len();
                let evaluated = self
                    .eval_scenarios_interpreter(scenarios, outputs)
                    .map_err(batch_row_error_to_py)?;
                place_by_row(evaluated, row_count)
            }
        };

        #[cfg(not(feature = "jit"))]
        let results = {
            let row_count = scenarios.len();
            let evaluated = self
                .eval_scenarios_interpreter(scenarios, outputs)
                .map_err(batch_row_error_to_py)?;
            place_by_row(evaluated, row_count)
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
        for input_name in input_names {
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
        for output_name in outputs {
            let values: Vec<f64> = results
                .iter()
                .map(|r| {
                    let (_, _, outputs) = r.as_ref().expect("missing scenario result");
                    *outputs
                        .get(output_name)
                        .expect("evaluated row missing a requested output")
                })
                .collect();
            output_cols.insert(output_name.clone(), values);
        }

        Ok((status_col, input_cols, output_cols))
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

        self.evaluate_and_columnize(scenarios, &input_names, &outputs)
    }

    /// Evaluate scenarios defined by zipping input columns (not crossing).
    ///
    /// All input vectors and statuses must have the same length N. Each row i
    /// is evaluated with inputs[i] and statuses[i].
    #[pyo3(signature = (inputs, statuses, outputs))]
    fn eval_scenarios_zip(
        &self,
        inputs: Bound<'_, PyDict>,
        statuses: Vec<String>,
        outputs: Vec<String>,
    ) -> PyResult<(
        Vec<String>,
        HashMap<String, Vec<f64>>,
        HashMap<String, Vec<f64>>,
    )> {
        let n = statuses.len();

        // Extract and validate inputs
        let inputs: Vec<(String, Vec<f64>)> = inputs
            .iter()
            .map(|(k, v)| {
                let key: String = k.extract()?;
                let values: Vec<f64> = v.extract()?;
                Ok((key, values))
            })
            .collect::<PyResult<Vec<_>>>()?;

        for (name, values) in &inputs {
            if values.len() != n {
                return Err(PyValueError::new_err(format!(
                    "Input '{}' has length {} but statuses has length {}",
                    name,
                    values.len(),
                    n
                )));
            }
        }

        // Parse per-row filing statuses
        let parsed_statuses: Vec<RsFilingStatus> = statuses
            .iter()
            .map(|s| parse_filing_status(s))
            .collect::<PyResult<Vec<_>>>()?;

        // Build scenarios by zipping
        let input_names: Vec<&String> = inputs.iter().map(|(name, _)| name).collect();
        let mut scenarios: Vec<(usize, RsFilingStatus, HashMap<String, f64>)> =
            Vec::with_capacity(n);

        for i in 0..n {
            let mut row_inputs = HashMap::new();
            for (name, values) in &inputs {
                row_inputs.insert(name.clone(), values[i]);
            }
            scenarios.push((i, parsed_statuses[i], row_inputs));
        }

        self.evaluate_and_columnize(scenarios, &input_names, &outputs)
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

    /// Total derivative of `output` with respect to a quantity written into
    /// several input nodes at once.
    ///
    /// A natural input such as wage income reaches more than one node, so its
    /// derivative is the sum of the partials over every node it is written to.
    /// Costs one backward pass regardless of how many are named. Nodes absent
    /// from the graph contribute nothing, which lets callers pass the full
    /// fan-out without first checking which forms are present in the graph.
    fn gradient_multi(&mut self, output: &str, inputs: Vec<String>) -> PyResult<f64> {
        self.inner.with_runtime_mut(|rt| {
            let output_id = rt
                .graph()
                .node_id_by_name(output)
                .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", output)))?;
            let input_ids: Vec<_> = inputs
                .iter()
                .filter_map(|name| rt.graph().node_id_by_name(name))
                .collect();
            if input_ids.is_empty() {
                return Err(PyValueError::new_err(format!(
                    "None of the input nodes were found: {}",
                    inputs.join(", ")
                )));
            }

            autodiff::gradient_sum(rt, output_id, &input_ids)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    /// Total derivative of the sum of `outputs` with respect to a quantity
    /// written into several input nodes.
    fn gradient_multi_output(
        &mut self,
        outputs: Vec<String>,
        inputs: Vec<String>,
    ) -> PyResult<f64> {
        self.inner.with_runtime_mut(|rt| {
            if outputs.is_empty() {
                return Err(PyValueError::new_err(
                    "At least one output node is required",
                ));
            }
            let output_ids = outputs
                .iter()
                .map(|name| {
                    rt.graph()
                        .node_id_by_name(name)
                        .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", name)))
                })
                .collect::<PyResult<Vec<_>>>()?;
            let input_ids: Vec<_> = inputs
                .iter()
                .filter_map(|name| rt.graph().node_id_by_name(name))
                .collect();
            if input_ids.is_empty() {
                return Err(PyValueError::new_err(format!(
                    "None of the input nodes were found: {}",
                    inputs.join(", ")
                )));
            }

            autodiff::gradient_sum_outputs(rt, &output_ids, &input_ids)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    /// Grouped total derivatives of the sum of `outputs`.
    ///
    /// Each inner input vector is one natural input's complete graph fan-out.
    /// Smooth groups share one reverse traversal per output; groups crossing
    /// an active kink preserve `gradient_multi_output`'s composed right-hand
    /// derivative convention.
    fn gradients_multi_output(
        &mut self,
        outputs: Vec<String>,
        input_groups: Vec<Vec<String>>,
    ) -> PyResult<Vec<f64>> {
        self.inner.with_runtime_mut(|rt| {
            if outputs.is_empty() {
                return Err(PyValueError::new_err(
                    "At least one output node is required",
                ));
            }
            let output_ids = outputs
                .iter()
                .map(|name| {
                    rt.graph()
                        .node_id_by_name(name)
                        .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", name)))
                })
                .collect::<PyResult<Vec<_>>>()?;
            let input_ids = input_groups
                .iter()
                .map(|inputs| {
                    let ids: Vec<_> = inputs
                        .iter()
                        .filter_map(|name| rt.graph().node_id_by_name(name))
                        .collect();
                    if ids.is_empty() {
                        Err(PyValueError::new_err(format!(
                            "None of the input nodes were found: {}",
                            inputs.join(", ")
                        )))
                    } else {
                        Ok(ids)
                    }
                })
                .collect::<PyResult<Vec<_>>>()?;

            autodiff::gradient_sums_outputs(rt, &output_ids, &input_ids)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    /// Solve for the value of a quantity written into several input nodes.
    ///
    /// Each Newton step assigns the trial value to every named node and steps
    /// on their combined derivative. See `gradient_multi` for why.
    #[pyo3(signature = (output, target, for_inputs, initial_guess=None))]
    fn solve_multi(
        &mut self,
        output: &str,
        target: f64,
        for_inputs: Vec<String>,
        initial_guess: Option<f64>,
    ) -> PyResult<f64> {
        let guess = initial_guess.unwrap_or(target);
        self.inner.with_runtime_mut(|rt| {
            let output_id = rt
                .graph()
                .node_id_by_name(output)
                .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", output)))?;
            let input_ids: Vec<_> = for_inputs
                .iter()
                .filter_map(|name| rt.graph().node_id_by_name(name))
                .collect();
            if input_ids.is_empty() {
                return Err(PyValueError::new_err(format!(
                    "None of the input nodes were found: {}",
                    for_inputs.join(", ")
                )));
            }

            solver::solve_multi(rt, output_id, target, &input_ids, guess)
                .map_err(|e| PyValueError::new_err(format!("{}", e)))
        })
    }

    /// Solve for an input quantity against the sum of several output nodes.
    #[pyo3(signature = (outputs, target, for_inputs, initial_guess=None))]
    fn solve_multi_output(
        &mut self,
        outputs: Vec<String>,
        target: f64,
        for_inputs: Vec<String>,
        initial_guess: Option<f64>,
    ) -> PyResult<f64> {
        let guess = initial_guess.unwrap_or(target);
        self.inner.with_runtime_mut(|rt| {
            if outputs.is_empty() {
                return Err(PyValueError::new_err(
                    "At least one output node is required",
                ));
            }
            let output_ids = outputs
                .iter()
                .map(|name| {
                    rt.graph()
                        .node_id_by_name(name)
                        .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", name)))
                })
                .collect::<PyResult<Vec<_>>>()?;
            let input_ids: Vec<_> = for_inputs
                .iter()
                .filter_map(|name| rt.graph().node_id_by_name(name))
                .collect();
            if input_ids.is_empty() {
                return Err(PyValueError::new_err(format!(
                    "None of the input nodes were found: {}",
                    for_inputs.join(", ")
                )));
            }

            solver::solve_multi_output(rt, &output_ids, target, &input_ids, guess)
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

#[pymodule]
fn graphlib(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<FilingStatus>()?;
    m.add_class::<Graph>()?;
    m.add_class::<Runtime>()?;
    Ok(())
}
