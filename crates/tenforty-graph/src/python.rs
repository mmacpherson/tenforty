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
        let inputs: HashMap<String, Vec<f64>> = inputs
            .iter()
            .map(|(k, v)| {
                let key: String = k.extract()?;
                let values: Vec<f64> = v.extract()?;
                Ok((key, values))
            })
            .collect::<PyResult<HashMap<_, _>>>()?;

        // Parse filing statuses
        let parsed_statuses: Vec<RsFilingStatus> = statuses
            .iter()
            .map(|s| parse_filing_status(s))
            .collect::<PyResult<Vec<_>>>()?;

        // Build cartesian product of all inputs × statuses
        let input_names: Vec<&String> = inputs.keys().collect();
        let input_values: Vec<&Vec<f64>> = input_names.iter().map(|k| &inputs[*k]).collect();

        // Generate all combinations using itertools-style cartesian product
        let mut scenarios: Vec<(RsFilingStatus, HashMap<String, f64>)> = Vec::new();

        fn cartesian_recurse(
            input_names: &[&String],
            input_values: &[&Vec<f64>],
            statuses: &[RsFilingStatus],
            current: &mut HashMap<String, f64>,
            depth: usize,
            scenarios: &mut Vec<(RsFilingStatus, HashMap<String, f64>)>,
        ) {
            if depth == input_names.len() {
                // At leaf: add one scenario per status
                for &status in statuses {
                    scenarios.push((status, current.clone()));
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
            &mut scenarios,
        );

        // Evaluate scenarios (parallel when available)
        let graph = &self.inner;

        #[cfg(feature = "parallel")]
        let iter = scenarios.into_par_iter();
        #[cfg(not(feature = "parallel"))]
        let iter = scenarios.into_iter();

        let results: Vec<(RsFilingStatus, HashMap<String, f64>, HashMap<String, f64>)> = iter
            .map(|(status, input_vals)| {
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
                for output in &outputs {
                    if let Ok(value) = rt.eval(output) {
                        output_vals.insert(output.clone(), value);
                    }
                }
                (status, input_vals, output_vals)
            })
            .collect();

        // Build column-oriented data
        let status_col: Vec<String> = results
            .iter()
            .map(|(s, _, _)| match s {
                RsFilingStatus::Single => "single".to_string(),
                RsFilingStatus::MarriedJoint => "married_joint".to_string(),
                RsFilingStatus::MarriedSeparate => "married_separate".to_string(),
                RsFilingStatus::HeadOfHousehold => "head_of_household".to_string(),
                RsFilingStatus::QualifyingWidow => "qualifying_widow".to_string(),
            })
            .collect();

        // Input columns
        let mut input_cols: HashMap<String, Vec<f64>> = HashMap::new();
        for input_name in &input_names {
            let values: Vec<f64> = results
                .iter()
                .map(|(_, inputs, _)| inputs.get(*input_name).copied().unwrap_or(0.0))
                .collect();
            input_cols.insert((*input_name).clone(), values);
        }

        // Output columns
        let mut output_cols: HashMap<String, Vec<f64>> = HashMap::new();
        for output_name in &outputs {
            let values: Vec<f64> = results
                .iter()
                .map(|(_, _, outputs)| outputs.get(output_name).copied().unwrap_or(0.0))
                .collect();
            output_cols.insert(output_name.clone(), values);
        }

        Ok((status_col, input_cols, output_cols))
    }
}

#[pyclass]
pub struct Runtime {
    graph: Arc<RsGraph>,
    inner: RsRuntime<'static>,
}

#[pymethods]
impl Runtime {
    #[new]
    fn new(graph: &Graph, filing_status: &FilingStatus) -> Self {
        let graph_arc = Arc::clone(&graph.inner);
        let graph_ref: &'static RsGraph = unsafe { &*(Arc::as_ptr(&graph_arc) as *const RsGraph) };
        Runtime {
            graph: graph_arc,
            inner: RsRuntime::new(graph_ref, filing_status.0),
        }
    }

    fn set(&mut self, name: &str, value: f64) -> PyResult<()> {
        self.inner
            .set(name, value)
            .map_err(|e| PyValueError::new_err(format!("{}", e)))
    }

    fn eval(&mut self, name: &str) -> PyResult<f64> {
        self.inner
            .eval(name)
            .map_err(|e| PyValueError::new_err(format!("{}", e)))
    }

    fn gradient(&mut self, output: &str, input: &str) -> PyResult<f64> {
        let graph_ref: &RsGraph = unsafe { &*(Arc::as_ptr(&self.graph) as *const RsGraph) };
        let output_id = graph_ref
            .node_id_by_name(output)
            .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", output)))?;
        let input_id = graph_ref
            .node_id_by_name(input)
            .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", input)))?;

        autodiff::gradient(&mut self.inner, output_id, input_id)
            .map_err(|e| PyValueError::new_err(format!("{}", e)))
    }

    #[pyo3(signature = (output, target, for_input, initial_guess=None))]
    fn solve(
        &mut self,
        output: &str,
        target: f64,
        for_input: &str,
        initial_guess: Option<f64>,
    ) -> PyResult<f64> {
        let graph_ref: &RsGraph = unsafe { &*(Arc::as_ptr(&self.graph) as *const RsGraph) };
        let output_id = graph_ref
            .node_id_by_name(output)
            .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", output)))?;
        let input_id = graph_ref
            .node_id_by_name(for_input)
            .ok_or_else(|| PyValueError::new_err(format!("Node not found: {}", for_input)))?;

        let guess = initial_guess.unwrap_or(target);

        solver::solve(&mut self.inner, output_id, target, input_id, guess)
            .map_err(|e| PyValueError::new_err(format!("{}", e)))
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
