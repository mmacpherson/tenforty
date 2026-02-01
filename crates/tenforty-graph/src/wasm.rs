use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use crate::eval::Runtime as RsRuntime;
use crate::graph::{FilingStatus as RsFilingStatus, Graph as RsGraph};
use crate::link::{GraphSet as RsGraphSet, UnresolvedImport as RsUnresolvedImport};
use crate::{autodiff, solver};

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct FilingStatus(RsFilingStatus);

#[wasm_bindgen]
impl FilingStatus {
    pub fn single() -> Self {
        FilingStatus(RsFilingStatus::Single)
    }

    pub fn married_joint() -> Self {
        FilingStatus(RsFilingStatus::MarriedJoint)
    }

    pub fn married_separate() -> Self {
        FilingStatus(RsFilingStatus::MarriedSeparate)
    }

    pub fn head_of_household() -> Self {
        FilingStatus(RsFilingStatus::HeadOfHousehold)
    }

    pub fn qualifying_widow() -> Self {
        FilingStatus(RsFilingStatus::QualifyingWidow)
    }

    #[wasm_bindgen(js_name = fromString)]
    pub fn from_string(s: &str) -> Result<FilingStatus, JsError> {
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
            _ => Err(JsError::new(&format!("Unknown filing status: {}", s))),
        }
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string_js(&self) -> String {
        match self.0 {
            RsFilingStatus::Single => "single".to_string(),
            RsFilingStatus::MarriedJoint => "married_joint".to_string(),
            RsFilingStatus::MarriedSeparate => "married_separate".to_string(),
            RsFilingStatus::HeadOfHousehold => "head_of_household".to_string(),
            RsFilingStatus::QualifyingWidow => "qualifying_widow".to_string(),
        }
    }
}

#[wasm_bindgen]
pub struct Graph {
    inner: Rc<RsGraph>,
}

#[wasm_bindgen]
impl Graph {
    #[wasm_bindgen(js_name = fromJson)]
    pub fn from_json(json: &str) -> Result<Graph, JsError> {
        let graph = RsGraph::from_json(json)
            .map_err(|e| JsError::new(&format!("Failed to parse graph: {}", e)))?;
        Ok(Graph {
            inner: Rc::new(graph),
        })
    }

    #[wasm_bindgen(js_name = toJson)]
    pub fn to_json(&self) -> Result<String, JsError> {
        self.inner
            .to_json()
            .map_err(|e| JsError::new(&format!("Failed to serialize graph: {}", e)))
    }

    #[wasm_bindgen(js_name = inputNames)]
    pub fn input_names(&self) -> Vec<String> {
        self.inner
            .inputs
            .iter()
            .filter_map(|id| self.inner.nodes.get(id).and_then(|n| n.name.clone()))
            .collect()
    }

    #[wasm_bindgen(js_name = outputNames)]
    pub fn output_names(&self) -> Vec<String> {
        self.inner
            .outputs
            .iter()
            .filter_map(|id| self.inner.nodes.get(id).and_then(|n| n.name.clone()))
            .collect()
    }
}

#[wasm_bindgen]
pub struct Runtime {
    graph: Rc<RsGraph>,
    inner: RefCell<RsRuntime<'static>>,
}

#[wasm_bindgen]
impl Runtime {
    #[wasm_bindgen(constructor)]
    pub fn new(graph: &Graph, filing_status: &FilingStatus) -> Self {
        let graph_rc = Rc::clone(&graph.inner);
        let graph_ref: &'static RsGraph = unsafe { &*(Rc::as_ptr(&graph_rc) as *const RsGraph) };
        Runtime {
            graph: graph_rc,
            inner: RefCell::new(RsRuntime::new(graph_ref, filing_status.0)),
        }
    }

    pub fn set(&self, name: &str, value: f64) -> Result<(), JsError> {
        self.inner
            .borrow_mut()
            .set(name, value)
            .map_err(|e| JsError::new(&format!("{}", e)))
    }

    pub fn eval(&self, name: &str) -> Result<f64, JsError> {
        self.inner
            .borrow_mut()
            .eval(name)
            .map_err(|e| JsError::new(&format!("{}", e)))
    }

    pub fn gradient(&self, output: &str, input: &str) -> Result<f64, JsError> {
        let graph_ref: &RsGraph = unsafe { &*(Rc::as_ptr(&self.graph) as *const RsGraph) };
        let output_id = graph_ref
            .node_id_by_name(output)
            .ok_or_else(|| JsError::new(&format!("Node not found: {}", output)))?;
        let input_id = graph_ref
            .node_id_by_name(input)
            .ok_or_else(|| JsError::new(&format!("Node not found: {}", input)))?;

        autodiff::gradient(&mut self.inner.borrow_mut(), output_id, input_id)
            .map_err(|e| JsError::new(&format!("{}", e)))
    }

    pub fn solve(
        &self,
        output: &str,
        target: f64,
        for_input: &str,
        initial_guess: Option<f64>,
    ) -> Result<f64, JsError> {
        let graph_ref: &RsGraph = unsafe { &*(Rc::as_ptr(&self.graph) as *const RsGraph) };
        let output_id = graph_ref
            .node_id_by_name(output)
            .ok_or_else(|| JsError::new(&format!("Node not found: {}", output)))?;
        let input_id = graph_ref
            .node_id_by_name(for_input)
            .ok_or_else(|| JsError::new(&format!("Node not found: {}", for_input)))?;

        let guess = initial_guess.unwrap_or(target);

        solver::solve(
            &mut self.inner.borrow_mut(),
            output_id,
            target,
            input_id,
            guess,
        )
        .map_err(|e| JsError::new(&format!("{}", e)))
    }
}

#[wasm_bindgen]
pub struct UnresolvedImport {
    form: String,
    line: String,
    year: u16,
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

#[wasm_bindgen]
impl UnresolvedImport {
    #[wasm_bindgen(getter)]
    pub fn form(&self) -> String {
        self.form.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn line(&self) -> String {
        self.line.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn year(&self) -> u16 {
        self.year
    }
}

#[wasm_bindgen]
pub struct GraphSet {
    inner: RsGraphSet,
}

#[wasm_bindgen]
impl GraphSet {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        GraphSet {
            inner: RsGraphSet::new(),
        }
    }

    pub fn add(&mut self, form_id: &str, graph: &Graph) {
        self.inner.add_mut(form_id, (*graph.inner).clone());
    }

    pub fn forms(&self) -> Vec<String> {
        self.inner.forms().to_vec()
    }

    #[wasm_bindgen(js_name = unresolvedImports)]
    pub fn unresolved_imports(&self) -> Vec<UnresolvedImport> {
        self.inner
            .unresolved_imports()
            .into_iter()
            .map(UnresolvedImport::from)
            .collect()
    }

    pub fn link(&self) -> Result<Graph, JsError> {
        let linked = self
            .inner
            .link()
            .map_err(|e| JsError::new(&format!("Link error: {}", e)))?;
        Ok(Graph {
            inner: Rc::new(linked),
        })
    }
}
