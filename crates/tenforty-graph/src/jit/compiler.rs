use crate::graph::{FilingStatus, Graph, GraphError, NodeId};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use thiserror::Error;

use super::lower::{lower_graph, lower_graph_simd};

#[derive(Debug, Error)]
pub enum JitError {
    #[error("Cranelift error: {0}")]
    Cranelift(String),
    #[error("Module error: {0}")]
    Module(#[from] Box<cranelift_module::ModuleError>),
    #[error("Node {0} not found")]
    NodeNotFound(NodeId),
    #[error("Table '{0}' not found")]
    TableNotFound(String),
    #[error(transparent)]
    Graph(#[from] GraphError),
}

pub struct JitCompiler {
    isa: isa::OwnedTargetIsa,
}

impl JitCompiler {
    pub fn new() -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("use_colocated_libcalls", "false")
            .map_err(|e| JitError::Cranelift(e.to_string()))?;
        flag_builder
            .set("is_pic", "false")
            .map_err(|e| JitError::Cranelift(e.to_string()))?;
        flag_builder
            .set("opt_level", "speed")
            .map_err(|e| JitError::Cranelift(e.to_string()))?;

        let isa_builder =
            cranelift_native::builder().map_err(|e| JitError::Cranelift(e.to_string()))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| JitError::Cranelift(e.to_string()))?;

        Ok(JitCompiler { isa })
    }

    pub fn compile(
        &self,
        graph: &Graph,
        filing_status: FilingStatus,
    ) -> Result<CompiledGraph, JitError> {
        let builder =
            JITBuilder::with_isa(self.isa.clone(), cranelift_module::default_libcall_names());
        let mut module = JITModule::new(builder);
        let mut ctx = module.make_context();

        let ptr_type = module.target_config().pointer_type();

        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(ptr_type));
        sig.params.push(AbiParam::new(ptr_type));
        ctx.func.signature = sig;

        let func_id = module
            .declare_function("eval", Linkage::Export, &ctx.func.signature)
            .map_err(Box::new)?;

        let (node_to_slot, num_slots) = build_slot_map(graph, filing_status)?;

        lower_graph(
            &mut ctx.func,
            &mut module,
            graph,
            filing_status,
            &node_to_slot,
            num_slots,
        )?;

        module
            .define_function(func_id, &mut ctx)
            .map_err(Box::new)?;
        module.clear_context(&mut ctx);
        module.finalize_definitions().map_err(Box::new)?;

        let code_ptr = module.get_finalized_function(func_id);

        let input_offsets = build_input_offsets(graph, &node_to_slot);
        let output_offsets = build_output_offsets(graph, &node_to_slot);

        Ok(CompiledGraph {
            _module: module,
            func_ptr: code_ptr,
            num_slots,
            input_offsets,
            output_offsets,
            filing_status,
        })
    }

    pub fn compile_batch(
        &self,
        graph: &Graph,
        filing_status: FilingStatus,
    ) -> Result<CompiledBatchGraph, JitError> {
        let builder =
            JITBuilder::with_isa(self.isa.clone(), cranelift_module::default_libcall_names());
        let mut module = JITModule::new(builder);
        let mut ctx = module.make_context();

        let ptr_type = module.target_config().pointer_type();

        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(ptr_type));
        sig.params.push(AbiParam::new(ptr_type));
        ctx.func.signature = sig;

        let func_id = module
            .declare_function("eval_batch", Linkage::Export, &ctx.func.signature)
            .map_err(Box::new)?;

        let (input_offsets, num_inputs) = build_batch_input_offsets(graph);
        let (output_offsets, num_outputs) = build_batch_output_offsets(graph);

        lower_graph_simd(
            &mut ctx.func,
            &mut module,
            graph,
            filing_status,
            &input_offsets,
            &output_offsets,
        )?;

        module
            .define_function(func_id, &mut ctx)
            .map_err(Box::new)?;
        module.clear_context(&mut ctx);
        module.finalize_definitions().map_err(Box::new)?;

        let code_ptr = module.get_finalized_function(func_id);

        Ok(CompiledBatchGraph {
            _module: module,
            func_ptr: code_ptr,
            num_inputs,
            num_outputs,
            input_offsets,
            output_offsets,
            filing_status,
        })
    }
}

impl Default for JitCompiler {
    fn default() -> Self {
        Self::new().expect("Failed to create JIT compiler")
    }
}

fn build_slot_map(
    graph: &Graph,
    filing_status: FilingStatus,
) -> Result<(HashMap<NodeId, usize>, usize), JitError> {
    let order = graph.topological_order()?;
    let mut node_to_slot = HashMap::new();
    let mut slot = 0;

    for node_id in order {
        if let Some(node) = graph.nodes.get(&node_id) {
            let effective_id = match &node.op {
                crate::graph::Op::ByStatus { values } => *values.get(filing_status),
                _ => node_id,
            };
            if let Entry::Vacant(e) = node_to_slot.entry(effective_id) {
                e.insert(slot);
                slot += 1;
            }
            if effective_id != node_id {
                let effective_slot = *node_to_slot.get(&effective_id).unwrap();
                node_to_slot.insert(node_id, effective_slot);
            }
        }
    }

    Ok((node_to_slot, slot))
}

fn build_input_offsets(
    graph: &Graph,
    node_to_slot: &HashMap<NodeId, usize>,
) -> HashMap<NodeId, usize> {
    let mut offsets = HashMap::new();
    for &input_id in &graph.inputs {
        if let Some(&slot) = node_to_slot.get(&input_id) {
            offsets.insert(input_id, slot);
        }
    }
    offsets
}

fn build_output_offsets(
    graph: &Graph,
    node_to_slot: &HashMap<NodeId, usize>,
) -> HashMap<NodeId, usize> {
    let mut offsets = HashMap::new();
    for &output_id in &graph.outputs {
        if let Some(&slot) = node_to_slot.get(&output_id) {
            offsets.insert(output_id, slot);
        }
    }
    offsets
}

fn build_batch_input_offsets(graph: &Graph) -> (HashMap<NodeId, usize>, usize) {
    let mut offsets = HashMap::new();
    for (slot, &input_id) in graph.inputs.iter().enumerate() {
        offsets.insert(input_id, slot);
    }
    (offsets, graph.inputs.len())
}

fn build_batch_output_offsets(graph: &Graph) -> (HashMap<NodeId, usize>, usize) {
    let mut offsets = HashMap::new();
    for (slot, &output_id) in graph.outputs.iter().enumerate() {
        offsets.insert(output_id, slot);
    }
    (offsets, graph.outputs.len())
}

pub struct CompiledGraph {
    _module: JITModule,
    func_ptr: *const u8,
    num_slots: usize,
    input_offsets: HashMap<NodeId, usize>,
    output_offsets: HashMap<NodeId, usize>,
    filing_status: FilingStatus,
}

impl CompiledGraph {
    pub fn num_slots(&self) -> usize {
        self.num_slots
    }

    pub fn input_offset(&self, node_id: NodeId) -> Option<usize> {
        self.input_offsets.get(&node_id).copied()
    }

    pub fn output_offset(&self, node_id: NodeId) -> Option<usize> {
        self.output_offsets.get(&node_id).copied()
    }

    pub fn filing_status(&self) -> FilingStatus {
        self.filing_status
    }

    /// Call the compiled function with the given inputs and outputs arrays.
    ///
    /// # Safety
    ///
    /// - `inputs` must point to a valid array of at least `num_slots()` f64 values
    /// - `outputs` must point to a valid, mutable array of at least `num_slots()` f64 values
    /// - The arrays must remain valid for the duration of the call
    pub unsafe fn call(&self, inputs: *const f64, outputs: *mut f64) {
        // SAFETY: func_ptr was produced by Cranelift with the target's default calling convention
        // and signature (i64, i64) -> void matching (ptr, ptr). _module keeps the backing code
        // memory alive.
        let func: unsafe extern "C" fn(*const f64, *mut f64) = std::mem::transmute(self.func_ptr);
        func(inputs, outputs);
    }
}

// SAFETY: The JIT code pointed to by func_ptr is immutable after compilation and _module
// prevents deallocation. No interior mutability; all mutation is via the caller's arrays.
unsafe impl Send for CompiledGraph {}
unsafe impl Sync for CompiledGraph {}

pub struct CompiledBatchGraph {
    _module: JITModule,
    func_ptr: *const u8,
    num_inputs: usize,
    num_outputs: usize,
    input_offsets: HashMap<NodeId, usize>,
    output_offsets: HashMap<NodeId, usize>,
    filing_status: FilingStatus,
}

impl CompiledBatchGraph {
    pub fn num_inputs(&self) -> usize {
        self.num_inputs
    }

    pub fn num_outputs(&self) -> usize {
        self.num_outputs
    }

    pub fn input_offset(&self, node_id: NodeId) -> Option<usize> {
        self.input_offsets.get(&node_id).copied()
    }

    pub fn output_offset(&self, node_id: NodeId) -> Option<usize> {
        self.output_offsets.get(&node_id).copied()
    }

    pub fn filing_status(&self) -> FilingStatus {
        self.filing_status
    }

    /// Call the compiled batch function with the given inputs and outputs arrays.
    ///
    /// # Safety
    ///
    /// - `inputs` must point to a valid array of at least `num_inputs() * BATCH_SIZE` f64 values
    /// - `outputs` must point to a valid, mutable array of at least `num_outputs() * BATCH_SIZE` f64 values
    /// - The arrays must remain valid for the duration of the call
    /// - Memory layout is SoA: [input0×4][input1×4]... and [output0×4][output1×4]...
    pub unsafe fn call(&self, inputs: *const f64, outputs: *mut f64) {
        // SAFETY: func_ptr was produced by Cranelift with the target's default calling convention
        // and signature (i64, i64) -> void matching (ptr, ptr). _module keeps the backing code
        // memory alive.
        let func: unsafe extern "C" fn(*const f64, *mut f64) = std::mem::transmute(self.func_ptr);
        func(inputs, outputs);
    }
}

// SAFETY: The JIT code pointed to by func_ptr is immutable after compilation and _module
// prevents deallocation. No interior mutability; all mutation is via the caller's arrays.
unsafe impl Send for CompiledBatchGraph {}
unsafe impl Sync for CompiledBatchGraph {}
