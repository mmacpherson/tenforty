use crate::graph::{Bracket, FilingStatus, Graph, NodeId, Op};
use cranelift::prelude::*;
use cranelift_module::Module;
use std::collections::HashMap;

use super::compiler::JitError;

pub const BATCH_SIZE: usize = 2;

pub fn lower_graph<M: Module>(
    func: &mut codegen::ir::Function,
    module: &mut M,
    graph: &Graph,
    filing_status: FilingStatus,
    node_to_slot: &HashMap<NodeId, usize>,
    _num_slots: usize,
) -> Result<(), JitError> {
    let mut node_values: HashMap<NodeId, Value> = HashMap::new();

    let order = graph.topological_order()?;

    for node_id in &order {
        let node = match graph.nodes.get(node_id) {
            Some(n) => n,
            None => continue,
        };

        let value = lower_op(
            &mut builder,
            &node.op,
            *node_id,
            graph,
            filing_status,
            &node_values,
            node_to_slot,
            inputs_ptr,
        )?;

        node_values.insert(*node_id, value);
    }

    for &output_id in &graph.outputs {
        if let (Some(&value), Some(&slot)) =
            (node_values.get(&output_id), node_to_slot.get(&output_id))
        {
            let offset = (slot * 8) as i32;
            builder
                .ins()
                .store(MemFlags::new(), value, outputs_ptr, offset);
        }
    }

    builder.ins().return_(&[]);
    builder.finalize();

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_op(
    builder: &mut FunctionBuilder,
    op: &Op,
    node_id: NodeId,
    graph: &Graph,
    filing_status: FilingStatus,
    node_values: &HashMap<NodeId, Value>,
    node_to_slot: &HashMap<NodeId, usize>,
    inputs_ptr: Value,
) -> Result<Value, JitError> {
    match op {
        Op::Input | Op::Import { .. } => {
            let slot = node_to_slot
                .get(&node_id)
                .ok_or(JitError::NodeNotFound(node_id))?;
            let offset = (*slot * 8) as i32;
            Ok(builder
                .ins()
                .load(types::F64, MemFlags::new(), inputs_ptr, offset))
        }

        Op::Literal { value } => Ok(builder.ins().f64const(*value)),

        Op::Add { left, right } => {
            let l = get_node_value(*left, node_values)?;
            let r = get_node_value(*right, node_values)?;
            Ok(builder.ins().fadd(l, r))
        }

        Op::Sub { left, right } => {
            let l = get_node_value(*left, node_values)?;
            let r = get_node_value(*right, node_values)?;
            Ok(builder.ins().fsub(l, r))
        }

        Op::Mul { left, right } => {
            let l = get_node_value(*left, node_values)?;
            let r = get_node_value(*right, node_values)?;
            Ok(builder.ins().fmul(l, r))
        }

        Op::Div { left, right } => {
            let l = get_node_value(*left, node_values)?;
            let r = get_node_value(*right, node_values)?;
            let zero = builder.ins().f64const(0.0);
            let is_zero = builder.ins().fcmp(FloatCC::Equal, r, zero);
            let one = builder.ins().f64const(1.0);
            let safe_r = builder.ins().select(is_zero, one, r);
            let result = builder.ins().fdiv(l, safe_r);
            Ok(builder.ins().select(is_zero, zero, result))
        }

        Op::Max { left, right } => {
            let l = get_node_value(*left, node_values)?;
            let r = get_node_value(*right, node_values)?;
            Ok(builder.ins().fmax(l, r))
        }

        Op::Min { left, right } => {
            let l = get_node_value(*left, node_values)?;
            let r = get_node_value(*right, node_values)?;
            Ok(builder.ins().fmin(l, r))
        }

        Op::Floor { arg } => {
            let v = get_node_value(*arg, node_values)?;
            Ok(builder.ins().floor(v))
        }

        Op::Neg { arg } => {
            let v = get_node_value(*arg, node_values)?;
            Ok(builder.ins().fneg(v))
        }

        Op::Abs { arg } => {
            let v = get_node_value(*arg, node_values)?;
            Ok(builder.ins().fabs(v))
        }

        Op::Clamp { arg, min, max } => {
            let v = get_node_value(*arg, node_values)?;
            let min_val = builder.ins().f64const(*min);
            let max_val = builder.ins().f64const(*max);
            let clamped_min = builder.ins().fmax(v, min_val);
            Ok(builder.ins().fmin(clamped_min, max_val))
        }

        Op::ByStatus { values } => {
            let target_id = *values.get(filing_status);
            get_node_value(target_id, node_values)
        }

        Op::IfPositive {
            cond,
            then,
            otherwise,
        } => {
            let c = get_node_value(*cond, node_values)?;
            let t = get_node_value(*then, node_values)?;
            let e = get_node_value(*otherwise, node_values)?;
            let zero = builder.ins().f64const(0.0);
            let is_positive = builder.ins().fcmp(FloatCC::GreaterThan, c, zero);
            Ok(builder.ins().select(is_positive, t, e))
        }

        Op::BracketTax { table, income } => {
            let table_data = graph
                .tables
                .get(table)
                .ok_or_else(|| JitError::TableNotFound(table.clone()))?;
            let brackets = table_data.brackets.get(filing_status);
            let inc = get_node_value(*income, node_values)?;
            Ok(lower_bracket_tax(builder, brackets, inc))
        }

        Op::PhaseOut {
            base,
            threshold,
            rate,
            agi,
        } => {
            let thresh = *threshold.get(filing_status);
            let agi_val = get_node_value(*agi, node_values)?;
            Ok(lower_phase_out(builder, *base, thresh, *rate, agi_val))
        }
    }
}

fn get_node_value(
    node_id: NodeId,
    node_values: &HashMap<NodeId, Value>,
) -> Result<Value, JitError> {
    node_values
        .get(&node_id)
        .copied()
        .ok_or(JitError::NodeNotFound(node_id))
}

fn lower_bracket_tax(builder: &mut FunctionBuilder, brackets: &[Bracket], income: Value) -> Value {
    if brackets.is_empty() {
        return builder.ins().f64const(0.0);
    }

    let zero = builder.ins().f64const(0.0);

    let mut tax = zero;
    let mut prev_threshold = 0.0;

    for bracket in brackets {
        let thresh_val = bracket.threshold;
        let rate_val = bracket.rate;

        let prev_thresh = builder.ins().f64const(prev_threshold);
        let thresh = builder.ins().f64const(thresh_val);
        let rate = builder.ins().f64const(rate_val);

        let bracket_width = builder.ins().fsub(thresh, prev_thresh);
        let income_in_bracket = builder.ins().fsub(income, prev_thresh);

        let clamped_income = builder.ins().fmin(income_in_bracket, bracket_width);
        let clamped_income = builder.ins().fmax(clamped_income, zero);

        let bracket_tax = builder.ins().fmul(clamped_income, rate);
        tax = builder.ins().fadd(tax, bracket_tax);

        prev_threshold = thresh_val;
    }

    let is_non_positive = builder.ins().fcmp(FloatCC::LessThanOrEqual, income, zero);
    builder.ins().select(is_non_positive, zero, tax)
}

fn lower_phase_out(
    builder: &mut FunctionBuilder,
    base: f64,
    threshold: f64,
    rate: f64,
    agi: Value,
) -> Value {
    let base_val = builder.ins().f64const(base);
    let thresh_val = builder.ins().f64const(threshold);
    let rate_val = builder.ins().f64const(rate);
    let zero = builder.ins().f64const(0.0);

    let is_below = builder
        .ins()
        .fcmp(FloatCC::LessThanOrEqual, agi, thresh_val);
    let excess = builder.ins().fsub(agi, thresh_val);
    let reduction = builder.ins().fmul(excess, rate_val);
    let reduced = builder.ins().fsub(base_val, reduction);
    let floored = builder.ins().fmax(reduced, zero);

    builder.ins().select(is_below, base_val, floored)
}

pub fn lower_graph_simd<M: Module>(
    func: &mut codegen::ir::Function,
    _module: &mut M,
    graph: &Graph,
    filing_status: FilingStatus,
    input_offsets: &HashMap<NodeId, usize>,
    output_offsets: &HashMap<NodeId, usize>,
) -> Result<(), JitError> {
    let mut func_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(func, &mut func_ctx);

    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    let inputs_ptr = builder.block_params(entry_block)[0];
    let outputs_ptr = builder.block_params(entry_block)[1];

    let mut node_values: HashMap<NodeId, Value> = HashMap::new();

    let order = graph.topological_order()?;

    for node_id in &order {
        let node = match graph.nodes.get(node_id) {
            Some(n) => n,
            None => continue,
        };

        let value = lower_op_simd(
            &mut builder,
            &node.op,
            *node_id,
            graph,
            filing_status,
            &node_values,
            input_offsets,
            inputs_ptr,
        )?;

        node_values.insert(*node_id, value);
    }

    for (&output_id, &slot) in output_offsets {
        if let Some(&value) = node_values.get(&output_id) {
            let offset = (slot * BATCH_SIZE * 8) as i32;
            builder
                .ins()
                .store(MemFlags::new(), value, outputs_ptr, offset);
        }
    }

    builder.ins().return_(&[]);
    builder.finalize();

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_op_simd(
    builder: &mut FunctionBuilder,
    op: &Op,
    node_id: NodeId,
    graph: &Graph,
    filing_status: FilingStatus,
    node_values: &HashMap<NodeId, Value>,
    input_offsets: &HashMap<NodeId, usize>,
    inputs_ptr: Value,
) -> Result<Value, JitError> {
    match op {
        Op::Input | Op::Import { .. } => {
            let slot = input_offsets
                .get(&node_id)
                .ok_or(JitError::NodeNotFound(node_id))?;
            let offset = (*slot * BATCH_SIZE * 8) as i32;
            Ok(builder
                .ins()
                .load(types::F64X2, MemFlags::new(), inputs_ptr, offset))
        }

        Op::Literal { value } => {
            let scalar = builder.ins().f64const(*value);
            Ok(builder.ins().splat(types::F64X2, scalar))
        }

        Op::Add { left, right } => {
            let l = get_node_value_simd(*left, node_values)?;
            let r = get_node_value_simd(*right, node_values)?;
            Ok(builder.ins().fadd(l, r))
        }

        Op::Sub { left, right } => {
            let l = get_node_value_simd(*left, node_values)?;
            let r = get_node_value_simd(*right, node_values)?;
            Ok(builder.ins().fsub(l, r))
        }

        Op::Mul { left, right } => {
            let l = get_node_value_simd(*left, node_values)?;
            let r = get_node_value_simd(*right, node_values)?;
            Ok(builder.ins().fmul(l, r))
        }

        Op::Div { left, right } => {
            let l = get_node_value_simd(*left, node_values)?;
            let r = get_node_value_simd(*right, node_values)?;
            let zero_scalar = builder.ins().f64const(0.0);
            let zero = builder.ins().splat(types::F64X2, zero_scalar);
            let one_scalar = builder.ins().f64const(1.0);
            let one = builder.ins().splat(types::F64X2, one_scalar);
            let is_zero = builder.ins().fcmp(FloatCC::Equal, r, zero);
            let mask = builder
                .ins()
                .bitcast(types::F64X2, MemFlags::new(), is_zero);
            let safe_r = builder.ins().bitselect(mask, one, r);
            let result = builder.ins().fdiv(l, safe_r);
            Ok(builder.ins().bitselect(mask, zero, result))
        }

        Op::Max { left, right } => {
            let l = get_node_value_simd(*left, node_values)?;
            let r = get_node_value_simd(*right, node_values)?;
            Ok(builder.ins().fmax(l, r))
        }

        Op::Min { left, right } => {
            let l = get_node_value_simd(*left, node_values)?;
            let r = get_node_value_simd(*right, node_values)?;
            Ok(builder.ins().fmin(l, r))
        }

        Op::Floor { arg } => {
            let v = get_node_value_simd(*arg, node_values)?;
            Ok(builder.ins().floor(v))
        }

        Op::Neg { arg } => {
            let v = get_node_value_simd(*arg, node_values)?;
            Ok(builder.ins().fneg(v))
        }

        Op::Abs { arg } => {
            let v = get_node_value_simd(*arg, node_values)?;
            Ok(builder.ins().fabs(v))
        }

        Op::Clamp { arg, min, max } => {
            let v = get_node_value_simd(*arg, node_values)?;
            let min_scalar = builder.ins().f64const(*min);
            let min_val = builder.ins().splat(types::F64X2, min_scalar);
            let max_scalar = builder.ins().f64const(*max);
            let max_val = builder.ins().splat(types::F64X2, max_scalar);
            let clamped_min = builder.ins().fmax(v, min_val);
            Ok(builder.ins().fmin(clamped_min, max_val))
        }

        Op::ByStatus { values } => {
            let target_id = *values.get(filing_status);
            get_node_value_simd(target_id, node_values)
        }

        Op::IfPositive {
            cond,
            then,
            otherwise,
        } => {
            let c = get_node_value_simd(*cond, node_values)?;
            let t = get_node_value_simd(*then, node_values)?;
            let e = get_node_value_simd(*otherwise, node_values)?;
            let zero_scalar = builder.ins().f64const(0.0);
            let zero = builder.ins().splat(types::F64X2, zero_scalar);
            let is_positive = builder.ins().fcmp(FloatCC::GreaterThan, c, zero);
            let mask = builder
                .ins()
                .bitcast(types::F64X2, MemFlags::new(), is_positive);
            Ok(builder.ins().bitselect(mask, t, e))
        }

        Op::BracketTax { table, income } => {
            let table_data = graph
                .tables
                .get(table)
                .ok_or_else(|| JitError::TableNotFound(table.clone()))?;
            let brackets = table_data.brackets.get(filing_status);
            let inc_vec = get_node_value_simd(*income, node_values)?;
            Ok(lower_bracket_tax_simd(builder, brackets, inc_vec))
        }

        Op::PhaseOut {
            base,
            threshold,
            rate,
            agi,
        } => {
            let thresh = *threshold.get(filing_status);
            let agi_val = get_node_value_simd(*agi, node_values)?;
            Ok(lower_phase_out_simd(builder, *base, thresh, *rate, agi_val))
        }
    }
}

fn get_node_value_simd(
    node_id: NodeId,
    node_values: &HashMap<NodeId, Value>,
) -> Result<Value, JitError> {
    node_values
        .get(&node_id)
        .copied()
        .ok_or(JitError::NodeNotFound(node_id))
}

fn lower_bracket_tax_simd(
    builder: &mut FunctionBuilder,
    brackets: &[Bracket],
    income_vec: Value,
) -> Value {
    let inc0 = builder.ins().extractlane(income_vec, 0);
    let inc1 = builder.ins().extractlane(income_vec, 1);

    let tax0 = lower_bracket_tax(builder, brackets, inc0);
    let tax1 = lower_bracket_tax(builder, brackets, inc1);

    let zero_vec_bytes: [u8; 16] = [0; 16];
    let zero_const = builder
        .func
        .dfg
        .constants
        .insert((&zero_vec_bytes[..]).into());
    let mut tax_vec = builder.ins().vconst(types::F64X2, zero_const);
    tax_vec = builder.ins().insertlane(tax_vec, tax0, 0);
    tax_vec = builder.ins().insertlane(tax_vec, tax1, 1);

    tax_vec
}

fn lower_phase_out_simd(
    builder: &mut FunctionBuilder,
    base: f64,
    threshold: f64,
    rate: f64,
    agi_vec: Value,
) -> Value {
    let base_scalar = builder.ins().f64const(base);
    let base_val = builder.ins().splat(types::F64X2, base_scalar);
    let thresh_scalar = builder.ins().f64const(threshold);
    let thresh_val = builder.ins().splat(types::F64X2, thresh_scalar);
    let rate_scalar = builder.ins().f64const(rate);
    let rate_val = builder.ins().splat(types::F64X2, rate_scalar);
    let zero_scalar = builder.ins().f64const(0.0);
    let zero = builder.ins().splat(types::F64X2, zero_scalar);

    let is_below = builder
        .ins()
        .fcmp(FloatCC::LessThanOrEqual, agi_vec, thresh_val);
    let mask = builder
        .ins()
        .bitcast(types::F64X2, MemFlags::new(), is_below);
    let excess = builder.ins().fsub(agi_vec, thresh_val);
    let reduction = builder.ins().fmul(excess, rate_val);
    let reduced = builder.ins().fsub(base_val, reduction);
    let floored = builder.ins().fmax(reduced, zero);

    builder.ins().bitselect(mask, base_val, floored)
}
