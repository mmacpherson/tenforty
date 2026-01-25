use std::collections::HashMap;
use thiserror::Error;

use crate::graph::{
    BracketTable, ByStatus, Graph, GraphMeta, Invariant, Node, NodeId, Op, TableId,
};

#[derive(Debug, Error)]
pub enum LinkError {
    #[error("Unresolved import: form '{form}' line '{line}' year {year}")]
    UnresolvedImport { form: String, line: String, year: u16 },
    #[error("Form not found: '{0}'")]
    FormNotFound(String),
    #[error("Output node not found: form '{form}' line '{line}'")]
    OutputNotFound { form: String, line: String },
    #[error("Duplicate form ID: '{0}'")]
    DuplicateFormId(String),
    #[error("Circular dependency detected: {0:?}")]
    CircularDependency(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct UnresolvedImport {
    pub form: String,
    pub line: String,
    pub year: u16,
    pub node_id: NodeId,
}

#[derive(Debug, Default)]
pub struct GraphSet {
    graphs: HashMap<String, Graph>,
    insertion_order: Vec<String>,
}

impl GraphSet {
    pub fn new() -> Self {
        GraphSet {
            graphs: HashMap::new(),
            insertion_order: Vec::new(),
        }
    }

    pub fn add(mut self, form_id: &str, graph: Graph) -> Self {
        if !self.graphs.contains_key(form_id) {
            self.insertion_order.push(form_id.to_string());
        }
        self.graphs.insert(form_id.to_string(), graph);
        self
    }

    pub fn add_mut(&mut self, form_id: &str, graph: Graph) {
        if !self.graphs.contains_key(form_id) {
            self.insertion_order.push(form_id.to_string());
        }
        self.graphs.insert(form_id.to_string(), graph);
    }

    pub fn forms(&self) -> &[String] {
        &self.insertion_order
    }

    pub fn get(&self, form_id: &str) -> Option<&Graph> {
        self.graphs.get(form_id)
    }

    pub fn unresolved_imports(&self) -> Vec<UnresolvedImport> {
        let mut unresolved = Vec::new();

        for (_form_id, graph) in &self.graphs {
            for node in graph.nodes.values() {
                if let Op::Import { form, line, year } = &node.op {
                    let resolved = self.graphs.get(form).and_then(|target_graph| {
                        target_graph
                            .outputs
                            .iter()
                            .filter_map(|id| target_graph.nodes.get(id))
                            .find(|n| {
                                n.name
                                    .as_deref()
                                    .is_some_and(|name| name == line || name.starts_with(&format!("{line}_")))
                            })
                    });

                    if resolved.is_none() {
                        unresolved.push(UnresolvedImport {
                            form: form.clone(),
                            line: line.clone(),
                            year: *year,
                            node_id: node.id,
                        });
                    }
                }
            }
        }

        unresolved
    }

    pub fn link(&self) -> Result<Graph, LinkError> {
        if self.graphs.is_empty() {
            return Ok(Graph {
                meta: None,
                nodes: HashMap::new(),
                tables: HashMap::new(),
                inputs: Vec::new(),
                outputs: Vec::new(),
                invariants: Vec::new(),
            });
        }

        // Calculate node ID offsets for each graph
        let mut offsets: HashMap<String, NodeId> = HashMap::new();
        let mut current_offset: NodeId = 0;

        for form_id in &self.insertion_order {
            let graph = self.graphs.get(form_id).unwrap();
            offsets.insert(form_id.clone(), current_offset);

            // Find max node ID in this graph
            let max_id = graph.nodes.keys().copied().max().unwrap_or(0);
            current_offset = current_offset + max_id + 1;
        }

        // Build import resolution map: (form, line) -> remapped node ID
        let mut import_targets: HashMap<(String, String), NodeId> = HashMap::new();

        for form_id in &self.insertion_order {
            let graph = self.graphs.get(form_id).unwrap();
            let offset = offsets[form_id];

            // Collect output nodes that can be imported
            for &output_id in &graph.outputs {
                if let Some(node) = graph.nodes.get(&output_id) {
                    if let Some(ref name) = node.name {
                        import_targets.insert((form_id.clone(), name.clone()), output_id + offset);
                        if let Some((base, _)) = name.split_once('_') {
                            import_targets
                                .entry((form_id.clone(), base.to_string()))
                                .or_insert(output_id + offset);
                        }
                    }
                }
            }

            // Also collect named nodes that aren't outputs (some imports may reference interior nodes)
            for node in graph.nodes.values() {
                if let Some(ref name) = node.name {
                    import_targets
                        .entry((form_id.clone(), name.clone()))
                        .or_insert(node.id + offset);
                    if let Some((base, _)) = name.split_once('_') {
                        import_targets
                            .entry((form_id.clone(), base.to_string()))
                            .or_insert(node.id + offset);
                    }
                }
            }
        }

        // Build merged graph
        let mut merged_nodes: HashMap<NodeId, Node> = HashMap::new();
        let mut merged_tables: HashMap<TableId, BracketTable> = HashMap::new();
        let mut merged_inputs: Vec<NodeId> = Vec::new();
        let mut merged_outputs: Vec<NodeId> = Vec::new();
        let mut merged_invariants: Vec<Invariant> = Vec::new();

        // Map from old Import nodes to their resolved targets
        let mut import_redirects: HashMap<NodeId, NodeId> = HashMap::new();

        for form_id in &self.insertion_order {
            let graph = self.graphs.get(form_id).unwrap();
            let offset = offsets[form_id];
            let table_prefix = format!("{}__", form_id);

            // Process nodes
            for node in graph.nodes.values() {
                let new_id = node.id + offset;

                if let Op::Import { form, line, year } = &node.op {
                    // Try to resolve the import
                    if let Some(&target_id) = import_targets.get(&(form.clone(), line.clone())) {
                        // Record redirect - don't create node, references will point to target
                        import_redirects.insert(new_id, target_id);
                    } else {
                        // Unresolved import - keep as Input node
                        let new_node = Node {
                            id: new_id,
                            op: Op::Input,
                            name: Some(format!("import__{}__{}__{}_{}", form_id, form, line, year)),
                        };
                        merged_nodes.insert(new_id, new_node);
                        merged_inputs.push(new_id);
                    }
                } else {
                    // Remap the operation's node references
                    let new_op = remap_op(&node.op, offset, &import_redirects);
                    let new_name = node.name.as_ref().map(|n| format!("{}_{}", form_id, n));

                    let new_node = Node {
                        id: new_id,
                        op: new_op,
                        name: new_name,
                    };
                    merged_nodes.insert(new_id, new_node);
                }
            }

            // Remap inputs (excluding Import nodes that were resolved)
            for &input_id in &graph.inputs {
                let new_id = input_id + offset;
                if !import_redirects.contains_key(&new_id) {
                    merged_inputs.push(new_id);
                }
            }

            // Remap outputs
            for &output_id in &graph.outputs {
                let new_id = output_id + offset;
                merged_outputs.push(new_id);
            }

            // Merge tables with prefixed names
            for (table_id, table) in &graph.tables {
                let new_table_id = format!("{}{}", table_prefix, table_id);
                merged_tables.insert(new_table_id, table.clone());
            }

            // Remap invariants
            for inv in &graph.invariants {
                let new_inv = remap_invariant(inv, form_id, &table_prefix);
                merged_invariants.push(new_inv);
            }
        }

        // Second pass: fix up node references that point to Import nodes
        let nodes_to_update: Vec<NodeId> = merged_nodes.keys().copied().collect();
        for node_id in nodes_to_update {
            if let Some(node) = merged_nodes.get(&node_id) {
                let updated_op = resolve_imports_in_op(&node.op, &import_redirects);
                if let Some(new_op) = updated_op {
                    merged_nodes.get_mut(&node_id).unwrap().op = new_op;
                }
            }
        }

        // Update table references in nodes
        for form_id in &self.insertion_order {
            let offset = offsets[form_id];
            let table_prefix = format!("{}__", form_id);
            let graph = self.graphs.get(form_id).unwrap();

            for node in graph.nodes.values() {
                let new_id = node.id + offset;
                if let Some(merged_node) = merged_nodes.get_mut(&new_id) {
                    if let Op::BracketTax { table, income } = &merged_node.op {
                        let new_table = format!("{}{}", table_prefix, table);
                        merged_node.op = Op::BracketTax {
                            table: new_table,
                            income: *income,
                        };
                    }
                }
            }
        }

        Ok(Graph {
            meta: Some(GraphMeta {
                form_id: Some("linked".to_string()),
                year: None,
                generated_by: Some("GraphSet::link".to_string()),
            }),
            nodes: merged_nodes,
            tables: merged_tables,
            inputs: merged_inputs,
            outputs: merged_outputs,
            invariants: merged_invariants,
        })
    }
}

fn remap_node_id(id: NodeId, offset: NodeId, redirects: &HashMap<NodeId, NodeId>) -> NodeId {
    let new_id = id + offset;
    *redirects.get(&new_id).unwrap_or(&new_id)
}

fn remap_op(op: &Op, offset: NodeId, redirects: &HashMap<NodeId, NodeId>) -> Op {
    match op {
        Op::Input => Op::Input,
        Op::Literal { value } => Op::Literal { value: *value },
        Op::Import { form, line, year } => Op::Import {
            form: form.clone(),
            line: line.clone(),
            year: *year,
        },
        Op::Add { left, right } => Op::Add {
            left: remap_node_id(*left, offset, redirects),
            right: remap_node_id(*right, offset, redirects),
        },
        Op::Sub { left, right } => Op::Sub {
            left: remap_node_id(*left, offset, redirects),
            right: remap_node_id(*right, offset, redirects),
        },
        Op::Mul { left, right } => Op::Mul {
            left: remap_node_id(*left, offset, redirects),
            right: remap_node_id(*right, offset, redirects),
        },
        Op::Div { left, right } => Op::Div {
            left: remap_node_id(*left, offset, redirects),
            right: remap_node_id(*right, offset, redirects),
        },
        Op::Max { left, right } => Op::Max {
            left: remap_node_id(*left, offset, redirects),
            right: remap_node_id(*right, offset, redirects),
        },
        Op::Min { left, right } => Op::Min {
            left: remap_node_id(*left, offset, redirects),
            right: remap_node_id(*right, offset, redirects),
        },
        Op::Floor { arg } => Op::Floor {
            arg: remap_node_id(*arg, offset, redirects),
        },
        Op::Neg { arg } => Op::Neg {
            arg: remap_node_id(*arg, offset, redirects),
        },
        Op::Abs { arg } => Op::Abs {
            arg: remap_node_id(*arg, offset, redirects),
        },
        Op::Clamp { arg, min, max } => Op::Clamp {
            arg: remap_node_id(*arg, offset, redirects),
            min: *min,
            max: *max,
        },
        Op::BracketTax { table, income } => Op::BracketTax {
            table: table.clone(), // Table name updated separately
            income: remap_node_id(*income, offset, redirects),
        },
        Op::PhaseOut {
            base,
            threshold,
            rate,
            agi,
        } => Op::PhaseOut {
            base: *base,
            threshold: threshold.clone(),
            rate: *rate,
            agi: remap_node_id(*agi, offset, redirects),
        },
        Op::ByStatus { values } => Op::ByStatus {
            values: ByStatus {
                single: remap_node_id(values.single, offset, redirects),
                married_joint: remap_node_id(values.married_joint, offset, redirects),
                married_separate: remap_node_id(values.married_separate, offset, redirects),
                head_of_household: remap_node_id(values.head_of_household, offset, redirects),
                qualifying_widow: remap_node_id(values.qualifying_widow, offset, redirects),
            },
        },
        Op::IfPositive { cond, then, otherwise } => Op::IfPositive {
            cond: remap_node_id(*cond, offset, redirects),
            then: remap_node_id(*then, offset, redirects),
            otherwise: remap_node_id(*otherwise, offset, redirects),
        },
    }
}

fn resolve_imports_in_op(op: &Op, redirects: &HashMap<NodeId, NodeId>) -> Option<Op> {
    let mut changed = false;

    let new_op = match op {
        Op::Add { left, right } => {
            let new_left = redirects.get(left).copied();
            let new_right = redirects.get(right).copied();
            if new_left.is_some() || new_right.is_some() {
                changed = true;
                Op::Add {
                    left: new_left.unwrap_or(*left),
                    right: new_right.unwrap_or(*right),
                }
            } else {
                op.clone()
            }
        }
        Op::Sub { left, right } => {
            let new_left = redirects.get(left).copied();
            let new_right = redirects.get(right).copied();
            if new_left.is_some() || new_right.is_some() {
                changed = true;
                Op::Sub {
                    left: new_left.unwrap_or(*left),
                    right: new_right.unwrap_or(*right),
                }
            } else {
                op.clone()
            }
        }
        Op::Mul { left, right } => {
            let new_left = redirects.get(left).copied();
            let new_right = redirects.get(right).copied();
            if new_left.is_some() || new_right.is_some() {
                changed = true;
                Op::Mul {
                    left: new_left.unwrap_or(*left),
                    right: new_right.unwrap_or(*right),
                }
            } else {
                op.clone()
            }
        }
        Op::Div { left, right } => {
            let new_left = redirects.get(left).copied();
            let new_right = redirects.get(right).copied();
            if new_left.is_some() || new_right.is_some() {
                changed = true;
                Op::Div {
                    left: new_left.unwrap_or(*left),
                    right: new_right.unwrap_or(*right),
                }
            } else {
                op.clone()
            }
        }
        Op::Max { left, right } => {
            let new_left = redirects.get(left).copied();
            let new_right = redirects.get(right).copied();
            if new_left.is_some() || new_right.is_some() {
                changed = true;
                Op::Max {
                    left: new_left.unwrap_or(*left),
                    right: new_right.unwrap_or(*right),
                }
            } else {
                op.clone()
            }
        }
        Op::Min { left, right } => {
            let new_left = redirects.get(left).copied();
            let new_right = redirects.get(right).copied();
            if new_left.is_some() || new_right.is_some() {
                changed = true;
                Op::Min {
                    left: new_left.unwrap_or(*left),
                    right: new_right.unwrap_or(*right),
                }
            } else {
                op.clone()
            }
        }
        Op::Floor { arg } => {
            if let Some(&new_arg) = redirects.get(arg) {
                changed = true;
                Op::Floor { arg: new_arg }
            } else {
                op.clone()
            }
        }
        Op::Neg { arg } => {
            if let Some(&new_arg) = redirects.get(arg) {
                changed = true;
                Op::Neg { arg: new_arg }
            } else {
                op.clone()
            }
        }
        Op::Abs { arg } => {
            if let Some(&new_arg) = redirects.get(arg) {
                changed = true;
                Op::Abs { arg: new_arg }
            } else {
                op.clone()
            }
        }
        Op::Clamp { arg, min, max } => {
            if let Some(&new_arg) = redirects.get(arg) {
                changed = true;
                Op::Clamp { arg: new_arg, min: *min, max: *max }
            } else {
                op.clone()
            }
        }
        Op::BracketTax { table, income } => {
            if let Some(&new_income) = redirects.get(income) {
                changed = true;
                Op::BracketTax { table: table.clone(), income: new_income }
            } else {
                op.clone()
            }
        }
        Op::PhaseOut { base, threshold, rate, agi } => {
            if let Some(&new_agi) = redirects.get(agi) {
                changed = true;
                Op::PhaseOut {
                    base: *base,
                    threshold: threshold.clone(),
                    rate: *rate,
                    agi: new_agi,
                }
            } else {
                op.clone()
            }
        }
        Op::ByStatus { values } => {
            let new_single = redirects.get(&values.single).copied();
            let new_mj = redirects.get(&values.married_joint).copied();
            let new_ms = redirects.get(&values.married_separate).copied();
            let new_hoh = redirects.get(&values.head_of_household).copied();
            let new_qw = redirects.get(&values.qualifying_widow).copied();

            if new_single.is_some() || new_mj.is_some() || new_ms.is_some()
                || new_hoh.is_some() || new_qw.is_some()
            {
                changed = true;
                Op::ByStatus {
                    values: ByStatus {
                        single: new_single.unwrap_or(values.single),
                        married_joint: new_mj.unwrap_or(values.married_joint),
                        married_separate: new_ms.unwrap_or(values.married_separate),
                        head_of_household: new_hoh.unwrap_or(values.head_of_household),
                        qualifying_widow: new_qw.unwrap_or(values.qualifying_widow),
                    },
                }
            } else {
                op.clone()
            }
        }
        Op::IfPositive { cond, then, otherwise } => {
            let new_cond = redirects.get(cond).copied();
            let new_then = redirects.get(then).copied();
            let new_otherwise = redirects.get(otherwise).copied();

            if new_cond.is_some() || new_then.is_some() || new_otherwise.is_some() {
                changed = true;
                Op::IfPositive {
                    cond: new_cond.unwrap_or(*cond),
                    then: new_then.unwrap_or(*then),
                    otherwise: new_otherwise.unwrap_or(*otherwise),
                }
            } else {
                op.clone()
            }
        }
        _ => op.clone(),
    };

    if changed {
        Some(new_op)
    } else {
        None
    }
}

fn remap_invariant(inv: &Invariant, form_id: &str, table_prefix: &str) -> Invariant {
    match inv {
        Invariant::Monotonic { input, output } => Invariant::Monotonic {
            input: format!("{}_{}", form_id, input),
            output: format!("{}_{}", form_id, output),
        },
        Invariant::Bounds { node, min, max } => Invariant::Bounds {
            node: format!("{}_{}", form_id, node),
            min: *min,
            max: *max,
        },
        Invariant::Ordering { table, field } => Invariant::Ordering {
            table: format!("{}{}", table_prefix, table),
            field: field.clone(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn simple_graph(prefix: &str) -> Graph {
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
                op: Op::Literal { value: 1000.0 },
                name: Some("deduction".to_string()),
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

        Graph {
            meta: Some(GraphMeta {
                form_id: Some(prefix.to_string()),
                year: Some(2024),
                generated_by: None,
            }),
            nodes,
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![2],
            invariants: vec![],
        }
    }

    fn importing_graph() -> Graph {
        let mut nodes = HashMap::new();
        nodes.insert(
            0,
            Node {
                id: 0,
                op: Op::Import {
                    form: "form_a".to_string(),
                    line: "taxable".to_string(),
                    year: 2024,
                },
                name: Some("imported_taxable".to_string()),
            },
        );
        nodes.insert(
            1,
            Node {
                id: 1,
                op: Op::Literal { value: 0.1 },
                name: Some("rate".to_string()),
            },
        );
        nodes.insert(
            2,
            Node {
                id: 2,
                op: Op::Mul { left: 0, right: 1 },
                name: Some("tax".to_string()),
            },
        );

        Graph {
            meta: Some(GraphMeta {
                form_id: Some("form_b".to_string()),
                year: Some(2024),
                generated_by: None,
            }),
            nodes,
            tables: HashMap::new(),
            inputs: vec![0],
            outputs: vec![2],
            invariants: vec![],
        }
    }

    #[test]
    fn test_link_single_graph() {
        let gs = GraphSet::new().add("form_a", simple_graph("form_a"));
        let linked = gs.link().unwrap();

        assert_eq!(linked.inputs.len(), 1);
        assert_eq!(linked.outputs.len(), 1);
        assert!(linked.node_by_name("form_a_income").is_some());
        assert!(linked.node_by_name("form_a_taxable").is_some());
    }

    #[test]
    fn test_link_two_independent_graphs() {
        let gs = GraphSet::new()
            .add("form_a", simple_graph("form_a"))
            .add("form_b", simple_graph("form_b"));

        let linked = gs.link().unwrap();

        assert_eq!(linked.inputs.len(), 2);
        assert_eq!(linked.outputs.len(), 2);
        assert!(linked.node_by_name("form_a_income").is_some());
        assert!(linked.node_by_name("form_b_income").is_some());
    }

    #[test]
    fn test_link_with_import() {
        let gs = GraphSet::new()
            .add("form_a", simple_graph("form_a"))
            .add("form_b", importing_graph());

        let linked = gs.link().unwrap();

        // form_b's import should be resolved, so only form_a's income is an input
        assert_eq!(linked.inputs.len(), 1);
        assert_eq!(linked.outputs.len(), 2);

        // The tax node should reference form_a's taxable output
        let tax_node = linked.node_by_name("form_b_tax").unwrap();
        if let Op::Mul { left, .. } = &tax_node.op {
            // left should point to form_a_taxable (id 2 with offset 0 = 2)
            assert_eq!(*left, 2);
        } else {
            panic!("Expected Mul operation");
        }
    }

    #[test]
    fn test_unresolved_imports() {
        let gs = GraphSet::new().add("form_b", importing_graph());

        let unresolved = gs.unresolved_imports();
        assert_eq!(unresolved.len(), 1);
        assert_eq!(unresolved[0].form, "form_a");
        assert_eq!(unresolved[0].line, "taxable");
    }

    #[test]
    fn test_link_empty() {
        let gs = GraphSet::new();
        let linked = gs.link().unwrap();

        assert!(linked.nodes.is_empty());
        assert!(linked.inputs.is_empty());
        assert!(linked.outputs.is_empty());
    }
}
