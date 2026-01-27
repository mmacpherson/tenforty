use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub type NodeId = u32;
pub type TableId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FilingStatus {
    Single,
    MarriedJoint,
    MarriedSeparate,
    HeadOfHousehold,
    QualifyingWidow,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ByStatus<T> {
    pub single: T,
    pub married_joint: T,
    pub married_separate: T,
    pub head_of_household: T,
    pub qualifying_widow: T,
}

impl<T: Clone> ByStatus<T> {
    pub fn get(&self, status: FilingStatus) -> &T {
        match status {
            FilingStatus::Single => &self.single,
            FilingStatus::MarriedJoint => &self.married_joint,
            FilingStatus::MarriedSeparate => &self.married_separate,
            FilingStatus::HeadOfHousehold => &self.head_of_household,
            FilingStatus::QualifyingWidow => &self.qualifying_widow,
        }
    }

    pub fn uniform(value: T) -> Self {
        ByStatus {
            single: value.clone(),
            married_joint: value.clone(),
            married_separate: value.clone(),
            head_of_household: value.clone(),
            qualifying_widow: value,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bracket {
    pub threshold: f64,
    pub rate: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BracketTable {
    pub brackets: ByStatus<Vec<Bracket>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Op {
    Input,
    Literal {
        value: f64,
    },
    Add {
        left: NodeId,
        right: NodeId,
    },
    Sub {
        left: NodeId,
        right: NodeId,
    },
    Mul {
        left: NodeId,
        right: NodeId,
    },
    Div {
        left: NodeId,
        right: NodeId,
    },
    Max {
        left: NodeId,
        right: NodeId,
    },
    Min {
        left: NodeId,
        right: NodeId,
    },
    Floor {
        arg: NodeId,
    },
    Neg {
        arg: NodeId,
    },
    Abs {
        arg: NodeId,
    },
    BracketTax {
        table: TableId,
        income: NodeId,
    },
    PhaseOut {
        base: f64,
        threshold: ByStatus<f64>,
        rate: f64,
        agi: NodeId,
    },
    ByStatus {
        values: ByStatus<NodeId>,
    },
    Clamp {
        arg: NodeId,
        min: f64,
        max: f64,
    },
    IfPositive {
        cond: NodeId,
        then: NodeId,
        otherwise: NodeId,
    },
    Import {
        form: String,
        line: String,
        year: u16,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    pub form: String,
    pub line: String,
    pub year: u16,
}

impl Op {
    pub fn dependencies(&self) -> Vec<NodeId> {
        match self {
            Op::Input | Op::Literal { .. } | Op::Import { .. } => vec![],
            Op::Add { left, right }
            | Op::Sub { left, right }
            | Op::Mul { left, right }
            | Op::Div { left, right }
            | Op::Max { left, right }
            | Op::Min { left, right } => vec![*left, *right],
            Op::Floor { arg } | Op::Neg { arg } | Op::Abs { arg } | Op::Clamp { arg, .. } => {
                vec![*arg]
            }
            Op::BracketTax { income, .. } => vec![*income],
            Op::PhaseOut { agi, .. } => vec![*agi],
            Op::ByStatus { values } => vec![
                values.single,
                values.married_joint,
                values.married_separate,
                values.head_of_household,
                values.qualifying_widow,
            ],
            Op::IfPositive {
                cond,
                then,
                otherwise,
            } => vec![*cond, *then, *otherwise],
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub id: NodeId,
    pub op: Op,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphMeta {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub form_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub year: Option<u16>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generated_by: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Invariant {
    Monotonic { input: String, output: String },
    Bounds { node: String, min: f64, max: f64 },
    Ordering { table: TableId, field: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Graph {
    #[serde(default)]
    pub meta: Option<GraphMeta>,
    #[serde(default)]
    pub imports: Vec<Import>,
    pub nodes: HashMap<NodeId, Node>,
    #[serde(default)]
    pub tables: HashMap<TableId, BracketTable>,
    #[serde(default)]
    pub inputs: Vec<NodeId>,
    pub outputs: Vec<NodeId>,
    #[serde(default)]
    pub invariants: Vec<Invariant>,
}

impl Graph {
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    pub fn node_by_name(&self, name: &str) -> Option<&Node> {
        self.nodes
            .values()
            .find(|n| n.name.as_deref() == Some(name))
    }

    pub fn node_id_by_name(&self, name: &str) -> Option<NodeId> {
        self.node_by_name(name).map(|n| n.id)
    }

    pub fn output_node_by_name(&self, name: &str) -> Option<&Node> {
        self.outputs
            .iter()
            .filter_map(|id| self.nodes.get(id))
            .find(|n| n.name.as_deref() == Some(name))
    }

    pub fn output_node_id_by_name(&self, name: &str) -> Option<NodeId> {
        self.output_node_by_name(name).map(|n| n.id)
    }

    pub fn topological_order(&self) -> Vec<NodeId> {
        let mut result = Vec::with_capacity(self.nodes.len());
        let mut visited = std::collections::HashSet::new();
        let mut temp_mark = std::collections::HashSet::new();

        fn visit(
            id: NodeId,
            graph: &Graph,
            visited: &mut std::collections::HashSet<NodeId>,
            temp_mark: &mut std::collections::HashSet<NodeId>,
            result: &mut Vec<NodeId>,
        ) {
            if visited.contains(&id) {
                return;
            }
            if temp_mark.contains(&id) {
                panic!("Graph has a cycle at node {}", id);
            }
            temp_mark.insert(id);

            if let Some(node) = graph.nodes.get(&id) {
                for dep in node.op.dependencies() {
                    visit(dep, graph, visited, temp_mark, result);
                }
            }

            temp_mark.remove(&id);
            visited.insert(id);
            result.push(id);
        }

        for &id in self.nodes.keys() {
            visit(id, self, &mut visited, &mut temp_mark, &mut result);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_by_status_get() {
        let values = ByStatus {
            single: 12950.0,
            married_joint: 25900.0,
            married_separate: 12950.0,
            head_of_household: 19400.0,
            qualifying_widow: 25900.0,
        };

        assert_eq!(*values.get(FilingStatus::Single), 12950.0);
        assert_eq!(*values.get(FilingStatus::MarriedJoint), 25900.0);
    }

    #[test]
    fn test_graph_json_roundtrip() {
        let json = r#"{
            "nodes": {
                "0": {"id": 0, "op": {"type": "input"}, "name": "income"},
                "1": {"id": 1, "op": {"type": "literal", "value": 12950}, "name": "std_ded"},
                "2": {"id": 2, "op": {"type": "sub", "left": 0, "right": 1}, "name": "taxable"}
            },
            "tables": {},
            "inputs": [0],
            "outputs": [2]
        }"#;

        let graph = Graph::from_json(json).unwrap();
        assert_eq!(graph.nodes.len(), 3);
        assert_eq!(graph.inputs, vec![0]);
        assert_eq!(graph.outputs, vec![2]);
    }
}
