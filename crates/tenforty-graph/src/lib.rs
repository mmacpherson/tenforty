pub mod autodiff;
pub mod eval;
pub mod graph;
pub mod link;
pub mod primitives;
pub mod solver;
pub mod viz;

pub use graph::{FilingStatus, Graph, Node, NodeId, Op, TableId};
pub use eval::{Runtime, Scenario, ScenarioResult, eval_batch, eval_batch_named};
pub use autodiff::gradient;
pub use solver::solve;
pub use link::{GraphSet, LinkError, UnresolvedImport};

#[cfg(feature = "jit")]
pub mod jit;

#[cfg(feature = "python")]
mod python;

#[cfg(feature = "wasm")]
mod wasm;
