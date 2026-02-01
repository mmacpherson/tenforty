pub mod autodiff;
pub mod eval;
pub mod graph;
pub mod link;
pub mod primitives;
pub mod solver;
pub mod viz;

pub use autodiff::gradient;
pub use eval::{eval_batch, eval_batch_named, Runtime, Scenario, ScenarioResult};
pub use graph::{FilingStatus, Graph, Node, NodeId, Op, TableId};
pub use link::{GraphSet, LinkError, UnresolvedImport};
pub use solver::solve;

#[cfg(feature = "jit")]
pub mod jit;

#[cfg(feature = "python")]
mod python;

#[cfg(feature = "wasm")]
mod wasm;
