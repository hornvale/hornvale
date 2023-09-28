use std::borrow::BorrowMut;

pub mod node;
pub use node::Node as TraceTreeNode;
pub mod node_type;
pub use node_type::NodeType as TraceTreeNodeType;

/// The `TraceTree` struct.
#[derive(Debug, Default)]
pub struct TraceTree {
  /// The root node of the `TraceTree`.
  root: Option<TraceTreeNode>,
}

impl TraceTree {
  /// Creates a new `TraceTree`.
  pub fn new() -> Self {
    Self { root: None }
  }

  /// Gets the root node of the `TraceTree`.
  pub fn get_root(&self) -> Option<&TraceTreeNode> {
    self.root.as_ref()
  }

  /// Prune the tree of any branches whose nodes are older than the given
  /// `timestamp`.
  pub fn prune(&mut self, timestamp: u64) {
    debug!("Pruning trace tree at timestamp {}.", timestamp);
    if let Some(root) = &mut self.root {
      let root = root.borrow_mut();
      root.prune(timestamp);
    }
  }
}
