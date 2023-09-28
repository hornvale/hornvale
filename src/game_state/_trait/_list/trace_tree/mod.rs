use crate::trace_tree::TraceTree as TraceTreeStruct;

/// The `TraceTree` trait.
pub trait TraceTree {
  /// Get the trace tree.
  fn get_trace_tree(&self) -> &TraceTreeStruct;
  /// Set the trace tree.
  fn set_trace_tree(&mut self, value: TraceTreeStruct);
  /// Get a mutable reference to the trace tree.
  fn get_trace_tree_mut(&mut self) -> &mut TraceTreeStruct;
  /// Prune the trace tree, removing events older than the given timestamp.
  fn prune_trace_tree(&mut self, timestamp: u64);
}
