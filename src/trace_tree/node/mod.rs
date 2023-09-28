use std::cell::RefCell;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::rc::{Rc, Weak};

use crate::time::get_timestamp;
use crate::trace_tree::TraceTreeNodeType;

/// Represents a node in a trace tree.
#[derive(Debug)]
pub struct Node {
  pub r#type: TraceTreeNodeType,
  pub timestamp: u64,
  parent: Option<Weak<RefCell<Node>>>,
  children: Vec<Rc<RefCell<Node>>>,
}

impl Node {
  /// Creates a new node with the given type and parent.
  pub fn new(r#type: TraceTreeNodeType, parent: Option<&Rc<RefCell<Node>>>) -> Rc<RefCell<Self>> {
    let node = Rc::new(RefCell::new(Node {
      r#type,
      timestamp: get_timestamp(),
      parent: parent.map(Rc::downgrade),
      children: Vec::new(),
    }));

    if let Some(p) = parent {
      p.borrow_mut().children.push(node.clone());
    }

    node
  }

  /// Adds a child to this node.
  pub fn add_child(node: &Rc<RefCell<Self>>, child_type: TraceTreeNodeType) -> Rc<RefCell<Node>> {
    Node::new(child_type, Some(node))
  }

  /// Prune the tree of any branches whose nodes are older than the given
  /// `timestamp`.
  pub fn prune(&mut self, timestamp: u64) {
    debug!(
      "Pruning --> children of node {} at timestamp {}.",
      self.r#type.get_name(),
      timestamp
    );
    self.children.retain(|child| {
      let mut child = child.borrow_mut();
      child.prune(timestamp);
      let keep = child.timestamp <= timestamp;
      debug!(
        "{} <-- node {} at timestamp {}.",
        if keep { "Keeping" } else { "Pruning" },
        child.r#type.get_name(),
        child.timestamp
      );
      keep
    });
  }

  /// Returns the backtrace of this node.
  pub fn backtrace(&self) -> Vec<String> {
    let mut trace = Vec::new();
    let mut current = self.parent.as_ref().and_then(Weak::upgrade);
    trace.push(format!("{}", self));
    while let Some(current_node_rc) = current {
      trace.push(format!("{}", current_node_rc.borrow()));
      let next_parent = current_node_rc.borrow().parent.as_ref().and_then(Weak::upgrade);
      current = next_parent;
    }
    trace
  }
}

impl Display for Node {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    // Format the current node as follows:
    // - the timestamp of the node, formatted to 10 characters wide
    // - the type of the node, formatted to 8 characters wide and right-aligned
    // - the name of the node, formatted to 16 characters wide and left-aligned
    write!(
      f,
      "{:16} {:>8} {:<16}",
      self.timestamp,
      self.r#type.get_kind(),
      self.r#type.get_name()
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_new() {
    init();
    let node = Node::new(TraceTreeNodeType::Action("Test".to_string()), None);
    assert_eq!(node.borrow().r#type, TraceTreeNodeType::Action("Test".to_string()));
    assert!(node.borrow().parent.is_none());
    assert_eq!(node.borrow().children.len(), 0);
  }

  #[test]
  fn test_add_child() {
    init();
    let node = Node::new(TraceTreeNodeType::Action("Test".to_string()), None);
    let _child = Node::add_child(&node, TraceTreeNodeType::Action("Test".to_string()));
    assert_eq!(node.borrow().children.len(), 1);
  }

  #[test]
  fn test_backtrace() {
    init();
    let node = Node::new(TraceTreeNodeType::Action("Test 1".to_string()), None);
    let child = Node::add_child(&node, TraceTreeNodeType::Action("Test 2".to_string()));
    let grandchild = Node::add_child(&child, TraceTreeNodeType::Action("Test 3".to_string()));
    let trace = grandchild.borrow().backtrace();
    println!("{:?}", trace);
    assert_eq!(trace.len(), 3);
    assert!(trace[0].contains("Test 3"));
    assert!(trace[1].contains("Test 2"));
    assert!(trace[2].contains("Test 1"));
  }

  #[test]
  fn test_prune1() {
    // Set up a tree like this:
    //
    // ```
    // Test 1
    //   Test 2
    //     Test 3
    //       Test 4
    //         Test 5
    //       Test 6
    //     Test 7
    //       Test 8
    //
    // ```
    //
    // Then prune the tree at 3, which should remove Test 4, 5, 6, 7, and 8.
    init();
    let test1 = Node::new(TraceTreeNodeType::Action("Test 1".to_string()), None);
    let test2 = Node::add_child(&test1, TraceTreeNodeType::Action("Test 2".to_string()));
    let test3 = Node::add_child(&test2, TraceTreeNodeType::Action("Test 3".to_string()));
    let test4 = Node::add_child(&test3, TraceTreeNodeType::Action("Test 4".to_string()));
    let test5 = Node::add_child(&test4, TraceTreeNodeType::Action("Test 5".to_string()));
    let test6 = Node::add_child(&test3, TraceTreeNodeType::Action("Test 6".to_string()));
    let test7 = Node::add_child(&test2, TraceTreeNodeType::Action("Test 7".to_string()));
    let test8 = Node::add_child(&test7, TraceTreeNodeType::Action("Test 8".to_string()));
    // Set the timestamps of the nodes to reflect their numbering in the graph
    // above.
    test1.borrow_mut().timestamp = 1;
    test2.borrow_mut().timestamp = 2;
    test3.borrow_mut().timestamp = 3;
    test4.borrow_mut().timestamp = 4;
    test5.borrow_mut().timestamp = 5;
    test6.borrow_mut().timestamp = 6;
    test7.borrow_mut().timestamp = 7;
    test8.borrow_mut().timestamp = 8;
    // Prune the tree at 3.
    test1.borrow_mut().prune(3);
    // Check that the tree has been pruned as expected.
    assert_eq!(test1.borrow().children.len(), 1);
    assert_eq!(test2.borrow().children.len(), 1);
    assert_eq!(test3.borrow().children.len(), 0);
  }

  #[test]
  fn test_prune2() {
    // Set up a tree like this:
    //
    // ```
    // Test 1
    //   Test 2
    //     Test 3
    //     Test 4
    //   Test 5
    //     Test 6
    //   Test 7
    //     Test 8
    //
    // ```
    //
    // Then prune the tree at 3, which should remove Test 4, 5, 6, 7, and 8.
    init();
    let test1 = Node::new(TraceTreeNodeType::Action("Test 1".to_string()), None);
    let test2 = Node::add_child(&test1, TraceTreeNodeType::Action("Test 2".to_string()));
    let test3 = Node::add_child(&test2, TraceTreeNodeType::Action("Test 3".to_string()));
    let test4 = Node::add_child(&test2, TraceTreeNodeType::Action("Test 4".to_string()));
    let test5 = Node::add_child(&test1, TraceTreeNodeType::Action("Test 5".to_string()));
    let test6 = Node::add_child(&test5, TraceTreeNodeType::Action("Test 6".to_string()));
    let test7 = Node::add_child(&test1, TraceTreeNodeType::Action("Test 7".to_string()));
    let test8 = Node::add_child(&test7, TraceTreeNodeType::Action("Test 8".to_string()));
    // Set the timestamps of the nodes to reflect their numbering in the graph
    // above.
    test1.borrow_mut().timestamp = 1;
    test2.borrow_mut().timestamp = 2;
    test3.borrow_mut().timestamp = 3;
    test4.borrow_mut().timestamp = 4;
    test5.borrow_mut().timestamp = 5;
    test6.borrow_mut().timestamp = 6;
    test7.borrow_mut().timestamp = 7;
    test8.borrow_mut().timestamp = 8;
    // Prune the tree at 3.
    test1.borrow_mut().prune(3);
    // Check that the tree has been pruned as expected.
    assert_eq!(test1.borrow().children.len(), 1);
    assert_eq!(test2.borrow().children.len(), 1);
    assert_eq!(test3.borrow().children.len(), 0);
  }
}
