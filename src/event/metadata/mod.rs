use uuid::Uuid;

use crate::event::EventStack;
use crate::event::EventStackEntry;
use crate::event::EventTag;

/// The `EventMetadata` struct.
#[derive(Clone, Debug, Default)]
pub struct Metadata {
  stack: EventStack,
  tags: Vec<EventTag>,
  uuid: Uuid,
}

impl Metadata {
  /// Creates a new `Metadata` object.
  pub fn new() -> Self {
    Self {
      stack: EventStack::new(),
      tags: vec![],
      uuid: Uuid::new_v4(),
    }
  }

  /// Get the UUID for this event.
  pub fn get_uuid(&self) -> uuid::Uuid {
    self.uuid
  }

  /// Get the stacktrace for this event.
  pub fn get_stacktrace(&self) -> Vec<String> {
    self.stack.iter().rev().map(|e| e.to_string()).collect()
  }

  /// Get the stack for this event.
  pub fn get_stack(&self) -> &crate::event::EventStack {
    &self.stack
  }

  /// Set the stack for this event.
  pub fn set_stack(&mut self, stack: crate::event::EventStack) {
    self.stack = stack;
  }

  /// Add an entry to the stack for this event.
  pub fn add_stack_entry(&mut self, entry: EventStackEntry) {
    let mut stack = self.get_stack().clone();
    stack.push(entry);
    self.set_stack(stack);
  }

  /// Remove an entry from the stack for this event.
  pub fn remove_stack_entry(&mut self, entry: EventStackEntry) {
    let mut stack = self.get_stack().clone();
    stack.retain(|e| *e != entry);
    self.set_stack(stack);
  }

  /// Get tags for this event.
  pub fn get_tags(&self) -> Vec<EventTag> {
    self.tags.clone()
  }

  /// Set tags for this event.
  pub fn set_tags(&mut self, tags: Vec<EventTag>) {
    self.tags = tags;
  }

  /// Add a tag to this event.
  pub fn add_tag(&mut self, tag: EventTag) {
    let mut tags = self.get_tags();
    tags.push(tag);
    self.set_tags(tags);
  }

  /// Remove a tag from this event.
  pub fn remove_tag(&mut self, tag: EventTag) {
    let mut tags = self.get_tags();
    tags.retain(|t| *t != tag);
    self.set_tags(tags);
  }
}
