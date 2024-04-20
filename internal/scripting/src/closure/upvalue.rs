use crate::garbage_collection::collector::Collector as GarbageCollector;
use crate::garbage_collection::trace::Trace;
use crate::value::Value;
use derive_more::Display;
use std::any::Any;
use std::fmt::{Formatter, Result as FmtResult};
use std::mem::size_of;

/// What's upvalue?  Nawmuch, man, what's up with you?
#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "location: {}, closed: {:#?}", location, closed)]
pub struct Upvalue {
  /// The index of this item.
  pub location: usize,
  /// A closed-over value.
  pub closed: Option<Value>,
}

impl Upvalue {
  /// Constructor.
  pub fn new(location: usize) -> Self {
    let closed = None;
    Upvalue { location, closed }
  }
}

impl Trace for Upvalue {
  fn format(&self, f: &mut Formatter, _garbage_collector: &GarbageCollector) -> FmtResult {
    let result = write!(f, "upvalue");
    result
  }

  fn get_size(&self) -> usize {
    size_of::<Upvalue>()
  }

  fn trace(&self, garbage_collector: &mut GarbageCollector) {
    if let Some(object) = self.closed {
      garbage_collector.mark_value(object);
    }
  }

  fn as_any(&self) -> &dyn Any {
    self
  }

  fn as_any_mut(&mut self) -> &mut dyn Any {
    self
  }
}
