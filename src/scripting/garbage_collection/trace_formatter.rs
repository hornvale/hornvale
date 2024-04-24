use std::fmt::{Display, Formatter as FmtFormatter, Result as FmtResult};

use crate::scripting::garbage_collection::collector::Collector;
use crate::scripting::garbage_collection::trace::Trace;

/// The `Formatter` type.
#[derive(Debug)]
pub struct TraceFormatter<'garbage, T: Trace> {
  /// A garbage collector.
  pub collector: &'garbage Collector,
  /// The inner object.
  pub object: T,
}

impl<'garbage, T: Trace> TraceFormatter<'garbage, T> {
  /// Constructor.
  pub fn new(object: T, collector: &'garbage Collector) -> Self {
    TraceFormatter { object, collector }
  }
}

impl<'garbage, T: Trace> Display for TraceFormatter<'garbage, T> {
  fn fmt(&self, f: &mut FmtFormatter) -> FmtResult {
    self.object.format(f, self.collector)
  }
}
