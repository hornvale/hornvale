//! # Output
//!
//! This module provides a way to output text to the user.

/// An error type.
pub mod error;
/// An output queue.
pub mod queue;
/// A sink is a destination for output.
pub mod sink;
/// Some commonly used sinks.
pub mod sinks;
/// A writer to both stdout and stderr.
pub mod writer;

/// The prelude.
pub mod prelude {
  pub use super::error::OutputError;
  pub use super::queue::{OutputQueue, StderrQueue, StdoutQueue};
  pub use super::sink::OutputSink;
  pub use super::sinks::{
    generic::{FileSink, GenericSink, StderrSink, StdoutSink},
    string::StringSink,
  };
  pub use super::writer::OutputWriter;
}
