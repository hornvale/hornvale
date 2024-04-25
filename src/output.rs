//! # Output
//!
//! This module provides a way to output text to the user.

/// An error type.
pub mod error;
/// Query the world for information or to perform actions.
pub mod query;
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
  pub use super::query as output_query;
  pub use super::queue::{OutputQueue, StderrQueue, StdoutQueue};
  pub use super::sink::OutputSink;
  pub use super::sinks::{
    generic::{FileSink, GenericSink, StderrSink, StdoutSink},
    string::StringSink,
  };
  pub use super::writer::OutputWriter;
}
