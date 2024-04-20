//! This section borrows heavily from Manuel Ceron's Loxido, because he put a
//! ton of excellent thought and work into the garbage collection system.
//!
//! @see https://github.com/ceronman/loxido/
//! @see https://ceronman.com/2021/07/22/my-experience-crafting-an-interpreter-with-rust/
//!
//! I learned a lot from reading his code and his blog.  A lot about garbage
//! collection. A lot about Rust. And a lot about love.

/// The actual collector.
pub mod collector;
/// An object header.
pub mod object_header;
/// A reference to an object.
pub mod reference;
/// The trace trait.
pub mod trace;
/// The trace formatter.
pub mod trace_formatter;
