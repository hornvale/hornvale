//! # Parser
//!
//! The parser module reads the player's input and translates it into commands
//! that the game can understand.
//!
//! This is accomplished via a Chain-of-Responsibility pattern, where different
//! parsers will be asked to parse the input in turn. If a parser is able to
//! parse the input, it will return a command; otherwise, it will pass the
//! input to the next parser in the chain.
//!
//! This allows us to start with a very simple parser that can only understand
//! a few commands, and then add more parsers as needed to increase the
//! complexity of the commands that the game can understand.

/// An error type.
pub mod error;
/// The `Parser` trait, registry, etc.
pub mod parser;
/// A collection of parsers.
pub mod parsers;

/// The prelude.
pub mod prelude {
  pub use crate::error::ParserError;
  pub use crate::parser::{manager::ParserManager, registry::ParserRegistry, Parser};
  pub use crate::parsers::{fail::FailParser, no_op::NoOpParser};
}