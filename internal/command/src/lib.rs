//! # Command
//!
//! The command module provides a way to define and execute commands in the
//! game.
//!
//! The command parser reads the player's input and translates it into commands
//! that the game can understand.
//!
//! Originally, I was going to use a chain-of-responsibility pattern, but I
//! decided to use a more traditional parser instead. This will allow me to
//! handle more complex commands and provide better error messages.
//!
//! The parser evaluates the tokens within the context of the world, uses them
//! to match the player's input to a command, and binds them to entities within
//! the world.
//!
//! This binding process has some implications:
//! - The parser must have access to the world.
//! - We can't parse multiple commands at once, as the bindings may change, so
//!   we split the input into individual commands at the input stage, prior to
//!   parsing.

/// A classifier that can be used to determine the type of a word.
pub mod classifier;
/// A collection of core commands.
pub mod commands;
/// An error type.
pub mod error;
/// The parser, a simple top-down recursive descent parser.
pub mod parser;
/// A registry for commands.
pub mod registry;
/// A scanner for breaking input into tokens.
pub mod scanner;
/// Tokens for the scanner.
pub mod token;

/// The prelude.
pub mod prelude {
  pub use super::commands::{fail::FailCommand, no_op::NoOpCommand, quit::QuitCommand};
  pub use super::error::CommandError;
  pub use super::registry::CommandRegistry;
  pub use crate::classifier::Classifier;
  pub use crate::parser::Parser;
  pub use crate::scanner::Scanner;
  pub use crate::token::{
    kind::{character::Character, her::Her, magic_word::MagicWord, word::Word, TokenKind},
    Token,
  };
}
