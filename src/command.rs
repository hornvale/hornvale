//! # Command
//!
//! The command module provides a way to define and execute commands in the
//! game.
//!
//! This is based around a traditional top-down recursive-descent parser, with
//! some additional features to make up for its limitations when it comes to
//! natural language processing.
//!
//! Command parsing works as follows:
//! - The input is scanned and tokenized by the Scanner. This will detect the
//!   prepositions, adverbs, magic words, and other closed groups, but will
//!   be unable to classify the open groups (nouns, verbs, etc).
//! - The Classifier is used to classify the open groups. This will determine
//!   the part of speech of the words based on their position in the sentence
//!   relative to classified tokens.
//! - The Parser is used to parse the tokens into a command. This will match
//!   the tokens to a command, bind them to entities in the world, and return
//!   some tuple of the command and the entities.
//! - The command is executed. This will do a final check to ensure that the
//!   command and its inputs are valid, and then execute the command; this will
//!   create an Action, which represents the actual attempt to alter the world.
//!
//! This binding process has some implications:
//! - The parser must have access to the world.
//! - We can't parse multiple commands at once, as the bindings may change, so
//!   we split the input into individual commands at the input stage, prior to
//!   parsing.

/// A classifier that can be used to determine the type of a word.
pub mod classifier;
/// The command trait and related concepts.
#[allow(clippy::module_inception)]
pub mod command;
/// A collection of core commands.
pub mod commands;
/// An error type.
pub mod error;
/// Some macros.
#[macro_use]
pub mod macros;
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
  pub use super::classifier::Classifier;
  pub use super::command::{
    arity::CommandArity, function::CommandFunction, modifier::CommandModifier, syntax::CommandSyntax, Command,
  };
  pub use super::commands::{
    fail::FailCommand, go_direction::GoDirectionCommand, look_direction::LookDirectionCommand,
    look_here::LookHereCommand, no_op::NoOpCommand, quit::QuitCommand,
  };
  pub use super::error::CommandError;
  pub use super::parser::Parser;
  pub use super::registry::CommandRegistry;
  pub use super::scanner::Scanner;
  pub use super::token::{
    kind::{character::Character, her::Her, magic_word::MagicWord, word::Word, TokenKind},
    Token,
  };
}
