//! # Scripting
//!
//! This crate is an experimental programming language, VM, and domain-specific
//! library.
//!
//! As part of my work on **Hornvale**, I want to write a scripting language
//! that is:
//! - powerful
//! - performant
//! - tailored to the subject at hand, which is creating highly extensible
//!   virtual worlds
//!
//! I can afford to compromise the first and second points for the third
//! somewhat, given the nature of the project.
//!
//! This work is based on the scripting language **Lox**, from Robert Nystrom's
//! amazing book [Crafting Interpreters](https://craftinginterpreters.com), and
//! specifically his bytecode-based CLox, but I'm customizing it a bit based on
//! my personal preferences and intended use case.
//!
//! I'm less interested in matching the reference implementation of Lox than I
//! am in incorporating a full-featured and flexible scripting language. I have
//! slightly different syntax in mind than Bob has for Lox, and I'll add some
//! domain-specific extensions, etc.
//!
//! So **TL;DR**: if something's weird or ugly or broken, it's me and not Bob.
//! All hail Bob.  All... hail... Bob.

/// Macros for the scripting library.
#[macro_use]
pub mod macros;

/// A bound method is a method that is bound to an instance.
pub mod bound_method;
/// A chunk is a collection of instructions.
pub mod chunk;
/// A class is a blueprint for instances.
pub mod class;
/// The class compiler.
pub mod class_compiler;
/// A closure is a function with an environment.
pub mod closure;
/// The compiler.
pub mod compiler;
/// Constants for the scripting library.
pub mod constants;
/// An error type for the scripting library.
pub mod error;
/// A function is a named collection of instructions.
pub mod function;
/// Garbage collector implementation.
pub mod garbage_collection;
/// An instance is a class with state.
pub mod instance;
/// An instruction is a single operation.
pub mod instruction;
/// A collection of instructions.
pub mod instructions;
/// The scripting language interpreter.
pub mod interpreter;
/// Local variables.
pub mod local;
/// Native functions are functions written in Rust.
pub mod native_function;
/// The scripting language parser.
pub mod parser;
/// The scanner for the scripting language.
pub mod scanner;
/// The standard library for the scripting language.
pub mod standard_library;
/// A string is a sequence of characters.
pub mod string;
/// A table is a collection of key-value pairs.
pub mod table;
/// A token is a single unit of syntax.
pub mod token;
/// A value is a single piece of data.
pub mod value;
/// The virtual machine that executes bytecode.
pub mod virtual_machine;

/// The prelude.
pub mod prelude {
  pub use super::bound_method::BoundMethod;
  #[allow(unused_imports, unreachable_pub)]
  pub use super::macros::*;
  pub use super::virtual_machine::VirtualMachine;
}
