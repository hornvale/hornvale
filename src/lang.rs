//! S-expression language module.
//!
//! This module provides parsing for Hornvale's S-expression DSL:
//!
//! ```lisp
//! ;; Entity definitions
//! (entity goat (Name "goat") (HP 10))
//!
//! ;; Rules
//! (rule goat-baas
//!   :pattern (= (get ?e Name) "goat")
//!   :every 10
//!   :effect (emit-message "Baa!"))
//! ```
//!
//! # Example
//!
//! ```
//! use hornvale::lang::{parse, parse_all};
//!
//! // Parse a single expression
//! let expr = parse("(+ 1 2)").unwrap();
//! assert!(expr.is_call("+"));
//!
//! // Parse multiple expressions
//! let exprs = parse_all("(a) (b) (c)").unwrap();
//! assert_eq!(exprs.len(), 3);
//! ```

mod ast;
mod error;
mod function;
mod lexer;
mod loader;
mod parser;
mod token;

pub use ast::{Atom, SExpr};
pub use error::{LexError, ParseError, Span};
pub use function::{FunctionDef, FunctionRegistry};
pub use lexer::Lexer;
pub use loader::{LoadError, WorldLoader};
pub use parser::{Parser, is_complete, parse, parse_all};
pub use token::{Token, TokenKind};
