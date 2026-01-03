//! Grammar system for command parsing.
//!
//! This module provides a unified command system featuring:
//! - **Type predicates**: DSL expressions evaluated against world state
//! - **Command definitions**: Aliases + typed forms in one place
//! - **Trie-based intent dispatch**: Longest prefix match for aliases
//! - **Linear form matching**: Typed slots with predicate validation
//!
//! ## Architecture
//!
//! The grammar system uses a hybrid trie/pattern approach:
//!
//! 1. **Intent Dispatch (Trie)**: Aliases route to commands via longest prefix match.
//!    Multi-word aliases like "pick up" are supported.
//!
//! 2. **Form Matching**: Once a command is identified, remaining tokens are matched
//!    against the command's forms. Forms are tried in priority order (longer first).
//!
//! 3. **Type Validation**: Slots can have type constraints (e.g., `:portable`).
//!    Type predicates are DSL expressions evaluated against resolved entities.
//!
//! ## DSL Syntax
//!
//! ```lisp
//! ;; Type predicates
//! (type portable (has? entity :Portable))
//! (type container (has? entity :Container))
//!
//! ;; Command definition
//! (command look
//!   :aliases ("l" "examine" "x")
//!   :forms
//!     (()                        -> look-around)
//!     (("at" obj:noun)           -> (examine obj))
//!     (("in" obj:container)      -> (search obj))
//!     ((dir:direction)           -> (look-direction dir)))
//!
//! (command take
//!   :aliases ("get" "grab" ("pick" "up"))
//!   :forms
//!     ((obj:portable)            -> (take obj))
//!     ((obj:portable "from" c:container) -> (take-from obj c)))
//! ```
//!
//! ## Future Directions
//!
//! This system is designed to eventually support:
//! - Nested clauses ("tell harry that there is a fish in the percolator")
//! - NP-list parsing ("harry and dale")
//! - Full PEG parsing for complex constructions
//! - Proposition types for NPC belief systems

mod command;
mod matcher;
mod predicate;
mod registry;
mod trie;
mod types;

pub use command::{Command, Form, FormAction, FormElement, GrammarMatch, SlotValue};
pub use registry::CommandRegistry;
pub use trie::IntentTrie;
pub use types::{SlotType, TypePredicate, TypeRegistry};
