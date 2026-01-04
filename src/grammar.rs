//! Grammar system for command parsing.
//!
//! This module provides a unified command system featuring:
//! - **Type predicates**: DSL expressions evaluated against world state
//! - **Command definitions**: Aliases + typed forms in one place
//! - **Trie-based intent dispatch**: Longest prefix match for aliases
//! - **Linear form matching**: Typed slots with predicate validation
//! - **Fallback patterns**: DSL-defined error messages for partial matches
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
//! 4. **Fallback Matching**: If no form matches, command-specific fallbacks provide
//!    helpful error messages. A global fallback handles completely unrecognized input.
//!
//! ## DSL Syntax
//!
//! ```lisp
//! ;; Type predicates
//! (type portable (has? entity :Portable))
//! (type container (has? entity :Container))
//!
//! ;; Command definition with fallbacks
//! (command kill
//!   :aliases ("attack" "slay")
//!   :forms
//!     (((obj:living) -> (kill obj)))
//!   :fallbacks
//!     (((obj:noun) -> (say "You can't kill " (the obj) "."))
//!      ((_) -> (say "Kill what?"))))
//!
//! (command take
//!   :aliases ("get" "grab" ("pick" "up"))
//!   :forms
//!     ((obj:portable)            -> (take obj))
//!     ((obj:portable "from" c:container) -> (take-from obj c)))
//!
//! ;; Global fallback for unrecognized commands
//! (fallback :default (say "I don't understand that."))
//! ```
//!
//! ## Fallback Patterns
//!
//! Fallbacks use a simpler pattern syntax than forms:
//! - `obj:noun` - matches any resolvable entity (no type checking)
//! - `_` or `rest` - catch-all that matches remaining tokens as text
//! - `"word"` - literal word match
//!
//! Fallbacks are tried in priority order after all forms fail.
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

pub use command::{
    Command, Fallback, FallbackElement, FallbackMatch, Form, FormAction, FormElement, GrammarMatch,
    SlotValue,
};
pub use registry::{CommandRegistry, MatchResult};
pub use trie::IntentTrie;
pub use types::{SlotType, TypePredicate, TypeRegistry};
