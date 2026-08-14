//! The hearsay window: what a community holds to be true, derived from the
//! committed ledger and nothing else.
//!
//! Decision 0100 puts myth in the derived register and says it "has no channel
//! today". This crate is that channel's read side. It draws nothing, commits
//! nothing, and owns no seed labels — it is a window, not a domain.
#![warn(missing_docs)]

pub mod lineage;
