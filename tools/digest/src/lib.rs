//! The Digest — the project's compacted knowledge of itself.
//!
//! A non-`World` fact ledger in `hornvale_kernel`'s shape. Asserting a fact
//! whose predicate is *functional* replaces its predecessor, so the committed
//! store always holds only what is currently true; superseded facts leave the
//! artifact and survive in git alone (spec §4.3, decision 0088).
//!
//! The ledger carries NO time. `Fact::place` and `Fact::day` are always
//! `None`; project time is git's (spec §4.2).
#![warn(missing_docs)]

pub mod scan;
pub mod store;
pub mod vocabulary;
