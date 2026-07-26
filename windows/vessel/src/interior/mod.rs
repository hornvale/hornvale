//! The room INTERIOR (The Hearth): a small graph of named anchors with declared
//! topological relations, derived from authored patterns. The fine layer is the
//! coarse layer one scale down — rooms:ways :: anchors:relations — so it reuses
//! the kernel planner and the field shape rather than paralleling them.
//!
//! NOTHING HERE IS SERIALIZED (decision 0069): an anchor has no coordinate, and
//! outcomes read TOPOLOGY, never metrics (spec §2.1), so a future rendering
//! solve can be retuned forever without an epoch.

pub mod anchor;
pub mod relation;

pub use anchor::{Anchor, AnchorId, AnchorKind, Interior};
pub use relation::{Rcc8, compose, converse, is_symmetric, is_transitive};
