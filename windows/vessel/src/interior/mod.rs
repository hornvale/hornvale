//! The room INTERIOR (The Hearth): a small graph of named anchors with declared
//! topological relations, derived from authored patterns. The fine layer is the
//! coarse layer one scale down — rooms:ways :: anchors:relations — so it reuses
//! the kernel planner and the field shape rather than paralleling them.
//!
//! NOTHING HERE IS SERIALIZED (decision 0069): an anchor has no coordinate, and
//! outcomes read TOPOLOGY, never metrics (spec §2.1), so a future rendering
//! solve can be retuned forever without an epoch.

pub mod anchor;
pub mod field;
pub mod pattern;
pub mod relation;
pub mod route;

pub use anchor::{Anchor, AnchorId, AnchorKind, Interior};
pub use field::{HEARTH_WARMTH, WARMTH_DECAY, warmth_at};
pub use pattern::{Attach, INVENTORY, Pattern, compose, permits, selection};
// `relation::compose` and `pattern::compose` collide; the relation one is
// re-exported under an unambiguous name rather than shadowing either.
pub use relation::{Rcc8, compose as compose_relations, converse, is_symmetric, is_transitive};
pub use route::{InteriorSpace, route_within};
