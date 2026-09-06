//! The warp clause (The Warp, Task 3; spec §4.1–4.2): the walk-band
//! sentence names the rock underfoot and how the ground tilts, through the
//! one implementation `hornvale_worldgen::warp` holds for both words. A
//! per-vertex rock reads constant across a vertex's ~4,096 rooms; that is
//! the field's real resolution, disclosed rather than refined (decision
//! 0123), the same way the water kind is.

use hornvale_terrain::lithology::RockClass;
use hornvale_worldgen::{Steepness, rock_word};

/// " Underfoot, pale limestone; the ground is level." — leading space, so it
/// splices after the weft clause exactly as `weft_clause` splices after the
/// ruin clause. Never empty: every land facet has a rock and a pitch.
/// type-audit: bare-ok(prose: return)
#[must_use]
pub fn warp_clause(rock: RockClass, steep: Steepness) -> String {
    format!(" Underfoot, {}; {}.", rock_word(rock), ground_phrase(steep))
}

fn ground_phrase(steep: Steepness) -> &'static str {
    match steep {
        Steepness::Level => "the ground is level",
        Steepness::Sloping => "the ground slopes",
        Steepness::Steep => "the ground pitches steeply",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every `Steepness` value renders through the exhaustive match without
    /// panicking, mirroring `weft_prose::every_kind_renders_nonempty_prose`.
    #[test]
    fn every_steepness_renders_nonempty_prose() {
        for steep in [Steepness::Level, Steepness::Sloping, Steepness::Steep] {
            assert!(!ground_phrase(steep).is_empty());
        }
    }

    /// The clause is never empty — the same "always non-silent for land"
    /// contract the module doc states.
    #[test]
    fn clause_is_never_empty() {
        let clause = warp_clause(RockClass::ReefLimestone, Steepness::Level);
        assert!(!clause.is_empty());
        assert!(clause.starts_with(" Underfoot, "));
    }
}
