//! Arithmetic on refinement DEPTH — where chambers sit ([`chamber_depth`]) and
//! how to get back up to the walk band ([`truncate_to_walk`]) before reading
//! any walk-band-keyed datum.
//!
//! **This module is named `depth`, not `band`, and the rename is the point**
//! (The Lexicon of Place). It holds no `Band` type and never did — the kernel's
//! `Band` is the cave ladder, a different thing entirely, and a module here
//! called `band` claimed a word that belongs to it. Everything in this file is
//! a function of a [`Facet`]'s path length, which is depth.
//!
//! The band notation (Rose Window metaplan §1b.3) is still what the depths
//! MEAN: the walk band is the locale a body commits to, the chamber band is
//! the human-scale place inside a structure. An address below the walk band
//! is IDENTITY, NOT SHAPE (§1b.3 law 3) — its triangle geometry means nothing,
//! and connectivity comes from the structure's own graph.
//!
//! **THE SIZES ARE NOT STATED HERE ANY MORE, AND THAT IS THE FIX.** This doc
//! said "the ~1.7 km locale" and "the ~3.3 m place", and both are
//! PRE-CUBE-SPHERE figures — an icosphere depth-12 triangle edge and its nine
//! halvings, from decision 0082, written when the walk band was six levels
//! below the canonical grid. The Pavement moved the walk band to
//! `globe_level + 7` on a cube-sphere, where **depth 13 is 1.126 km per
//! side** and nine halvings is **~2.2 m**, not 3.3. The owner of that number
//! is `hornvale_locale::walk_depth`, whose own doc derives it; ask it rather
//! than trusting a figure restated here, which is the discipline The Pavement
//! adopted after finding sixteen restatements of the walk offset, two of them
//! a whole band stale.
//!
//! It is worth knowing that the stale figure did damage before it was caught:
//! H3, this project's site-density baseline
//! (`windows/lab/tests/suite/site_density.rs`), published its headline gap
//! **2x too large** because 1.7 km makes one facet ~2.89 km2 — very nearly a
//! square mile — so "a site per facet" and "a site per square mile" looked
//! like the same target. At 1.126 km a square mile is 2.04 facets.
//!
//! **The figure survives elsewhere and this file cannot fix that**: decision
//! 0082 (ratified, so it needs superseding rather than editing), decision
//! 0101's band table, `docs/design/room-scale/p2-subdivision-design.md` and
//! several plans all still say ~1.7 km / ~3.3 m. Filed, not fixed here.
//!
//! There is deliberately no `band_of(addr)` classifier: the question a session
//! actually asks is "am I inside a structure?", which is session state
//! (`Session::inside`), not a property an address can answer. An address at
//! chamber depth is a chamber only because a structure put one there.

use hornvale_kernel::Facet;

/// How many refinements below the walk band a chamber sits.
///
/// Nine halvings of the walk-band edge — 1.126 km at depth 13, per
/// `hornvale_locale::walk_depth` — is **~2.2 m**, a human-scale room. This
/// doc said "nine halvings of a ~1.7 km locale edge is ≈3.3 m"; the offset is
/// right and both figures were the pre-cube-sphere ones (see this module's
/// doc). The constant's justification is unchanged by the correction — 2.2 m
/// is still a room a person stands in — which is exactly why nobody noticed.
///
/// Declared as a constant because it is a shape of the world, not a tuning
/// knob: changing it changes which addresses are chambers.
/// type-audit: bare-ok(count)
pub const CHAMBER_DEPTH_OFFSET: u32 = 9;

/// The address depth chambers live at, given the world's walk depth.
/// type-audit: bare-ok(count: walk_depth), bare-ok(count: return)
pub fn chamber_depth(walk_depth: u32) -> u32 {
    walk_depth + CHAMBER_DEPTH_OFFSET
}

/// The walk-band ancestor of `addr` — 0077's path truncation, used DOWNWARD.
///
/// Every walk-band-keyed datum (the settlement-territory set, the locale
/// describer, the climate read) must be consulted with this, never with a raw
/// chamber address. An address at or above the walk band is returned unchanged,
/// so callers may apply this unconditionally — which is the whole point, and the
/// only thing this adds over the kernel primitive it delegates to.
/// type-audit: bare-ok(count: walk_depth)
pub fn truncate_to_walk(addr: &Facet, walk_depth: u32) -> Facet {
    // `Facet::ancestor` (kernel/src/room.rs) already does the bounds-checked
    // slice and returns `None` when `walk_depth` is deeper than the address.
    // Delegate: re-deriving the slice here would duplicate a save-format-
    // adjacent primitive, and a second copy is a second thing to get wrong.
    addr.ancestor(walk_depth).unwrap_or_else(|| addr.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Facet;

    /// The walk depth on the canonical globe (`GLOBE_LEVEL` 6 + 6).
    const WALK: u32 = 13;

    fn addr(depth: u32) -> Facet {
        Facet {
            face: 3,
            // a fixed, arbitrary child sequence: 0,1,2,3,0,1,2,3,...
            path: (0..depth).map(|i| (i % 4) as u8).collect(),
        }
    }

    #[test]
    fn truncation_is_the_identity_at_the_walk_band() {
        let a = addr(WALK);
        assert_eq!(truncate_to_walk(&a, WALK), a);
    }

    #[test]
    fn truncation_yields_the_walk_band_ancestor_of_a_chamber() {
        let chamber = addr(chamber_depth(WALK));
        let walk = truncate_to_walk(&chamber, WALK);
        assert_eq!(walk.depth(), WALK);
        assert_eq!(walk.path[..], chamber.path[..WALK as usize]);
        assert_eq!(walk.face, chamber.face);
    }

    #[test]
    fn a_chamber_and_its_ancestor_pack_to_different_ids() {
        // The footgun Task 2 defends against: a chamber id is NOT its
        // locale's id, so any walk-band-keyed set must be consulted with
        // the TRUNCATED address.
        let chamber = addr(chamber_depth(WALK));
        let walk = truncate_to_walk(&chamber, WALK);
        assert_ne!(chamber.pack().unwrap().0, walk.pack().unwrap().0);
    }

    #[test]
    fn the_chamber_depth_fits_the_packing_cap() {
        assert!(chamber_depth(WALK) as usize <= hornvale_kernel::MAX_DEPTH);
        assert!(addr(chamber_depth(WALK)).pack().is_ok());
    }

    #[test]
    fn an_address_shallower_than_the_walk_band_truncates_to_itself() {
        // Coarser than the walk band: there is nothing to truncate, and this
        // must not panic on a slice out of range.
        let coarse = addr(8);
        assert_eq!(truncate_to_walk(&coarse, WALK), coarse);
    }
}
