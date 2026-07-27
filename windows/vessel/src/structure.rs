//! A STRUCTURE: the sparse set of chamber-band places standing at one built
//! locale, and how they connect. Existence below the walk band is a predicate
//! (metaplan §1b.3 law 1) and this module is that predicate.
//!
//! Connectivity is the structure's OWN graph, never mesh adjacency — a deep
//! address is identity, not shape (law 3), so two chambers being triangle
//! neighbours means nothing and is not consulted.

use crate::band::chamber_depth;
use crate::brief::Brief;
use crate::streams::ROOM_CHAMBERS;
use hornvale_kernel::{RoomAddr, Seed};

/// The most chambers one structure may have in v1. A bound, not a target: the
/// point of law 1 is that deep addresses are SPARSE, and an unbounded count
/// would make "every deep address is a place" true by accident.
/// type-audit: bare-ok(count)
pub const MAX_CHAMBERS: usize = 4;

// The collision scan in `structure_at` varies only the LAST base-4 digit
// (`RoomAddr.path` holds child indices 0..4; `pack` rejects anything else), so
// it can only guarantee a free value while `MAX_CHAMBERS <= 4`: with at most
// `MAX_CHAMBERS - 1` prior chambers sharing a prefix, pigeonhole leaves one of
// the four digit values open. Raising `MAX_CHAMBERS` past 4 without widening the
// scan reintroduces an unbounded loop — so the coupling is asserted at compile
// time rather than left as a coincidence of two independent `4`s.
const _: () = assert!(MAX_CHAMBERS <= 4);

/// The sparse set of chambers standing at one built locale.
/// type-audit: bare-ok(index: links)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Structure {
    /// The chamber `enter` arrives in from the locale.
    pub threshold: RoomAddr,
    /// Every chamber, threshold included. Length is `1..=MAX_CHAMBERS`.
    pub chambers: Vec<RoomAddr>,
    /// Undirected apertures as index pairs into `chambers`. Connected, so
    /// every chamber is reachable from `threshold`.
    pub links: Vec<(usize, usize)>,
}

/// The structure at `locale`, or `None` where nothing is built.
///
/// The draw is keyed to the locale's own seed under `room/chambers/v1`, so the
/// same locale in the same world always yields the same structure, and no other
/// locale's draw can perturb it.
/// type-audit: bare-ok(count: walk_depth)
pub fn structure_at(
    locale: &RoomAddr,
    brief: &Brief,
    seed: Seed,
    walk_depth: u32,
) -> Option<Structure> {
    if !brief.built {
        return None;
    }
    let mut stream = locale.seed(seed).derive(ROOM_CHAMBERS).stream();
    // How many chambers: 1..=MAX_CHAMBERS, one draw.
    let count = 1 + (stream.next_u64() as usize) % MAX_CHAMBERS;
    // Which descendants: one draw per chamber, rejecting repeats by scanning
    // forward deterministically rather than re-drawing (a re-draw loop would
    // consume a variable number of draws and make the stream position depend
    // on collisions).
    let depth = chamber_depth(walk_depth);
    let extra = (depth - locale.depth()) as usize;
    let mut chambers: Vec<RoomAddr> = Vec::with_capacity(count);
    for _ in 0..count {
        let draw = stream.next_u64();
        let mut candidate = child_path(locale, draw, extra);
        // Deterministic forward scan on collision: bump the last digit.
        while chambers.contains(&candidate) {
            let last = candidate.path.len() - 1;
            candidate.path[last] = (candidate.path[last] + 1) % 4;
        }
        chambers.push(candidate);
    }
    let threshold = chambers[0].clone();
    // A path graph rooted at the threshold: minimal, connected, and honest
    // about being minimal. Richer topologies are The Precincts' business.
    let links = (1..chambers.len()).map(|i| (i - 1, i)).collect();
    Some(Structure {
        threshold,
        chambers,
        links,
    })
}

/// Extend `locale`'s path by `extra` child digits taken from `draw`, two bits
/// at a time. Integer only.
fn child_path(locale: &RoomAddr, draw: u64, extra: usize) -> RoomAddr {
    let mut path = locale.path.clone();
    for i in 0..extra {
        path.push(((draw >> (2 * i)) & 0b11) as u8);
    }
    RoomAddr {
        face: locale.face,
        path,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use hornvale_kernel::Seed;

    const WALK: u32 = 12;

    fn locale() -> hornvale_kernel::RoomAddr {
        hornvale_kernel::RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    fn built_brief() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    #[test]
    fn an_unbuilt_locale_has_no_structure() {
        let wild = Brief::from_parts(None, None, None, None, false, true);
        assert!(structure_at(&locale(), &wild, Seed(42), WALK).is_none());
    }

    #[test]
    fn a_built_locale_has_a_bounded_chamber_set() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        assert!(
            (1..=MAX_CHAMBERS).contains(&s.chambers.len()),
            "sparseness: got {} chambers",
            s.chambers.len()
        );
    }

    #[test]
    fn every_chamber_sits_at_the_chamber_depth_under_this_locale() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        for c in &s.chambers {
            assert_eq!(c.depth(), chamber_depth(WALK));
            assert_eq!(c.face, locale().face);
            assert_eq!(c.path[..WALK as usize], locale().path[..]);
            assert!(c.pack().is_ok(), "a chamber must pack: {c:?}");
        }
    }

    #[test]
    fn chambers_are_distinct() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        let ids: std::collections::BTreeSet<u64> =
            s.chambers.iter().map(|c| c.pack().unwrap().0).collect();
        assert_eq!(ids.len(), s.chambers.len(), "no chamber may repeat");
    }

    #[test]
    fn derivation_is_pure() {
        let a = structure_at(&locale(), &built_brief(), Seed(42), WALK);
        let b = structure_at(&locale(), &built_brief(), Seed(42), WALK);
        assert_eq!(a, b);
    }

    #[test]
    fn a_different_locale_gives_a_different_structure() {
        // NOTE the honest name. This asserts only LOCALE-keying, and it is
        // near-tautological: `child_path` clones the locale's path before
        // appending drawn digits, so two locales differ at the inherited prefix
        // whatever the draw does. Kept because the prefix-inheritance property
        // is itself worth pinning — but it does NOT show the seed is read.
        // `the_draw_is_keyed_to_the_world_seed` below is what covers that.
        let here = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        let mut elsewhere_path = locale().path;
        elsewhere_path[0] = (elsewhere_path[0] + 1) % 4;
        let elsewhere = hornvale_kernel::RoomAddr {
            face: 3,
            path: elsewhere_path,
        };
        let there = structure_at(&elsewhere, &built_brief(), Seed(42), WALK).expect("built");
        assert_ne!(
            here.chambers, there.chambers,
            "a structure is keyed to its own locale"
        );
    }

    #[test]
    fn the_draw_is_keyed_to_the_world_seed() {
        // Hold the locale FIXED and vary only the seed. Asserting that two
        // arbitrary seeds differ would be a coin flip on a small space (a count
        // in 1..=4 plus 18 drawn bits), so assert the weaker non-flaky property
        // that actually matters: the seed is read at all.
        let l = locale();
        let b = built_brief();
        let structures: Vec<_> = (0..8u64)
            .map(|s| structure_at(&l, &b, Seed(s), WALK).expect("built"))
            .collect();
        assert!(
            structures.iter().any(|s| *s != structures[0]),
            "eight seeds at one locale produced identical structures — the draw ignores the world seed"
        );
    }

    #[test]
    fn the_threshold_is_a_chamber_and_the_graph_is_connected() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        assert!(s.chambers.contains(&s.threshold));
        // Every chamber reachable from the threshold by `links`.
        let ti = s.chambers.iter().position(|c| *c == s.threshold).unwrap();
        let mut seen = std::collections::BTreeSet::from([ti]);
        let mut frontier = vec![ti];
        while let Some(i) = frontier.pop() {
            for &(a, b) in &s.links {
                for (x, y) in [(a, b), (b, a)] {
                    if x == i && seen.insert(y) {
                        frontier.push(y);
                    }
                }
            }
        }
        assert_eq!(
            seen.len(),
            s.chambers.len(),
            "closure (§1b.8 rule 3): every chamber reachable from the threshold"
        );
    }
}
