//! Founder-handle collisions must not kill a world.
//!
//! `founder_handle` keys on `(people, site, founded, ended, peak_population)`
//! and deliberately excludes the occupation's entity id (decision 0051), so two
//! occupations that agree on all five collide **by construction**. Until this
//! battery, `select_founders` asserted the collision away and a world that
//! produced one died: seeds 283 and 705 — both inside the census range 0–999 —
//! panicked with `two selected founders share handle …`, and the once-per-
//! campaign census could not run.
//!
//! The authorized repair (Nathan, The Radiation) is a **fidelity cut**, not a
//! key change: the later of two indistinguishable occupations is dropped from
//! the cast and reported, so that people remembers one founder fewer. Widening
//! the key is the real fix and is deferred as an epoch —
//! `MEM-founder-handle-epoch` in the idea registry.
//!
//! What this battery pins:
//!
//! 1. **liveness** — the five known colliding seeds build to `Full` depth,
//!    which is the depth the panic used to fire at;
//! 2. **the size of the cut** — how many founders each seed loses, so a later
//!    change that silently alters promotion coverage reddens here;
//! 3. **no backfill** — the losing people ends one short of `MEMORY_DEPTH`
//!    rather than pulling a smaller occupation up into the cast.
//!
//! Seed 2793 is deliberately pinned at **zero** drops: it is `main`'s known
//! collision (on `bugbear`), and it does *not* collide on this tree. It is here
//! so that a future absorption that re-exposes it is visible rather than
//! surprising.
//!
//! Cost: eight `BuildDepth::Settlements` builds and two `Full` builds, ~2 s
//! each on an optimized dev profile — cheap enough for the commit gate, which
//! is where a liveness guard belongs.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::person_promote::{MEMORY_DEPTH, select_founders};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, occupation_records,
};
use std::collections::BTreeSet;

/// Build one seed to `depth` with every pin at its default — the same
/// configuration the census sweeps.
fn build(seed: u64, depth: BuildDepth) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        depth,
    )
    .unwrap_or_else(|e| panic!("seed {seed} failed to build to {depth:?}: {e:?}"))
}

/// The whole promotion path, on the two census-range seeds that used to kill
/// it. `Full` is the depth `promote` runs at, so this is the end-to-end
/// liveness claim rather than a claim about `select_founders` alone.
///
/// claim: structural(seed: [283, 705]) — two named worlds, built once each. No
/// search: the seeds come from a completed 0–2999 sweep, not from this test.
#[test]
fn a_colliding_seed_builds_to_full_depth_instead_of_panicking() {
    for seed in [283u64, 705] {
        let w = build(seed, BuildDepth::Full);
        let people = w.ledger.find("is-person").count();
        assert!(
            people > 0,
            "seed {seed} built, but promoted no person at all — the drop must \
             cost one founder, not the whole cast"
        );
    }
}

/// How large the cut is, seed by seed. These are measurements, not targets: a
/// change that moves them is a change in promotion coverage and must be read,
/// not re-pinned reflexively.
///
/// claim: structural(seed: [42, 283, 705, 2403, 2634, 2793, 2898]) — seven
/// named worlds with pinned per-seed values. Not a sweep and not a search: the
/// enumeration is the whole of a completed 0–2999 sweep's positive set plus two
/// controls, so nothing here scans for an instance.
#[test]
fn the_dropped_founders_are_pinned_per_seed() {
    // (seed, founders dropped). The five colliding seeds are the whole of
    // `the-radiation`'s failure set over seeds 0–2999 (the campaign's
    // founder-collision diagnosis); 42 and 2793 are the controls.
    let expected: [(u64, usize); 7] = [
        (42, 0),
        (283, 1),
        (705, 1),
        (2403, 1),
        (2634, 1),
        (2793, 0),
        (2898, 1),
    ];
    for (seed, drops) in expected {
        let w = build(seed, BuildDepth::Settlements);
        let occs = occupation_records(&w);
        let cast = select_founders(&occs);
        assert_eq!(
            cast.unremembered.len(),
            drops,
            "seed {seed} dropped {} founders, expected {drops}",
            cast.unremembered.len()
        );
        let handles: BTreeSet<u64> = cast.remembered.iter().map(|f| f.handle.0).collect();
        assert_eq!(
            handles.len(),
            cast.remembered.len(),
            "seed {seed}: the promoted cast must carry distinct handles — that \
             is the property the drop exists to restore"
        );
        for u in &cast.unremembered {
            assert_ne!(
                u.occupation, u.kept,
                "seed {seed}: a founder cannot be dropped in favour of itself"
            );
            assert_eq!(
                u.handle.0,
                hornvale_history::flesh::founder_handle(&occs[u.kept]).0,
                "seed {seed}: a drop is only ever justified by an equal handle"
            );
        }
    }
}

/// The cut is a drop, not a substitution: nothing is pulled up to fill the
/// hole, so the losing people ends `MEMORY_DEPTH - 1` deep. That is the more
/// honest of the two shapes — the world forgets a founder rather than
/// remembering a different one — and it is what makes the change invisible to
/// every occupation that did not collide.
#[test]
fn a_dropped_founder_is_not_backfilled() {
    let w = build(283, BuildDepth::Settlements);
    let occs = occupation_records(&w);
    let cast = select_founders(&occs);
    let dropped = cast
        .unremembered
        .first()
        .expect("seed 283 drops exactly one founder");
    let people = dropped.people;
    let promoted = cast
        .remembered
        .iter()
        .filter(|f| f.people == people)
        .count();
    let available = occs.iter().filter(|o| o.core.people == people).count();
    assert!(
        available > MEMORY_DEPTH,
        "seed 283's {people:?} must have more occupations ({available}) than \
         the memory depth, or this test proves nothing about the cap"
    );
    assert_eq!(
        promoted,
        MEMORY_DEPTH - 1,
        "the drop must leave {people:?} one short of the depth, not backfilled \
         to it"
    );
}
