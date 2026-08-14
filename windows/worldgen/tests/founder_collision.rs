//! Founder-handle collisions must not kill a world, and the census range must
//! not have any.
//!
//! `founder_handle` excludes the occupation's entity id (decision 0051), so two
//! occupations agreeing on everything it does read collide **by construction**.
//! Until this battery, `select_founders` asserted the collision away and a
//! world that produced one died: under the pre-Ell key, seeds 283 and 705 —
//! both inside the census range 0–999 — panicked with `two selected founders
//! share handle …`, and the once-per-campaign census could not run.
//!
//! Two changes stand behind the numbers below, in order:
//!
//! 1. **The Radiation** made a collision a **fidelity cut** rather than a
//!    death: the later of two indistinguishable occupations is dropped from the
//!    cast and reported, so that people remembers one founder fewer.
//! 2. **The Ell** widened the key — the founding's one-hop ancestry
//!    (`record::founding_key_from`) joined `(people, site, founded, ended,
//!    peak_population)` — and the census range came out clean. Swept over the
//!    whole of seeds 0–999 on this tree (`BuildDepth::Settlements`, default
//!    pins, 2026-08-11): **0 colliding worlds, 0 founders dropped**, against
//!    2 and 2 before it.
//!
//! The drop stays because the widening is **not total**, and this file is where
//! that is visible: at a colliding seed the pair's two *parents* are themselves
//! twins, so the ancestry hop folds identically and the pair ties on everything
//! else too. Restoring `select_founders`'s old fatal assert would therefore stop
//! legal seeds from building at all — which is why The Ell measured the proposal
//! instead of shipping it.
//!
//! **WHICH seeds those are is a property of the placement, not of the key, and
//! decision 0132 proved it — twice now.** The Ell's residual over 0–2999 was
//! 2634 and 2898. The craton-rescale terrain epoch re-placed every settlement
//! and cleared both; a fresh full sweep found `[20, 514, 1412, 1505, 1738,
//! 1892]`, one drop each. **The Glasshouse's thermostat (Stage B Task 4)
//! re-placed every settlement again** and cleared five of those six; a second
//! fresh full sweep of 0–2999 on the post-thermostat tree (768.72 s, ten
//! threads) found `[1892, 2078]`, one drop each — 1892 survives both epochs,
//! 2078 is new. The rate fell again (2 in 3000, against 6 after the terrain
//! epoch and 2 after The Ell). Read the rate, never the membership: the seeds
//! in this file are witnesses that the residual exists and is small, and they
//! will not survive the next epoch either.
//!
//! What this battery pins:
//!
//! 1. **liveness** — the seeds that used to die build to `Full` depth, which is
//!    the depth the panic used to fire at;
//! 2. **the size of the cut** — how many founders each seed loses, so a later
//!    change that silently alters promotion coverage reddens here. Nine of the
//!    eleven rows now read **zero**, and they are the campaign's headline: they
//!    are seeds that used to lose a founder and no longer do, so a regression
//!    that reintroduces the collisions is visible rather than silent;
//! 3. **no backfill** — the losing people ends one short of `MEMORY_DEPTH`
//!    rather than pulling a smaller occupation up into the cast.
//!
//! Seed 2793 is pinned at **zero** drops for a different reason: it is `main`'s
//! known collision (on `bugbear`), and it did not collide on this tree even
//! before the widening. It is here so that a future absorption that re-exposes
//! it is visible rather than surprising.
//!
//! Cost: eleven `BuildDepth::Settlements` builds and two `Full` builds, ~2 s
//! each on an optimized dev profile — cheap enough for the commit gate, which
//! is where a liveness guard belongs.
//!
use hornvale_astronomy::SkyPins;
use hornvale_history::record::{Founding, FoundingCoords};
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::person_promote::{MEMORY_DEPTH, select_founders};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, occupation_records,
};
use std::collections::{BTreeMap, BTreeSet};

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

/// The whole promotion path, on the two seeds that still collide. `Full` is
/// the depth `promote` runs at, so this is the end-to-end liveness claim rather
/// than a claim about `select_founders` alone.
///
/// **These are 1892 and 2078, not 20 and 514 (The Glasshouse, Stage B Task
/// 4).** The thermostat (a damped, greenhouse-forced insolation baseline
/// replacing the fixed 288 K blackbody one) re-placed every settlement in
/// every world a second time this campaign, clearing 20, 514, 1412, 1505 and
/// 1738 along with the rest of the post-craton-rescale positive set. Building
/// a cleared seed would prove only that a world without a collision survives,
/// which is the vacuous form of this test, and the assertion below is what
/// made the clearing visible instead of letting it hollow the test out
/// silently.
///
/// The seeds come from a **re-run of the full 0–2999 sweep** on the
/// post-thermostat tree (768.72 s, ten threads), so the positive-set claim
/// below is a fresh measurement and not an inherited one: `[1892, 2078]`, one
/// drop each. 1892 is the one seed that survives from the previous positive
/// set; 2078 is new. The rate falls from six worlds in three thousand to two
/// — the smallest this file has recorded.
///
/// claim: structural(seed: [1892, 2078]) — two named worlds, built once each.
/// No search: the seeds come from a completed 0–2999 sweep, not from this
/// test.
#[test]
fn a_colliding_seed_builds_to_full_depth_instead_of_panicking() {
    for seed in [1892u64, 2078] {
        let w = build(seed, BuildDepth::Full);
        let people = w.ledger.find("is-person").count();
        assert!(
            people > 0,
            "seed {seed} built, but promoted no person at all — the drop must \
             cost one founder, not the whole cast"
        );
        let cast = select_founders(&occupation_records(&w));
        assert!(
            !cast.unremembered.is_empty(),
            "seed {seed} no longer collides, so this test now proves only that \
             an uncontested world builds. Re-pin it on a seed that does collide, \
             or delete it if none remains"
        );
    }
}

/// How large the cut is, seed by seed. These are measurements, not targets: a
/// change that moves them is a change in promotion coverage and must be read,
/// not re-pinned reflexively.
///
/// **All ten of these read zero after decision 0132, except 1892 — and now
/// 2078 joins it (The Glasshouse, Stage B Task 4).** The thermostat re-placed
/// every settlement in every world a second time this campaign; a **fresh
/// 0–2999 sweep** (768.72 s, ten threads, run outside this test) found `[1892,
/// 2078]`, one drop each — down from six positives after the terrain epoch
/// and two after The Ell.
///
/// The old seeds are KEPT as zero rows rather than deleted. They are the
/// cheapest possible statement of what an epoch did — a collision set does not
/// survive a re-placement — and they redden if a future change resurrects one,
/// which is a thing worth knowing. The cost is five extra `Settlements` builds
/// (eleven seeds against six positives, roughly +9 s).
///
/// claim: structural(seed: [20, 42, 514, 1412, 1505, 1738, 1892, 2078, 2634,
/// 2793, 2898]) — eleven named worlds with pinned per-seed values. Not a sweep
/// and not a search: the enumeration is the whole of a completed 0–2999
/// sweep's positive set plus two controls and the five superseded rows, so
/// nothing here scans for an instance.
#[test]
fn the_dropped_founders_are_pinned_per_seed() {
    // (seed, founders dropped). 1892 and 2078 are the whole of the current
    // positive set over seeds 0-2999; 42 and 2793 are the long-standing
    // controls; the rest are prior positives, kept to record that they
    // cleared. Values measured on this tree.
    let expected: [(u64, usize); 11] = [
        (20, 0),
        (42, 0),
        (514, 0),
        (1412, 0),
        (1505, 0),
        (1738, 0),
        (1892, 1),
        (2078, 1),
        (2634, 0),
        (2793, 0),
        (2898, 0),
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
        // The parent hop the handle folds, resolved the way every caller of
        // `founder_handle` must: by looking the predecessor up in the record
        // set, never by reading an id as a value.
        let coords: BTreeMap<hornvale_kernel::EntityId, FoundingCoords<'static>> = occs
            .iter()
            .map(|o| (o.id, hornvale_history::record::founding_coords(&o.core)))
            .collect();
        let parent_of = |r: &hornvale_history::record::OccupationRecord| match r.founded_from {
            Founding::From(e) => coords.get(&e).copied(),
            Founding::Genesis(_) => None,
        };
        for u in &cast.unremembered {
            assert_ne!(
                u.occupation, u.kept,
                "seed {seed}: a founder cannot be dropped in favour of itself"
            );
            let kept = &occs[u.kept];
            assert_eq!(
                u.handle.0,
                hornvale_history::flesh::founder_handle(kept, parent_of(kept)).0,
                "seed {seed}: a drop is only ever justified by an equal handle"
            );
            // A TRIPWIRE FOR A FUTURE NARROWING — not a detector of new
            // collision shapes, which it cannot be. Under the shipped key the
            // parent's coordinates are folded into the handle by
            // `record::founding_key`, so two records with an equal handle
            // necessarily have equal parent coordinates and this assertion is
            // ENTAILED: it cannot fail while the identity step reads the
            // parent. It is not dead — patching `founder_handle` parent-blind
            // makes it fire, which is how that was established — and that is
            // precisely what it is here to catch. If someone ever removes the
            // ancestry hop from the identity key, worlds start colliding on
            // pairs whose parents differ, and this line reddens before the
            // per-seed counts above have to be re-read one by one.
            let dropped = &occs[u.occupation];
            assert_eq!(
                parent_of(dropped).map(|p| (p.site, p.founded)),
                parent_of(kept).map(|p| (p.site, p.founded)),
                "seed {seed}: two records collided despite different parents, \
                 which the shipped key makes impossible — the identity step has \
                 stopped folding the ancestry hop"
            );
        }
    }
}

/// The cut is a drop, not a substitution: nothing is pulled up to fill the
/// hole, so the losing people ends one short of what it could have remembered.
/// That is the more honest of the two shapes — the world forgets a founder
/// rather than remembering a different one — and it is what makes the change
/// invisible to every occupation that did not collide.
///
/// **The `MEMORY_DEPTH`-binding form of this claim no longer has a live world
/// to stand on.** It used to run on seed 283, whose losing people held more
/// than `MEMORY_DEPTH` occupations, so a backfill would have had a real
/// candidate below the cut to pull up. The Ell cleared 283, and neither seed
/// that still collided could replace it. So the depth-binding case is asserted
/// where it can be constructed — `person_promote.rs`'s
/// `a_drop_costs_one_founder_and_is_not_backfilled`, on a synthetic record set
/// — and what runs here is the live-world half: whatever the cap does, the
/// people ends one short and nothing was substituted in.
///
/// **Seed 1892, not 20 (The Glasshouse, Stage B Task 4).** The thermostat
/// re-placed every world a second time this campaign and cleared 20 along
/// with the rest of the post-craton-rescale positive set; 1892 is the one
/// seed that survives from that set into the fresh sweep's `[1892, 2078]`.
/// The `available > 1` premise below is what makes the substitution question
/// answerable at all, and it is asserted rather than assumed for exactly this
/// reason.
#[test]
fn a_dropped_founder_is_not_backfilled() {
    let w = build(1892, BuildDepth::Settlements);
    let occs = occupation_records(&w);
    let cast = select_founders(&occs);
    let dropped = cast
        .unremembered
        .first()
        .expect("seed 1892 drops exactly one founder");
    let people = dropped.people;
    let promoted = cast
        .remembered
        .iter()
        .filter(|f| f.people == people)
        .count();
    let available = occs.iter().filter(|o| o.core.people == people).count();
    assert!(
        available > 1,
        "seed 1892's {people:?} must hold more than one occupation \
         ({available}), or there is nothing a backfill could have reached for"
    );
    assert_eq!(
        promoted,
        available.min(MEMORY_DEPTH) - 1,
        "the drop must leave {people:?} one short of what it could have \
         remembered ({available} available, cap {MEMORY_DEPTH}), not backfilled \
         to it"
    );
}
