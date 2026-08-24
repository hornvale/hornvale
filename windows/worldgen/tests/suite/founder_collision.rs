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
//! decision 0134 proved it — three times now.** The Ell's residual over
//! 0–2999 was 2634 and 2898. The craton-rescale terrain epoch re-placed every
//! settlement and cleared both; a fresh full sweep found `[20, 514, 1412,
//! 1505, 1738, 1892]`, one drop each. **The Glasshouse's thermostat (Stage B
//! Task 4)** re-placed every settlement again and cleared five of those six;
//! a second fresh full sweep of 0–2999 on the post-thermostat tree (768.72 s,
//! ten threads) found `[1892, 2078]`, one drop each. **The Glasshouse's
//! area-mean-zero latitude profile (Stage B Task 5)** re-placed every
//! settlement a third time; a third fresh full sweep (798.87 s, ten threads)
//! found `[1741, 1866, 1892]`, one drop each — 1892 survives all three
//! epochs, 1741 and 1866 are new, 2078 cleared. The rate held at 3 in 3000
//! (against 6 after the terrain epoch, 2 after the thermostat alone, and 2
//! after The Ell). Read the rate, never the membership: the seeds in this
//! file are witnesses that the residual exists and is small, and they will
//! not survive the next epoch either.
//!
//! **The Glasshouse's `k` re-decision (0.4 → 0.3)** re-placed every
//! settlement a FOURTH time; a fourth fresh full sweep (692.89 s, ten
//! threads) found `[238, 1439, 1892, 2031, 2465, 2871]`, one drop each.
//! 1892 survives all four epochs and is now the only seed that ever has;
//! 1741 and 1866 cleared after a single epoch, exactly as the paragraph
//! above predicted they would.
//!
//! **AND THIS TIME THE RATE MOVED, WHICH IS THE THING THIS HEADER TELLS YOU
//! TO READ: 3 in 3000 → 6 in 3000.** It has now run 2 → 6 → 2 → 3 → 6 across
//! five placements, so a doubling is inside its observed range and is not by
//! itself alarming. What makes this one legible rather than noise is that an
//! independent quantity moved with it and in the same direction: warming the
//! population raised seed 42 from 620 occupations across 217 sites to 826
//! across 302. More occupations mean more chances that two of them agree on
//! every material fact `founder_handle` reads, so a rising collision count
//! is the expected shadow of a rising settlement count, not evidence about
//! the key. The key is unchanged; the ground under it is not.
//!
//! **The Underworld's node-index re-key (Task 8, spec §4.6)** re-placed every
//! settlement a FIFTH time; a fifth fresh full sweep (744.21 s, ten threads,
//! `--release`) found `[2208, 2465]`, one drop each. 1892's four-epoch run
//! ends here, so no seed has ever survived five. **THE RATE MOVED AGAIN, AND
//! DOWN: 6 in 3000 → 2 in 3000** — back to where The Ell and the thermostat
//! left it, and inside the observed range of 2 → 6 → 2 → 3 → 6 → 2.
//!
//! It is legible for exactly the reason the paragraph above is, running the
//! other way: re-keying the node index on `(vertex, rung)` takes drow out of the
//! competition for surface vertices, and seed 42 falls from 826 occupations across
//! 302 sites to **625 across 264**. Fewer occupations are fewer chances that
//! two of them agree on every material fact `founder_handle` reads. The
//! independent quantity and the collision rate have now moved together, in the
//! same direction, on two consecutive epochs and in OPPOSITE directions — which
//! is a better test of the reading than two rises would have been.
//!
//! **That "625 across 264" is a CORRECTION, and the correction is the point.**
//! The figure first published here was 521 across 217, measured mid-campaign
//! and before Task 9 repaired `chamber_fit`'s genus join. The repair moved
//! every underworld seating and therefore re-placed every world again, and
//! nothing re-ran this paragraph's arithmetic — the sweep it argues from is a
//! 744 s job and the paragraph is prose. Re-measured 2026-08-18 on this tree.
//! The direction of the argument is unchanged (826 → 625 is still a fall) and
//! the magnitude is about half what was claimed.
//!
//! **What was NOT re-run, stated so nobody reads more into the table below
//! than it holds.** `the_dropped_founders_are_pinned_per_seed` is green on this
//! tree, so every pinned per-seed value is still exact. What has not been
//! re-established post-repair is the *completeness* claim — that `[2208, 2465]`
//! is the whole 0–2999 positive set — because that needs a fresh full sweep and
//! this wave did not pay for one. Read the table as "these nineteen seeds drop
//! exactly these counts", which is what it asserts, and not as "no other seed
//! in 0–2999 drops a founder", which it does not.
//!
//! What this battery pins:
//!
//! 1. **liveness** — the seeds that used to die build to `Full` depth, which is
//!    the depth the panic used to fire at;
//! 2. **the size of the cut** — how many founders each seed loses, so a later
//!    change that silently alters promotion coverage reddens here. Seventeen
//!    of the nineteen rows now read **zero**, and they are the campaign's
//!    headline: they
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
//! Cost: nineteen `BuildDepth::Settlements` builds and two `Full` builds, ~2 s
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

/// The whole promotion path, on the seeds that still collide. `Full` is
/// the depth `promote` runs at, so this is the end-to-end liveness claim rather
/// than a claim about `select_founders` alone.
///
/// **These are 238, 1439 and 1892 (The Glasshouse, `k` re-decided 0.4 →
/// 0.3).** The previous set — 1741, 1866, 1892 — lasted exactly one
/// placement: 1741 and 1866 both cleared, and the assertion below is what
/// made that visible rather than letting the test hollow out into "an
/// uncontested world builds". It has now caught a cleared seed on two
/// consecutive re-pins, which is the whole reason it is phrased as a
/// liveness claim with a self-check rather than as three builds.
///
/// The seeds come from a **re-run of the full 0–2999 sweep** on the
/// post-`k` tree (692.89 s, ten threads, run outside this test), so the
/// positive-set claim is a fresh measurement and not an inherited one:
/// `[238, 1439, 1892, 2031, 2465, 2871]`, one drop each. Three of the six
/// are built here; 1892 is the one seed that has survived all four epochs.
/// The rate DOUBLED to six in three thousand — see the module header for why
/// that tracks settlement volume rather than the key.
///
/// **The set turns over a fifth time, and the rate HALVES (The Underworld,
/// Task 8, spec §4.6's node-index re-key).** A fifth fresh 0–2999 sweep
/// (744.21 s, ten threads, `--release`, run outside this test) found
/// `[2208, 2465]`, one drop each. Five of the previous six clear — 238,
/// 1439, 1892, 2031 and 2871 — and 1892's four-epoch run finally ends. Only
/// 2465 survives, joined by one newcomer. That tracks the module header's
/// reading rather than contradicting it: re-keying the node index takes drow
/// out of the competition for surface vertices and seed 42's settlement volume
/// falls by a third (521 occupations across 217 sites, against 826 across
/// 302), and fewer occupations are fewer chances for two of them to collide.
/// **BOTH positives are built here, not three of six** — there is no third.
///
/// claim: structural(seed: [2208, 2465]) — two named worlds, built once each.
/// No search: the seeds come from a completed 0–2999 sweep, not from this
/// test.
#[test]
fn a_colliding_seed_builds_to_full_depth_instead_of_panicking() {
    for seed in [2208u64, 2465] {
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
/// **All ten of these read zero after decision 0134, except 1892 — and
/// 2078 joined it (The Glasshouse, Stage B Task 4).** The thermostat
/// re-placed every settlement in every world a second time this campaign; a
/// **fresh 0–2999 sweep** (768.72 s, ten threads) found `[1892, 2078]`, one
/// drop each.
///
/// **2078 clears and 1741/1866 join 1892 (The Glasshouse, Stage B Task 5).**
/// The area-mean-zero latitude profile re-placed every settlement a third
/// time; a **fresh 0–2999 sweep** (798.87 s, ten threads, run outside this
/// test) found `[1741, 1866, 1892]`, one drop each. 2078 is kept as a zero
/// row, the same convention as every prior cleared positive.
///
/// **The set turns over again, and the rate doubles (The Glasshouse, `k`
/// re-decided 0.4 → 0.3).** A fourth fresh 0–2999 sweep (692.89 s, ten
/// threads, run outside this test) found `[238, 1439, 1892, 2031, 2465,
/// 2871]`, one drop each. 1741 and 1866 clear after one placement; five new
/// positives join 1892. Both cleared seeds are kept as zero rows, the same
/// convention as every prior cleared positive, which is why this table grows
/// rather than turning over — it is now eighteen rows, of which six are the
/// live positive set and twelve are the record of what four epochs cleared.
///
/// The old seeds are KEPT as zero rows rather than deleted. They are the
/// cheapest possible statement of what an epoch did — a collision set does not
/// survive a re-placement — and they redden if a future change resurrects one,
/// which is a thing worth knowing.
///
/// **The fifth turnover, and the rate halves (The Underworld, Task 8).** A
/// fifth fresh 0–2999 sweep (744.21 s, ten threads, `--release`, run outside
/// this test) found `[2208, 2465]`, one drop each. 238, 1439, 1892, 2031 and
/// 2871 all clear — including 1892, whose four-epoch survival this file has
/// been narrating since The Glasshouse — and 2208 is the only newcomer. Every
/// cleared seed is kept as a zero row by the standing convention, so the table
/// is nineteen rows now, of which two are the live positive set and seventeen
/// are the record of what five epochs cleared.
///
/// **The sweep predates Task 9's genus repair; the ROWS were re-verified and
/// the SWEEP was not.** Every value below is green on this tree, re-run
/// 2026-08-18, so each named seed's drop count is exact post-repair. The
/// sentence "2208 and 2465 are the whole of the current positive set over seeds
/// 0–2999" rests on the pre-repair sweep and has **not** been re-established:
/// the repair re-placed every world, and a fresh sweep is a 744 s job nobody
/// has paid for since. A seed outside these nineteen may now drop a founder
/// without anything here noticing. That is a gap in coverage, not a suspected
/// defect, and it is written down rather than left implicit — a completeness
/// claim inherited across an epoch it was not re-run under is exactly the
/// shape this campaign spent itself finding.
///
/// claim: structural(seed: [20, 42, 238, 514, 1412, 1439, 1505, 1738, 1741,
/// 1866, 1892, 2031, 2078, 2208, 2465, 2634, 2793, 2871, 2898]) — nineteen
/// named worlds with pinned per-seed values. Not a sweep and not a search: the enumeration is the whole of a
/// completed 0–2999 sweep's positive set plus two controls and every
/// superseded row, so nothing here scans for an instance.
#[test]
fn the_dropped_founders_are_pinned_per_seed() {
    // (seed, founders dropped). 42 and 2793 are the long-standing controls;
    // the rest are prior positives, kept to record that they cleared. Every
    // value here is measured on this tree and green post-repair.
    //
    // 2208 and 2465 were the whole of the positive set over seeds 0-2999 AS
    // OF THE PRE-REPAIR SWEEP, and that completeness claim has NOT been
    // re-established since Task 9's genus repair re-placed every world — see
    // this test's own docs. The rows below are exact; "no other seed in
    // 0-2999 drops a founder" is not currently checked by anything.
    let expected: [(u64, usize); 19] = [
        (20, 0),
        (42, 0),
        (238, 0),
        (514, 0),
        (1412, 0),
        (1439, 0),
        (1505, 0),
        (1738, 0),
        (1741, 0),
        (1866, 0),
        (1892, 0),
        (2031, 0),
        (2078, 0),
        (2208, 1),
        (2465, 1),
        (2634, 0),
        (2793, 0),
        (2871, 0),
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
///
/// **Seed 2465, not 1892 (The Underworld, Task 8).** The node-index re-key
/// cleared 1892 — the end of its four-epoch run — and the fifth 0–2999 sweep
/// left `[2208, 2465]`. **Both** qualify on the `available > 1` premise, and
/// they were measured rather than assumed before either was picked: 2208's
/// losing people (human) holds 13 occupations against a `MEMORY_DEPTH` of 20,
/// and 2465's holds 18. 2465 is taken because it is the SURVIVOR of the two —
/// the one seed carried over from the previous positive set — so this pin
/// changes as little as the measurement permits. 2208 is held in reserve.
#[test]
fn a_dropped_founder_is_not_backfilled() {
    let w = build(2465, BuildDepth::Settlements);
    let occs = occupation_records(&w);
    let cast = select_founders(&occs);
    let dropped = cast
        .unremembered
        .first()
        .expect("seed 2465 drops exactly one founder");
    let people = dropped.people;
    let promoted = cast
        .remembered
        .iter()
        .filter(|f| f.people == people)
        .count();
    let available = occs.iter().filter(|o| o.core.people == people).count();
    assert!(
        available > 1,
        "seed 2465's {people:?} must hold more than one occupation \
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
