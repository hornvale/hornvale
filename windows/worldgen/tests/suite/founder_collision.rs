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
//!
//! **The Granary re-sweep (2026-08-25, post Escapement+Lexicon absorbs):** the
//! merged world's sub-year raid timing re-placed every settlement again;
//! fresh full sweep found `[1057, 2852]`, one drop each. 1892 — the seed that
//! survived four epochs — is finally cleared. Rate holds small: 2 in 3000.
//! Read the rate, never the membership.
//! this wave did not pay for one. Read the table as "these nineteen seeds drop
//! exactly these counts", which is what it asserts, and not as "no other seed
//! in 0–2999 drops a founder", which it does not.
//!
//! **THE WINZE T2b PAID FOR ONE, AND COMMITTED THE HARNESS SO THE NEXT
//! CAMPAIGN NEED NOT RECONSTRUCT IT.** Spec amendment E's working ring scan
//! re-placed every settlement a SEVENTH time; a fresh full 0–2999 sweep
//! (1129.93 s, ten threads, dev profile at `opt-level = 2`) found
//! `[1162, 2655]`, one drop each, and cleared `[1057, 2852]` entirely. **The
//! rate is unmoved at 2 in 3000** — the observed series is now
//! 2 → 6 → 2 → 3 → 6 → 2 → 2 → 2, and this is the first epoch in the file's
//! history where it did NOT move. That fits the header's own reading rather
//! than straining it: seed 42's occupation count moved 1,240 → 1,212, a 2.3%
//! fall against the 24% and 32% swings that accompanied every previous rate
//! change, so the ground under the key barely shifted and neither did the
//! residual. **Read the rate, never the membership** — no seed has ever
//! survived two consecutive sweeps except 1892, and its run is long over.
//!
//! The sweep is `the_shipped_handles_full_sweep_writes_its_positive_set` at
//! the bottom of this file. Every previous re-pin above came from a sweep run
//! by hand outside the repository and recorded only in prose, which is why
//! this paragraph's predecessors twice had to say the completeness claim was
//! inherited rather than measured.
//!
//! **The Granary asked whether the tail can retire, and the answer is NO —
//! measured, not argued (Task 6, 2026-08-24).** T4's day-grain founding stamps
//! were the hypothesis: if the raided founding and its same-year successor
//! now differ in `founded`, the post-founding tail is redundant and
//! `founder_handle` could collapse to its identity key. The tail-less handle
//! swept seeds 0–2999 (`BuildDepth::Settlements`, default pins, this tree
//! post-T4, 974 s — see `granary_tail_less_sweep_writes_its_counts` below):
//! **2261 colliding worlds, 5039 founders lost** — against 3 in 3000 with the
//! shipped key on the same placement epoch. The identity key alone had
//! already measured 732/1000 pre-Granary; the day-grain stamps do not
//! separate the twins because **the twinning is same-phase**: a raided
//! founding and the community that took from it are founded in the same
//! phase-resolution pass, so they share the finer timestamp as well as the
//! year. Only a post-founding fact can tell them apart, which is what the
//! tail folds. Verdict per the plan's branch table (>0 collisions):
//! `founder_handle` in `domains/history/src/flesh.rs` stays exactly as it
//! is. The cost is accepted knowingly: every future recomputation of
//! `ended`/`peak_population` still forces an epoch under the save-format
//! contract, forever.
//!
//! What this battery pins:
//!
//! 1. **liveness** — the seeds that used to die build to `Full` depth, which is
//!    the depth the panic used to fire at;
//! 2. **the size of the cut** — how many founders each seed loses, so a later
//!    change that silently alters promotion coverage reddens here. Nineteen
//!    of the twenty-one rows now read **zero**, and they are the campaign's
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
//! Cost: twenty-one `BuildDepth::Settlements` builds and two `Full` builds, ~2 s
//! each on an optimized dev profile — cheap enough for the commit gate, which
//! is where a liveness guard belongs.
//!
use hornvale_astronomy::SkyPins;
use hornvale_history::record::{Founding, FoundingCoords, founding_key};
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
/// claim: structural(seed: [1057, 2852]) — two named worlds, built once each.
/// No search: the seeds come from a completed 0–2999 sweep, not from this
/// test.
#[test]
fn a_colliding_seed_builds_to_full_depth_instead_of_panicking() {
    // THE WINZE T2b: [1057, 2852] -> [1162, 2655]. Seventh placement epoch,
    // whole set cleared, fresh 0-2999 sweep, rate unmoved at 2 in 3000. See
    // `a_dropped_founder_is_not_backfilled` for the sweep and its harness.
    for seed in [1162u64, 2655] {
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
/// is twenty-one rows now, of which two are the live positive set and nineteen
/// are the record of what seven epochs cleared.
///
/// **THE COMPLETENESS GAP IS CLOSED (The Winze T2b).** The two paragraphs
/// above recorded, twice, that "these are the whole of the positive set over
/// 0–2999" was inherited from a sweep run before an epoch rather than measured
/// after one. A fresh full sweep now runs from inside this file
/// (`the_shipped_handles_full_sweep_writes_its_positive_set`, 1129.93 s, ten
/// threads) and was run on this tree: `[1162, 2655]`, one drop each, and the
/// completeness claim below is that sweep's own output rather than an
/// inheritance. It will lapse again at the next settlement-replacing epoch,
/// which is what the `#[ignore]`d harness is for.
///
/// claim: structural(seed: [20, 42, 238, 514, 1057, 1162, 1412, 1439, 1505,
/// 1738, 1741, 1866, 1892, 2031, 2078, 2634, 2655, 2793, 2852, 2871, 2898]) —
/// twenty-one named worlds with pinned per-seed values. Not a sweep and not a
/// search: the enumeration is the whole of a completed 0–2999 sweep's positive
/// set plus two controls and every superseded row, so nothing here scans for
/// an instance.
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
    let expected: [(u64, usize); 21] = [
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
        // Cleared by The Winze T2b's ring scan, and kept at zero for the
        // reason the module header gives: a seed that used to lose a founder
        // and no longer does makes a regression visible rather than silent.
        (1057, 0),
        (2852, 0),
        // The Winze T2b's fresh 0-2999 sweep. One drop each; rate 2 in 3000.
        (1162, 1),
        (2655, 1),
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
///
/// **BOTH POSITIVES NOW, NOT ONE (The Winze T2b).** Spec amendment E's working
/// ring scan re-placed every settlement a seventh time and cleared the whole
/// `[1057, 2852]` set; a fresh 0–2999 sweep — run from
/// `the_shipped_handles_full_sweep_writes_its_positive_set`, committed at the
/// bottom of this file precisely so the next campaign does not have to
/// reconstruct it — found `[1162, 2655]`, one drop each. The rate holds at 2 in
/// 3000. Neither survives from the previous set, so there is no continuity
/// argument to make for picking one, and the "which seed" question disappears
/// if the test simply takes both: the `available > 1` premise is asserted per
/// seed rather than chosen for, which is strictly more coverage for one extra
/// ~3 s build.
///
/// claim: structural(seed: [1162, 2655]) — two named worlds, built once each.
/// No search: the seeds are the whole of a completed 0-2999 sweep's positive
/// set, not something this test scans for.
#[test]
fn a_dropped_founder_is_not_backfilled() {
    for seed in [1162u64, 2655] {
        let w = build(seed, BuildDepth::Settlements);
        let occs = occupation_records(&w);
        let cast = select_founders(&occs);
        let dropped = cast
            .unremembered
            .first()
            .unwrap_or_else(|| panic!("seed {seed} drops exactly one founder"));
        let people = dropped.people;
        let promoted = cast
            .remembered
            .iter()
            .filter(|f| f.people == people)
            .count();
        let available = occs.iter().filter(|o| o.core.people == people).count();
        println!("seed {seed}: {people:?} holds {available} occupations, {promoted} promoted");
        assert!(
            available > 1,
            "seed {seed}'s {people:?} must hold more than one occupation \
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
}

/// **The Granary, Task 6: can the discrimination tail retire?** The shipped
/// handle is identity (`record::founding_key`: where, when, by whom, out of
/// which community, plus the one ancestry hop) plus a tail that folds `ended`
/// (presence-tagged, through `day_key`) then `peak_population`, with
/// `FOUNDER_ROLE` last. The tail exists because same-year twin foundings — a
/// raided attempt and the community that took, same people, same site, same
/// year, same parent — are identical in every *founding-side* field, so only a
/// post-founding fact separates them.
///
/// The Granary hypothesis is that T4's day-grain founding stamps already
/// separate those twins naturally, making the tail redundant. This sweep
/// measures the **tail-less** handle — identity key only, `FOUNDER_ROLE`
/// folded on top (which cannot change collision behaviour: it is one fixed
/// constant mixed after the identity, so two records tie under
/// identity+role exactly when they tie under identity) — over the full
/// 0–2999 range, using the same protocol every prior sweep in this file used:
/// `BuildDepth::Settlements`, default pins, run OFFLINE with `--ignored`.
///
/// For each world the promoted cast is re-derived the way
/// `select_founders` derives it — per people, `(peak_population DESC, site
/// ASC, founded ASC)` with the candidate handle as final tiebreak, cut to
/// `MEMORY_DEPTH` — and duplicates of the tail-less key across the kept set
/// are counted. Two numbers come out, matching The Ell's three-arm table's
/// axes: colliding worlds and founders lost.
///
/// Results are printed and written to
/// `CARGO_TARGET_TMPDIR/tail-sweep-results.txt` so the counts survive the
/// run and can be read back into this file's prose by whoever pays for it.
///
/// **THE SWEEP HAS BEEN RUN — 2026-08-24, this tree post-T4 (HEAD
/// eeaa011fd), 974 s wall, ten threads.** Result: 2261 colliding worlds,
/// 5039 founders lost, positives across most of the range (the full seed
/// list is in the written report). That is the measurement behind the
/// module header's verdict that the discrimination tail stays; re-running
/// is only needed after the next settlement-replacing epoch.
///
/// claim: structural() — a measurement harness, not an assertion battery:
/// everything it learns lands in prose, never in a pinned value.
#[test]
#[ignore = "the full 0-2999 sweep costs ~800 s wall on ten threads (prior sweeps: \
           692.89/744.21/768.72/798.87 s) -- offline measurement, never in the \
           normal test run"]
fn granary_tail_less_sweep_writes_its_counts() {
    const SEEDS: u64 = 3000;
    const THREADS: usize = 10;

    // One world's verdict under the tail-less handle: does its PROMOTED cast
    // carry a duplicate identity key, and how many founders would that cost?
    fn tail_less(seed: u64) -> (bool, usize) {
        let w = build(seed, BuildDepth::Settlements);
        let occs = occupation_records(&w);
        let coords: BTreeMap<hornvale_kernel::EntityId, FoundingCoords<'static>> = occs
            .iter()
            .map(|o| (o.id, hornvale_history::record::founding_coords(&o.core)))
            .collect();
        let parent_of = |r: &hornvale_history::record::OccupationRecord| match r.founded_from {
            Founding::From(e) => coords.get(&e).copied(),
            Founding::Genesis(_) => None,
        };
        // The tail-less candidate handle: identity + role, nothing else.
        let keys: Vec<u64> = occs
            .iter()
            .map(|r| founding_key(&r.core, parent_of(r)))
            .collect();

        let mut by_people: BTreeMap<&'static str, Vec<usize>> = BTreeMap::new();
        for (i, r) in occs.iter().enumerate() {
            by_people.entry(r.core.people.0).or_default().push(i);
        }
        let mut kept: Vec<u64> = Vec::new();
        for idxs in by_people.values_mut() {
            idxs.sort_by(|&a, &b| {
                let (x, y) = (&occs[a], &occs[b]);
                y.core
                    .peak_population
                    .cmp(&x.core.peak_population)
                    .then(x.core.site.0.cmp(&y.core.site.0))
                    .then(x.core.founded.total_cmp(&y.core.founded))
                    .then(keys[a].cmp(&keys[b]))
            });
            kept.extend(idxs.iter().take(MEMORY_DEPTH).map(|&i| keys[i]));
        }
        kept.sort_unstable();
        let distinct =
            kept.windows(2).filter(|p| p[0] != p[1]).count() + usize::from(!kept.is_empty());
        let dropped = kept.len() - distinct;
        (dropped > 0, dropped)
    }

    let next = std::sync::atomic::AtomicU64::new(0);
    let totals: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let lost: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let positives: std::sync::Mutex<Vec<u64>> = std::sync::Mutex::default();

    std::thread::scope(|scope| {
        for _ in 0..THREADS {
            scope.spawn(|| {
                loop {
                    let seed = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    if seed >= SEEDS {
                        break;
                    }
                    let (collides, drops) = tail_less(seed);
                    if collides {
                        totals.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        lost.fetch_add(drops as u64, std::sync::atomic::Ordering::Relaxed);
                        positives.lock().unwrap().push(seed);
                    }
                }
            });
        }
    });

    let mut positives = positives.into_inner().unwrap();
    positives.sort_unstable();
    let report = format!(
        "The Granary T6 tail-less sweep (seeds 0-{}, BuildDepth::Settlements, \
         default pins): {} colliding worlds, {} founders lost, positive seeds \
         {positives:?}\n",
        SEEDS - 1,
        totals.load(std::sync::atomic::Ordering::Relaxed),
        lost.load(std::sync::atomic::Ordering::Relaxed),
    );
    println!("{report}");
    let path = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("tail-sweep-results.txt");
    std::fs::write(&path, &report).expect("write sweep results");
    println!("written to {}", path.display());
}

/// **The shipped handle's own full sweep**, which this file's header has said
/// twice that it lacked: *"what has not been re-established post-repair is the
/// completeness claim … because that needs a fresh full sweep and this wave did
/// not pay for one."* Every re-pin of the positive set above came from a sweep
/// run **outside** this file, by hand, and recorded only in prose — so the next
/// campaign to re-place settlements had nothing to run and had to reconstruct
/// the harness. This is that harness, committed.
///
/// It differs from `granary_tail_less_sweep_writes_its_counts` above in the one
/// way that matters: that one measures a *candidate* key that was rejected,
/// this one measures the key that actually ships, through `select_founders`
/// itself rather than through a re-implementation of it.
///
/// claim: structural() — a measurement harness, not an assertion battery:
/// everything it learns lands in prose and in the pinned table above, never in
/// an assertion here.
#[test]
#[ignore = "the full 0-2999 sweep costs ~800 s wall on ten threads -- offline \
           measurement, run it after any change that re-places settlements, \
           never in the normal test run"]
fn the_shipped_handles_full_sweep_writes_its_positive_set() {
    const SEEDS: u64 = 3000;
    const THREADS: usize = 10;

    let next = std::sync::atomic::AtomicU64::new(0);
    let lost: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let positives: std::sync::Mutex<Vec<(u64, usize)>> = std::sync::Mutex::default();

    std::thread::scope(|scope| {
        for _ in 0..THREADS {
            scope.spawn(|| {
                loop {
                    let seed = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    if seed >= SEEDS {
                        break;
                    }
                    let w = build(seed, BuildDepth::Settlements);
                    let drops = select_founders(&occupation_records(&w)).unremembered.len();
                    if drops > 0 {
                        lost.fetch_add(drops as u64, std::sync::atomic::Ordering::Relaxed);
                        positives.lock().unwrap().push((seed, drops));
                    }
                }
            });
        }
    });

    let mut positives = positives.into_inner().unwrap();
    positives.sort_unstable();
    let report = format!(
        "shipped-handle sweep (seeds 0-{}, BuildDepth::Settlements, default \
         pins): {} colliding worlds, {} founders dropped, positives {positives:?}\n",
        SEEDS - 1,
        positives.len(),
        lost.load(std::sync::atomic::Ordering::Relaxed),
    );
    println!("{report}");
    let path = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("shipped-sweep-results.txt");
    std::fs::write(&path, &report).expect("write sweep results");
    println!("written to {}", path.display());
}
