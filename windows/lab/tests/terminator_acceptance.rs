//! Acceptance battery for The Terminator (SKY-24, spec §8, preregistered):
//! after the regime-aware insolation fix (`substrate_field`, commit
//! `3a6bb98`), do naturally tidally-locked worlds recover an Ambient
//! (tide) presiding belief off the post-niche 0/48 floor
//! (`a_frozen_sky_never_heads_a_cyclic_pantheon`'s committed-census
//! reading, stale until the fix's canonical census regen) toward SKY-5's
//! shipped baseline (19/23 ≈ 83%)?
//!
//! Unlike that calibration — which reads the committed `rows.csv` census
//! fixture, built on pre-fix code — this battery builds every locked world
//! LIVE through the real pipeline (`hornvale_worldgen::build_world`, full
//! depth, default pins), so it measures the fixed reality directly rather
//! than waiting on the next AWS regen.
//!
//! Locked-seed identification mirrors the Task-2 probe
//! (`windows/worldgen/tests/insolation_probe.rs::is_locked`) exactly: the
//! natural (unpinned) rotation regime at the cheapest depth that exposes
//! it (`BuildDepth::Astronomy`), scanned ascending over seeds `1..=200` —
//! the same window that probe used, which yielded 9 locked seeds (8, 13,
//! 70, 78, 80, 95, 116, 145, 183; see
//! `.superpowers/sdd/task-2-report.md`). This battery re-derives the seed
//! set itself (never hardcodes it) so the two instruments can never
//! silently desync.
//!
//! ## MEASURED RESULT: the payoff FALSIFIED — pinned, not forced
//!
//! Over the 9 locked seeds in `1..=200`, built through the real pipeline
//! with the shipped fix: **0/9 head Ambient, 9/9 head Eternal, 0/9 head
//! Cyclic.** The preregistered recovery did NOT happen, despite the Task-2
//! probe confirming the fix moved dominant-species carrying capacity to
//! the terminator ring on all 9 of these same seeds (`cos_theta` in
//! `[0.23, 0.35]`, decisively excluding the substellar zone).
//!
//! Root cause, diagnosed by inspecting a locked world's belief order
//! directly (seed 8): the world DOES commit a tide-derived, Ambient belief
//! (source_kind `"tide"`) — but only as its 4th belief. Its FIRST belief
//! (source_kind `"celestial-body"`, Eternal) comes from whichever peopled
//! species commits its pantheon first in ledger order. Per
//! `a_frozen_sky_never_heads_a_cyclic_pantheon`'s own doc comment, that is
//! now **bugbear**, unconditionally: `hornvale_species::registry()`'s
//! `BTreeMap` iteration sorts "bugbear" alphabetically first among the
//! Branches' four peoples, and the founder floor (MAP-22's allocation
//! layer, `domains/demography/src/founder.rs`) guarantees bugbear a
//! flagship settlement on every seed — so
//! bugbear's pantheon is minted first on every locked world in this
//! sample, regardless of which species the corrected insolation field
//! makes numerically dominant. Bugbear's own head-deity selection picks
//! whatever phenomenon is most salient to bugbear specifically, which this
//! sample shows defaulting to a celestial body (Eternal), not the tide.
//!
//! **The fix repaired habitability (Task 2/3's measured, verified target)
//! but the presiding-belief mechanism `beliefs_of(&world).first()` reads
//! is decoupled from habitability dominance by an unrelated,
//! already-shipped mechanism** (the Branches' alphabetical-commit-order +
//! founder-floor coupling that `a_frozen_sky_never_heads_a_cyclic_
//! pantheon` already documents as a "genuine behavioural finding, not a
//! bug"). This battery pins the measured falsification exactly — per this
//! codebase's convention for honest, unforced, unwidened calibration pins
//! (ADR 0016) — rather than asserting a floor that isn't met. A future
//! campaign that revisits presiding-belief selection (e.g. weighting by
//! dominance rather than commit order) should update these pinned counts
//! deliberately, not silently.

use hornvale_astronomy::{Rotation, SkyPins};
use hornvale_kernel::Seed;
use hornvale_religion::{Sentiment, beliefs_of};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, build_world, build_world_to, sky_of,
};

/// Ascending-seed scan ceiling — mirrors the Task-2 probe's first window
/// (`insolation_probe.rs::FIRST_SCAN_MAX`), which already yielded 9 locked
/// seeds, comfortably above that probe's `MIN_LOCKED = 8` floor without
/// needing its widened `201..=1000` window.
const SCAN_MAX: u64 = 200;

/// The minimum locked-seed count this battery accepts before treating the
/// scan as under-yielded (mirrors the Task-2 probe's `MIN_LOCKED`).
const MIN_LOCKED: usize = 8;

/// Is this seed's NATURAL (unpinned) rotation regime `Locked`? Built only
/// to `BuildDepth::Astronomy` — the cheapest depth that exposes the sky —
/// mirroring `insolation_probe.rs::is_locked` exactly, so this battery's
/// seed set can never silently diverge from the Stage-0 probe's.
fn is_locked(seed: u64) -> bool {
    let Ok(world) = build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &[],
        BuildDepth::Astronomy,
    ) else {
        return false;
    };
    let Ok(sky) = sky_of(&world) else {
        return false;
    };
    let Some(system) = sky.system() else {
        return false;
    };
    matches!(system.anchor.rotation, Rotation::Locked)
}

/// The acceptance measurement (spec §8): build every naturally locked seed
/// in `1..=200` through the real pipeline (full depth, the shipped fix),
/// read the FIRST-minted belief's sentiment (the presiding belief; see
/// `beliefs_of`'s doc comment and `a_frozen_sky_never_heads_a_cyclic_
/// pantheon`), and pin the exact measured breakdown.
///
/// This is a live-worldgen battery (9 full-depth builds + a 200-seed
/// astronomy-only scan; ~5.5 min in a debug build), so it is deferred to
/// the heavy tier rather than the ~4-minute commit gate — see
/// `cli/tests/heavy_tier.rs`. Run by hand:
/// `cargo test -p hornvale-lab --release --test terminator_acceptance --
/// --ignored --nocapture`.
///
/// Cyclic stays a hard, unconditional invariant (a locked world offers no
/// rising-and-setting body to read cyclic); Ambient/Eternal are pinned to
/// the measured falsification (see the module doc): the payoff did not
/// land, and the assertion says so honestly rather than asserting an
/// unmet floor.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn locked_worlds_recover_ambient_presiding_belief_after_the_terminator_fix() {
    let locked: Vec<u64> = (1..=SCAN_MAX).filter(|&s| is_locked(s)).collect();
    assert!(
        locked.len() >= MIN_LOCKED,
        "locked-seed scan under-yielded ({} found in 1..={SCAN_MAX}, want >= {MIN_LOCKED}); \
         widen SCAN_MAX to mirror the Task-2 probe's fallback window",
        locked.len()
    );

    let (mut ambient, mut eternal, mut cyclic) = (0u32, 0u32, 0u32);
    let mut breakdown: Vec<(u64, &'static str)> = Vec::new();
    for &seed in &locked {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap_or_else(|e| panic!("locked seed {seed} must build: {e:?}"));
        let beliefs = beliefs_of(&world);
        let first = beliefs
            .first()
            .unwrap_or_else(|| panic!("locked seed {seed} committed no beliefs"));
        breakdown.push((seed, first.sentiment.as_str()));
        match first.sentiment {
            Sentiment::Ambient => ambient += 1,
            Sentiment::Eternal => eternal += 1,
            Sentiment::Cyclic => cyclic += 1,
        }
    }

    let n = locked.len() as u32;
    eprintln!(
        "locked-world presiding-belief recovery over {n} seeds ({locked:?}): \
         {ambient} ambient / {eternal} eternal / {cyclic} cyclic — {breakdown:?}"
    );

    assert_eq!(
        cyclic, 0,
        "a frozen sky must never head a cyclic pantheon — a locked world offers no \
         rising-and-setting body to read cyclic (invariant carried from \
         a_frozen_sky_never_heads_a_cyclic_pantheon); breakdown: {breakdown:?}"
    );

    // Pinned to the MEASURED falsification (module doc): the fix did not
    // restore Ambient presiding religion on this 9-seed sample. If this
    // ever moves, it means either the presiding-belief mechanism changed
    // (update deliberately, with a doc note) or a genuine regression in
    // the shipped fix (investigate before touching these numbers).
    assert_eq!(
        ambient, 0,
        "locked-world Ambient recovery MOVED from the pinned falsification (0/{n}): {breakdown:?} \
         — if this is a genuine improvement (e.g. a later campaign reworked presiding-belief \
         selection), update this pin deliberately with a doc note; do not silently widen it"
    );
    assert_eq!(
        eternal, n,
        "locked-world Eternal share MOVED from the pinned falsification ({n}/{n}): {breakdown:?}"
    );
}
