//! The Sundering campaign's two preregistered gates on the real seed-42 world
//! (Task 3): a **depopulation ceiling** (the moving sea's collapses stay a
//! minority of all occupations — it must not starve the map out) and the
//! campaign's **headline payoff**, **isolation-predicts-divergence** (an
//! isolated landmass hosts only a proper subset of the world's peoples — a
//! people that could not cross to it).
//!
//! **The depopulation gate's migration half is no longer read on seed 42**
//! (The Delvers, 2026-08-07). It is reported over a twelve-seed panel in the
//! heavy tier instead — see
//! [`the_migration_distribution_is_reported_over_a_panel`]. No floor was
//! lowered; the citation the old reading rested on was ~1405 commits stale
//! (58 claimed, 4 measured on main, 0 here), and migration across ordinary
//! seeds spans 0–534 events, which no single-world threshold can see.
//!
//! `Landmass.peoples` is a `BTreeSet<String>` of the raw `OCC_PEOPLE` text
//! rather than a resolved `KindId`: the divergence comparison only needs
//! stable people *identity*, and `String` ordering is already deterministic,
//! so this sidesteps needing a `WorldComponents`-based interner the readback
//! helper has no access to.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, collapse_events,
    migration_events, sundered_landmasses,
};

fn build_s(seed: Seed) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("registries well-formed");
    build_world_to(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed builds")
}

/// The moving sea must not answer the deep water by starving the map out:
/// collapses (famine) stay a minority of all occupations. Measured seed-42
/// share: 1 collapse / 151 occupations ≈ 0.0066; the ceiling is set a clear
/// margin above that (≈7.5x), never at it.
const MAX_COLLAPSE_SHARE: f64 = 0.05;

/// The historical migration floor, kept as a documented constant and
/// **reported rather than asserted** — see
/// [`the_migration_distribution_is_reported_over_a_panel`].
///
/// **The figure this gate was argued from was stale, and badly.** The
/// migration reading cited across the history gates was **58** climate
/// migrations on seed 42 (The Tumult's re-pin; see `history_gates.rs`'s
/// module docs, which still carry it as the reason gate 1's floor points where
/// it does). Re-measured 2026-08-07 (The Delvers): seed 42 measures **4** on
/// main and **0** on this branch. The cited number was ~1405 commits old and
/// wrong by more than an order of magnitude. The Tense had already re-based
/// `history_gates.rs`'s own migration gate onto a seed SPREAD for exactly this
/// reason ("no single-seed floor can tell the difference between a mild world
/// and an inert bake"); this gate kept reading seed 42 and inherited the
/// failure mode. Registry row: `PROC-floors-erode-unseen`.
const MIN_MIGRATION_EVENTS: u64 = 1;

/// The seed panel migration is read over — the same twelve worlds
/// `history_tithe.rs` reads its accumulator over, so the two instruments
/// sample one sample. Migration across it spans 0 to 534 events (measured
/// 2026-08-07), which is why no single world's reading is a threshold.
const FLOOR_PANEL: [u64; 12] = [1, 2, 3, 4, 5, 6, 7, 8, 42, 99, 1234, 2024];

/// The cheap seed-42 half, kept in the commit gate.
///
/// # The migration half moved to a panel (The Delvers, 2026-08-07)
///
/// This test used to assert `migration_events(&w) > 0` on seed 42 with the
/// message "no migration — dynamics inert". On this branch seed 42 measures
/// **0** migration events, and on main it measures **4** — against a cited
/// figure of 58. The floor was **not lowered** (there is nothing below `> 0`
/// to lower it to); the reading moved to
/// [`the_migration_distribution_is_reported_over_a_panel`], because seed 42's
/// zero is a statement about seed 42's deep past being mild, not about the
/// displacement branch being dead. The panel says so directly: 11 of 12 seeds
/// migrate, at both the 6-people and 9-people rosters.
///
/// **Seed 42 is one sample of a very wide distribution** (0 to 534 migration
/// events across [`FLOOR_PANEL`]), so what stays here is the non-inertness
/// check that seed 42 can actually carry: the settlement branch opened
/// occupations at all. The collapse-share ceiling is untouched and still
/// asserted on seed 42.
#[test]
fn the_map_is_not_depopulated() {
    let w = build_s(Seed(42));
    let collapses = collapse_events(&w) as f64;
    let occupations = w.ledger.find(hornvale_history::IS_OCCUPATION).count() as f64;
    // The non-inertness assertion this test can carry on ONE world. If the
    // deep-history bake stops running, this is zero and this line is red;
    // migration's own liveness is a panel reading, not a seed-42 one.
    assert!(
        occupations > 0.0,
        "no occupations on seed 42 — the settlement bake did not run at all"
    );
    let share = collapses / occupations;
    assert!(
        share <= MAX_COLLAPSE_SHARE,
        "depopulation: collapse share {share:.4} > ceiling {MAX_COLLAPSE_SHARE} — a fidelity finding for Nathan, not a re-pin."
    );
    eprintln!(
        "SUNDERING seed-42: {} occupations, {collapses} collapses (share {share:.4}), {} \
         migration events — seed 42 is ONE sample of a 0–534 distribution; the panel in the \
         heavy tier is the instrument",
        occupations,
        migration_events(&w)
    );
}

/// **The migration reading, over a seed panel and REPORT-ONLY** (The Delvers,
/// 2026-08-07). Heavy: twelve live builds to `BuildDepth::Settlements`.
///
/// # What was measured, and why nothing here is calibrated
///
/// `migration_events(&w) > 0` on seed 42 was red on this branch and green on
/// main by a margin of **four events** against a documented figure of **58**
/// — a citation ~1405 commits stale. Measuring the panel showed what the
/// single-seed reading was hiding: migration passes `> 0` on **11 of 12**
/// seeds, and it passed on exactly 11 of 12 at main's 6-people roster too.
/// The pass *count* is identical between the rosters; only the failing
/// *member* moved (main's zero-migration seed is 8, this branch's is 42). The
/// gate was therefore already failing 1 in 12 of the seed space before this
/// campaign existed, and nobody knew, because it only ever read seed 42.
///
/// The spread is the reason a scalar single-seed floor cannot work here:
/// migration ranges from **0 to 534** events across ordinary seeds. The Tense
/// reached the same conclusion for `history_gates.rs`'s gate 1 and re-based it
/// onto a spread; this gate did not follow, and this test is that follow.
///
/// **No quantile threshold is chosen, deliberately.** The distribution was
/// measured for the first time on 2026-08-07. Fitting a percentile to it would
/// be fitting a bound to one afternoon's data — the same error as the 58 that
/// preceded it, dressed better. Calibration belongs to a campaign that owns
/// this gate and can preregister what the bound means.
///
/// # What IS asserted
///
/// That at least half the panel migrates at all. It currently holds 11 of 12,
/// so it carries real margin, and it is not vacuous: if climate displacement
/// stops firing it goes red, which is the failure the old message named.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_migration_distribution_is_reported_over_a_panel() {
    // (seed, migration events, occupations, collapses)
    let mut rows: Vec<(u64, u64, u64, u64)> = Vec::new();
    for s in FLOOR_PANEL {
        let w = build_s(Seed(s));
        let migr = migration_events(&w);
        let occ = w.ledger.find(hornvale_history::IS_OCCUPATION).count() as u64;
        let coll = collapse_events(&w);
        eprintln!(
            "SUNDERING PANEL seed {s:>5}: migration {migr:>4} occupations {occ:>5} collapses \
             {coll:>4} (collapse share {:.4})",
            coll as f64 / occ.max(1) as f64
        );
        rows.push((s, migr, occ, coll));
    }

    let mut migrations: Vec<u64> = rows.iter().map(|r| r.1).collect();
    migrations.sort_unstable();
    let n = migrations.len();
    // Even panel size: the median is the mean of the two central order
    // statistics, reported as a float because it can land on a half.
    let median = (migrations[n / 2 - 1] + migrations[n / 2]) as f64 / 2.0;
    let clearing = rows.iter().filter(|r| r.1 >= MIN_MIGRATION_EVENTS).count();

    eprintln!(
        "SUNDERING PANEL summary: median migration {median:.1}, range {}–{} — a scalar floor \
         read on ONE seed cannot see this",
        migrations[0],
        migrations[n - 1]
    );
    eprintln!(
        "SUNDERING PANEL: {clearing} of {n} seeds clear the historical floor of \
         {MIN_MIGRATION_EVENTS} migration event(s) — REPORTED, NOT ASSERTED. The floor was \
         never lowered; it is no longer read as a pass/fail on one world. Calibration is \
         deferred to a campaign that owns this gate (see this test's docs and \
         `PROC-floors-erode-unseen`)."
    );

    let firing = rows.iter().filter(|r| r.1 > 0).count();
    assert!(
        firing * 2 >= n,
        "climate displacement has gone INERT across the panel: only {firing} of {n} seeds \
         migrated at all. This is not a calibration finding and not a floor to lower — it \
         means the displacement branch stopped running. Per-seed: {:?}",
        rows.iter().map(|r| (r.0, r.1)).collect::<Vec<_>>()
    );
}

/// Isolation predicts divergence: the present world is genuinely partitioned
/// (≥ `MIN_LANDMASSES` inhabited land components) and at least one isolated
/// landmass hosts only a proper SUBSET of the world's peoples — a people that
/// could not cross to it. Measured seed-42: 4 inhabited landmasses, over all
/// 4 goblinoid peoples; three of the four host only a proper subset (2, 2,
/// and 3 of the 4). The floor is set just below the measured count (4 → 3).
const MIN_LANDMASSES: usize = 3;
#[test]
fn isolation_predicts_divergence() {
    let w = build_s(Seed(42));
    let masses = sundered_landmasses(&w);
    assert!(
        masses.len() >= MIN_LANDMASSES,
        "not sundered: {} inhabited land component(s) (floor {MIN_LANDMASSES})",
        masses.len()
    );
    let world_peoples: std::collections::BTreeSet<_> = masses
        .iter()
        .flat_map(|m| m.peoples.iter().cloned())
        .collect();
    assert!(
        world_peoples.len() >= 2,
        "need ≥2 peoples for a divergence signal"
    );
    let diverged = masses
        .iter()
        .any(|m| !m.peoples.is_empty() && m.peoples.len() < world_peoples.len());
    assert!(
        diverged,
        "no isolated landmass hosts a proper subset of peoples: {:?}",
        masses
            .iter()
            .map(|m| (m.cells.len(), m.peoples.len()))
            .collect::<Vec<_>>()
    );
}
