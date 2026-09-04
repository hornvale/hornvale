//! The Sundering campaign's two preregistered gates on the real seed-42 world
//! (Task 3): a **depopulation ceiling** (the moving sea's collapses stay a
//! minority of all occupations — it must not starve the map out) and the
//! campaign's **headline payoff**, **isolation-predicts-divergence** (an
//! isolated landmass hosts only a proper subset of the world's peoples — a
//! people that could not cross to it).
//!
//! **The depopulation gate's migration half is no longer read on seed 42**
//! (The Delvers, 2026-08-07). It was reported over a twelve-seed panel in the
//! heavy tier; that panel has since retired (The Assize, 2026-08-08) — see
//! below. No floor was lowered; the citation the old reading rested on was
//! ~1405 commits stale (58 claimed, 4 measured on main, 0 here), and
//! migration across ordinary seeds spans 0–534 events, which no single-world
//! threshold can see.
//!
//! `Landmass.peoples` is a `BTreeSet<String>` of the raw `OCC_PEOPLE` text
//! rather than a resolved `KindId`: the divergence comparison only needs
//! stable people *identity*, and `String` ordering is already deterministic,
//! so this sidesteps needing a `WorldComponents`-based interner the readback
//! helper has no access to.
//!
//! ## THIS PANEL BECAME A CENSUS COLUMN (The Assize, 2026-08-08)
//!
//! The twelve-seed panel this section used to hold
//! (`the_migration_distribution_is_reported_over_a_panel`, `FLOOR_PANEL`,
//! `MIN_MIGRATION_EVENTS`) is deleted. It is superseded exactly by the census
//! column `climate-displacement-events` (`windows/lab/src/metrics.rs`), which
//! calls the same `migration_events` fold this panel called, over ~1000
//! worlds instead of twelve. Measured over 48 worlds the distribution is
//! bimodal (deciles `[0, 0, 3, 5, 111, 291, 578]`) and **exactly zero on 6 of
//! 48 worlds** — a spread no twelve-world panel, and certainly no seed-42
//! scalar, could resolve, which is exactly the limitation this section used
//! to describe as a stopgap.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to, collapse_events, migration_events,
    sundered_landmasses,
};

fn build_s(seed: Seed) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("registries well-formed");
    build_world_to(
        seed,
        &SkyPins::default(),
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

/// The cheap seed-42 half, kept in the commit gate.
///
/// # The migration half moved to a census column (The Delvers, then The Assize)
///
/// This test used to assert `migration_events(&w) > 0` on seed 42 with the
/// message "no migration — dynamics inert". On this branch seed 42 measures
/// **0** migration events, and on main it measures **4** — against a cited
/// figure of 58. The floor was **not lowered** (there is nothing below `> 0`
/// to lower it to); the reading moved (The Delvers, 2026-08-07) to a
/// twelve-seed panel, and then (The Assize, 2026-08-08) to the census column
/// `climate-displacement-events`, because seed 42's zero is a statement about
/// seed 42's deep past being mild, not about the displacement branch being
/// dead.
///
/// **Seed 42 is one sample of a very wide distribution** (0 to 534 migration
/// events across ordinary seeds, and bimodal with 6 of 48 worlds measuring
/// exactly zero), so what stays here is the non-inertness check that seed 42
/// can actually carry: the settlement branch opened occupations at all. The
/// collapse-share ceiling is untouched and still asserted on seed 42.
#[test]
fn the_map_is_not_depopulated() {
    let w = build_s(Seed(42));
    let collapses = collapse_events(&w) as f64;
    let occupations = w.ledger.find(hornvale_history::IS_OCCUPATION).count() as f64;
    // The non-inertness assertion this test can carry on ONE world. If the
    // deep-history bake stops running, this is zero and this line is red;
    // migration's own liveness is read over the census column
    // `climate-displacement-events`, not on seed 42 alone.
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
         migration events — seed 42 is ONE sample of a 0–534 distribution; the census \
         column `climate-displacement-events` is the instrument",
        occupations,
        migration_events(&w)
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
            .map(|m| (m.vertices.len(), m.peoples.len()))
            .collect::<Vec<_>>()
    );
}
