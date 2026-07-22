//! The Sundering campaign's two preregistered gates on the real seed-42 world
//! (Task 3): a **depopulation ceiling** (the moving sea's collapses stay a
//! minority of all occupations — it must not starve the map out) and the
//! campaign's **headline payoff**, **isolation-predicts-divergence** (an
//! isolated landmass hosts only a proper subset of the world's peoples — a
//! people that could not cross to it).
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
#[test]
fn the_map_is_not_depopulated() {
    let w = build_s(Seed(42));
    let collapses = collapse_events(&w) as f64;
    let occupations = w.ledger.find(hornvale_history::IS_OCCUPATION).count() as f64;
    assert!(occupations > 0.0, "no occupations");
    let share = collapses / occupations;
    assert!(
        share <= MAX_COLLAPSE_SHARE,
        "depopulation: collapse share {share:.4} > ceiling {MAX_COLLAPSE_SHARE} — a fidelity finding for Nathan, not a re-pin."
    );
    assert!(migration_events(&w) > 0, "no migration — dynamics inert");
}

/// Isolation predicts divergence: the present world is genuinely partitioned
/// (≥ `MIN_LANDMASSES` inhabited land components) and at least one isolated
/// landmass hosts only a proper SUBSET of the world's peoples — a people that
/// could not cross to it. Measured seed-42: 4 inhabited landmasses, over all
/// 4 goblinoid peoples; two masses host only 2 of the 4 (a genuine proper
/// subset). The floor is set just below the measured count (4 → 3).
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
