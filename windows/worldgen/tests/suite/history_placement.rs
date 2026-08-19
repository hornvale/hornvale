//! Task 5 (the epoch): the deep-history bake is the SOLE settlement provider.
//! The retired demography condensation placer is gone from genesis — every
//! `is-settlement` a world carries is now an alive occupation the bake placed
//! and `emit_history` committed. These two gates guard that structural fact
//! and the no-regression quality band (history must not regress the walkable
//! map).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to};

fn build(seed: Seed, depth: BuildDepth) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        depth,
    )
    .expect("seed builds")
}

/// Sole-provider: every `is-settlement` subject also carries `is-occupation`.
/// A settlement minted by any surviving parallel placer would lack the
/// occupation skeleton, so this fails loudly if the draft placer ever returns.
#[test]
fn history_is_the_sole_settlement_provider() {
    let w = build(Seed(42), BuildDepth::Settlements);
    let settlements: Vec<_> = w
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .collect();
    assert!(
        !settlements.is_empty(),
        "seed 42 must place settlements at Settlements depth"
    );
    for subject in settlements {
        assert!(
            w.ledger
                .facts_about(subject)
                .any(|g| g.predicate == hornvale_history::IS_OCCUPATION),
            "settlement {subject:?} carries no is-occupation — a non-history placer survives"
        );
    }
}

/// The no-regression quality gate: history must land the seed-42 settlement
/// count in the walkable band. NOT a post-filter — the count is whatever the
/// bake's emergent dynamics + founding density produce; if it drifts out of
/// band the bake is tuned in `history_bake.rs`, never clamped here.
#[test]
fn emergent_settlement_count_stays_in_the_sane_band() {
    let n = hornvale_settlement::all_settlements(&build(Seed(42), BuildDepth::Settlements)).len();
    assert!((40..=400).contains(&n), "regressed settlement count: {n}");
}
