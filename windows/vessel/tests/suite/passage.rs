//! The Latch — restricted passage. Task 1 is a measurement, not a guard.
//!
//! Discharges the spec's top risk (a time-varying room graph against the
//! pure-function nav caches) by grep, not by argument: `delve` is a session
//! verb (`windows/vessel/src/action.rs` has no `Delve` `Action` variant) and
//! the catch-up path (`windows/vessel/src/liveness.rs`) never mentions
//! `delve`. Both greps returned no matches (2026-08-28), so the gate lands
//! on a mode change into the chamber lattice, never on an edge the NPC
//! catch-up path walks, and the room graph never becomes time-varying.
//!
//! Builds a real seed-42 world through `build_world_to_with_artifacts` (the
//! idiom `windows/vessel/tests/suite/lantern_fabric.rs` uses), the same
//! reason that file gives: `hornvale-vessel` is the shallowest crate that
//! can see both `hornvale-worldgen`'s `barrier_of` and a built
//! `GeneratedTerrain`.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BarrierPins, BarrierState, BuildDepth, SettlementPins, SkyChoice, WorldComponents, barrier_of,
    build_world_to_with_artifacts,
};

/// Task 1's probe: how many cave-bearing vertices in seed 42's terrain carry
/// each barrier state. Prints a census and asserts only that a barred vertex
/// EXISTS — if none does, acceptance criterion 1 is unreachable and the cut
/// must move.
///
/// Measured 2026-08-28 on seed 42's terrain (`BuildDepth::Settlements`,
/// `Band::Undercroft`, branch 0, non-ocean cave-bearing vertices only):
/// sealed=215 warded=209 thin=215 open=235 (639 barred of 874 total). Close
/// to the quarter-per-state prediction the brief made ahead of the run.
#[test]
fn seed_42_places_at_least_one_barred_cave_mouth() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed 42 failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .as_ref()
        .expect("BuildDepth::Settlements produces terrain");
    let pins = BarrierPins::default();

    let mut sealed = 0usize;
    let mut warded = 0usize;
    let mut thin = 0usize;
    let mut open = 0usize;

    for vertex in terrain.geosphere().vertices() {
        if terrain.is_ocean(vertex) {
            continue;
        }
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        match barrier_of(Seed(42), vertex, Band::Undercroft, 0, &pins) {
            BarrierState::Sealed => sealed += 1,
            BarrierState::Warded => warded += 1,
            BarrierState::Thin => thin += 1,
            BarrierState::Open => open += 1,
        }
    }

    println!(
        "barrier census (seed 42, Undercroft, branch 0): sealed={sealed} warded={warded} thin={thin} open={open}"
    );
    assert!(
        sealed + warded + thin > 0,
        "no barred cave mouth exists in seed 42 — acceptance criterion 1 is \
         unreachable and the campaign's cut must move"
    );
}

/// The address encoding must be injective across every field of
/// `ChamberAddr` — two different addresses must never collide on one key, or
/// clearing one passage would silently clear another.
///
/// MUTATION this must fail against: drop `branch` from `addr_key`'s format
/// string. Both addresses below then produce the same key and the assertion
/// fires.
///
/// Confirmed 2026-08-28: `assertion `left == right` failed: addr_key
/// collided: ["7/Undercroft/0", "8/Undercroft/0", "7/Undercroft/0",
/// "7/Undercroft/1"] left: 3 right: 4` — `base` and `by_branch` collided,
/// as expected once `branch` drops out of the key.
#[test]
fn addr_key_distinguishes_every_field() {
    use hornvale_kernel::{Band, Vertex};
    use hornvale_vessel::passage::addr_key;
    use hornvale_worldgen::chamber::ChamberAddr;

    let base = ChamberAddr {
        vertex: Vertex(7),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let by_vertex = ChamberAddr {
        vertex: Vertex(8),
        ..base
    };
    let by_branch = ChamberAddr { branch: 1, ..base };
    let by_level = ChamberAddr { level: 1, ..base };

    let keys = [
        addr_key(&base),
        addr_key(&by_vertex),
        addr_key(&by_branch),
        addr_key(&by_level),
    ];
    let unique: std::collections::BTreeSet<&String> = keys.iter().collect();
    assert_eq!(unique.len(), keys.len(), "addr_key collided: {keys:?}");
}

/// The address encoding is a SAVE-FORMAT CONTRACT, and the test above cannot
/// see it move. Injectivity survives any renaming: `addr_key` spells the band
/// with `{:?}`, so renaming a `Band` variant keeps every key distinct while
/// changing what every key SAYS.
///
/// That matters now rather than in principle. `possess --out` saves the
/// session ledger into a world file (decision 0368), so committed
/// `passage-cleared` objects carry this exact string. A renamed variant would
/// leave `effective_state` looking up a spelling no committed fact uses — it
/// would find nothing, fall back to the seeded barrier, and a passage the
/// player cleared would quietly re-bar itself. Nothing else in the tree would
/// go red: the derive still compiles, the keys are still unique, and the
/// worldgen-side rename looks local.
///
/// So this pins the literal, the way the project's other save-format
/// contracts are pinned (root CLAUDE.md: "deliberate regeneration uses an
/// epoch suffix, never a rename"). Reddening here is the intended outcome of
/// a rename, not an obstacle to one — the fix is an epoch, and the choice
/// should be made deliberately rather than discovered by a player.
///
/// MUTATION this must fail against: rename `Band::Undercroft` (any variant
/// reachable from a cave entrance address will do). Confirmed 2026-08-28 by
/// the cheaper equivalent that perturbs the same output — swapping
/// `addr_key`'s `{:?}` band field for `{}`-formatted `addr.band as u8`,
/// which is what a `Debug`-spelling change amounts to on the wire:
///
/// ```text
/// assertion `left == right` failed: addr_key's on-disk spelling changed —
/// this is a save-format contract; see the doc comment
///   left: "7/1/0/0"
///  right: "7/Undercroft/0/0"
/// ```
///
/// A genuine behavioural red, not a compile error; restored and re-run green.
#[test]
fn addr_key_spelling_is_the_permanent_on_disk_key() {
    use hornvale_kernel::{Band, Vertex};
    use hornvale_vessel::passage::addr_key;
    use hornvale_worldgen::chamber::ChamberAddr;

    let addr = ChamberAddr {
        vertex: Vertex(7),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    assert_eq!(
        addr_key(&addr),
        "7/Undercroft/0/0",
        "addr_key's on-disk spelling changed — this is a save-format \
         contract; see the doc comment"
    );
}

/// A clearing fact must not open the passage for days BEFORE it. This is what
/// separates a time-correct fold from a mutable flag, and it is what lets any
/// replay evaluating a past instant stay honest.
///
/// MUTATION this must fail against: delete the `<= day` filter in
/// `effective_state` (accept every clearing fact regardless of its day). The
/// day-1 assertion below then reports Open.
///
/// Confirmed 2026-08-28: `assertion `left == right` failed: a passage
/// cleared on day 5 must be barred on day 1 left: Open right: Sealed`.
#[test]
fn a_clearing_fact_does_not_open_the_passage_before_it_happened() {
    use hornvale_kernel::{Band, ConceptRegistry, Ledger, Seed, Vertex, WorldTime};
    use hornvale_vessel::passage::{PASSAGE_CLEARED, cleared_fact, effective_state};
    use hornvale_worldgen::chamber::ChamberAddr;
    use hornvale_worldgen::{BarrierPins, BarrierState};

    // A barrier the seed makes non-Open, forced through the pin so this test
    // does not depend on which vertex the terrain happens to bar.
    let pins = BarrierPins {
        state: Some(BarrierState::Sealed),
    };
    let addr = ChamberAddr {
        vertex: Vertex(1),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };

    let mut reg = ConceptRegistry::default();
    reg.register_predicate(PASSAGE_CLEARED, false, "t").unwrap();
    let mut ledger = Ledger::default();
    let who = ledger.mint_entity(hornvale_kernel::test_lineage(0));

    let cleared_on = WorldTime::from_std_days(5.0).expect("5 days is in range");
    ledger
        .commit(cleared_fact(who, &addr, cleared_on), &reg)
        .unwrap();

    let day1 = WorldTime::from_std_days(1.0).expect("1 day is in range");
    let day9 = WorldTime::from_std_days(9.0).expect("9 days is in range");
    let before = effective_state(&ledger, Seed(42), &addr, day1, &pins);
    let after = effective_state(&ledger, Seed(42), &addr, day9, &pins);

    assert_eq!(
        before,
        BarrierState::Sealed,
        "a passage cleared on day 5 must be barred on day 1"
    );
    assert_eq!(
        after,
        BarrierState::Open,
        "a passage cleared on day 5 must be open on day 9"
    );
}
