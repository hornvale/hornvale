//! The mechanical statement of "derived features are never committed facts"
//! (The Weft, Task 8, spec §6/Half B, and the plan's own step 6): the
//! seed-42 world's committed fact count is unchanged by the whole derived
//! surface — prevalence, occurrence, the residency window, and the walk-band
//! clause that reads it. If a future change ever let a weft derivation reach
//! `Ledger::commit`, this reddens; `fixture.rs`'s own byte-identity check
//! would too (the golden `cli/tests/fixtures/world-seed-42.json`), but that
//! one is written only by `make rebaseline-goldens` and could silently
//! accept the drift if someone ran it without reading the plan's own
//! branch table first. This test has no such escape hatch: the count is a
//! plain assertion, not an artifact a rebaseline can move.
//!
//! **Pinned to an exact count, not a floor.** `fixture.rs`'s own
//! `the_fixture_is_a_real_world` uses `> 20_000` as an anti-vacuity floor,
//! which is the right shape for "the world is not empty" but the wrong
//! shape for "nothing new landed" — a count that can only grow past a floor
//! would not catch a single stray fact. `21,728` is `seed_42_world()`'s own
//! `ledger.len()` as measured on this branch before Task 8's session/prose
//! wiring landed; if the derived surface is ever wired to commit anything,
//! this number moves and the assertion below says so by name.
use hornvale_worldgen::seed_42_world;

/// `seed_42_world()`'s own fact count, measured directly (not `build_world`,
/// which `fixture.rs`'s own `the_fixture_equals_a_live_build` already keeps
/// honest against the fixture) — the number this test pins.
const SEED_42_FACT_COUNT: usize = 21_728;

#[test]
fn the_derived_surface_commits_no_facts() {
    let world = seed_42_world();
    assert_eq!(
        world.ledger.len(),
        SEED_42_FACT_COUNT,
        "seed 42's committed fact count moved from {SEED_42_FACT_COUNT} to {} \
         -- if this is the derived weft surface reaching the ledger, STOP: \
         the plan's own branch table forbids it (derived features are never \
         committed facts). If it is a different, deliberate change, update \
         SEED_42_FACT_COUNT in the same commit as the change that moved it.",
        world.ledger.len()
    );
}
