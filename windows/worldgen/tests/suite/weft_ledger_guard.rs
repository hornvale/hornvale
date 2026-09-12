//! A byte-golden backstop on seed 42's committed fact count — **not**, as an
//! earlier draft of this file claimed, a guard on "derived features are
//! never committed facts."
//!
//! **Corrected in fix round 1 (reviewer IMPORTANT, F2).** `seed_42_world()`
//! reads the committed fixture OFF DISK: no weft code runs here at all, so
//! this test cannot fail because a derived feature reached the ledger — it
//! can only fail if someone rebaselines the golden. That makes it a valid
//! instrument for a DIFFERENT, narrower claim ("the committed fixture's fact
//! count has not silently drifted"), which is worth pinning on its own
//! merits (`fixture.rs`'s own `the_fixture_is_a_real_world` uses a `> 20_000`
//! floor, the wrong shape for "nothing new landed" — a floor cannot catch a
//! single stray fact) — but it is not, and was never, evidence that the weft
//! surface itself commits nothing.
//!
//! **The guard that CAN fail from the thing it guards** is
//! `windows/vessel/tests/suite/the_weft.rs`'s
//! `walking_through_dense_weft_facets_commits_only_agent_at_facts`: it runs
//! a live session through a real, dense stretch of seed 42 (every step
//! prefills the weft window; every `look` reads the weft clause through it)
//! and asserts the SESSION's own committed ledger grew by exactly one fact
//! per successful step — a count downstream of code that could actually
//! commit a derived feature, unlike this file.
use hornvale_worldgen::seed_42_world;

/// `seed_42_world()`'s own fact count, measured directly (not `build_world`,
/// which `fixture.rs`'s own `the_fixture_equals_a_live_build` already keeps
/// honest against the fixture) — the number this test pins.
///
/// **Re-pinned 2026-09-11 (The Trencher, Task 4): 20,109 → 24,931, +4,822
/// facts, +24.0%.** This is the largest single move in this count's history
/// and it is worth naming the cause rather than the number. Task 4 routes
/// `EnergySource::DetritalImport` onto the `DETRITUS` axis, which had been a
/// flat `DETRITUS_AMBIENT = 0.2` on all land: a subterranean rung now reads
/// `0.2 + <import>` instead, a median `+0.12` at `Band::Undercroft` and up to
/// `+0.77`, and `drow` — the one `HabitatRealm::Subterranean` PEOPLE — carries
/// the roster's largest `DETRITUS` weight at 0.50. So the deep-history bake
/// sees a materially different competition and produces more turnover:
/// **more occupation layers, therefore more committed facts.** Seed 42's
/// living settlements in fact FELL, 307 → 284, while the total fact count
/// rose — dead layers are committed too (`is-ruin`), and vertex 10626 alone
/// went from one human layer to ten kobold ones.
///
/// The count is not a quality signal in either direction and this note is not
/// an endorsement: it is a pointer at the one change that moved it, so the
/// next reader of a red here does not start from `windows/vessel`'s weft.
const SEED_42_FACT_COUNT: usize = 24_931;

/// Pinned to an exact count, not a floor, for the same reason
/// `fixture.rs`'s own doc gives for its `> 20_000` check being the wrong
/// shape for this job: a count that can only grow past a floor would not
/// catch a single stray fact. If this reddens, read the module doc above
/// before assuming it means anything about the weft surface — check
/// `cli/tests/fixtures/world-seed-42.json`'s own diff and the plan's branch
/// table first.
#[test]
fn the_seed_42_byte_golden_fact_count_is_unmoved() {
    let world = seed_42_world();
    assert_eq!(
        world.ledger.len(),
        SEED_42_FACT_COUNT,
        "seed 42's committed fact count moved from {SEED_42_FACT_COUNT} to {} -- this is a \
         byte-golden backstop, not a live guard (see this file's own module doc): check \
         cli/tests/fixtures/world-seed-42.json's diff and the plan's branch table before \
         assuming why. If it is a deliberate, reviewed change, update SEED_42_FACT_COUNT in \
         the same commit.",
        world.ledger.len()
    );
}
