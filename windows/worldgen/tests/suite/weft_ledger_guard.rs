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
/// **The mechanism, which survives the merge even though the number does not.**
/// The Trencher's Task 4 routes `EnergySource::DetritalImport` onto the
/// `DETRITUS` axis, which had been a flat `DETRITUS_AMBIENT = 0.2` on all
/// land: a subterranean rung now reads `0.2 + <import>`, a median `+0.12` at
/// `Band::Undercroft` and up to `+0.77`, and `drow` carries the roster's
/// largest `DETRITUS` weight at 0.50. The deep-history bake therefore sees a
/// different competition and commits more dead layers (`is-ruin`), so the
/// count can rise while LIVING settlements fall.
///
/// **The number below is the MERGED world's, re-derived 2026-09-12 in The
/// Trencher's repair pass (ledger #25/#26).** Both pre-merge pins are dead:
/// The Trencher's 24,931 and the Underworld Peoples' 21,524 were each
/// measured against a world that no longer exists. The merged world -- Task
/// 4's per-metabolite supply change AND the four new underworld peoples
/// together -- commits **20,518** facts. The count fell relative to both
/// sides, which is not a quality signal in either direction: the bake sees a
/// different competition, so living settlements and dead layers move
/// independently of each other.
const SEED_42_FACT_COUNT: usize = 20_518;

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
