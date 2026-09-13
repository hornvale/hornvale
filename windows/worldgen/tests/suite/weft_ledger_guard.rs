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
/// TWO CAMPAIGNS MOVE THIS TOGETHER, and the merged number is MEASURED on the
/// merged world rather than added from their separate pins (22_835 on The
/// Tidemark's side, 21_524 on the Underworld Peoples' — both from 20_109;
/// the merged world carries 23_431).
/// Ten new peoples enter the roster and nine of them settle, so the committed
/// world carries more settlements, more occupations, more people and more
/// species rows. A deliberate, reviewed move of the byte golden itself, not a
/// weft change.
///
/// **23_431 -> 25_280 at The Tidemark's close (2026-09-13).** Attribution was
/// MEASURED off the golden's own predicate histogram rather than reasoned
/// from which commits landed — `"predicate": "x"` counts, old fixture against
/// new:
///
/// ```text
///   is-settlement    396 -> 435   (+39)
///   is-place         396 -> 435   (+39)
///   is-occupation   1148 -> 1259  (+111)
///   is-ruin          752 -> 824   (+72)
///   is-person        260 -> 255   (-5)
/// ```
///
/// That shape is a SETTLEMENT-PLACEMENT move, and the bulk of it is
/// `land_settlement selects on habitat realm, not biome` (9912bc7ab), which
/// widened the eligible vertex set by 39 settlements. The campaign's headline
/// mechanism — a failing vent ending the occupation it was holding up — is a
/// small minority of the occupation delta: its own measurement (M3b,
/// `vent_expiry.rs`) counts **2** occupations ending on a vent's failure, plus
/// the refounds those trigger. Do not read the +1,849 as the vent ending's
/// cost; two of the +111 occupations are its, and the rest is the realm fix.
const SEED_42_FACT_COUNT: usize = 25_280;

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
