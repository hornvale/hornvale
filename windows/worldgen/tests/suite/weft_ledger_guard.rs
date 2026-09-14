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
/// MERGE NOTE (The Trencher absorbing origin/main, 2026-09-13): BOTH
/// sides above re-pinned this against worlds that no longer exist --
/// The Trencher's census delivery, and the Orrery/astronomy delivery on
/// main. main's value is taken here to compile. The merged value is
/// re-derived ONCE after the post-absorb census, per ledger #30: a
/// census is a world-mover, not only a remedy.
// The Underworld Peoples delivery adds the four peoples' committed facts;
// seed 42 now carries 21,524 ledger facts.
//
// MERGE RE-PIN (2026-09-13, The Trencher absorbing 41 commits of
// origin/main -- the Orrery/astronomy delivery): 21_525 -> 20_519,
// measured on the merged tree. The deferral the doc note above records
// ("re-derived ONCE after the post-absorb census") is WITHDRAWN rather
// than honoured, because it rested on a false premise: THIS ROW DOES NOT
// READ CENSUS OUTPUT. `seed_42_world()` reads
// `cli/tests/fixtures/world-seed-42.json`, which the merge REGENERATED
// into an object byte-identical to neither side (branch 10bbddb8..., main
// c8b56418..., merged 73c25a8f...), so the number below is the merged
// world's own and the post-merge census cannot move it. It is therefore
// FINAL, unlike the ten calibration pins re-measured alongside it in this
// commit, every one of which reads the committed census CSV and every one
// of which is expected to move again.
//
// The cause is the conjunction the doc comment above already names: this
// campaign's per-metabolite supply change composed with main's astronomy
// delivery, each of which re-decides the deep-history bake's competition.
// The count falls against both pre-merge pins, which is not a quality
// signal in either direction.
// THE TRENCHER, TASK 13 RE-PIN: 20_519 -> 23_810, and the composition of the
// move is the part that matters, not the total. Widening `carbonate` and
// `metamorphic_grade` (`domains/terrain/src/lithology.rs`) moved elevation
// through `erodibility`, and it moved the porosity axis that
// `hydrogeology`/`hydro_at` classify, so seed 42's world came out more
// habitable. Counted from the regenerated fixture against the committed one:
//
//   is-settlement / is-place / population    334 -> 413   (+23.7%)
//   is-occupation                          1_030 -> 1_211
//   is-ruin                                  698 ->   800
//   is-person                                229 ->   244
//
// The +3_291 total is those four families and their dependents; nothing new
// is emitted and no predicate appeared or vanished.
//
// **AN OPEN FINDING RIDES WITH THIS NUMBER, AND IT IS NOT SETTLED BY PINNING
// IT.** 413 settlements puts seed 42 outside the `[75, 400]` "sane band"
// three other tests assert (`confluence`, `history_placement`,
// `history_tumult`), by 3%. The likely driver is the `Hydro::Spring` share,
// which doubled (3.69% -> 7.36% of land) because `promote_to_spring` scores
// the aquifer set's PERIMETER and a continuous porosity field fragments that
// set — see `CLASTIC_AQUIFER_MIN_POROSITY`'s doc for the measurement. That
// was deliberately NOT chased by nudging a porosity constant (decision 0016:
// report the measurement, do not tune to a downstream count), so this pin
// records the world as it is and the band breach is the campaign's to decide.
// If that decision moves the settlement count, this number moves with it.
//
// **THE "LIKELY DRIVER" SENTENCE IS FALSE, AND IT WAS FALSIFIED BY RUNNING
// THE THING** (The Trencher, Task 14). `Hydro::Spring` has exactly two
// readers in the whole tree: `windows/worldgen`'s `is_spring_vertex`, which
// is consumed ONLY by the linguistic exposure classifier (whether a people is
// `Steeped` in the concept "spring"), and one lab metric. Nothing on the
// settlement path reads it — the deep-history bake places settlements from
// `carrying_capacity`, which never asks about hydrogeology. Measured rather
// than argued: with the promotion neutralised so the world contains ZERO
// springs, the counts are unchanged to the settlement —
//
//   seeds 0 / 7 / 42, `all_settlements` at `build_world`
//     pre-Task-13            266 / 282 / 332
//     shipped                325 / 340 / 411
//     shipped, springs off   325 / 340 / 411
//
// — so no achievable change to `promote_to_spring` can move this pin by one
// fact. The rise is Task 13's porosity move reaching carrying capacity
// through rock, soil and fertility, and it is +22% on ALL THREE seeds: seed
// 42 breaches the band because it started nearest the ceiling (332 of 400),
// not because seed 42 is special. Whoever settles the band breach should
// start there and not at the spring rule.
const SEED_42_FACT_COUNT: usize = 23_810;

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
