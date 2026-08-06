//! A value-pinned affect-trace fixture (the-waymark, Task 4 carry-over from
//! Task 3's re-review): the health pins (`the_null_control_reads_no_chronic_
//! distress`, the stuck/recovery/self-determinism batteries) can only detect
//! a handful of SHAPE properties — none of them can catch a sampling
//! regression that shifts WHICH affect a creature reads on WHICH tick while
//! every existing assertion still passes. This commits a byte-stable digest
//! of `simulate_world`'s seed-42 output (species + per-tick affect label/
//! arousal/valence/object, one line per creature-tick) as a committed golden
//! (the `session_snapshot` precedent, `hornvale_kernel::golden::assert_
//! golden`), so the-waymark's plan-cache/geometry-memo work — and any future
//! sim performance work — has a standing byte witness that VALUES, not just
//! shapes, did not move.
//!
//! Generated at the-waymark's Task 4 base commit (5e2de827, before the
//! plan-cache change), which The Waymark's own A/B already proved
//! trace-equivalent to pre-campaign (Task 3's re-reviewer, ledger #7/#10).
//! `seed_42_affect_trace_reproduces_the_pinned_bytes` is the task's own
//! acceptance evidence: the plan cache must reproduce this digest bit for
//! bit, since it is a pure-function result cache — a search runs less
//! often, never differently.
//!
//! **Regenerated once more** (Task 4 fix round, rider (c)) after the digest
//! itself gained quantization — the fixture is a serialization boundary
//! (decision 0033), so its floats go through `hornvale_kernel::quantize`
//! exactly like the ledger/CSV/scene emit boundaries do, collapsing any
//! sub-ULP libm noise that would otherwise be a false diff across
//! platforms/toolchains. Regenerated the same way as the first cut: the
//! implementation stashed (`git stash push` on the tracked liveness.rs/
//! session.rs/health.rs), `REBASELINE=1` run against the base commit's
//! plan-cache-free code with THIS quantized digest function, then the
//! implementation restored (`git stash apply` + `git stash drop`) — so the
//! fixture stays a base-generated witness, not one authored by the code it
//! is meant to check.

//! ## Why a change to ONE species' niche drifts EVERY creature's trace
//!
//! Recorded because it is not obvious and it has now cost one investigation
//! (The Deep Realm, Task 6, ledger #28). Re-authoring the xorn's and rust
//! monster's condition niches drifted this fixture by ~1e-4 on the arousal of
//! *unrelated peopled creatures* — human, goblin, hobgoblin — at an unchanged
//! roster and tick count.
//!
//! The tempting explanation is a shared resource-competition normalisation.
//! **There is none:** `per_species_suitability` hoists the supply fields out
//! of its per-species loop, so each species' suitability is computed
//! independently of every other's niche. The real path is longer:
//!
//! ```text
//!   a species' condition niche
//!     -> per_species_suitability
//!     -> the demography report      (a COEXISTENCE FIT over the whole roster)
//!     -> predator_pressure_from / prey_pressure_from   (SHARED fields)
//!     -> every other creature's danger-sense and hunger
//!     -> its affect arousal
//! ```
//!
//! So the blast radius of *any* niche edit is every creature in the trace, not
//! just the species edited. If this fixture drifts after a change that looks
//! unrelated to the creatures whose lines moved, check whether the change
//! touched anything the demography fit reads before concluding something is
//! wrong.

use hornvale_kernel::quantize::quantize;
use hornvale_lab::health::simulate_world;

fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// A byte-stable digest of every derived creature's full affect trace:
/// species, then one line per simulated tick naming the label, the
/// arousal/valence — quantized at this emit boundary (decision 0033, rider
/// (c): the fixture is serialized output, not a compute-path value) — and
/// the object drive. Line-oriented so a future diff (on a REBASELINE accept)
/// reads as "creature N, tick T changed", not an opaque blob.
fn digest(world: &hornvale_kernel::World) -> String {
    let traces = simulate_world(world);
    let mut out = String::new();
    for (i, trace) in traces.iter().enumerate() {
        out.push_str(&format!("creature {i} species={}\n", trace.species));
        for (t, affect) in trace.affects.iter().enumerate() {
            out.push_str(&format!(
                "  tick {t}: label={:?} arousal={:?} valence={:?} object={:?}\n",
                affect.label,
                quantize(affect.arousal),
                quantize(affect.valence),
                affect.object
            ));
        }
    }
    out
}

#[test]
fn seed_42_affect_trace_reproduces_the_pinned_bytes() {
    let world = world();
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/affect-trace-seed-42.txt"
        )),
        &digest(&world),
        "the seed-42 affect trace moved — the-waymark's plan-cache/geometry- \
         memo work must be VALUE-preserving (a cache changes WHEN a search \
         runs, never WHAT it returns); a diff here means some change altered \
         what a creature feels, not just how fast, and needs investigation \
         before acceptance",
    );
}

#[test]
fn the_affect_trace_digest_is_itself_deterministic() {
    // One world build, two independent `simulate_world` runs over it (Task 4
    // fix round, rider (b)): the world is built ONCE here rather than twice
    // (once per `digest()` call) — this still proves `simulate_world`'s own
    // determinism (same world, run twice, same output), it just stops paying
    // for a second, redundant seed-42 build to do it.
    let world = world();
    assert_eq!(
        digest(&world),
        digest(&world),
        "same seed, same code ⇒ same bytes, twice in a row"
    );
}
