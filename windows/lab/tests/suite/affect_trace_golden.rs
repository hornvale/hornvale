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

//! **Regenerated again by The Escapement** (decision 0186), which retyped
//! `WorldTime` from fractional `f64` days to an exact `i64` tick count.
//! Adjudicated rather than accepted: **5 of the fixture's 411 lines moved, all
//! of them in the arousal digits alone** — not one label, object or valence
//! changed, so no creature felt a different thing on any tick. Max delta
//! 2.5e-7, against a predicted ceiling of ~1.5e-6 (`FATIGUE_RISE` is 0.3/day
//! and an instant now moves by at most half a tick, 0.432 s). The path is the
//! same long one the section below describes, entered one step earlier: the
//! walk band's emitted `drank`/`rested`/`eaten` fact days land on the tick
//! lattice, `last_drank`/`last_rested`/`last_ate` read them back, and the
//! drive integration carries the sub-second shift into arousal. Spec §2.1
//! ratified exactly this ("committed artifacts move") before the flip.
//!
//! **Regenerated again by The Wicket, Task 9** (the sleep-debt rate becomes
//! per-species and per-PLANETARY-day rather than one constant applied per
//! standard day). Adjudicated, not merely accepted, because this move
//! crossed labels — unlike The Escapement's arousal-digits-only move above.
//! Measured (fix round 1 review): **10 of 410 lines moved in label** (Eager
//! → Content x3, Frustrated → Lost x3, Content → Eager x2, Eager →
//! Searching x1, Frustrated → Eager x1), 7 object changes, 2 valence
//! changes, max arousal delta 0.35652593. Three transitions land IN `Lost`,
//! which `health.rs`'s distress classification counts, and one LEAVES it —
//! so the health metric's own distress count and by-cause attribution moved
//! along with this fixture, not merely its raw numbers. The mechanism is
//! the SAME chain the section below already documents for a niche edit:
//! seed 42's real rotation period is not exactly one standard day, so
//! `fatigue_from_rests`'s local-day conversion (new this task) answers a
//! genuinely different question than the old `TickSpan::as_std_days()` did,
//! for every creature whose Fatigue drive is ever live.
//!
//! **Regenerated again by The Wicket, fix round 1** (Important 2: the FALL
//! terms — `FATIGUE_FALL`/`REST_FALL` — now convert through the same
//! local-day arithmetic the RISE term already did; leaving them on
//! `as_std_days()` was a rotation-dependent recovery bug, not a scope
//! boundary — see `fatigue_from_rests`'s own doc). Measured against the
//! Task-9 fixture immediately above (not against pre-Task-9): **123 of 410
//! lines moved**, 15 in label (Content → Eager x5, Eager → Content x4,
//! Lost → Frustrated x3, Eager → Frustrated x2, Searching → Eager x1), 12
//! object changes, 3 valence changes, max arousal delta 0.39576761. The
//! coverage floor below still holds after this move (both `Lost` and
//! `Frustrated` remain reachable). Same mechanism as Task 9's own move,
//! entered at the fold's other term: seed 42's local day is not one
//! standard day, so a `rested`/`slept` bout's REPAYMENT — previously still
//! measured in standard days — now scales with the same local day its
//! accrual does, for every creature whose Fatigue drive is ever live.
//!
//! **Regenerated again by The Plumb, Task 5** (`REST_BOUT` -- a conscious
//! rest's length -- converts from a flat quarter of the STANDARD day to a
//! quarter of the world's own LOCAL day, the same axis Task 9 and fix round 1
//! above already converted the fatigue RISE/FALL rate terms onto).
//! Adjudicated, not merely accepted, because this move crosses labels again:
//! **89 of 410 lines moved**, 22 in label (Frustrated -> Lost x16, Eager ->
//! Content x5, Content -> Eager x1), 6 object changes, 0 valence changes, 67
//! arousal-only lines, max arousal delta 0.35920095. 16 of the 22 label
//! transitions land IN `Lost` (0 leave it) -- nearly 9x fix round 1's line
//! count and, unlike either precedent above, a move concentrated almost
//! entirely in one direction rather than spread across several labels, which
//! is exactly the shape Task 9's own note warns moves `health.rs`'s distress
//! classification and its by-cause attribution. Re-run after the accept: the
//! coverage floor below still holds (both `Lost` and `Frustrated` remain
//! reachable -- six labels and six species sampled, same as before the
//! move), and the full `hornvale-lab` health-calibration suite
//! (`health_calibration.rs`, 19 tests, the one `heavy`-tagged seed sweep
//! excluded) passes unchanged. The mechanism is the SAME chain Task 9's own
//! move documents, entered at the bout's LENGTH rather than its repayment
//! rate: seed 42's local day (87,988 ticks, 0.87988 std days) is not one
//! standard day, so a rest's span -- previously a flat quarter of the
//! standard day regardless of the world it was taken on -- now scales with
//! the same local day its accrual and repayment already did, for every
//! creature whose Fatigue drive is ever live.
//!
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
    // ONE world build, two guarantees. These were two tests until The Tense
    // measured what that cost: nextest is process-per-test (see
    // `windows/lab/CLAUDE.md`), so a separate coverage test cannot share this
    // build and simply pays for seed 42 twice — 22 s of the gate to re-derive a
    // digest computed three lines up. Ordering matters and is deliberate: the
    // byte pin runs FIRST, and the coverage floor runs after it, so a
    // `REBASELINE=1` accept still has to clear the floor. That is exactly the
    // case the ratchet exists for.
    let digest_for_coverage = digest(&world);
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/affect-trace-seed-42.txt"
        )),
        &digest_for_coverage,
        "the seed-42 affect trace moved — the-waymark's plan-cache/geometry- \
         memo work must be VALUE-preserving (a cache changes WHEN a search \
         runs, never WHAT it returns); a diff here means some change altered \
         what a creature feels, not just how fast, and needs investigation \
         before acceptance",
    );

    // The fixture's COVERAGE, ratcheted — added by The Tense (2026-08-05) because
    // accepting that campaign's regeneration silently narrowed what the golden
    // above witnesses, and nothing would have said so.
    //
    // The golden is a byte pin. A byte pin cannot tell "the values moved" from
    // "the trace stopped exercising half the affect space", and the two want
    // opposite responses. Measured across The Tense's regeneration:
    //
    // ```text
    //   peoples sampled   bugbear 2, hobgoblin 1, human 1, gnoll 2   ->  bugbear 1, hobgoblin 5
    //   affect labels     Content Eager Searching Helpless Lost Frustrated  ->  first four only
    //   (solitaries rust-monster / otyugh / xorn / carrion-crawler: 1 each, unchanged)
    // ```
    //
    // **`Lost` and `Frustrated` are no longer reached at all**, and they were the
    // only negative-valence affect anywhere in the fixture — all nine lines of it
    // sat on the two gnolls, which the re-placement removed from the sample. That
    // is a real loss in what this witness can catch, and it is recorded as a debt
    // rather than accepted: the floor below is the ACHIEVED value, and 6 is the
    // target to get back to. Deliberately not weakened to track a future fall —
    // the same posture `menagerie`'s preregistered `>= 6` dominant target takes.

    let digest = digest_for_coverage;
    let labels: std::collections::BTreeSet<&str> = digest
        .lines()
        .filter_map(|l| l.split("label=").nth(1))
        .filter_map(|r| r.split_whitespace().next())
        .collect();
    let peoples: std::collections::BTreeSet<&str> = digest
        .lines()
        .filter_map(|l| l.split("species=").nth(1))
        .collect();
    assert!(
        labels.len() >= 4,
        "the affect trace exercises only {} distinct labels ({labels:?}) — it reached six \
         before The Tense, and a byte golden cannot tell a narrowing sample from a value \
         change. Do NOT lower this floor; the target is to restore Lost and Frustrated.",
        labels.len()
    );
    assert!(
        peoples.len() >= 6,
        "the affect trace samples only {} distinct species ({peoples:?}); a homogeneous \
         sample makes the golden a witness to one creature's life rather than the roster's",
        peoples.len()
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
