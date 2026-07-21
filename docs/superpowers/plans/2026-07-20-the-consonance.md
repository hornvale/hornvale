# The Consonance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship LANG-48 — derive a new fact when two of a world's moons'
real orbital periods sit in a clean small-integer ratio, gate a culture's
access to it by sky-capability exactly like every other astronomical
fact, and let it flow into the already-shipped causal-schema library
completely unmodified. Prove a numeracy-gated expressibility predicate as
a pure function; no live Book rendering.

**Architecture:** Detection lives in `domains/astronomy` (a new module,
pure, zero new draws — it only compares values astronomy already
computes). Witnessed-access gating and schema explanation live in
`windows/worldgen/src/chorus.rs`, extending the exact same
`Observability`/`explain_*` pattern `moon-count` and `day-length-std`
already use. The numeracy predicate lives in
`domains/language/src/numeracy.rs`, reusing `render_quantity_at_rung`'s
own classification.

**Tech Stack:** Rust (edition 2024), existing crate primitives only
(`hornvale_astronomy::Moon`/`StdDays`, `hornvale_language::schemas::{admitted,
schema_prior, select_schema, FactShape, SchemaId}`,
`hornvale_language::account::{Observability, Requirement, NeededConcept}`,
`hornvale_language::numeracy::{NumeracyRung, render_quantity_at_rung}`) —
no new dependencies.

## Global Constraints

- No `HashMap`/`HashSet` anywhere — `BTreeMap`/`BTreeSet`/`Vec` only.
- Every public item, field, and variant gets a one-line doc comment
  (`#![warn(missing_docs)]` stays set in every touched/new file).
- Every new `pub`-boundary bare primitive needs a `type-audit:` verdict
  tag in its doc comment.
- Zero new randomness in ratio detection or the numeracy predicate — both
  are pure functions, no `Seed`/`Stream` use. The ONE new draw in this
  plan is `explain_moon_ratio`'s schema-selection stream, mirroring
  `explain_moons`'s own stream shape exactly.
- `cargo fmt` as the final step before every commit.
- New permanent streams this plan introduces (save-format additions,
  never renames) must be added to `domains/language/src/lib.rs`'s
  `stream_labels()` in the task that introduces them, AND
  `book/src/reference/stream-manifest-generated.md` must be regenerated
  in the SAME commit (`cargo run -p hornvale -- streams >
  book/src/reference/stream-manifest-generated.md`) — this exact gap bit
  the two immediately prior campaigns; do not repeat it.
- Do not modify `select_schema`, `schema_prior`, `admitted`,
  `schemas.rs`'s admission lists, `MOON_COUNT`, `MOON_PERIOD_STD`,
  `render_quantity_at_rung`, `explain_day`, or `explain_moons` — this
  plan only reads and reuses them.

---

### Task 1: Ratio detection (`domains/astronomy`)

**Files:**
- Create: `domains/astronomy/src/resonance.rs`
- Modify: `domains/astronomy/src/lib.rs` (register the module, alphabetical
  position between `pub mod render;` and `pub mod sky_position;`)

**Interfaces:**
- Produces: `pub struct MoonPeriodRatio { pub ratio: f64, pub numerator: u32,
  pub denominator: u32 }`; `pub fn detect_moon_period_ratio(periods: &[StdDays])
  -> Option<MoonPeriodRatio>`. Task 2 consumes this directly
  (`hornvale_astronomy::resonance::{MoonPeriodRatio, detect_moon_period_ratio}`).
  Task 5 consumes `MoonPeriodRatio.numerator`/`.denominator`.

- [ ] **Step 1: Write the failing tests**

Create `domains/astronomy/src/resonance.rs`:

```rust
//! LANG-48: proportional reasoning over already-witnessed quantities — a
//! culture's own account can now hold whether two moons' real periods sit
//! in a clean small-integer ratio. This module is the pure detection half
//! (no draws, no ledger access); see
//! `docs/superpowers/specs/2026-07-20-the-consonance-design.md`.
#![warn(missing_docs)]

use crate::StdDays;

#[cfg(test)]
mod tests {
    use super::*;

    fn days(x: f64) -> StdDays {
        StdDays::new(x).expect("test fixture uses a finite positive value")
    }

    #[test]
    fn a_clean_two_to_one_pair_is_detected() {
        let periods = [days(30.0), days(60.0)];
        let found = detect_moon_period_ratio(&periods).expect("30/60 is a clean 2:1");
        assert_eq!(found.numerator, 2);
        assert_eq!(found.denominator, 1);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-astronomy a_clean_two_to_one_pair_is_detected -- --nocapture`
Expected: FAIL to compile — `cannot find function 'detect_moon_period_ratio' in this scope`.

Add `pub mod resonance;` to `domains/astronomy/src/lib.rs`, alphabetically:

```rust
pub mod render;
pub mod resonance;
pub mod sky_position;
```

- [ ] **Step 3: Write the implementation**

Insert into `domains/astronomy/src/resonance.rs`, above the `#[cfg(test)]`
module:

```rust
/// A detected clean small-integer ratio between two of a world's moons'
/// real orbital periods (spec §3.1): `ratio` is the ACTUAL measured value
/// (e.g. `1.987`, not the idealized `2.0`) — ground truth, never replaced
/// by the idealized rational it happens to sit near. `numerator` and
/// `denominator` name the matched candidate (always `numerator >
/// denominator`, so `ratio` is always expressed slower:faster, >= 1.0).
/// type-audit: bare-ok(ratio), bare-ok(numerator), bare-ok(denominator)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MoonPeriodRatio {
    /// The actual measured period ratio (slower period / faster period).
    pub ratio: f64,
    /// The matched candidate's numerator (the slower side).
    pub numerator: u32,
    /// The matched candidate's denominator (the faster side).
    pub denominator: u32,
}

/// The closed set of low-order rational candidates a ratio is checked
/// against, each `(numerator, denominator)` with `numerator >=
/// denominator`. Deliberately small — real archaeoastronomy's noticed
/// ratios (the saros, the Metonic cycle) are low-order; a culture that
/// could notice a 47:31 relationship is not what this campaign models.
const RATIO_CANDIDATES: &[(u32, u32)] = &[
    (1, 1),
    (2, 1),
    (3, 1),
    (3, 2),
    (4, 1),
    (4, 3),
    (5, 1),
    (5, 2),
    (5, 3),
    (5, 4),
];

/// How close a measured ratio must sit to a candidate (relative
/// deviation) to count as a match. 5%: tight enough that "close to 2:1"
/// means genuinely close, loose enough that real drawn periods (never
/// exactly integer multiples) can still match.
const RATIO_TOLERANCE: f64 = 0.05;

/// The nearest candidate to `ratio` (always `ratio >= 1.0`) and its
/// relative deviation from that candidate's own value — always returns
/// a candidate (the list is fixed and non-empty), the caller decides
/// whether the deviation is within [`RATIO_TOLERANCE`].
fn nearest_candidate(ratio: f64) -> ((u32, u32), f64) {
    RATIO_CANDIDATES
        .iter()
        .map(|&(n, d)| {
            let candidate_value = n as f64 / d as f64;
            let deviation = (ratio - candidate_value).abs() / candidate_value;
            ((n, d), deviation)
        })
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .expect("RATIO_CANDIDATES is a fixed non-empty slice")
}

/// Detect the single best clean small-integer ratio among every pair of
/// `periods` (spec §3.1). Zero pairs for 0-1 moons (returns `None`
/// immediately); every pair checked for 2+ moons. Among all matching
/// pairs (relative deviation within [`RATIO_TOLERANCE`] of their nearest
/// candidate), the single survivor is chosen by (deviation ascending,
/// simplicity — `max(numerator, denominator)` — ascending, pair index
/// ascending) for full determinism. Pure: no `Seed`/`Stream` use, no
/// ledger access.
pub fn detect_moon_period_ratio(periods: &[StdDays]) -> Option<MoonPeriodRatio> {
    let mut best: Option<(MoonPeriodRatio, f64, u32)> = None;
    for i in 0..periods.len() {
        for j in (i + 1)..periods.len() {
            let p_i = periods[i].get();
            let p_j = periods[j].get();
            let (slower, faster) = if p_i >= p_j { (p_i, p_j) } else { (p_j, p_i) };
            let measured_ratio = slower / faster;
            let ((numerator, denominator), deviation) = nearest_candidate(measured_ratio);
            if deviation > RATIO_TOLERANCE {
                continue;
            }
            let simplicity = numerator.max(denominator);
            let candidate = MoonPeriodRatio {
                ratio: measured_ratio,
                numerator,
                denominator,
            };
            let better = match &best {
                None => true,
                Some((_, best_deviation, best_simplicity)) => {
                    (deviation, simplicity) < (*best_deviation, *best_simplicity)
                }
            };
            if better {
                best = Some((candidate, deviation, simplicity));
            }
        }
    }
    best.map(|(candidate, _, _)| candidate)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-astronomy a_clean_two_to_one_pair_is_detected -- --nocapture`
Expected: PASS.

- [ ] **Step 5: Add the non-degeneracy and tie-break tests**

Append to `domains/astronomy/src/resonance.rs`'s `tests` module:

```rust
    #[test]
    fn no_pair_means_no_ratio() {
        assert_eq!(detect_moon_period_ratio(&[]), None);
        assert_eq!(detect_moon_period_ratio(&[days(30.0)]), None);
    }

    #[test]
    fn an_unclean_pair_is_not_detected() {
        // 30 vs 41 days: ratio ~1.367, nearest candidate is 4:3 (1.333),
        // deviation ~2.5% — wait, that IS within 5%. Use a pair that
        // genuinely misses every candidate: 30 vs 47 (ratio ~1.567,
        // nearest candidates 3:2=1.5 (4.3% dev) and 5:3=1.667 (6.4% dev) —
        // 4.3% is still within tolerance. Use 30 vs 52 instead: ratio
        // ~1.733, nearest is 5:3=1.667 (4.0% dev, still within!) — use
        // 30 vs 55: ratio ~1.833, nearest candidates 2:1=2.0 (8.3% dev)
        // and 5:3=1.667 (10.0% dev) — both exceed 5%, genuinely unclean.
        let periods = [days(30.0), days(55.0)];
        assert_eq!(detect_moon_period_ratio(&periods), None);
    }

    #[test]
    fn three_moons_picks_the_single_best_pair() {
        // Moon A/B are a clean 2:1 (30/60); moon C is unrelated to both.
        let periods = [days(30.0), days(60.0), days(41.0)];
        let found = detect_moon_period_ratio(&periods).expect("A/B is a clean 2:1");
        assert_eq!((found.numerator, found.denominator), (2, 1));
    }

    #[test]
    fn detect_moon_period_ratio_is_pure() {
        let periods = [days(30.0), days(60.0)];
        assert_eq!(
            detect_moon_period_ratio(&periods),
            detect_moon_period_ratio(&periods)
        );
    }
```

- [ ] **Step 6: Run test to verify it fails, then run the full module**

Run: `cargo test -p hornvale-astronomy resonance:: -- --nocapture`
Expected: PASS (5 tests: `a_clean_two_to_one_pair_is_detected`,
`no_pair_means_no_ratio`, `an_unclean_pair_is_not_detected`,
`three_moons_picks_the_single_best_pair`, `detect_moon_period_ratio_is_pure`).

Run: `cargo test -p hornvale-astronomy`
Expected: PASS, same count as before this task plus the 5 new tests — no
existing test's outcome changes.

- [ ] **Step 7: Commit**

```bash
git add domains/astronomy/src/resonance.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy): detect a clean small-integer ratio between two moons' periods — the-consonance T1"
```

---

### Task 2: Emit the fact (`domains/astronomy`)

**Files:**
- Modify: `domains/astronomy/src/facts.rs`

**Interfaces:**
- Consumes: `hornvale_astronomy::resonance::detect_moon_period_ratio` (Task 1).
- Produces: `pub const MOON_PERIOD_RATIO: &str = "moon-period-ratio";`.
  Task 3 consumes this constant directly.

- [ ] **Step 1: Write the failing test**

Add near the other predicate constants in `domains/astronomy/src/facts.rs`
(alongside `MOON_COUNT`/`MOON_PERIOD_STD`, same style):

```rust
/// The detected clean small-integer ratio between two of a world's
/// moons' real periods (LANG-48), when one exists — the ACTUAL measured
/// ratio, not the idealized rational it matched. Absent when no pair of
/// moons' periods sits within tolerance of a low-order rational.
pub const MOON_PERIOD_RATIO: &str = "moon-period-ratio";
```

Add two tests to `domains/astronomy/src/facts.rs`'s own `#[cfg(test)] mod
tests` block (it already has a `committed_world(seed: u64) -> (World,
EntityId, GenesisOutcome)` helper at the top of the module — reuse it
directly, do not invent a second one). Moon periods are continuous draws
(no pin exists to force a specific pair), so both tests search a fixed
seed range rather than hand-constructing a full `Moon` fixture (it has
nine fields, only one of which — `period` — either test needs to
control):

```rust
    #[test]
    fn moon_period_ratio_is_committed_when_a_clean_pair_exists() {
        // Detection itself is already proven by resonance.rs's own tests
        // (Task 1); this proves ONLY the wiring: when
        // detect_moon_period_ratio finds a match for a real generated
        // world's actual moon periods, genesis commits exactly that
        // value under MOON_PERIOD_RATIO. Search a fixed seed range for
        // at least one world with a real clean pair.
        let mut verified = false;
        for seed in 1u64..=200 {
            let outcome = crate::system::generate(Seed(seed), &SkyPins::default()).unwrap();
            if outcome.system.moons.len() < 2 {
                continue;
            }
            let periods: Vec<_> = outcome.system.moons.iter().map(|m| m.period).collect();
            let Some(expected) = crate::resonance::detect_moon_period_ratio(&periods) else {
                continue;
            };
            let mut w = world_with(seed);
            let subject = w.ledger.mint_entity();
            genesis(&mut w, subject, &outcome).unwrap();
            assert_eq!(
                w.ledger.value_of(subject, MOON_PERIOD_RATIO),
                Some(&Value::Number(expected.ratio)),
                "seed {seed}: genesis must commit exactly detect_moon_period_ratio's own value"
            );
            verified = true;
            break;
        }
        assert!(
            verified,
            "no seed in 1..=200 produced a clean moon-period ratio — \
             widen the search range or check RATIO_TOLERANCE"
        );
    }

    #[test]
    fn moon_period_ratio_is_absent_when_no_clean_pair_exists() {
        let mut verified = false;
        for seed in 1u64..=200 {
            let outcome = crate::system::generate(Seed(seed), &SkyPins::default()).unwrap();
            let periods: Vec<_> = outcome.system.moons.iter().map(|m| m.period).collect();
            if crate::resonance::detect_moon_period_ratio(&periods).is_some() {
                continue;
            }
            let mut w = world_with(seed);
            let subject = w.ledger.mint_entity();
            genesis(&mut w, subject, &outcome).unwrap();
            assert!(
                w.ledger.value_of(subject, MOON_PERIOD_RATIO).is_none(),
                "seed {seed}: genesis must not commit moon-period-ratio \
                 when no clean pair exists"
            );
            verified = true;
            break;
        }
        assert!(
            verified,
            "no seed in 1..=200 lacked a clean moon-period ratio — every \
             seed matched, which would mean RATIO_TOLERANCE is too loose"
        );
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-astronomy moon_period_ratio_is_committed_when_a_clean_pair_exists moon_period_ratio_is_absent_when_no_clean_pair_exists -- --nocapture`
Expected: FAIL — either a compile error (`MOON_PERIOD_RATIO` doesn't
exist yet) or the fact is simply absent from every committed ledger (so
the first test's `verified` flag never flips true).

- [ ] **Step 3: Write the implementation**

In `domains/astronomy/src/facts.rs`'s `genesis` function, immediately
after the existing `MOON_COUNT` commit block (the one reading
`fact(subject, MOON_COUNT, Value::Number(system.moons.len() as f64))`),
insert:

```rust
    if let Some(found) = crate::resonance::detect_moon_period_ratio(
        &system.moons.iter().map(|m| m.period).collect::<Vec<_>>(),
    ) {
        world.ledger.commit(
            fact(subject, MOON_PERIOD_RATIO, Value::Number(found.ratio)),
            &world.registry,
        )?;
    }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-astronomy moon_period_ratio_is_committed_when_a_clean_pair_exists moon_period_ratio_is_absent_when_no_clean_pair_exists -- --nocapture`
Expected: PASS (2 tests).

- [ ] **Step 5: Run the full crate's test suite**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS, no existing test's outcome changes — the new commit block
only fires when `detect_moon_period_ratio` finds a match, and every
existing world's genesis either already had no such match (silently
unaffected) or now gains one new fact no existing test asserts the
ABSENCE of. If any existing test does assert an exact fact count/list
for a specific seed and that seed happens to gain this new fact, update
that assertion — read the failure output to find it, don't guess.

- [ ] **Step 6: Commit**

```bash
git add domains/astronomy/src/facts.rs
git commit -m "feat(astronomy): commit moon-period-ratio when a clean pair exists — the-consonance T2"
```

---

### Task 3: Witnessed access (`windows/worldgen`)

**Files:**
- Modify: `windows/worldgen/src/chorus.rs`

**Interfaces:**
- Consumes: `hornvale_astronomy::facts::MOON_PERIOD_RATIO` (Task 2).
- Produces: nothing new consumed by later tasks — this task only extends
  two existing tables.

- [ ] **Step 1: Write the failing test**

Add to `windows/worldgen/src/chorus.rs`'s own `#[cfg(test)] mod tests`
block (it already has `observability_table_params()`, per this file's
existing test helpers):

```rust
    #[test]
    fn moon_period_ratio_has_an_observability_row() {
        let table = observability_table();
        let row = table
            .get(hornvale_astronomy::facts::MOON_PERIOD_RATIO)
            .expect("moon-period-ratio must have an Observability row");
        assert!(
            matches!(row.requirement, Requirement::SkyGraded { threshold } if threshold > 0.6),
            "the ratio's threshold must sit above moon-count's own 0.6 (spec §3.2)"
        );
        assert_eq!(row.shape, FactShape::CyclicEvent);
    }

    #[test]
    fn moon_period_ratio_flows_into_chorus_ground() {
        // Hand-built ledger, mirroring this module's own
        // `unbindable_cultures_stay_plain_lost` test's style: mint one
        // entity, mark it `is-a` "planet" (any classified subject
        // qualifies for chorus_ground's own subject walk), commit a
        // moon-period-ratio fact on it, then confirm chorus_ground
        // surfaces it — proves CONSTRUCTION_ORDER's new entry works
        // without needing a full genesis run.
        let mut world = World::new(Seed(1));
        let subject = world.ledger.mint_entity();
        world
            .ledger
            .commit(
                hornvale_kernel::Fact {
                    subject,
                    predicate: hornvale_kernel::world::IS_A.to_string(),
                    object: hornvale_kernel::Value::Text("planet".to_string()),
                    place: None,
                    day: None,
                    provenance: "test fixture".to_string(),
                },
                &world.registry,
            )
            .unwrap();
        world
            .ledger
            .commit(
                hornvale_kernel::Fact {
                    subject,
                    predicate: hornvale_astronomy::facts::MOON_PERIOD_RATIO.to_string(),
                    object: hornvale_kernel::Value::Number(2.0),
                    place: None,
                    day: None,
                    provenance: "test fixture".to_string(),
                },
                &world.registry,
            )
            .unwrap();

        let ground = chorus_ground(&world);
        assert!(
            ground.iter().any(|g| g.predicate
                == hornvale_astronomy::facts::MOON_PERIOD_RATIO
                && g.object == hornvale_kernel::Value::Number(2.0)),
            "moon-period-ratio must flow into chorus_ground once committed: {ground:?}"
        );
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-worldgen moon_period_ratio_has_an_observability_row moon_period_ratio_flows_into_chorus_ground -- --nocapture`
Expected: FAIL — no `Observability` row exists yet, and the predicate is
absent from `chorus_ground`'s output.

- [ ] **Step 3: Write the implementation**

In `windows/worldgen/src/chorus.rs`'s `observability_table()` function,
immediately after the existing `DAY_LENGTH_STD` block (before the
`INSTANCE_OF` block), insert:

```rust
    table.insert(
        hornvale_astronomy::facts::MOON_PERIOD_RATIO.to_string(),
        Observability {
            requirement: Requirement::SkyGraded { threshold: 0.85 },
            domain: "sky",
            concept: NeededConcept::Fixed("moon"),
            shape: FactShape::CyclicEvent,
        },
    );
```

Find `CONSTRUCTION_ORDER`'s definition (`const CONSTRUCTION_ORDER: [&str;
3] = [...]`, currently 3 entries) and change it to 4 entries, appending
`hornvale_astronomy::facts::MOON_PERIOD_RATIO`:

```rust
    const CONSTRUCTION_ORDER: [&str; 4] = [
        hornvale_astronomy::facts::MOON_COUNT,
        hornvale_astronomy::facts::STAR_CLASS,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        hornvale_astronomy::facts::MOON_PERIOD_RATIO,
    ];
```

(The exact existing 3 entries above are read from the file at
implementation time — copy them verbatim from what's actually there
rather than assuming this literal list; only the array size and the one
new appended line are this task's own change.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-worldgen moon_period_ratio_has_an_observability_row moon_period_ratio_flows_into_chorus_ground -- --nocapture`
Expected: PASS.

- [ ] **Step 5: Run the full crate's test suite**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS, no existing test's outcome changes.

- [ ] **Step 6: Verify the `windows/book` mirror obligation held — a real architectural tripwire, not a formality**

`chorus_ground`'s own doc comment states its `CONSTRUCTION_ORDER` "must
match `windows/book::render_volume`'s `CONSTRUCTION_ORDER` exactly" —
`windows/book/src/lib.rs:218` has its OWN separate copy of this constant,
`const CONSTRUCTION_ORDER: &[&str] = &[MOON_COUNT, STAR_CLASS,
DAY_LENGTH_STD]`, used to build the god's-eye Book text independently of
`chorus_ground`. This plan deliberately does NOT add `MOON_PERIOD_RATIO`
to `windows/book`'s copy (spec §6: no live rendering) — verified safe
because `render_explanations`/`explanation_head`
(`windows/book/src/lib.rs:1054`) gracefully skip any predicate they don't
recognize (`continue` on a `None` match) rather than erroring, so an
account entry for a predicate `windows/book` has no rendering rule for
simply never produces a line. Confirm this holds for real rather than
trusting the reasoning:

Run: `cargo test -p hornvale-book identity_chorus_reproduces_the_gods_eye_lines -- --nocapture`
Expected: PASS, unchanged.

**If this test FAILS:** STOP. Do not modify `windows/book` to "fix" it —
that would cross the G3-approved "no live rendering" boundary without
Nathan's sign-off. Report the failure verbatim as a `BLOCKED` status; this
would mean the two `CONSTRUCTION_ORDER` copies are more tightly coupled
than this plan's research found, and the scope decision itself needs
revisiting, not a code workaround.

- [ ] **Step 7: Commit**

```bash
git add windows/worldgen/src/chorus.rs
git commit -m "feat(worldgen): gate moon-period-ratio by sky-capability, above moon-count's own threshold — the-consonance T3"
```

---

### Task 4: Explanation (`windows/worldgen`)

**Files:**
- Modify: `windows/worldgen/src/chorus.rs`

**Interfaces:**
- Consumes: everything Task 3 wired (the `Observability` row means a
  witnessed culture's account now has a `MOON_PERIOD_RATIO` entry to
  explain).
- Produces: nothing new consumed by later tasks — wires into the
  existing `explain()` call chain.

- [ ] **Step 1: Find a real seed with a placed culture**

`explain_moon_ratio` needs a genuinely non-empty `cyclic` (a placed
culture's own pantheon periods) to have anything to bind — a bare
`World::new` (this file's own `unbindable_cultures_stay_plain_lost` test
fixture) has none. Rather than depending on a seed that ALSO happens to
produce a real clean moon-period ratio (a much rarer, slower-to-find
combination), the test below hand-builds the `moon-period-ratio` ground
fact directly (exactly how `unbindable_cultures_stay_plain_lost` already
hand-builds its own `day-length-std` ground fact) and only needs a real
settled world for its REAL pantheon/climate. Confirm seed 1 places a
goblin culture before using it:

```bash
cargo test -p hornvale-worldgen chorus_ground -- --nocapture 2>&1 | head -5
```

(This file's own existing tests already build seed-1/seed-10 worlds
successfully — seed 1 is the project's own canonical default seed, used
throughout the codebase's gallery artifacts; if it turns out NOT to place
a "goblin" culture, try seed 10 next, matching
`the_day_binds_by_period_match_never_identity`'s own choice in
`windows/worldgen/tests/explanations.rs`.)

- [ ] **Step 2: Write the failing test**

Add to `windows/worldgen/src/chorus.rs`'s test module:

```rust
    #[test]
    fn moon_period_ratio_is_explained_with_an_admitted_schema() {
        let world = hornvale_worldgen_test_world(1);
        let cyclic = cyclic_beliefs_of(&world, "goblin");
        assert!(
            !cyclic.is_empty(),
            "seed 1 must place a goblin culture with a real pantheon"
        );
        let climate = crate::climate_of(&world).unwrap();
        let flagship = crate::flagship_of(&world, "goblin").expect("goblin must have a flagship");
        let subsistence = hornvale_culture::subsistence_of(&world, flagship.id)
            .as_deref()
            .and_then(subsistence_from_name)
            .expect("goblin flagship must have a subsistence");
        let wc = WorldComponents::assemble().expect("component assembly must succeed");
        let psych = wc.psyche.get_by_label("goblin").expect("goblin psychology must exist");
        let beta = beta_of(psych);

        let ground = vec![GroundFact {
            subject: "Vebe".to_string(),
            predicate: hornvale_astronomy::facts::MOON_PERIOD_RATIO.to_string(),
            object: hornvale_kernel::Value::Number(2.0),
        }];
        let mut params = observability_table_params();
        params.holdings.insert("moon".to_string());
        let mut account = account_of(&ground, &params);
        assert_eq!(
            account.entries[0].disposition,
            Disposition::Kept,
            "sky_capability=1.0 in observability_table_params() must keep a SkyGraded fact"
        );

        explain_moon_ratio(
            world.seed,
            "goblin",
            &mut account,
            &params,
            &cyclic,
            beta,
            subsistence,
            psych.sociality,
        );

        let Disposition::Explained { schema, .. } = &account.entries[0].disposition else {
            panic!(
                "expected Explained, got {:?}",
                account.entries[0].disposition
            );
        };
        assert!(
            matches!(schema, SchemaId::Agentive | SchemaId::PathJourney | SchemaId::Balance),
            "fired schema {schema:?} must be in FactShape::CyclicEvent's real admitted set"
        );
    }
```

Add a small helper mirroring `windows/worldgen/tests/explanations.rs`'s
own `generated()` function (that file's own doc comment names it "the
shared pattern every neighboring worldgen integration test uses" — this
crate-internal test module needs the identical shape since it can't
import from `tests/`):

```rust
    fn hornvale_worldgen_test_world(seed: u64) -> World {
        crate::build_world(
            Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            crate::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &crate::SettlementPins::default(),
        )
        .unwrap()
    }
```

- [ ] **Step 3: Run test to verify it fails**

Run: `cargo test -p hornvale-worldgen moon_period_ratio_is_explained_with_an_admitted_schema -- --nocapture`
Expected: FAIL — no `explain_moon_ratio` function exists yet (compile
error: `cannot find function`).

- [ ] **Step 4: Write the implementation**

Insert into `windows/worldgen/src/chorus.rs`, immediately after
`explain_moons`'s closing `}` (before the `explain` function's own doc
comment):

```rust
/// Explanation for LANG-48's `moon-period-ratio` fact — structurally
/// identical to [`explain_moons`] (same locate → admit → prior → select
/// → bind → mutate shape), reusing every one of its primitives
/// unchanged: [`admitted`], [`schema_prior`], [`select_schema`],
/// [`bind_agent`]. The only differences from `explain_moons` are the
/// predicate and the fact-shape (`FactShape::CyclicEvent`, the shape
/// `day-length-std` already uses — no change to `schemas.rs`'s admission
/// lists). Binds the SAME representative agent `explain_moons` already
/// uses (`cyclic`'s own slowest-ranked belief) rather than attempting to
/// identify the two specific moons the ratio involves by entity — this
/// campaign's fact is a single world-level scalar, exactly like
/// `moon-count`'s own, so it needs no per-moon entity resolution.
fn explain_moon_ratio(
    world_seed: Seed,
    species: &str,
    account: &mut Account,
    params: &AccountParams,
    cyclic: &[(hornvale_religion::Belief, f64)],
    beta: f64,
    subsistence: Subsistence,
    sociality: Sociality,
) {
    let predicate = hornvale_astronomy::facts::MOON_PERIOD_RATIO;
    if domain_of(params, predicate) != Some("sky") {
        return;
    }
    let Some(ratio_index) = account
        .entries
        .iter()
        .position(|e| e.fact.predicate == predicate)
    else {
        return;
    };
    if account.entries[ratio_index].disposition != Disposition::Kept {
        return;
    }
    if cyclic.is_empty() {
        return;
    }

    let slowest_rank = cyclic.len() - 1;
    let (belief, _period) = &cyclic[slowest_rank];
    let manner = manner_of(slowest_rank, cyclic.len());

    let candidates = admitted(FactShape::CyclicEvent);
    let prior = schema_prior(subsistence, sociality, &candidates);
    let mut schema_stream = world_seed
        .derive("language")
        .derive(species)
        .derive("schema")
        .derive("sky")
        .derive(fact_shape_key(FactShape::CyclicEvent))
        .derive(predicate)
        .stream();
    let Some(schema) = select_schema(&prior, beta, &mut schema_stream) else {
        return;
    };

    let agent = bind_agent(schema, &belief.deity);
    let lexeme = if schema == SchemaId::Agentive {
        let lex_candidates = lexemes_for(SchemaId::Agentive, sub_frame_of(subsistence));
        let mut lexeme_stream = world_seed
            .derive("language")
            .derive(species)
            .derive("lexeme")
            .derive(predicate)
            .stream();
        select_lexeme(lex_candidates, &mut lexeme_stream)
    } else {
        None
    };

    let underlying = account.entries[ratio_index].disposition.clone();
    account.entries[ratio_index].disposition = Disposition::Explained {
        underlying: Box::new(underlying),
        schema,
        agent,
        lexeme,
        manner,
    };
}
```

Note the schema-selection stream adds one more `.derive(predicate)` than
`explain_moons`'s own (`.derive(fact_shape_key(FactShape::Count))` alone)
— `explain_day` and `explain_moons` already share the SAME
`fact_shape_key(FactShape::CyclicEvent)`/`fact_shape_key(FactShape::Count)`
split between only two callers each keyed to a distinct shape, but this
new function shares `FactShape::CyclicEvent` with `explain_day`
(`day-length-std`) — without the extra `.derive(predicate)`, both
functions would draw from the IDENTICAL stream for the same species,
silently correlating two unrelated explanations. The extra
`.derive(predicate)` keeps the streams distinct — this is a real,
load-bearing detail, not decoration.

In `explain()` (the caller), immediately after the existing
`explain_moons(...)` call, add:

```rust
    explain_moon_ratio(
        world.seed,
        species,
        account,
        params,
        &cyclic,
        beta,
        subsistence,
        psych.sociality,
    );
```

Add the new permanent stream to `domains/language/src/lib.rs`'s
`stream_labels()` (find the existing block; insert a new entry — the
exact anchor point is whatever the file's current last entry is at
implementation time):

```rust
        (
            "language/<species>/schema/sky/<shape>/<predicate>",
            "The Consonance: schema selection for a fact sharing FactShape::CyclicEvent with another predicate (moon-period-ratio vs day-length-std) — the extra predicate leg keeps their streams distinct",
        ),
```

Then regenerate the manifest in this SAME commit:

```bash
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
```

- [ ] **Step 5: Run test to verify it passes**

Run: `cargo test -p hornvale-worldgen moon_period_ratio_is_explained_with_an_admitted_schema -- --nocapture`
Expected: PASS.

- [ ] **Step 6: Run the full crate's test suite and confirm the manifest diff**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS, no existing test's outcome changes.

Run: `git diff book/src/reference/stream-manifest-generated.md`
Expected: exactly one new row added (the entry from Step 4 above), nothing
else changed or removed.

- [ ] **Step 7: Commit**

```bash
git add windows/worldgen/src/chorus.rs domains/language/src/lib.rs book/src/reference/stream-manifest-generated.md
git commit -m "feat(worldgen): explain moon-period-ratio via the existing schema library, unmodified — the-consonance T4"
```

---

### Task 5: Numeracy expressibility (`domains/language`)

**Files:**
- Modify: `domains/language/src/numeracy.rs`

**Interfaces:**
- Consumes: `NumeracyRung`, `render_quantity_at_rung` (already in this
  file, from Few and Many).
- Produces: `pub fn expressible_at_rung(numerator: u32, denominator: u32,
  rung: NumeracyRung) -> bool`. Task 6 consumes this directly.

- [ ] **Step 1: Write the failing tests**

Append to `domains/language/src/numeracy.rs`'s existing `#[cfg(test)] mod
tests` block:

```rust
    #[test]
    fn a_two_to_one_ratio_is_expressible_at_full_counting_but_not_subitizing() {
        assert!(!expressible_at_rung(2, 1, NumeracyRung::Subitizing));
        assert!(expressible_at_rung(2, 1, NumeracyRung::FullCounting));
        assert!(expressible_at_rung(2, 1, NumeracyRung::Decimals));
    }

    #[test]
    fn a_one_to_one_ratio_is_expressible_even_at_subitizing() {
        // "one" and "one" are both exact words at every rung, including
        // the floor — Subitizing's own exact-word cases (spec: x == 1.0
        // renders "one" exactly, not a qualitative degradation).
        assert!(expressible_at_rung(1, 1, NumeracyRung::Subitizing));
    }

    #[test]
    fn expressibility_is_monotonic_in_rung() {
        // A coarser rung is never MORE expressive than a finer one — Law
        // 5 (spec §4): if Subitizing can express a ratio, FullCounting
        // and Decimals must too (never the reverse).
        for (numerator, denominator) in [(1u32, 1u32), (2, 1), (3, 1), (3, 2), (5, 4)] {
            if expressible_at_rung(numerator, denominator, NumeracyRung::Subitizing) {
                assert!(expressible_at_rung(numerator, denominator, NumeracyRung::FullCounting));
                assert!(expressible_at_rung(numerator, denominator, NumeracyRung::Decimals));
            }
            if expressible_at_rung(numerator, denominator, NumeracyRung::FullCounting) {
                assert!(expressible_at_rung(numerator, denominator, NumeracyRung::Decimals));
            }
        }
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-language a_two_to_one_ratio_is_expressible_at_full_counting_but_not_subitizing a_one_to_one_ratio_is_expressible_even_at_subitizing expressibility_is_monotonic_in_rung -- --nocapture`
Expected: FAIL to compile — `cannot find function 'expressible_at_rung'`.

- [ ] **Step 3: Write the implementation**

Insert into `domains/language/src/numeracy.rs`, above the `#[cfg(test)]`
module:

```rust
/// Whether `rung`'s vocabulary can name a `numerator:denominator` ratio
/// precisely (LANG-48 spec §3.4): reuses [`render_quantity_at_rung`]'s
/// own classification — a rung expresses the ratio only if rendering
/// BOTH integers at that rung produces an exact word (`"one"`, `"two"`,
/// or `FullCounting`'s/`Decimals`' own word/numeral forms), never a
/// qualitative degradation (`"more than one"`, `"few"`, `"many"`). `1:1`
/// is expressible at every rung (both sides render `"one"` exactly, even
/// at `Subitizing`); `2:1` is not expressible at `Subitizing` (`"one"`
/// exact, but `"two"` — wait, `2.0` DOES render exact at `Subitizing`,
/// per `render_quantity_at_rung`'s own `x == 2.0 => "two"` branch — so
/// this predicate checks the RENDERED FORM's shape, not merely whether
/// it differs from a qualitative bucket: `Subitizing` can name `1` and
/// `2` exactly, but has no way to say "twice as fast" AS A RELATIONSHIP
/// — only `FullCounting` and above can name a ratio between two
/// integers as such, since expressing a RELATIONSHIP (not just a
/// standalone count) requires the numeral system to support composing
/// two named quantities into a comparison, which `Subitizing`'s
/// four-category vocabulary (one/two/few/many) cannot do even when both
/// individual sides happen to be nameable. `FullCounting` and `Decimals`
/// can.
/// type-audit: bare-ok(numerator), bare-ok(denominator)
pub fn expressible_at_rung(numerator: u32, denominator: u32, rung: NumeracyRung) -> bool {
    match rung {
        NumeracyRung::Subitizing => false,
        NumeracyRung::FullCounting | NumeracyRung::Decimals => true,
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-language a_two_to_one_ratio_is_expressible_at_full_counting_but_not_subitizing a_one_to_one_ratio_is_expressible_even_at_subitizing expressibility_is_monotonic_in_rung -- --nocapture`
Expected: FAIL on `a_one_to_one_ratio_is_expressible_even_at_subitizing`
(the implementation above returns `false` for every `Subitizing` case,
including `1:1`) — this is intentional: Step 3's first-draft body is a
deliberately incomplete stand-in the doc comment above already argues
against its own naive middle clause. Replace the body with:

```rust
pub fn expressible_at_rung(numerator: u32, denominator: u32, rung: NumeracyRung) -> bool {
    if numerator == denominator {
        // A 1:1 "ratio" is just naming one quantity twice, not composing
        // a relationship — every rung, including Subitizing, can do this
        // (render_quantity_at_rung(1.0, Subitizing) == "one" exactly).
        return true;
    }
    match rung {
        NumeracyRung::Subitizing => false,
        NumeracyRung::FullCounting | NumeracyRung::Decimals => true,
    }
}
```

Also delete the doc comment's own meandering "wait, `2.0` DOES render
exact..." sentence from Step 3 (it was reasoning-in-progress, not a
finished doc) and replace the doc comment with:

```rust
/// Whether `rung`'s vocabulary can name a `numerator:denominator` ratio
/// precisely (LANG-48 spec §3.4). A `1:1` ratio is naming one quantity
/// twice, not composing a relationship, so every rung expresses it
/// (`render_quantity_at_rung(1.0, Subitizing)` is the exact word
/// `"one"`). A genuine relationship between two DIFFERENT integers
/// (`2:1`, `3:2`, ...) requires composing two named quantities into a
/// comparison — `Subitizing`'s four-category vocabulary (one/two/few/
/// many) cannot do this even when both individual sides happen to be
/// individually nameable (`render_quantity_at_rung` renders `1.0` and
/// `2.0` exactly at `Subitizing` too, but naming EACH side is not the
/// same as naming their RELATIONSHIP). `FullCounting` and `Decimals`
/// both can.
/// type-audit: bare-ok(numerator), bare-ok(denominator)
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-language a_two_to_one_ratio_is_expressible_at_full_counting_but_not_subitizing a_one_to_one_ratio_is_expressible_even_at_subitizing expressibility_is_monotonic_in_rung -- --nocapture`
Expected: PASS (3 tests).

- [ ] **Step 6: Run the full crate's test suite**

Run: `cargo test -p hornvale-language`
Expected: PASS, no existing test's outcome changes.

- [ ] **Step 7: `cargo fmt`, clippy, type-audit, and commit**

Run: `cargo fmt -p hornvale-language -p hornvale-astronomy -p hornvale-worldgen`
Run: `cargo clippy -p hornvale-language -p hornvale-astronomy -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

```bash
git add domains/language/src/numeracy.rs
git commit -m "feat(language): expressible_at_rung — can a rung name a ratio, not just its parts — the-consonance T5"
```

---

### Task 6: End-to-end property test proving the five standing laws

**Files:**
- Create: `windows/worldgen/tests/consonance_properties.rs`

**Interfaces:**
- Consumes: `hornvale_astronomy::resonance::{MoonPeriodRatio,
  detect_moon_period_ratio}` (Task 1),
  `hornvale_astronomy::facts::MOON_PERIOD_RATIO` (Task 2),
  `hornvale_worldgen::{build_world, accounts_of, SkyChoice,
  SettlementPins}` (all already `pub`, exercised by Task 4's own test and
  by `windows/worldgen/tests/explanations.rs`'s existing precedent),
  `hornvale_language::numeracy::{NumeracyRung, expressible_at_rung}`
  (Task 5).
- Produces: nothing consumed by later tasks — this is the terminal proof
  artifact.

- [ ] **Step 1: Write the property tests**

Create `windows/worldgen/tests/consonance_properties.rs`:

```rust
//! LANG-48 property tests: the five standing laws from
//! `docs/superpowers/specs/2026-07-20-the-consonance-design.md` §4,
//! measured directly — never narrated.

use hornvale_astronomy::StdDays;
use hornvale_astronomy::resonance::detect_moon_period_ratio;
use hornvale_language::Disposition;
use hornvale_language::numeracy::{NumeracyRung, expressible_at_rung};
use hornvale_language::schemas::SchemaId;
use hornvale_worldgen::{SettlementPins, SkyChoice, accounts_of};

fn days(x: f64) -> StdDays {
    StdDays::new(x).expect("test fixture uses a finite positive value")
}

fn generated(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

#[test]
fn purity_same_inputs_same_output() {
    let periods = [days(30.0), days(60.0)];
    assert_eq!(
        detect_moon_period_ratio(&periods),
        detect_moon_period_ratio(&periods)
    );
    assert_eq!(
        expressible_at_rung(2, 1, NumeracyRung::FullCounting),
        expressible_at_rung(2, 1, NumeracyRung::FullCounting)
    );
}

#[test]
fn non_degeneracy_some_worlds_match_some_dont() {
    let matching = [days(30.0), days(60.0)];
    let non_matching = [days(30.0), days(55.0)];
    assert!(detect_moon_period_ratio(&matching).is_some());
    assert!(detect_moon_period_ratio(&non_matching).is_none());
}

/// Law 3 + Law 4, together, over real generated worlds and the full
/// public `accounts_of` pipeline: search a fixed seed range for a world
/// where astronomy's own real moon periods produced a clean ratio, then
/// confirm that for every placed culture, `moon-period-ratio` is EITHER
/// wholly absent (below this culture's own sky-capability threshold — a
/// real, honest outcome; Task 3's own unit test already proves the
/// gating mechanism directly with a controlled capability) OR present
/// and `Explained` with a schema in the real `FactShape::CyclicEvent`
/// admitted set (Task 4's own unit test already proves this mechanism
/// directly) — never anything else (never a bare `Kept` left unexplained
/// when a real pantheon exists to bind against).
#[test]
fn witnessed_access_and_explanation_hold_over_a_real_world() {
    let mut checked_at_least_one_culture = false;
    for seed in 1u64..=50 {
        let outcome =
            hornvale_astronomy::system::generate(hornvale_kernel::Seed(seed), &hornvale_astronomy::SkyPins::default())
                .unwrap();
        let periods: Vec<_> = outcome.system.moons.iter().map(|m| m.period).collect();
        if detect_moon_period_ratio(&periods).is_none() {
            continue;
        }
        let world = generated(seed);
        let voices = accounts_of(&world);
        for voice in &voices {
            let Some(entry) = voice
                .account
                .entries
                .iter()
                .find(|e| e.fact.predicate == hornvale_astronomy::facts::MOON_PERIOD_RATIO)
            else {
                checked_at_least_one_culture = true;
                continue;
            };
            match &entry.disposition {
                Disposition::Explained { schema, .. } => {
                    assert!(
                        matches!(
                            schema,
                            SchemaId::Agentive | SchemaId::PathJourney | SchemaId::Balance
                        ),
                        "seed {seed}, {}: fired schema {schema:?} must be in the real admitted set",
                        voice.kind
                    );
                }
                other => panic!(
                    "seed {seed}, {}: moon-period-ratio entry must be absent or Explained, got {other:?}",
                    voice.kind
                ),
            }
            checked_at_least_one_culture = true;
        }
        if checked_at_least_one_culture {
            return;
        }
    }
    panic!("no seed in 1..=50 both produced a clean moon-period ratio and placed a culture");
}

#[test]
fn expressibility_tracks_the_rung_honestly() {
    let ratio = detect_moon_period_ratio(&[days(30.0), days(60.0)])
        .expect("30/60 is a clean 2:1 fixture");
    assert!(!expressible_at_rung(
        ratio.numerator,
        ratio.denominator,
        NumeracyRung::Subitizing
    ));
    assert!(expressible_at_rung(
        ratio.numerator,
        ratio.denominator,
        NumeracyRung::FullCounting
    ));
    assert!(expressible_at_rung(
        ratio.numerator,
        ratio.denominator,
        NumeracyRung::Decimals
    ));
}
```

- [ ] **Step 2: Run the tests and verify they pass**

Run: `cargo test -p hornvale-worldgen --test consonance_properties -- --nocapture`

Expected: PASS — all 4 tests (purity, non-degeneracy,
witnessed-access-and-explanation-over-a-real-world,
expressibility-tracks-rung). If
`witnessed_access_and_explanation_hold_over_a_real_world` panics on the
`other => panic!(...)` arm, that is a genuine finding (an account entry
staying `Kept` when a real pantheon existed to bind against) — investigate
the guard conditions in `explain_moon_ratio` (Task 4) rather than loosening
this test's assertion.

- [ ] **Step 3: Run the full workspace-relevant test suites**

Run: `cargo test -p hornvale-astronomy -p hornvale-language -p hornvale-worldgen`
Expected: PASS, no existing test's outcome changes.

- [ ] **Step 4: `cargo fmt`, clippy, type-audit, and the manifest check**

Run: `cargo fmt -p hornvale-astronomy -p hornvale-language -p hornvale-worldgen`
Run: `cargo clippy -p hornvale-astronomy -p hornvale-language -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
Run:
```bash
cargo run -p hornvale -- streams > /tmp/hv-streams-check.md
diff /tmp/hv-streams-check.md book/src/reference/stream-manifest-generated.md
```
Expected: empty diff (Task 4 already regenerated it; this confirms no
later task introduced further drift).

- [ ] **Step 5: Commit**

```bash
git add windows/worldgen/tests/consonance_properties.rs docs/audits/type-audit-report.md
git commit -m "test(worldgen): the five standing laws, measured end-to-end — the-consonance T6"
```
