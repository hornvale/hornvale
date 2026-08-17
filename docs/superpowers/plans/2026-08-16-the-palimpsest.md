# The Palimpsest Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make claim distortion accumulate across many retellings, in units of
the teller's own people's generation length, so two peoples remember the same
event at different resolutions without ever contacting each other.

**Architecture:** A retelling's damage is a continuous *width* rather than a
whole rung. Per step the width grows by the generational span of that step —
the founding gap between teller and hearer, divided by the teller's people's
allometric generation length. The reported `Precision` is resolved from that
width at emit, against a ladder extended with two derived social rungs
(generation, lifespan) that vary per people. Three accumulation rules are
implemented side by side and all three are reported.

**Tech Stack:** Rust 2024, `windows/hearsay`, `windows/worldgen`,
`windows/lab`. No new external dependencies (decision 0004 allows only
`serde`, `serde_json`, `libm`).

**Spec:** `docs/superpowers/specs/2026-08-16-the-palimpsest-design.md` — read
it alongside this plan; every task argues from it.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` `disallowed-types`. Float sorting uses `total_cmp`.
- **No wall-clock time.** `Instant` is banned in tests too.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant needs a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag**
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`), and the
  committed report is regenerated **in the same commit** that changes a
  boundary.
- **`cargo fmt` is the final step before every commit.** Run `make quick`
  before committing; the pre-commit hook runs the full `make gate-commit`.
- **Campaign 2's tests keep their meaning.** `claims_about`, `stance`, and the
  maximum-antichain test are not to be modified. If a change would force one
  to change, stop and report rather than editing it.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A window may
  depend on many domains and on other windows, never upward.
  **`windows/hearsay`'s library must NOT depend on `windows/worldgen`** — its
  own `Cargo.toml` states this and worldgen is dev-only there.
- **Heavy-tier `#[ignore]` reasons must match the existing constant EXACTLY:**
  `"heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"`.
  `cli/tests/heavy_tier.rs` asserts equality against one constant, and
  `windows/lab/tests/preregistration_guard.rs` pattern-matches a substring of
  it. Changing the wording breaks tests in another crate that names it nowhere.

---

## Plan-level refinement of the spec, stated up front

Spec §4 says `PrecisionLadder::of` "gains a parameter". **Do not do that.**
There are 11 existing `PrecisionLadder::of` call sites in `windows/hearsay`'s
own tests (verified by grep), and campaign 2's tests must keep their meaning
per the Global Constraints. Instead **add a sibling constructor** and leave
`of` untouched. Task 2 does this.

Second refinement: because generation length varies per people, there is not
one ladder per world but **one ladder per people**. Task 2 introduces
`PeopleLadders` for that.

---

## File Structure

- `windows/hearsay/src/durations.rs` — **create.** `PeopleDurations`: the
  per-people generation and lifespan spans, as plain data. Knows nothing about
  where they came from.
- `windows/hearsay/src/ladder.rs` — **modify.** Add `with_social`, and
  `PeopleLadders`.
- `windows/hearsay/src/amplitude.rs` — **create.** `gen_span`, the per-step
  generational span. One function, no state.
- `windows/hearsay/src/accumulate.rs` — **create.** `Accumulation` (the three
  rules) and `Width`.
- `windows/hearsay/src/derive.rs` — **modify.** `variants_about_accumulating`,
  a sibling of `variants_about` which is left untouched.
- `windows/worldgen/src/lib.rs` — **modify.** Re-export `descent` so the
  durations can be assembled outside the crate.
- `windows/lab/src/…` — **modify.** Register the readout metrics.
- `studies/the-palimpsest.study.json` — **create.** The readout study.

Each file has one responsibility and can be tested alone. `durations.rs` is
deliberately dumb data so `hearsay` never needs the composition root.

---

### Task 1: `PeopleDurations`, and make the durations reachable

**Files:**
- Create: `windows/hearsay/src/durations.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod durations;`)
- Modify: `windows/worldgen/src/lib.rs` (re-export `descent`)
- Test: `windows/hearsay/tests/durations.rs`

**Interfaces:**
- Produces: `PeopleDurations::default()`, `PeopleDurations::insert(&mut self,
  people: &str, generation: Option<StdDays>, lifespan: Option<StdDays>)`,
  `PeopleDurations::get(&self, people: &str) -> (Option<StdDays>,
  Option<StdDays>)`.

**The visibility decision the spec deferred to this plan.** `descent` is a
private module in `windows/worldgen` (verified: `error[E0603]: module descent
is private`). `windows/lab` builds the study and needs the durations, and lab
may depend on worldgen. So: **re-export the module**, do not move the
function.

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/durations.rs`:

```rust
//! `PeopleDurations` is plain data: it never looks anything up.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::durations::PeopleDurations;

#[test]
fn a_people_with_no_entry_reports_neither_duration() {
    let d = PeopleDurations::default();
    assert_eq!(d.get("gnoll"), (None, None));
}

#[test]
fn inserted_durations_come_back_out() {
    let mut d = PeopleDurations::default();
    let gen = StdDays::new(11362.0).expect("positive");
    let life = StdDays::new(25399.0).expect("positive");
    d.insert("human", Some(gen), Some(life));
    let (g, l) = d.get("human");
    assert_eq!(g.map(|s| s.get()), Some(11362.0));
    assert_eq!(l.map(|s| s.get()), Some(25399.0));
}

#[test]
fn a_people_may_carry_a_generation_but_no_lifespan() {
    let mut d = PeopleDurations::default();
    d.insert("construct", Some(StdDays::new(100.0).expect("positive")), None);
    let (g, l) = d.get("construct");
    assert!(g.is_some());
    assert!(l.is_none());
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test durations`
Expected: FAIL to compile — `durations` module does not exist.

- [ ] **Step 3: Write the implementation**

Create `windows/hearsay/src/durations.rs`:

```rust
//! Per-people social durations, as plain data.
//!
//! This type deliberately performs NO lookup. The allometric derivation lives
//! at the composition root (`windows/worldgen`), which `windows/hearsay`'s
//! library may not depend on — see this crate's `Cargo.toml`. The caller
//! assembles the table and hands it in; the window reads only what it is
//! given, which is the same posture every other window takes toward the
//! ledger.

use hornvale_astronomy::units::StdDays;
use std::collections::BTreeMap;

/// Each people's generation length and lifespan, in std days.
#[derive(Clone, Debug, Default)]
pub struct PeopleDurations {
    /// people label -> (generation, lifespan); either may be absent.
    by_people: BTreeMap<String, (Option<StdDays>, Option<StdDays>)>,
}

impl PeopleDurations {
    /// Record one people's durations, replacing any earlier entry.
    /// type-audit: bare-ok(identifier-text: people)
    pub fn insert(&mut self, people: &str, generation: Option<StdDays>, lifespan: Option<StdDays>) {
        self.by_people
            .insert(people.to_string(), (generation, lifespan));
    }

    /// This people's `(generation, lifespan)`, both `None` when unknown.
    ///
    /// An absent people is not an error: a world may carry a kind with no
    /// mass-derived life history at all, exactly as a moonless world carries
    /// no lunar rung.
    /// type-audit: bare-ok(identifier-text: people)
    pub fn get(&self, people: &str) -> (Option<StdDays>, Option<StdDays>) {
        self.by_people.get(people).copied().unwrap_or((None, None))
    }

    /// Every people with an entry, ascending — the readout iterates this.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn peoples(&self) -> Vec<&str> {
        self.by_people.keys().map(|s| s.as_str()).collect()
    }
}
```

Add to `windows/hearsay/src/lib.rs`, beside the existing `pub mod` lines:

```rust
pub mod durations;
```

- [ ] **Step 4: Run the test and verify it passes**

Run: `cargo test -p hornvale-hearsay --test durations`
Expected: PASS, 3 tests.

- [ ] **Step 5: Re-export `descent` from worldgen**

In `windows/worldgen/src/lib.rs`, find the existing `mod descent;` or
`pub(crate) mod descent;` declaration and make it public:

```rust
pub mod descent;
```

Verify it is now reachable:

Run: `cargo build -p hornvale-worldgen`
Then confirm the path resolves by running the existing probe, which currently
copies the function body:

Run: `cargo test -p hornvale-hearsay --test probe_teller_relations does_generation -- --ignored`
Expected: PASS.

- [ ] **Step 6: Run the cheap gate and commit**

```bash
cargo fmt
make quick
git add windows/hearsay/src/durations.rs windows/hearsay/src/lib.rs windows/hearsay/tests/durations.rs windows/worldgen/src/lib.rs
git commit -m "feat(palimpsest): PeopleDurations, and make worldgen::descent reachable"
```

**Decision rule for the commit:** if `make quick` reddens on the layering
golden (`book/src/reference/layering-generated.md`), the dependency graph
moved — run `make rebaseline-goldens`, review that the diff is ONLY the
layering table, and `git add` it into this same commit. If any other generated
path moved, STOP and report; that is not expected from this task.

---

### Task 2: Social rungs and per-people ladders

**Files:**
- Modify: `windows/hearsay/src/ladder.rs`
- Test: `windows/hearsay/tests/ladder.rs` (append; do not edit existing tests)

**Interfaces:**
- Consumes: `PeopleDurations` from Task 1.
- Produces: `PrecisionLadder::with_social(ledger: &Ledger, generation:
  Option<StdDays>, lifespan: Option<StdDays>) -> PrecisionLadder`;
  `PeopleLadders::of(ledger: &Ledger, durations: &PeopleDurations) ->
  PeopleLadders`; `PeopleLadders::for_people(&self, people: &str) ->
  &PrecisionLadder`.

`PrecisionLadder::of` is **not** modified. Its 11 existing call sites and
their assertions stay exactly as they are.

- [ ] **Step 1: Write the failing test**

Append to `windows/hearsay/tests/ladder.rs`:

```rust
#[test]
fn social_rungs_append_above_the_astronomical_ones() {
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let gen = StdDays::new(11362.0).expect("positive");
    let life = StdDays::new(25399.0).expect("positive");
    let l = PrecisionLadder::with_social(&led, Some(gen), Some(life));
    assert_eq!(l.labels(), vec!["day", "moon 1", "year", "generation", "lifespan"]);
}

#[test]
fn a_people_with_no_life_history_gets_the_astronomical_ladder_only() {
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let bare = PrecisionLadder::of(&led);
    let l = PrecisionLadder::with_social(&led, None, None);
    assert_eq!(l.labels(), bare.labels());
}

#[test]
fn social_rungs_sort_by_span_like_every_other_rung() {
    // A generation SHORTER than the year must sort below it. Nothing about
    // a rung's origin gives it a fixed position.
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let gen = StdDays::new(100.0).expect("positive");
    let l = PrecisionLadder::with_social(&led, Some(gen), None);
    assert_eq!(l.labels(), vec!["day", "moon 1", "generation", "year"]);
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test ladder`
Expected: FAIL to compile — `with_social` not found.

- [ ] **Step 3: Implement `with_social`**

In `windows/hearsay/src/ladder.rs`, refactor `of`'s body into a private
`rungs_of(ledger) -> Vec<Rung>` that returns the rungs **unsorted**, then:

```rust
    /// This world's ladder as [`PrecisionLadder::of`] builds it, plus the two
    /// SOCIAL rungs a people carries: its generation length and its lifespan.
    ///
    /// Both are optional and both sort by actual span alongside the
    /// astronomical rungs — a generation shorter than this world's year sits
    /// BELOW the year, because a rung's position is its length and never its
    /// origin. A people with no mass-derived life history yields exactly the
    /// astronomical ladder, the same way a moonless world yields no lunar
    /// rung.
    ///
    /// Campaign 2's rungs were read from committed astronomy alone; these are
    /// read from committed allometry. Both are DERIVED from the world, which
    /// is the principle — an authored "century" rung would not be.
    pub fn with_social(
        ledger: &Ledger,
        generation: Option<StdDays>,
        lifespan: Option<StdDays>,
    ) -> PrecisionLadder {
        let mut rungs = Self::rungs_of(ledger);
        if let Some(span) = generation {
            rungs.push(Rung { label: "generation".to_string(), span });
        }
        if let Some(span) = lifespan {
            rungs.push(Rung { label: "lifespan".to_string(), span });
        }
        Self::finish(rungs)
    }
```

`finish` is the existing sort-ascending-by-span-and-dedup step, extracted from
`of` so both constructors share it. `of` becomes
`Self::finish(Self::rungs_of(ledger))`.

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-hearsay --test ladder`
Expected: PASS — the three new tests **and** every pre-existing test in that
file. If a pre-existing test fails, the extraction changed behaviour: revert
and redo it, do not amend the old test.

- [ ] **Step 5: Add `PeopleLadders`**

Append to `windows/hearsay/src/ladder.rs`:

```rust
/// One ladder per people, because generation length varies by people.
///
/// Built once per world. `for_people` falls back to the astronomical-only
/// ladder for a people with no entry, so a lookup never fails and a world
/// with no life history at all behaves exactly as campaign 2's did.
#[derive(Clone, Debug)]
pub struct PeopleLadders {
    by_people: BTreeMap<String, PrecisionLadder>,
    fallback: PrecisionLadder,
}

impl PeopleLadders {
    /// Build a ladder for every people named in `durations`.
    pub fn of(ledger: &Ledger, durations: &crate::durations::PeopleDurations) -> PeopleLadders {
        let mut by_people = BTreeMap::new();
        for people in durations.peoples() {
            let (generation, lifespan) = durations.get(people);
            by_people.insert(
                people.to_string(),
                PrecisionLadder::with_social(ledger, generation, lifespan),
            );
        }
        PeopleLadders { by_people, fallback: PrecisionLadder::of(ledger) }
    }

    /// This people's ladder, or the astronomical-only fallback.
    /// type-audit: bare-ok(identifier-text: people)
    pub fn for_people(&self, people: &str) -> &PrecisionLadder {
        self.by_people.get(people).unwrap_or(&self.fallback)
    }
}
```

Add `use std::collections::BTreeMap;` to the file's imports if absent.

- [ ] **Step 6: Test `PeopleLadders` and commit**

Append to `windows/hearsay/tests/ladder.rs`:

```rust
#[test]
fn an_unknown_people_falls_back_to_the_astronomical_ladder() {
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert("human", Some(StdDays::new(11362.0).expect("positive")), None);
    let ls = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);
    assert_eq!(ls.for_people("gnoll").labels(), PrecisionLadder::of(&led).labels());
    assert!(ls.for_people("human").labels().contains(&"generation"));
}
```

Run: `cargo test -p hornvale-hearsay --test ladder`
Expected: PASS.

```bash
cargo fmt && make quick
git add windows/hearsay/src/ladder.rs windows/hearsay/tests/ladder.rs
git commit -m "feat(palimpsest): social rungs and per-people ladders"
```

---

### Task 3: The generational amplitude

**Files:**
- Create: `windows/hearsay/src/amplitude.rs`
- Modify: `windows/hearsay/src/lib.rs`
- Test: `windows/hearsay/tests/amplitude.rs`

**Interfaces:**
- Produces: `gen_span(ledger: &Ledger, durations: &PeopleDurations, teller:
  EntityId, hearer: EntityId) -> f64` — the number of the TELLER's generations
  the step spans; `0.0` when it cannot be derived.

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/amplitude.rs`:

```rust
//! The amplitude is the generational span of ONE retelling.

mod common;

use common::eid;
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_kernel::ledger::{Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// A ledger where each entry is (occupation, people, founded-day).
fn founded(rows: &[(u64, &str, f64)]) -> Ledger {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_history::OCC_PEOPLE, true, "people")
        .expect("register");
    reg.register_predicate(hornvale_history::OCC_FOUNDED, true, "founded")
        .expect("register");
    let mut led = Ledger::default();
    for (occ, people, day) in rows {
        for (pred, object) in [
            (hornvale_history::OCC_PEOPLE, Value::Text(people.to_string())),
            (hornvale_history::OCC_FOUNDED, Value::Number(*day)),
        ] {
            led.commit(
                Fact {
                    subject: eid(*occ),
                    predicate: pred.to_string(),
                    object,
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &reg,
            )
            .expect("commit");
        }
    }
    led
}

fn durations(people: &str, generation_days: f64) -> PeopleDurations {
    let mut d = PeopleDurations::default();
    d.insert(people, Some(StdDays::new(generation_days).expect("positive")), None);
    d
}

#[test]
fn one_generation_of_gap_is_an_amplitude_of_one() {
    let led = founded(&[(1, "human", 0.0), (2, "human", 100.0)]);
    let d = durations("human", 100.0);
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 1.0);
}

#[test]
fn the_same_gap_is_fewer_generations_for_a_longer_lived_people() {
    let led = founded(&[(1, "elf", 0.0), (2, "elf", 100.0)]);
    let d = durations("elf", 400.0);
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 0.25);
}

#[test]
fn the_amplitude_is_unsigned() {
    let led = founded(&[(1, "human", 100.0), (2, "human", 0.0)]);
    let d = durations("human", 100.0);
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 1.0);
}

#[test]
fn a_people_with_no_generation_length_yields_zero_rather_than_infinity() {
    let led = founded(&[(1, "construct", 0.0), (2, "construct", 100.0)]);
    let d = PeopleDurations::default();
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 0.0);
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test amplitude`
Expected: FAIL to compile — `amplitude` module does not exist.

- [ ] **Step 3: Write the implementation**

Create `windows/hearsay/src/amplitude.rs`:

```rust
//! How many of the teller's own generations one retelling spans.
//!
//! A story handed down across three generations blurs more than one handed
//! across half a generation. The unit is the TELLER's people's generation
//! length, which varies 6.75x across seed 42's peoples — so the same gap in
//! days is a different amplitude for an elven lineage than a gnoll one, and
//! two peoples end up remembering the same event at different resolutions
//! with no contact between them.
//!
//! The teller's people rather than the hearer's is a free choice: fission
//! never crosses a people boundary (campaign 2 §3.1, zero of 658 typed edges
//! and zero of 780 after main moved), so they are always the same.

use crate::durations::PeopleDurations;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};

fn number(led: &Ledger, occ: EntityId, pred: &str) -> Option<f64> {
    match led.value_of(occ, pred) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// The generational span of the retelling `teller -> hearer`.
///
/// Zero — not infinity, not a panic — when the founding days or the teller's
/// generation length cannot be derived. Total by construction, the same
/// posture `hornvale_history::descent::remove` takes toward a non-positive
/// generation length.
/// type-audit: bare-ok(count: return)
pub fn gen_span(
    led: &Ledger,
    durations: &PeopleDurations,
    teller: EntityId,
    hearer: EntityId,
) -> f64 {
    let Some(Value::Text(people)) = led.value_of(teller, hornvale_history::OCC_PEOPLE) else {
        return 0.0;
    };
    let (Some(generation), _) = durations.get(people) else {
        return 0.0;
    };
    let g = generation.get();
    if !g.is_finite() || g <= 0.0 {
        return 0.0;
    }
    let (Some(ft), Some(fh)) = (
        number(led, teller, hornvale_history::OCC_FOUNDED),
        number(led, hearer, hornvale_history::OCC_FOUNDED),
    ) else {
        return 0.0;
    };
    (fh - ft).abs() / g
}
```

Add `pub mod amplitude;` to `windows/hearsay/src/lib.rs`.

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-hearsay --test amplitude`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add windows/hearsay/src/amplitude.rs windows/hearsay/src/lib.rs windows/hearsay/tests/amplitude.rs
git commit -m "feat(palimpsest): the generational amplitude"
```

---

### Task 4: The three accumulation rules, and precision at emit

**Files:**
- Create: `windows/hearsay/src/accumulate.rs`
- Modify: `windows/hearsay/src/lib.rs`
- Test: `windows/hearsay/tests/accumulate.rs`

**Interfaces:**
- Consumes: `PrecisionLadder` from Task 2.
- Produces: `Accumulation::{Additive, Quadrature, Multiplicative}`;
  `Accumulation::ALL: [Accumulation; 3]`; `Accumulation::label(self) -> &str`;
  `Accumulation::step(self, width: f64, span: f64) -> f64`;
  `precision_at(ladder: &PrecisionLadder, width: f64) -> Precision`.

**Spec §6.4 is binding here: none of the three is primary.** Implement all
three; do not add a `Default` impl that silently nominates one.

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/accumulate.rs`:

```rust
//! Three co-equal accumulation rules, and emit-time rung resolution.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::Ledger;

#[test]
fn every_rule_leaves_a_zero_span_step_unchanged() {
    for rule in Accumulation::ALL {
        assert_eq!(rule.step(5.0, 0.0), 5.0, "{}", rule.label());
    }
}

#[test]
fn every_rule_is_non_decreasing() {
    for rule in Accumulation::ALL {
        let after = rule.step(5.0, 2.0);
        assert!(after >= 5.0, "{} decreased: {after}", rule.label());
    }
}

#[test]
fn the_three_rules_are_actually_different() {
    let (w, s) = (3.0, 4.0);
    let vals: Vec<f64> = Accumulation::ALL.iter().map(|r| r.step(w, s)).collect();
    assert_eq!(vals[0], 7.0); // additive
    assert_eq!(vals[1], 5.0); // quadrature: sqrt(9+16)
    assert_eq!(vals[2], 15.0); // multiplicative: 3 * (1+4)
}

#[test]
fn an_empty_ladder_always_reports_the_finest_precision() {
    let l = PrecisionLadder::of(&Ledger::default());
    assert_eq!(precision_at(&l, 999.0), Precision::FINEST);
}

#[test]
fn precision_is_the_coarsest_rung_the_width_reaches() {
    let l = PrecisionLadder::with_social(
        &Ledger::default(),
        Some(StdDays::new(100.0).expect("positive")),
        Some(StdDays::new(1000.0).expect("positive")),
    );
    // Ladder is [generation(100), lifespan(1000)]. A width lands on the
    // COARSEST rung whose span it reaches, so 150 is still the generation
    // rung — it has not reached 1000.
    assert_eq!(precision_at(&l, 0.0), Precision(0));
    assert_eq!(precision_at(&l, 99.0), Precision(0));
    assert_eq!(precision_at(&l, 100.0), Precision(0));
    assert_eq!(precision_at(&l, 150.0), Precision(0));
    assert_eq!(precision_at(&l, 1000.0), Precision(1));
    assert_eq!(precision_at(&l, 99999.0), Precision(1)); // saturates, never overruns
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test accumulate`
Expected: FAIL to compile — `accumulate` module does not exist.

- [ ] **Step 3: Write the implementation**

Create `windows/hearsay/src/accumulate.rs`:

```rust
//! How damage accumulates, and how a width becomes a rung.
//!
//! THREE RULES, NO PRIMARY. Spec §6.4: substrate for all three was measured
//! before the rule was chosen, so nominating one would be selection on data
//! already seen. All three are implemented, all three are reported, and
//! adopting one is a separate dated decision citing this campaign's numbers.
//! Deliberately no `Default` impl — a default would nominate one silently.

use crate::ladder::PrecisionLadder;
use hornvale_kernel::Precision;

/// How a retelling's damage combines with the damage already carried.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Accumulation {
    /// Damage sums. The naive baseline; assumes perfectly correlated errors.
    Additive,
    /// Damage sums in quadrature — the standard rule for INDEPENDENT
    /// contributions.
    Quadrature,
    /// Width grows by a factor, which is additive in log space and therefore
    /// the rule commensurate with an approximately geometric ladder.
    Multiplicative,
}

impl Accumulation {
    /// Every rule, in a fixed order so a readout's columns are stable.
    pub const ALL: [Accumulation; 3] = [
        Accumulation::Additive,
        Accumulation::Quadrature,
        Accumulation::Multiplicative,
    ];

    /// This rule's short name, used as a metric-name suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Accumulation::Additive => "additive",
            Accumulation::Quadrature => "quadrature",
            Accumulation::Multiplicative => "multiplicative",
        }
    }

    /// The width after one retelling of generational span `span`.
    ///
    /// Non-decreasing for every rule and every non-negative `span`, which is
    /// what preserves campaign 2's precision-rank monotonicity: width only
    /// grows, so the rung index only rises.
    /// type-audit: bare-ok(count: width), bare-ok(count: span), bare-ok(count: return)
    pub fn step(self, width: f64, span: f64) -> f64 {
        if !span.is_finite() || span <= 0.0 {
            return width;
        }
        match self {
            Accumulation::Additive => width + span,
            Accumulation::Quadrature => (width * width + span * span).sqrt(),
            Accumulation::Multiplicative => width * (1.0 + span),
        }
    }
}

/// The coarsest rung whose span does not exceed `width`.
///
/// This is the project's quantize-at-emit-only discipline applied to a
/// non-float quantity: the width is carried at full resolution and the rung
/// is resolved when the claim is READ. Saturates at the ladder's coarsest
/// rung rather than running off the end, and returns `FINEST` for an empty
/// ladder, matching `PrecisionLadder::coarser`'s posture.
/// type-audit: bare-ok(count: width)
pub fn precision_at(ladder: &PrecisionLadder, width: f64) -> Precision {
    if ladder.is_empty() {
        return Precision::FINEST;
    }
    let mut out = Precision::FINEST;
    for i in 0..ladder.len() {
        let p = Precision(i as u8);
        match ladder.span(p) {
            Some(span) if span.get() <= width => out = p,
            _ => break,
        }
    }
    out
}
```

Note `Multiplicative` needs a non-zero starting width to move at all; Task 5
starts the width at the finest rung's span for that reason.

Add `pub mod accumulate;` to `windows/hearsay/src/lib.rs`.

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-hearsay --test accumulate`
Expected: PASS, 5 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add windows/hearsay/src/accumulate.rs windows/hearsay/src/lib.rs windows/hearsay/tests/accumulate.rs
git commit -m "feat(palimpsest): three co-equal accumulation rules"
```

---

### Task 5: The accumulating derivation

**Files:**
- Modify: `windows/hearsay/src/derive.rs`
- Test: `windows/hearsay/tests/derive.rs` (append only)

**Interfaces:**
- Consumes: Tasks 1–4.
- Produces: `variants_about_accumulating(ledger, lineage, ladders, durations,
  rule, subject, predicate) -> Vec<Claim>`.

**`variants_about` is NOT modified.** Campaign 2's tests pin it. This is a
sibling, exactly as `variants_about` was a sibling of `claims_about`.

- [ ] **Step 1: Write the failing test**

Append to `windows/hearsay/tests/derive.rs`:

```rust
#[test]
fn accumulating_distortion_can_pass_more_than_one_rung() {
    // A chain deep enough that campaign 2's boundary model would stop at one
    // rung. The property under test is that the accumulating model does not.
    let led = chain_with_foundings();
    let lin = lineage_of(&led);
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert("human", Some(StdDays::new(50.0).expect("positive")), Some(StdDays::new(500.0).expect("positive")));
    let ladders = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);

    let out = hornvale_hearsay::derive::variants_about_accumulating(
        &led, &lin, &ladders, &d,
        hornvale_hearsay::accumulate::Accumulation::Additive,
        eid(1), hornvale_history::OCC_ENDED,
    );
    let deepest = out.iter().map(|c| c.precision.rung()).max().expect("claims");
    assert!(deepest >= 2, "expected multi-rung accumulation, got {deepest}");
}

#[test]
fn every_rule_preserves_precision_rank_monotonicity_along_a_path() {
    // On a SINGLE chain, hop count orders the holders, so precision must be
    // non-decreasing in hops. Asserting `rung() >= 0` would be vacuous —
    // `rung()` returns u8 — and a vacuous guard is the exact failure campaign
    // 2's retrospective is about.
    let led = chain_with_foundings();
    let lin = lineage_of(&led);
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert("human", Some(StdDays::new(50.0).expect("positive")), None);
    let ladders = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);

    for rule in hornvale_hearsay::accumulate::Accumulation::ALL {
        let mut out = hornvale_hearsay::derive::variants_about_accumulating(
            &led, &lin, &ladders, &d, rule, eid(1), hornvale_history::OCC_ENDED,
        );
        out.sort_by_key(|c| c.hops);
        let mut seen = 0u8;
        for c in &out {
            assert!(
                c.precision.rung() >= seen,
                "{}: precision went FINER at hop {} ({} < {})",
                rule.label(),
                c.hops,
                c.precision.rung(),
                seen
            );
            seen = c.precision.rung();
        }
        // Positive control: the assertion above is only meaningful if the
        // precision actually moves. If nothing coarsens, this test proves
        // nothing about monotonicity.
        assert!(
            seen > 0,
            "{}: no coarsening happened at all — the test is vacuous",
            rule.label()
        );
    }
}
```

**Build `chain_with_foundings()` yourself** in `windows/hearsay/tests/common/mod.rs`:
a single descent chain of at least 6 occupations, each with `occ-people`
`"human"`, ascending `occ-founded` days, and an `occ-ended` on the root so the
predicate has a value. Do not copy a fixture from another test — the existing
`ledger_with` helper carries no founding days and will silently produce zero
amplitude everywhere.

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test derive accumulating`
Expected: FAIL to compile — `variants_about_accumulating` not found.

- [ ] **Step 3: Write the implementation**

Add to `windows/hearsay/src/derive.rs`, leaving `variants_about` untouched:

```rust
/// Campaign 3's accumulating sibling of [`variants_about`].
///
/// Each retelling widens a continuous damage width by the step's generational
/// span ([`crate::amplitude::gen_span`]) under `rule`, and the reported
/// [`Precision`] is resolved from that width at emit against the TELLER's
/// people's ladder. Where campaign 2 spends a whole rung per lossy step —
/// welding firing rate to firing depth — this separates them, so a frequent
/// small-amplitude step costs little and a rare large one costs a lot.
///
/// The width starts at the finest rung's span rather than zero, because
/// `Accumulation::Multiplicative` cannot move a zero width.
///
/// Multi-path holders keep the LEAST-corrupted telling, ordered by (1)
/// smallest final width, (2) fewest hops, (3) smallest witness `EntityId` —
/// the same shape as `variants_about`, with width replacing lossy-step count
/// because width is now the thing that varies.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn variants_about_accumulating(
    ledger: &Ledger,
    lineage: &Lineage,
    ladders: &crate::ladder::PeopleLadders,
    durations: &crate::durations::PeopleDurations,
    rule: crate::accumulate::Accumulation,
    subject: EntityId,
    predicate: &str,
) -> Vec<Claim> {
    let Some(object) = ledger.value_of(subject, predicate) else {
        return Vec::new();
    };
    let base = Claim {
        holder: subject,
        subject,
        predicate: predicate.to_string(),
        object: object.clone(),
        grade: Provenance::Witnessed,
        hops: 0,
        precision: Precision::FINEST,
    };
    let witnesses = witnesses_of(ledger, lineage, subject, predicate);
    let mut best: BTreeMap<EntityId, ((u64, u32, EntityId), Claim)> = BTreeMap::new();

    for w in &witnesses {
        let mut c = base.clone();
        c.holder = *w;
        best.insert(*w, ((0, 0, *w), c));
    }

    for w in &witnesses {
        for d in lineage.descendants_of(*w) {
            if witnesses.contains(&d) {
                continue;
            }
            let ancestry = lineage.ancestry(d);
            let Some(pos) = ancestry.iter().position(|a| a == w) else {
                continue;
            };
            let mut path: Vec<EntityId> = ancestry[..=pos].to_vec();
            path.reverse();

            let people = match ledger.value_of(*w, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => p.clone(),
                _ => String::new(),
            };
            let ladder = ladders.for_people(&people);
            let mut width = ladder
                .span(Precision::FINEST)
                .map(|s| s.get())
                .unwrap_or(0.0);

            let mut c = base.clone();
            c.holder = *w;
            for pair in path.windows(2) {
                let (teller, hearer) = (pair[0], pair[1]);
                width = rule.step(width, crate::amplitude::gen_span(ledger, durations, teller, hearer));
                let precision = crate::accumulate::precision_at(ladder, width);
                let object = match &c.object {
                    Value::Number(day) => Value::Number(ladder.apply(precision, *day)),
                    other => other.clone(),
                };
                c = c.retold_by_lossy(hearer, precision, object);
            }

            // f64 has no total order for a BTreeMap key; `to_bits` on a
            // non-negative finite width is monotone, so it orders correctly.
            let key = (width.to_bits(), c.hops, *w);
            match best.get(&d) {
                Some((best_key, _)) if *best_key <= key => {}
                _ => {
                    best.insert(d, (key, c));
                }
            }
        }
    }

    best.into_values().map(|(_, c)| c).collect()
}
```

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-hearsay --test derive`
Expected: PASS — the two new tests **and** every pre-existing test in the
file. A pre-existing failure means `variants_about` was touched; revert.

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add windows/hearsay/src/derive.rs windows/hearsay/tests/derive.rs windows/hearsay/tests/common/mod.rs
git commit -m "feat(palimpsest): the accumulating derivation"
```

---

### Task 6: The readout as a lab study

**Files:**
- Modify: `windows/lab/src/` — register metrics (read `windows/lab/CLAUDE.md`
  FIRST; it documents the registration mechanism and its traps)
- Create: `studies/the-palimpsest.study.json`
- Test: whatever `windows/lab`'s own conventions require for a new metric

**Interfaces:**
- Consumes: Tasks 1–5.
- Produces: 12 metrics, four per accumulation rule.

**Why a study and not a heavy test.** Decision 0011: studies are data, metrics
are code. The census panel is `{from: 0, count: 1000}` — a thousand worlds —
which is a study's job, not a test's.

**Read `windows/lab/CLAUDE.md` before starting.** It carries the metric
registration procedure and a documented trap; do not infer the mechanism from
neighbouring code.

- [ ] **Step 1: Read the lab guide and the existing hearsay metrics**

Run: `cargo run -p hornvale -- lab list-metrics | grep -i "hearsay\|claim\|variant"`
Note the naming convention actually in use and follow it.

- [ ] **Step 2: Register the four metric families**

For each `rule` in `Accumulation::ALL`, register:

- `palimpsest_rho_generation_precision_<rule>` — Spearman rho across held
  claims between the holder's people's generation length and the retained
  precision rung. **H1.**
- `palimpsest_distinct_rungs_<rule>` — the number of distinct retained rungs
  carrying at least one held claim. **H2.**
- `palimpsest_saturated_fraction_<rule>` — the fraction of held claims sitting
  at the ladder's coarsest rung. **The null detector**: spec §3.7 says
  saturation is a live outcome, and this is the metric that says so plainly.
- `palimpsest_rho_antichain_variants_<rule>` — Spearman rho between an
  ending's maximum-antichain width and its distinct variant count. **H3.**

Reuse the existing maximum-antichain implementation in
`windows/hearsay/src/divergence.rs`; do not write a second one.

- [ ] **Step 3: Write the study JSON**

Create `studies/the-palimpsest.study.json`, matching the field shape of
`studies/the-census.study.json` (`name`, `description`, `seeds`, `pin_sets`,
`metrics`).

**Decision rule for the seed panel, and it is a real decision:** start at
`{"from": 0, "count": 40}`. Forty worlds is the panel The Pyx used for a
cross-platform probe and is enough to see whether a direction is consistent.
**Do not set it to 1000 without measuring first** — nothing in this campaign
has measured the per-world cost of these metrics, and CLAUDE.md is explicit
that census cost must be read from `docs/timings.md` rather than assumed. If
a 40-world run is cheap, raising the count is a one-line change.

- [ ] **Step 4: Run the study and check it produces numbers**

Run: `cargo run -p hornvale -- lab run studies/the-palimpsest.study.json`
Expected: a CSV with 12 metric columns and 40 rows.

**Decision rule on the result:**
- All three rules give `saturated_fraction` near 1.0 → this is spec §3.7's
  predicted null. Record it; do NOT retune the amplitude to avoid it.
- `distinct_rungs` is 2 or fewer for every rule → H2 falsified. Record it.
- Any metric is constant across all 40 seeds → suspect a vacuous metric before
  believing the finding, and check it varies on a hand-built fixture.

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add windows/lab studies/the-palimpsest.study.json
git commit -m "feat(palimpsest): the readout study and its twelve metrics"
```

---

### Task 7: Type-audit tags and artifact regeneration

**Files:**
- Modify: any file from Tasks 1–6 with an untagged pub-boundary primitive
- Modify: `docs/audits/type-audit-report.md` (regenerated, never hand-edited)

- [ ] **Step 1: Find untagged boundaries**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: either clean, or a list of untagged pub-boundary primitives.

- [ ] **Step 2: Tag each one**

Add a `type-audit:` line to each flagged item's doc comment. The tags already
written into Tasks 1–5 cover the boundaries this plan introduces; anything
else the tool flags is a boundary this plan did not anticipate — tag it with
the class that fits, and if none fits, use `waiver(<reason>)` with a real
reason.

- [ ] **Step 3: Regenerate the committed report**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

- [ ] **Step 4: Regenerate every other drifted artifact**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Decision rule:** `docs/audits/` and `book/src/reference/layering-generated.md`
moving is expected. `book/src/domesday/` moving means a census CSV changed,
which this campaign has not done — STOP and report. Anything under
`clients/` moving is unexpected — STOP and report.

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
git commit -m "chore(palimpsest): regenerate type-audit report and layering golden"
```

---

### Task 8: Definition of Done — book, retrospective, registry

**Files:**
- Create: `book/src/chronicle/the-palimpsest.md`
- Create: `docs/retrospectives/the-palimpsest.md`
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `book/src/open-questions.md` (only if a bet moved)
- Modify: `book/src/SUMMARY.md`

- [ ] **Step 1: Write the chronicle entry**

`book/src/chronicle/the-palimpsest.md`. Technical and mathematical altitude,
comprehensible without reading the code. It must report the readout **as
measured**, including a null, and must state which figures were re-measured on
the merged tree — campaign 2's absorption moved seed 42 substantially and its
chronicle had to restate every published figure.

**Re-measure before publishing.** Run the study again after absorbing main and
use those numbers, not the ones from development.

- [ ] **Step 2: Add registry rows**

Add rows to `book/src/frontier/idea-registry.md` for:
- the sharpened ceiling statement (a descent-closed colouring cannot fire
  twice; the one non-closed class has cardinality one and is reachable only as
  a path origin);
- species-varying memory resolution as a divergence source needing no contact;
- the signed/directional amplitude (spec §7);
- non-monotone distortion being reachable, correcting campaign 2 §7's stated
  reason.

**Constraints, enforced by `cli/tests/docs_consistency.rs`:** exactly five
columns counting `\|` as an escape; Idea cell ≤ 600 characters; Status from
the closed vocabulary; non-empty **Where** cell; **no new numbered IDs** —
use slugs (decision 0026). Add each new `## ` section to the Contents ToC.

Run: `cargo test -p hornvale --test docs_consistency`

- [ ] **Step 3: Write the retrospective**

`docs/retrospectives/the-palimpsest.md`, one page, process lessons not
product. It must carry:
- three designs died to measurement, each measured after being proposed
  (spec §8);
- the positive control that caught the controller's own structural argument
  (predicted max 1 stance crossing, measured 2);
- the predicted second ceiling that did not exist;
- the worktree pool collision (board post `c1cc2209`);
- **the contaminated freeze and how it actually went** — this is the one
  campaign 2 explicitly asked about, and the honest answer belongs here.

- [ ] **Step 4: Freshness sweep**

Check every book chapter that describes claim transmission or precision for
staleness against what now ships. A chapter that says distortion is bounded at
one rung is now wrong.

Run: `mdbook build book`

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add book docs/retrospectives/the-palimpsest.md
git commit -m "docs(palimpsest): chronicle, retrospective, registry rows"
```

---

### Task 9: The four DoD residuals the other tasks do not cover

Found by checking this plan against spec §9. Each is small and each is easy to
forget, which is why they get their own task rather than a footnote.

- [ ] **Step 1: Decide the probe's fate, explicitly**

`windows/hearsay/tests/probe_teller_relations.rs` is a throwaway that now
carries three positive controls, one of which reproduces campaign 2's
published 3,237. Choose ONE and say why in the commit message:
- **Promote** — keep it as a permanent heavy battery.
- **Delete** — its findings are in spec §3 and the numbers are recorded there.

Do not leave it undecided; spec §9 requires an explicit call.

- [ ] **Step 2: Resolve the `hornvale-species` dev-dependency**

**This step comes after Step 1 deliberately** — whether the dependency can go
depends on whether the probe survives.

Task 1 makes `hearsay` take durations as data, so its **library** never needs
`hornvale-species`. Only the probe might.

Run: `cargo build -p hornvale-hearsay`, then remove the dev-dependency from
`windows/hearsay/Cargo.toml` and run `cargo test -p hornvale-hearsay --no-run`.

**Decision rule:** if everything still compiles, leave it removed. If the probe
survived Step 1 and needs it, keep it and add a comment naming the probe — an
unexplained dependency is worse than the dependency.

**Either way, the dependency graph moved**, so regenerate and stage the
layering golden in this task's commit:

```bash
make rebaseline-goldens
git diff --stat -- book/src/reference/layering-generated.md
```

- [ ] **Step 3: Check the sub-floor roster**

`hornvale-hearsay` currently has 48 entries in
`docs/timings/subfloor-roster.tsv`. New test targets from Tasks 1–5 must not
silently fall out of the commit gate the way that whole crate did for a
campaign.

Run: `cargo test -p hornvale --test subfloor_roster_coverage`

**Decision rule:** the roster is rewritten by a green chamber `gate` phase and
lands with the merge product — do NOT hand-edit it. If the coverage test
reddens for a crate with no entry, that is the guard working; report it rather
than editing the TSV.

- [ ] **Step 4: Decide the census refresh**

Task 6 registers 12 new metrics. CLAUDE.md: a census refresh at the pre-merge
close is required **if a metric is registered**.

**This is a carve-out — it costs the canonical box and it is Nathan's call.**
Do not dispatch it unprompted. When authorised, it runs on lefford only:

```bash
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'
```

Read the cost from `docs/timings.md` (`grep '| census |' docs/timings.md | tail`),
never from CLAUDE.md's prose — that file says explicitly that its own figures
are the one thing not to trust, and the last two projections were wrong by
2.2x and 6.7x in opposite directions.

- [ ] **Step 5: Commit**

```bash
cargo fmt && make quick
git add windows/hearsay/Cargo.toml book/src/reference/layering-generated.md
git commit -m "chore(palimpsest): resolve the dev-dependency, probe fate, and roster"
```

If Step 1 chose **delete**, also `git rm windows/hearsay/tests/probe_teller_relations.rs`
in this commit and say why in the message.

---

## Merge

Campaign branches absorb main at every plan-stage boundary. Submit
`make sluice-stage BRANCH=campaign/the-palimpsest REF=<full-sha>` at each of
Tasks 2, 5 and 7 — never `git merge` to main by hand, and never absorb
mid-measurement (finish Task 6's readout before absorbing).

At close: `make sluice BRANCH=campaign/the-palimpsest REF=<full-sha>`, then
the `closing-a-campaign` skill.
