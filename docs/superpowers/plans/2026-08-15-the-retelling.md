# The Retelling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make myth content vary in transmission, so that divergence becomes measurable, by shipping a two-filter communication chain and a correct maximum-antichain measure of divergent structure.

**Architecture:** A retelling passes through two deterministic filters — the teller's *incentive* (has this community ever raided?) and the hearer's *formation* (was it born of a catastrophe?). A retelling whose filter pair is **matched** is frictionless and carries content unchanged; a **mismatched** pair is lossy and coarsens the claim's day one rung down a ladder built from that world's own cycles. Everything is derived from committed facts, so `windows/hearsay` stays a window: no seeded draw, no `streams.rs` label, no epoch.

**Tech Stack:** Rust 2024, `hornvale-kernel` + `hornvale-history` only at runtime (`hornvale-worldgen`/`astronomy`/`terrain` are dev-dependencies for the live batteries). Std-only; the workspace dependency allowlist is `serde`, `serde_json`, `libm`.

**Spec:** `docs/superpowers/specs/2026-08-14-the-retelling-design.md`

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only; float sorts use `total_cmp` with a deterministic tie-break. Enforced by `clippy.toml`.
- **No wall-clock time.** `Claim` carries no time; `hops` is the only clock.
- **No new seeded draws.** Both filters are pure functions of committed facts. A draw would need a `streams.rs` label and an epoch, and would demote `hearsay` from window to domain.
- **Every crate sets `#![warn(missing_docs)]`** — every `pub` item, field and variant gets a one-line doc comment.
- **`type-audit:` tag on every primitive at a `pub` boundary**, and regenerate `docs/audits/type-audit-report.md` **in the same commit** that moves a pub boundary.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the single most common review finding.
- **`Claim` is derived and never serialized.** It must not gain `Serialize`/`Deserialize`. This is what keeps the campaign off the save-format surface.
- **The pre-commit hook runs `make quick` WORKSPACE-WIDE**, not on the crate
  you touched. A task may therefore never leave a sibling crate
  uncompilable "for the next task to fix" — nothing can be committed
  until the whole workspace builds, and bypassing the hook is forbidden
  without exception.
- **A dependency change drifts a generated artifact.** Adding or moving a
  crate dependency rewrites `book/src/reference/layering-generated.md`,
  which `cli/tests/architecture.rs::the_layering_page_matches_the_enforced_graph`
  asserts as a golden. Regenerate it with `REBASELINE=1` scoped to that
  test and commit it alongside the code change. Task 2b found this the
  hard way; it is not in `docs/generated-paths.txt`'s usual sweep.
- **HOMONYM WARNING:** `hornvale_lab::census_claim::Claim` and `windows/book`'s `ChorusLine::RevealedClaim` are **different types**. This plan never touches them. The `Claim` this plan changes is `kernel/src/claim.rs:18` only.

---

## File Structure

| File | Responsibility |
|---|---|
| `kernel/src/precision.rs` (create) | `Precision` — the rung INDEX only. No spans, no day arithmetic (see Task 1). |
| `windows/hearsay/src/ladder.rs` (create) | `PrecisionLadder` — the per-world rungs, read from committed sky facts. |
| `kernel/src/claim.rs` (modify) | `Claim` gains a `precision` field and two retelling constructors. |
| `windows/hearsay/src/filters.rs` (create) | The two filter keys and the matched/lossy predicate. |
| `windows/hearsay/src/divergence.rs` (create) | Maximum antichain over a witness set. |
| `windows/hearsay/src/derive.rs` (modify) | `variants_about` — the path-aware walk that applies filters. |
| `windows/hearsay/src/lib.rs` (modify) | Re-exports and the readout helpers H1/H2/H3 consume. |
| `windows/hearsay/tests/retelling_readout_seed42.rs` (create) | The preregistered §6 readout. |
| `cli/tests/heavy_tier.rs` (modify) | The canonical ignore-reason string. |

**There are exactly three full-literal `Claim { … }` construction sites** — `kernel/src/claim.rs:38`, `kernel/src/claim.rs:59`, `windows/hearsay/src/derive.rs:78`. Adding a field breaks all three and nothing else; verified by `grep -rn 'Claim {' --include=*.rs kernel/ windows/ cli/`.

---

### Task 1: `Precision`, the rung index — LANDED, AS REVISED

**Status: complete.** Kept here because a plan section describing something
other than what shipped is a trap for the next reader.

**What shipped** (`kernel/src/precision.rs`, commits `a161568b` then
`1b7da3ba`, `46e8613b`): a bare newtype `Precision(pub u8)` with `FINEST`,
`rung()` and an unbounded `coarser()`. No spans, no calendar units, no day
arithmetic. Three tests: precision rank only ever rises, coarsening saturates
rather than wrapping (a `u8` wrap would silently return a claim to first-hand
precision, the one transition the model forbids), and `FINEST` orders below
every other rung.

**What the original Task 1 got wrong, in three layers.** It specified a
five-variant enum `{Day, Season, Year, Decade, Generation}` carrying spans of
`1 / 91 / 365 / 3650 / 10950`.

1. **Those are Earth's calendar.** Every Hornvale world draws its own
   `year-length-std`, `day-length-std` and `moon-period-std`. A world with no
   moons, or no seasons (`Calendar::season_phase` returns `Option`), or a
   tidally-locked day, does not have those rungs at all.
2. **The nesting requirement was the wrong invariant.** The original test
   asserted that coarsening never reduces error, and it was FALSE against its
   own implementation (91 does not divide 365; at day 3661.75 the Year rung is
   more accurate than the Season rung). The first fix nudged Season to 91.25 to
   force nesting — fitting a physical constant to an arithmetic property. The
   real invariant is **precision-rank monotonicity**: a coarsened claim is an
   interval that widened, not a point that moved.
3. **The kernel cannot hold spans at all.** A duration at a `pub` boundary
   wants `StdDays` (design principle 5); `StdDays` lives in
   `domains/astronomy`; the kernel may not depend on a domain. `type-audit`
   refused the bare `f64` and was pointing at a layering fault, not a missing
   annotation.

Rungs, spans, saturation and snapping all live in Task 2b instead.

---

### Task 2: `Claim` learns to be retold

**Files:**
- Modify: `kernel/src/claim.rs` (struct at `:18`, `inherited_by` at `:37`, test helper at `:58`)
- Test: in-module `#[cfg(test)]` in `kernel/src/claim.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::Precision` (Task 1, landed).
- Produces: `Claim.precision: Precision` (new public field);
  `Claim::retold_by(&self, holder: EntityId) -> Claim` (frictionless);
  `Claim::retold_by_lossy(&self, holder: EntityId, precision: Precision, object: Value) -> Claim`.
  `Claim::inherited_by` keeps its exact existing behaviour and signature.

**The kernel performs NO day arithmetic.** The lossy constructor is handed an
already-coarsened `(precision, object)` by the window that owns the ladder. A
day span at a `pub` boundary wants `StdDays`; `StdDays` lives in
`domains/astronomy`; the kernel may not depend on a domain. `type-audit`
refuses the bare `f64` alternative, which is how this was found.

**`inherited_by` is kept, not replaced** — campaign 1's three tests assert on
it and their meaning must not shift. A test pins
`retold_by(h) == inherited_by(h)`.

- [ ] **Step 1: Write the failing tests**, added to the existing `mod tests`:

```rust
    #[test]
    fn a_frictionless_retelling_carries_content_and_precision_unchanged() {
        let heir = witnessed().retold_by(eid(2));
        assert_eq!(heir.object, witnessed().object);
        assert_eq!(heir.precision, Precision::FINEST);
        assert_eq!(heir.hops, 1);
        assert_eq!(heir.grade, Provenance::Taught);
    }

    #[test]
    fn retold_by_agrees_with_inherited_by() {
        assert_eq!(witnessed().inherited_by(eid(2)), witnessed().retold_by(eid(2)));
    }

    #[test]
    fn a_lossy_retelling_takes_the_coarsened_value_it_is_given() {
        let heir = witnessed().retold_by_lossy(eid(2), Precision(1), Value::Number(63875.0));
        assert_eq!(heir.precision, Precision(1));
        assert_eq!(heir.object, Value::Number(63875.0));
        assert_eq!(heir.grade, Provenance::Taught);
    }

    #[test]
    fn hops_and_grade_advance_identically_on_both_paths() {
        // The constructors differ ONLY in content and precision. If an edit
        // makes one skip a hop or a downgrade, this fails.
        let a = witnessed().retold_by(eid(2));
        let b = witnessed().retold_by_lossy(eid(2), Precision(1), Value::Number(0.0));
        assert_eq!((a.hops, a.grade, a.holder), (b.hops, b.grade, b.holder));
        assert_eq!((a.subject, a.predicate.clone()), (b.subject, b.predicate.clone()));
    }

    #[test]
    fn a_frictionless_retelling_never_sharpens_a_coarsened_claim() {
        // The one transition the model forbids.
        let c = witnessed()
            .retold_by_lossy(eid(2), Precision(2), Value::Number(0.0))
            .retold_by(eid(3));
        assert_eq!(c.precision, Precision(2));
        assert!(c.precision > Precision::FINEST);
    }
```

- [ ] **Step 2: Run to verify they FAIL.** Scope to `-p hornvale-kernel --lib claim`.
      Expected: `no method named retold_by`, `no field precision`.

- [ ] **Step 3: Implement.** Add the field after `hops`:

```rust
    /// Which rung of the world's ladder this holder remembers the day at.
    /// Witnesses hold [`Precision::FINEST`]; each lossy retelling descends.
    pub precision: Precision,
```

and both methods beside `inherited_by`:

```rust
    /// The claim as a new holder receives it through a FRICTIONLESS retelling:
    /// teller and hearer share a frame, so content and precision pass
    /// unchanged. Behaviourally identical to [`Claim::inherited_by`], pinned
    /// by test.
    pub fn retold_by(&self, holder: EntityId) -> Claim {
        Claim {
            holder,
            subject: self.subject,
            predicate: self.predicate.clone(),
            object: self.object.clone(),
            grade: self.grade.on_transmission(),
            hops: self.hops.saturating_add(1),
            precision: self.precision,
        }
    }

    /// The claim as a new holder receives it through a LOSSY retelling.
    ///
    /// `precision` and `object` arrive already coarsened from the window that
    /// owns the world's ladder — the kernel performs no day arithmetic,
    /// because a duration at a `pub` boundary wants `StdDays` and that type
    /// lives in a domain the kernel may not depend on.
    pub fn retold_by_lossy(
        &self,
        holder: EntityId,
        precision: Precision,
        object: Value,
    ) -> Claim {
        Claim {
            holder,
            subject: self.subject,
            predicate: self.predicate.clone(),
            object,
            grade: self.grade.on_transmission(),
            hops: self.hops.saturating_add(1),
            precision,
        }
    }
```

Then fix the construction sites: `precision: self.precision` in
`inherited_by`'s literal at `:38`, `precision: Precision::FINEST` in the test
helper at `:59`. `windows/hearsay/src/derive.rs:78` is Task 4's.

- [ ] **Step 4: Run to verify PASS** — 3 pre-existing plus 5 new.

- [ ] **Step 5: Regenerate the type-audit report and commit.** `Claim` gained a
      public field, so the report drifts and must move in the same commit.
      Run `type-audit check`, then `type-audit report` redirected over
      `docs/audits/type-audit-report.md`, then `cargo fmt`, then commit both
      files.

---

### Task 2b: The world's precision ladder

**Files:**
- Create: `windows/hearsay/src/ladder.rs`
- Modify: `windows/hearsay/src/lib.rs` (`pub mod ladder;`), `windows/hearsay/Cargo.toml` (add `hornvale-astronomy` to `[dependencies]`), `windows/hearsay/tests/common/mod.rs` (add `put_on`)
- Test: `windows/hearsay/tests/ladder.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::Precision`, `hornvale_astronomy::facts::{YEAR_LENGTH_STD, DAY_LENGTH_STD, MOON_PERIOD_STD}`, `hornvale_astronomy::units::StdDays`.
- Produces: `pub struct PrecisionLadder`; `PrecisionLadder::of(ledger: &Ledger) -> PrecisionLadder`;
  `len`, `is_empty`, `label(Precision) -> Option<&str>`, `span(Precision) -> Option<StdDays>`,
  `coarser(&self, Precision) -> Precision` (saturating at this world's coarsest),
  `apply(&self, Precision, day: f64) -> f64`;
  `labels(&self) -> Vec<&str>` (finest first, for the readout's report line).

**THE TRAP THIS TASK EXISTS TO AVOID — verified in the code, not inferred.**
`MOON_PERIOD_STD` is registered **non-functional** (`domains/astronomy/src/lib.rs:138`,
flag `false`) and committed **once per moon on the same subject**
(`facts.rs:417-421`). `Ledger::value_of` returns only *the first fact in commit
order* (`kernel/src/ledger.rs:361-369`). So `value_of(subject, MOON_PERIOD_STD)`
silently yields **one moon on a two-mooned world**, every test still passes, and
Nathan's explicit "both moons" decision is violated invisibly. Use the idiom
astronomy's own test uses at `facts.rs:719`:

```rust
ledger.facts_about(subject).filter(|f| f.predicate == MOON_PERIOD_STD)
```

`YEAR_LENGTH_STD` and `DAY_LENGTH_STD` **are** functional, so `value_of` is
correct for those two.

**Do not snap or nest the rungs.** Real cycles are incommensurable and that is
the modelled phenomenon (spec §5.2). A test below asserts that re-rounding can
carry a claim off the event entirely; if it fails, the fix is never to make the
spans divide each other.

- [ ] **Step 1: Write the failing tests** in `windows/hearsay/tests/ladder.rs`:

```rust
//! The per-world ladder, over hand-built ledgers.

mod common;

use common::put_on;
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{Ledger, Value};

fn sky(day: Option<f64>, moons: &[f64], year: Option<f64>) -> Ledger {
    let mut led = Ledger::default();
    if let Some(d) = day {
        put_on(&mut led, 1, hornvale_astronomy::facts::DAY_LENGTH_STD, Value::Number(d));
    }
    for m in moons {
        put_on(&mut led, 1, hornvale_astronomy::facts::MOON_PERIOD_STD, Value::Number(*m));
    }
    if let Some(y) = year {
        put_on(&mut led, 1, hornvale_astronomy::facts::YEAR_LENGTH_STD, Value::Number(y));
    }
    led
}

#[test]
fn both_moons_of_a_two_mooned_world_become_rungs() {
    // If this reports 3, someone reached for value_of and lost a moon.
    let l = PrecisionLadder::of(&sky(Some(1.0), &[29.3, 41.7], Some(372.4)));
    assert_eq!(l.len(), 4, "day, two moons, year: {l:?}");
}

#[test]
fn rungs_sort_by_actual_span_so_the_order_is_world_derived() {
    let l = PrecisionLadder::of(&sky(Some(1.0), &[41.7], Some(372.4)));
    assert_eq!(l.label(Precision(0)), Some("day"));
    assert_eq!(l.label(Precision(2)), Some("year"));
}

#[test]
fn a_moonless_world_simply_has_no_lunar_rung() {
    let l = PrecisionLadder::of(&sky(Some(1.0), &[], Some(300.0)));
    assert_eq!(l.len(), 2);
    assert_eq!(l.coarser(Precision(1)), Precision(1), "saturates at the year");
}

#[test]
fn an_empty_ladder_loses_no_precision() {
    let l = PrecisionLadder::of(&Ledger::default());
    assert!(l.is_empty());
    assert_eq!(l.apply(Precision::FINEST, 3661.75), 3661.75);
}

#[test]
fn two_moons_of_equal_period_contribute_one_rung() {
    let l = PrecisionLadder::of(&sky(Some(1.0), &[30.0, 30.0], Some(300.0)));
    assert_eq!(l.len(), 3);
}

#[test]
fn re_rounding_an_already_rounded_day_can_exclude_the_event() {
    // The consequence of NOT nesting. Do not repair a failure here by
    // snapping the rungs -- see spec section 5.2.
    let l = PrecisionLadder::of(&sky(Some(1.0), &[41.7], Some(372.4)));
    let year = Precision(2);
    let truth = 745.0;
    let width = l.span(year).expect("year rung").get();
    let twice = l.apply(year, l.apply(Precision(1), truth));
    assert!(
        truth < twice || truth >= twice + width,
        "re-rounding must be able to exclude the event: {truth} still in [{twice}, {})",
        twice + width
    );
    let direct = l.apply(year, truth);
    assert!(
        truth >= direct && truth < direct + width,
        "control: rounding the truth once must still contain it"
    );
}
```

`common/mod.rs` gains `put_on(led, subject, predicate, value)` — like the
existing `put`, but registering the predicate **non-functional** so several
`moon-period-std` facts can land on one subject. The existing `put` registers
with `true` and would reject the second moon.

- [ ] **Step 2: Run to verify FAIL.** Scope to `-p hornvale-hearsay --test ladder`.
      Expected: `unresolved import hornvale_hearsay::ladder`.

- [ ] **Step 3: Implement.** For each subject carrying them, collect rungs:
      `day-length-std` (label `"day"`, `value_of`); every `moon-period-std` on
      that subject (labels `"moon 1"`, `"moon 2"`, ... in commit order, via
      `facts_about().filter()`); `year-length-std` (label `"year"`, `value_of`).
      Drop non-finite and non-positive spans. Sort ascending by span with a
      label tie-break; de-duplicate on equal spans. Store spans as `StdDays`.
      `apply` takes and returns a bare `f64` day because that is what
      `Value::Number` holds; tag it. `coarser` saturates at `len() - 1`, and on
      an empty ladder returns its argument unchanged.

- [ ] **Step 4: Run to verify PASS** — 6 tests.

- [ ] **Step 5:** `type-audit check`, `cargo fmt`, commit `windows/hearsay/`.

---

### Task 3: The two filters

**Files:**
- Create: `windows/hearsay/src/filters.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod filters;`)
- Test: `windows/hearsay/tests/filters.rs`

**Interfaces:**
- Consumes: `Lineage` from `windows/hearsay/src/lineage.rs`.
- Produces: `pub struct Filters` with `Filters::of(ledger: &Ledger, lineage: &Lineage) -> Filters`, `Filters::raids(&self, occ: EntityId) -> bool`, `Filters::born_of_catastrophe(&self, occ: EntityId) -> bool`, `Filters::is_lossy(&self, teller: EntityId, hearer: EntityId) -> bool`.

**The lossy predicate:** a retelling is lossy when the teller's incentive key and the hearer's formation key **disagree** — `raids(teller) != born_of_catastrophe(hearer)`. Matched frames carry content; mismatched frames lose a rung.

- [ ] **Step 1: Write the failing test**

```rust
//! The two filter keys, over hand-built ledgers.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::filters::Filters;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_kernel::ledger::Value;

#[test]
fn a_community_that_appears_as_an_ender_is_a_raider() {
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(1))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(500.0));
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(3)),
    );
    let lin = lineage_of(&led);
    let f = Filters::of(&led, &lin);
    assert!(f.raids(eid(3)), "3 ended 1, so 3 raids");
    assert!(!f.raids(eid(1)), "1 was the victim, not the raider");
    assert!(!f.raids(eid(2)), "2 did nothing");
}

#[test]
fn a_child_founded_on_its_parents_ending_day_was_born_of_catastrophe() {
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(1))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(500.0));
    put(&mut led, 2, hornvale_history::OCC_FOUNDED, Value::Number(500.0));
    put(&mut led, 3, hornvale_history::OCC_FOUNDED, Value::Number(400.0));
    let lin = lineage_of(&led);
    let f = Filters::of(&led, &lin);
    assert!(f.born_of_catastrophe(eid(2)), "2 fled 1's ending");
    assert!(
        !f.born_of_catastrophe(eid(3)),
        "3 budded off before the ending and was elsewhere"
    );
}

#[test]
fn a_retelling_is_lossy_exactly_when_the_two_keys_disagree() {
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(1))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(500.0));
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(3)),
    );
    put(&mut led, 2, hornvale_history::OCC_FOUNDED, Value::Number(500.0));
    put(&mut led, 3, hornvale_history::OCC_FOUNDED, Value::Number(400.0));
    let lin = lineage_of(&led);
    let f = Filters::of(&led, &lin);
    // teller 3 raids (true); hearer 2 born of catastrophe (true) -> matched.
    assert!(!f.is_lossy(eid(3), eid(2)));
    // teller 1 does not raid (false); hearer 2 born of catastrophe (true).
    assert!(f.is_lossy(eid(1), eid(2)));
    // teller 1 does not raid (false); hearer 3 not born of it (false).
    assert!(!f.is_lossy(eid(1), eid(3)));
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-hearsay --test filters`
Expected: FAIL — `unresolved import hornvale_hearsay::filters`.

- [ ] **Step 3: Write the implementation**

```rust
//! The two filters of the transmission chain.
//!
//! `producer -> productive filter -> receptive filter -> receiver`. What a
//! teller encodes is keyed on its INCENTIVE (has this community ever raided?);
//! what a hearer decodes is keyed on its FORMATION (was it born of a
//! catastrophe?). Both are read from committed facts, so nothing here draws.
//!
//! Keying on `occ-people` was tried and falsified before this shipped: 0 of
//! 658 inheritance edges on seed 42 cross a people boundary, so a
//! species-keyed filter is inert (spec §3.2).

use crate::lineage::Lineage;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeSet;

/// The two filter keys for every occupation in a world.
#[derive(Clone, Debug, Default)]
pub struct Filters {
    raiders: BTreeSet<EntityId>,
    catastrophe_born: BTreeSet<EntityId>,
}

impl Filters {
    /// Read both keys out of a ledger. O(facts + edges); build once, ask many.
    pub fn of(ledger: &Ledger, lineage: &Lineage) -> Filters {
        let mut raiders = BTreeSet::new();
        for fact in ledger.find(hornvale_history::OCC_ENDED_BY) {
            if let Value::Entity(attacker) = &fact.object {
                raiders.insert(*attacker);
            }
        }
        let mut catastrophe_born = BTreeSet::new();
        for child in lineage.all() {
            let Some(parent) = lineage.parent(child) else {
                continue;
            };
            let (
                Some(Value::Number(ended)),
                Some(Value::Number(founded)),
            ) = (
                ledger.value_of(parent, hornvale_history::OCC_ENDED),
                ledger.value_of(child, hornvale_history::OCC_FOUNDED),
            ) else {
                continue;
            };
            if ended == founded {
                catastrophe_born.insert(child);
            }
        }
        Filters {
            raiders,
            catastrophe_born,
        }
    }

    /// The productive key: has this community ever ended another?
    pub fn raids(&self, occ: EntityId) -> bool {
        self.raiders.contains(&occ)
    }

    /// The receptive key: was this community founded on exactly its parent's
    /// ending day — the survivors who fled and refounded?
    pub fn born_of_catastrophe(&self, occ: EntityId) -> bool {
        self.catastrophe_born.contains(&occ)
    }

    /// Whether a retelling from `teller` to `hearer` loses a rung of precision.
    /// Lossy exactly when the two keys disagree: a shared frame carries
    /// content, a mismatched one costs precision.
    pub fn is_lossy(&self, teller: EntityId, hearer: EntityId) -> bool {
        self.raids(teller) != self.born_of_catastrophe(hearer)
    }
}
```

Add `pub mod filters;` to `windows/hearsay/src/lib.rs`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-hearsay --test filters`
Expected: PASS, 3 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/hearsay/src/filters.rs windows/hearsay/src/lib.rs windows/hearsay/tests/filters.rs
git commit -m "feat(hearsay): the productive and receptive filters, keyed on role and history"
```

---

### Task 4: The path-aware walk

**Files:**
- Modify: `windows/hearsay/src/derive.rs` (the `Claim` literal at `:78`; add `variants_about`)
- Test: `windows/hearsay/tests/derive.rs` (append)

**Interfaces:**
- Consumes: `Filters` (Task 3), `Claim::retold_by` and `Precision` (Tasks 1–2), `witnesses_of` and `Lineage` (existing).
- Produces: `pub fn variants_about(ledger: &Ledger, lineage: &Lineage, filters: &Filters, subject: EntityId, predicate: &str) -> Vec<Claim>`, ascending by holder.

**THE DESIGN DECISION THIS TASK LOCKS, and it is not in the spec because the spec does not reach implementation depth.** Campaign 1's `claims_about` takes the *minimum hop count* across witnesses. Distortion depends on the whole path, not its length, so a holder reachable from two witnesses now has two candidate variants. The rule is **the least-corrupted telling wins**, ordered by:

1. fewest lossy steps, then
2. fewest hops, then
3. smallest witness `EntityId`.

Rule 3 exists only to make the result total and deterministic; without it two equally-good paths would race. This is a semantic choice — a community holds the clearest version it has access to — and it must be stated in the doc comment so a later reader does not read it as an accident.

- [ ] **Step 1: Write the failing test**

Append to `windows/hearsay/tests/derive.rs`:

```rust
#[test]
fn a_frictionless_chain_carries_the_day_to_every_descendant() {
    // 1 ends; 2 is its survivor; 3 and 4 descend from 2. With no raider
    // anywhere, every teller's key is false, so a hearer born of catastrophe
    // mismatches and a hearer not born of it matches.
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(2)), (4, Some(3))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(63918.75));
    put(&mut led, 2, hornvale_history::OCC_FOUNDED, Value::Number(400.0));
    put(&mut led, 3, hornvale_history::OCC_FOUNDED, Value::Number(500.0));
    put(&mut led, 4, hornvale_history::OCC_FOUNDED, Value::Number(600.0));
    let lin = lineage_of(&led);
    let f = Filters::of(&led, &lin);
    let vs = variants_about(&led, &lin, &f, eid(1), hornvale_history::OCC_ENDED);
    for v in &vs {
        assert_eq!(
            v.precision,
            Precision::FINEST,
            "no key disagrees anywhere, so nothing is lossy: {v:?}"
        );
        assert_eq!(v.object, Value::Number(63918.75));
    }
    assert_eq!(vs.len(), 4, "the witness and three inheritors");
}

#[test]
fn a_lossy_step_coarsens_the_day_for_that_holder_and_its_descendants() {
    // 2 is born of catastrophe, so a non-raiding teller mismatches it.
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(2))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(63918.75));
    put(&mut led, 2, hornvale_history::OCC_FOUNDED, Value::Number(63918.75));
    put(&mut led, 3, hornvale_history::OCC_FOUNDED, Value::Number(64000.0));
    let lin = lineage_of(&led);
    let f = Filters::of(&led, &lin);
    let vs = variants_about(&led, &lin, &f, eid(1), hornvale_history::OCC_ENDED);
    let held = |who: u64| {
        vs.iter()
            .find(|v| v.holder == eid(who))
            .unwrap_or_else(|| panic!("{who} holds nothing"))
            .clone()
    };
    // 2 is a WITNESS (founded on 1's ending day), so it holds first-hand.
    assert_eq!(held(2).precision, Precision::FINEST);
    assert_eq!(held(2).hops, 0);
    // 3 hears it from 2: teller 2 does not raid, hearer 3 not born of
    // catastrophe -> keys agree -> frictionless.
    assert_eq!(held(3).precision, Precision::FINEST);
}

#[test]
fn the_least_corrupted_telling_is_the_one_held() {
    // A holder reachable from two witnesses takes the path with the fewest
    // lossy steps, not merely the fewest hops.
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(2))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(63918.75));
    put(&mut led, 2, hornvale_history::OCC_FOUNDED, Value::Number(63918.75));
    put(&mut led, 3, hornvale_history::OCC_FOUNDED, Value::Number(63918.75));
    let lin = lineage_of(&led);
    let f = Filters::of(&led, &lin);
    let vs = variants_about(&led, &lin, &f, eid(1), hornvale_history::OCC_ENDED);
    // 3 is founded on 1's ending day but its parent is 2, not 1 — so it is
    // NOT a direct child of the subject and therefore not a witness. It
    // inherits, and whichever path it takes must be recorded consistently.
    let three = vs.iter().find(|v| v.holder == eid(3)).expect("3 holds");
    assert!(
        three.hops >= 1,
        "3 is an inheritor, not a witness: {three:?}"
    );
}
```

Add the imports the file needs at its top: `use hornvale_hearsay::derive::variants_about; use hornvale_hearsay::filters::Filters; use hornvale_kernel::Precision;`

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-hearsay --test derive`
Expected: FAIL — `cannot find function variants_about`.

- [ ] **Step 3: Write the implementation**

**`derive.rs`'s literal is ALREADY FIXED** — Task 2 had to add
`precision: Precision::FINEST` to it (now at `derive.rs:88`) because the
pre-commit hook runs `make quick` WORKSPACE-WIDE, so a knowingly-broken
sibling crate blocks every commit, not just that crate's own. Do not
re-add it. Then add:

```rust
/// Every variant of `(subject, predicate)` held anywhere, ascending by holder.
///
/// Like [`claims_about`], but content varies: each inheritance step is a
/// retelling through the two filters, and a mismatched pair costs a rung of
/// precision (see [`crate::filters`]).
///
/// **A holder reachable from two witnesses holds the LEAST-CORRUPTED telling**
/// — ordered by fewest lossy steps, then fewest hops, then smallest witness
/// id. The first is the semantic rule (a community holds the clearest version
/// it can reach); the third exists only to make the order total, so two
/// equally-good paths cannot race. Campaign 1 ordered on hops alone, which is
/// insufficient once content varies.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn variants_about(
    ledger: &Ledger,
    lineage: &Lineage,
    filters: &Filters,
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
        precision: hornvale_kernel::Precision::FINEST,
    };
    let witnesses = witnesses_of(ledger, lineage, subject, predicate);
    // (lossy_steps, hops, witness) -> the claim reached by that path.
    let mut held: BTreeMap<EntityId, (u32, u32, EntityId, Claim)> = BTreeMap::new();
    for w in &witnesses {
        let mut c = base.clone();
        c.holder = *w;
        held.insert(*w, (0, 0, *w, c));
    }
    for w in &witnesses {
        for d in lineage.descendants_of(*w) {
            if witnesses.contains(&d) {
                continue; // a witness is never demoted to an inheritor
            }
            // Walk the ancestry from w down to d, retelling at each step.
            let anc = lineage.ancestry(d); // d first, root last
            let Some(pos) = anc.iter().position(|a| a == w) else {
                continue;
            };
            let mut chain: Vec<EntityId> = anc[..=pos].to_vec();
            chain.reverse(); // now w .. d
            let mut c = base.clone();
            c.holder = *w;
            let mut lossy_steps = 0u32;
            for pair in chain.windows(2) {
                let (teller, hearer) = (pair[0], pair[1]);
                let lossy = filters.is_lossy(teller, hearer);
                if lossy {
                    lossy_steps += 1;
                }
                c = c.retold_by(hearer, lossy);
            }
            let candidate = (lossy_steps, c.hops, *w, c);
            match held.get(&d) {
                Some(existing)
                    if (existing.0, existing.1, existing.2)
                        <= (candidate.0, candidate.1, candidate.2) => {}
                _ => {
                    held.insert(d, candidate);
                }
            }
        }
    }
    held.into_values().map(|(_, _, _, c)| c).collect()
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-hearsay`
Expected: PASS — the three new tests plus every pre-existing hearsay test, which must be unchanged.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/hearsay/src/derive.rs windows/hearsay/tests/derive.rs
git commit -m "feat(hearsay): variants_about — the path-aware walk that applies both filters"
```

---

### Task 5: Divergent structure is a maximum antichain

**Files:**
- Create: `windows/hearsay/src/divergence.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod divergence;`)
- Test: `windows/hearsay/tests/divergence.rs`

**Interfaces:**
- Consumes: `Lineage`.
- Produces: `pub fn maximum_antichain(lineage: &Lineage, witnesses: &[EntityId]) -> Vec<EntityId>`, ascending.

**The correctness argument, which belongs in the doc comment:** in a forest poset the maximum antichain of a witness set `S` is exactly the elements of `S` with no strict `S`-descendant. It is an antichain (if `x` were an ancestor of `y`, `y` would be an `S`-descendant of `x`), and it is maximum, because any antichain `A ⊆ S` injects into it by sending each `a` to a deepest `S`-descendant of `a` — injectively, because incomparable elements of a tree have disjoint descendant sets.

- [ ] **Step 1: Write the failing test**

```rust
//! Maximum antichain over a witness set.

mod common;

use common::{eid, ledger_with};
use hornvale_hearsay::divergence::maximum_antichain;
use hornvale_hearsay::lineage::lineage_of;

#[test]
fn the_motivating_scenario_returns_the_survivors_not_the_village() {
    // A is raided; B and C are its survivors. Campaign 1's minimal-elements
    // rule returned {A} and scored this scenario ZERO. The maximum antichain
    // is {B, C} — the pair the scenario is about.
    let led = ledger_with(&[(2, Some(1)), (3, Some(1))]);
    let lin = lineage_of(&led);
    let ws = vec![eid(1), eid(2), eid(3)];
    assert_eq!(maximum_antichain(&lin, &ws), vec![eid(2), eid(3)]);
}

#[test]
fn a_chain_of_witnesses_has_an_antichain_of_one() {
    let led = ledger_with(&[(2, Some(1)), (3, Some(2))]);
    let lin = lineage_of(&led);
    let ws = vec![eid(1), eid(2), eid(3)];
    assert_eq!(maximum_antichain(&lin, &ws), vec![eid(3)]);
}

#[test]
fn witnesses_in_unrelated_lineages_are_all_incomparable() {
    let led = ledger_with(&[(1, None), (2, None), (3, None)]);
    let lin = lineage_of(&led);
    let ws = vec![eid(1), eid(2), eid(3)];
    assert_eq!(maximum_antichain(&lin, &ws), vec![eid(1), eid(2), eid(3)]);
}

#[test]
fn an_empty_witness_set_has_an_empty_antichain() {
    let led = ledger_with(&[(2, Some(1))]);
    let lin = lineage_of(&led);
    assert_eq!(maximum_antichain(&lin, &[]), Vec::new());
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-hearsay --test divergence`
Expected: FAIL — `unresolved import hornvale_hearsay::divergence`.

- [ ] **Step 3: Write the implementation**

```rust
//! Divergent structure: the maximum antichain of a witness set.
//!
//! Deliberately NOT called corroboration. It measures the PRECONDITION —
//! whether two witnesses' accounts descend through communities that never
//! inherited from one another — not the event of confirmation.

use crate::lineage::Lineage;
use hornvale_kernel::ledger::EntityId;

/// The largest pairwise-incomparable subset of `witnesses`, ascending.
///
/// Corroboration is a SYMMETRIC relation; ancestry is a partial order and
/// therefore antisymmetric, so filtering a witness set by ancestry can never
/// express it. The symmetric relation available inside a partial order is
/// incomparability, and a pairwise-incomparable set is an antichain.
///
/// In a forest the maximum antichain of a set `S` is exactly the elements of
/// `S` with no strict `S`-descendant. It is an antichain: if `x` were an
/// ancestor of `y`, then `y` would be an `S`-descendant of `x`. It is maximum:
/// any antichain `A ⊆ S` injects into it by sending each `a` to a deepest
/// `S`-descendant of `a`, injectively because incomparable elements of a tree
/// have disjoint descendant sets. So no matching algorithm is needed.
///
/// For witnesses `{A, B, C}` with `B` and `C` survivors of `A`, this returns
/// `{B, C}`. The minimal elements would be `{A}`, which scores that scenario
/// zero — the error campaign 1 made.
pub fn maximum_antichain(lineage: &Lineage, witnesses: &[EntityId]) -> Vec<EntityId> {
    let mut out: Vec<EntityId> = witnesses
        .iter()
        .copied()
        .filter(|w| {
            !witnesses
                .iter()
                .any(|other| other != w && lineage.ancestry(*other).contains(w))
        })
        .collect();
    out.sort();
    out.dedup();
    out
}
```

Add `pub mod divergence;` to `windows/hearsay/src/lib.rs`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-hearsay --test divergence`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/hearsay/src/divergence.rs windows/hearsay/src/lib.rs windows/hearsay/tests/divergence.rs
git commit -m "feat(hearsay): divergent structure as a maximum antichain, not minimal elements"
```

---

### Task 6: The preregistered readout

**Files:**
- Create: `windows/hearsay/tests/retelling_readout_seed42.rs`
- Modify: `windows/hearsay/src/lib.rs` (add the three helpers below)
- Test: the readout file is itself the test.

**Interfaces:**
- Consumes: everything from Tasks 1–5.
- Produces: `pub fn variant_count(ledger, lineage, filters, subject, predicate) -> Option<usize>`; `pub fn finest_precision_hops(ledger, lineage, filters, subject, predicate) -> Vec<u32>`; `pub fn spearman(xs: &[f64], ys: &[f64]) -> Option<f64>`.

**This task reports numbers against §6's decision tables. It does NOT retune anything to make a prediction come true.** A falsified prediction is the finding; several campaigns have shipped the null as the headline.

- [ ] **Step 1: Write the helpers' unit tests first**

Add to `windows/hearsay/src/lib.rs` in a `#[cfg(test)] mod tests`:

```rust
    #[test]
    fn spearman_is_one_on_a_perfectly_monotone_pair() {
        let xs = [1.0, 2.0, 3.0, 4.0];
        let ys = [10.0, 20.0, 30.0, 40.0];
        let r = spearman(&xs, &ys).expect("defined");
        assert!((r - 1.0).abs() < 1e-9, "got {r}");
    }

    #[test]
    fn spearman_is_minus_one_when_reversed() {
        let xs = [1.0, 2.0, 3.0, 4.0];
        let ys = [40.0, 30.0, 20.0, 10.0];
        let r = spearman(&xs, &ys).expect("defined");
        assert!((r + 1.0).abs() < 1e-9, "got {r}");
    }

    #[test]
    fn spearman_is_undefined_when_a_side_has_no_variance() {
        // A constant column has no ranks to correlate. Returning 0.0 here
        // would read as "no relationship" when the truth is "not measurable".
        assert_eq!(spearman(&[1.0, 1.0, 1.0], &[1.0, 2.0, 3.0]), None);
    }
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-hearsay --lib spearman`
Expected: FAIL — `cannot find function spearman`.

- [ ] **Step 3: Implement the three helpers**

```rust
/// Spearman's rank correlation, or `None` when either side is constant.
///
/// A constant column has no ranks to correlate; returning `0.0` would read as
/// "no relationship" when the truth is "not measurable", which is the
/// distinction H3's NO VERDICT branch depends on.
/// type-audit: bare-ok(ratio: return)
pub fn spearman(xs: &[f64], ys: &[f64]) -> Option<f64> {
    if xs.len() != ys.len() || xs.len() < 2 {
        return None;
    }
    fn ranks(v: &[f64]) -> Option<Vec<f64>> {
        let mut idx: Vec<usize> = (0..v.len()).collect();
        idx.sort_by(|a, b| v[*a].total_cmp(&v[*b]));
        if v[idx[0]] == v[idx[idx.len() - 1]] {
            return None; // constant: no variance
        }
        let mut r = vec![0.0; v.len()];
        let mut i = 0;
        while i < idx.len() {
            let mut j = i;
            while j + 1 < idx.len() && v[idx[j + 1]] == v[idx[i]] {
                j += 1;
            }
            // average rank for ties, 1-based
            let avg = ((i + j) as f64) / 2.0 + 1.0;
            for k in i..=j {
                r[idx[k]] = avg;
            }
            i = j + 1;
        }
        Some(r)
    }
    let (rx, ry) = (ranks(xs)?, ranks(ys)?);
    let n = rx.len() as f64;
    let mx = rx.iter().sum::<f64>() / n;
    let my = ry.iter().sum::<f64>() / n;
    let mut num = 0.0;
    let mut dx = 0.0;
    let mut dy = 0.0;
    for i in 0..rx.len() {
        let (a, b) = (rx[i] - mx, ry[i] - my);
        num += a * b;
        dx += a * a;
        dy += b * b;
    }
    if dx == 0.0 || dy == 0.0 {
        return None;
    }
    Some(num / (dx * dy).sqrt())
}

/// How many distinct `(precision, object)` variants of one event are held, or
/// `None` when fewer than three holders qualify (§6's population rule).
/// type-audit: bare-ok(count: return)
pub fn variant_count(
    ledger: &Ledger,
    lineage: &Lineage,
    filters: &Filters,
    subject: EntityId,
    predicate: &str,
) -> Option<usize> {
    let vs = crate::derive::variants_about(ledger, lineage, filters, subject, predicate);
    if vs.len() < 3 {
        return None;
    }
    let mut seen: BTreeSet<(hornvale_kernel::Precision, String)> = BTreeSet::new();
    for v in &vs {
        seen.insert((v.precision, format!("{:?}", v.object)));
    }
    Some(seen.len())
}

/// The hop counts of holders still at the FINEST precision — H1's population.
pub fn finest_precision_hops(
    ledger: &Ledger,
    lineage: &Lineage,
    filters: &Filters,
    subject: EntityId,
    predicate: &str,
) -> Vec<u32> {
    crate::derive::variants_about(ledger, lineage, filters, subject, predicate)
        .into_iter()
        .filter(|c| c.precision == hornvale_kernel::Precision::FINEST)
        .map(|c| c.hops)
        .collect()
}
```

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-hearsay --lib`
Expected: PASS.

- [ ] **Step 5: Write the readout battery**

```rust
//! The preregistered readout (spec §6). Live worldgen.
//!
//! Reports H1, H2 and H3 against their decision tables. It asserts only the
//! NO VERDICT floors and the stated ceilings; the hypotheses themselves are
//! REPORTED, because a falsified prediction is a finding and this file must
//! not be edited to rescue one.

use hornvale_hearsay::derive::witnesses_of;
use hornvale_hearsay::divergence::maximum_antichain;
use hornvale_hearsay::filters::Filters;
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_hearsay::{finest_precision_hops, lineage::lineage_of, spearman, variant_count};

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loop binds
/// occupation ids, not seeds.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_retelling_readout_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);
    let f = Filters::of(led, &lin);
    let ladder = PrecisionLadder::of(led);

    // --- H1: per-hop counts of claims still at the finest precision ---
    let mut by_hop: std::collections::BTreeMap<u32, usize> = std::collections::BTreeMap::new();
    let mut finest_total = 0usize;
    for s in lin.all() {
        for h in finest_precision_hops(led, &lin, &f, s, hornvale_history::OCC_ENDED) {
            *by_hop.entry(h).or_default() += 1;
            finest_total += 1;
        }
    }
    let ratios: Vec<f64> = (1..=8)
        .filter_map(|k| {
            let a = *by_hop.get(&k)? as f64;
            let b = *by_hop.get(&(k + 1))? as f64;
            if a == 0.0 { None } else { Some(b / a) }
        })
        .collect();
    let mean = if ratios.is_empty() {
        f64::NAN
    } else {
        ratios.iter().sum::<f64>() / ratios.len() as f64
    };
    let var = if ratios.is_empty() {
        f64::NAN
    } else {
        ratios.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / ratios.len() as f64
    };

    // --- H2 and H3 ---
    let mut counts: Vec<f64> = Vec::new();
    let mut widths: Vec<f64> = Vec::new();
    for s in lin.all() {
        let Some(n) = variant_count(led, &lin, &f, s, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let ws = witnesses_of(led, &lin, s, hornvale_history::OCC_ENDED);
        counts.push(n as f64);
        widths.push(maximum_antichain(&lin, &ws).len() as f64);
    }
    let qualifying = counts.len();
    let mut sorted = counts.clone();
    sorted.sort_by(f64::total_cmp);
    let median = sorted.get(sorted.len() / 2).copied().unwrap_or(f64::NAN);
    let rho = spearman(&widths, &counts);

    println!(
        "H1 finest_precision_pairs={finest_total} per_hop={by_hop:?} \
         ratios={ratios:?} mean={mean:.4} var={var:.4}"
    );
    println!("H2 qualifying={qualifying} median_variants={median} distribution={sorted:?}");
    println!("H3 spearman_rho={rho:?}");

    // The only assertions are the floors and ceilings the spec states.
    assert!(
        finest_total >= 500 || qualifying < 100,
        "H1 NO VERDICT floor: {finest_total} finest-precision pairs"
    );
    // The ceiling is THIS WORLD'S ladder length, not a constant -- a
    // two-mooned world offers rungs a moonless one does not, which is why H2
    // reports the ladder alongside its distribution rather than comparing raw
    // variant counts across worlds.
    let ceiling = ladder.len() as f64;
    for n in &counts {
        assert!(
            *n <= ceiling,
            "a variant count above this world's {ceiling} rungs is impossible: {n}"
        );
    }
    println!("LADDER len={} rungs={:?}", ladder.len(), ladder.labels());
}
```

- [ ] **Step 6: Run the readout**

Run: `cargo test -p hornvale-hearsay --test retelling_readout_seed42 -- --ignored --nocapture`
Expected: PASS, printing three lines. **Record the printed numbers verbatim into the campaign's chronicle draft** — they are the campaign's result.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add windows/hearsay/src/lib.rs windows/hearsay/tests/retelling_readout_seed42.rs
git commit -m "feat(hearsay): the preregistered H1/H2/H3 readout"
```

---

### Task 7: Retier the batteries on measured cost

**Files:**
- Modify: `cli/tests/heavy_tier.rs:123` (the canonical reason string)
- Modify: `windows/hearsay/tests/hop_depth_seed42.rs`, `probe_filter_mismatch.rs`, `probe_filter_variation.rs`, `retelling_readout_seed42.rs` (ignore attributes)
- Modify: `windows/hearsay/tests/common/mod.rs:1-3` (the stale "is minutes" comment)

**Interfaces:** none; this task changes test tiering only.

**Measured facts this task acts on (spec §3.4):** `hop_depth_seed42` runs in **4.31 s**, `probe_filter_mismatch` in **1.96 s**, `probe_filter_variation` in **6.40 s**. The canonical ignore reason hard-codes "(minutes)", so a truthful new heavy test currently cannot exist.

- [ ] **Step 1: Measure the readout battery before deciding its tier**

Run: `/usr/bin/time -p cargo test -p hornvale-hearsay --test retelling_readout_seed42 -- --ignored --nocapture`

**Decision rule** (do not predict the number — branch on it):
- **under 20 s** → un-ignore it and every hearsay battery; they belong in the commit gate.
- **20–60 s** → un-ignore the three cheap ones, keep the readout heavy, and say so in the reason.
- **over 60 s** → keep the readout heavy; still un-ignore the two probes and `hop_depth_seed42`.

- [ ] **Step 2: Write the failing test for the canonical string**

In `cli/tests/heavy_tier.rs`, change the canonical constant to drop the false cost claim, and add:

```rust
#[test]
fn the_canonical_heavy_reason_states_no_duration() {
    // A duration baked into a ratchet freezes a measurement. The Retelling
    // measured the claim this string used to carry ("minutes") at 4.31 s.
    assert!(
        !CANONICAL_REASON.contains("minute")
            && !CANONICAL_REASON.contains("second")
            && !CANONICAL_REASON.contains("hour"),
        "the canonical reason must not assert a duration: {CANONICAL_REASON}"
    );
}
```

- [ ] **Step 3: Run to verify it fails**

Run: `cargo test -p hornvale --test heavy_tier`
Expected: FAIL — the string still contains "minutes".

- [ ] **Step 4: Apply the change**

Set the canonical reason to `"heavy: live-worldgen battery; deferred from the commit gate to make gate-full"` and update every heavy-tier ignore attribute to match verbatim. Apply Step 1's branch to the four hearsay batteries. Delete the "is minutes" sentence from `windows/hearsay/tests/common/mod.rs` and the explanatory comment added above `probe_filter_variation`'s ignore attribute, which exists only to describe the defect this task removes.

- [ ] **Step 5: Run the full gate**

Run: `make gate`
Expected: PASS. This is the first full-workspace run of the campaign; scoped runs to here have been deliberate.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add cli/tests/heavy_tier.rs windows/hearsay/tests/
git commit -m "fix(gate): the heavy-tier reason no longer asserts a duration it cannot know"
```

---

### Task 8: The book, the record, and the close

**Files:**
- Create: `book/src/chronicle/the-retelling.md`
- Create: `docs/retrospectives/the-retelling.md`
- Modify: `book/src/SUMMARY.md`, `docs/retrospectives/README.md`
- Modify: `book/src/frontier/idea-registry.md` (flip statuses)
- Modify: `book/src/open-questions.md` **only if** a Confidence Gradient bet moved

- [ ] **Step 1: Write the chronicle entry**

Report H1/H2/H3 as measured, including any null, at the book's altitude: technical and mathematical, comprehensible without reading the code. State the numbers, the decision-table branch each landed in, and the ladder ceiling so a reader can tell a saturated measure from a real one.

- [ ] **Step 2: Flip the registry rows**

`KNOW-two-filter-chain` `spec'd` → `shipped`, **Where** repointed at the chronicle. `KNOW-mismatch-needs-contact` `raw` → `elaborated` or `shipped` per what actually landed. If H3 refuted, flip `KNOW-divergence-antichain` to `refuted (The Retelling)` — the seventh status exists for exactly this, and the parenthetical is required.

- [ ] **Step 3: Write the retrospective**

Process lessons, not product (decision 0020). The heavy-tier ratchet finding belongs here.

- [ ] **Step 4: Run the drift check**

```bash
cargo test -p hornvale --test docs_consistency
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Branch on the diff, do not predict it:**
- `docs/audits/` moved → expected (pub boundaries changed); commit it in this commit.
- `book/src/domesday/` or a census CSV moved → **STOP**; that is a census refresh, which is a lefford-only act and a G6 item.
- nothing moved → also fine; the type-audit report may already be current from Task 2.

- [ ] **Step 5: Commit and stop at G6**

```bash
cargo fmt
git add book/ docs/
git commit -m "docs(the-retelling): chronicle, retrospective, and the registry statuses"
```

Then **stop**. G6 is a hard stop: present the post-G3 ledger digest to Nathan before any merge.

---

## Self-Review

**Spec coverage.** §1.1 two-filter chain → Task 3. §1.2 derived keys → Task 3. §1.3 maximum antichain → Task 5. §1.4 preregistered measurement → Task 6. §5.2 precision ladder and `retold_by` → Tasks 1–2. §3.4 stale tiering → Task 7. §8 DoD → Tasks 7–8. §6 H1/H2/H3 decision tables → Task 6 reports, Task 8 records.

**Deliberately not covered, and why:** §7's blind reconstruction, misattribution drift and filter width are all `Carried forward` — campaign 3. No task implements them.

**Type consistency.** `Precision` (Task 1) is used by name in Tasks 2, 4, 6. `Filters::is_lossy` (Task 3) is called in Task 4 only. `variants_about` (Task 4) is consumed by `variant_count` and `finest_precision_hops` (Task 6). `maximum_antichain` (Task 5) is consumed by Task 6. `Claim.precision` is added in Task 2 and read in Tasks 4 and 6.

**Known plan risk, stated rather than hidden.** Task 4's `variants_about` walks `lineage.ancestry(d)` inside a loop over `descendants_of(w)`, which is `O(n²)`-ish on a 704-node tree and fine there, but it is the function a census metric would call. If Task 6's battery exceeds Task 7's 20 s branch, the cause is most likely here, and the fix is to memoise ancestry once per world rather than to weaken the measurement.
