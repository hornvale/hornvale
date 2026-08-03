# The Namesake Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the world's historical figures personal names, derived from a person-descent graph that reprojects the already-committed community tree, under a per-culture naming grammar derived from `SocietyVector`.

**Architecture:** Three layers, following the constitutional `kernel → domains → windows → cli` ladder. `domains/history` gains the pure descent *arithmetic* (a remove is a gap divided by a generation length). `domains/language` gains the anthroponymic *schema* and its rendering, kernel-only and plain-data-in, exactly as `MorphOptions`/`SiteConcepts` already are. `windows/worldgen`, the composition root, is the only place that assembles them — it is the sole layer permitted to read history and species together. Nothing is committed: every name is a pure total function of committed facts plus the seed.

**Tech Stack:** Rust edition 2024, std only plus `serde`/`serde_json`/`libm`. `cargo nextest` for tests, `cargo test --doc` for doctests.

**Spec:** `docs/superpowers/specs/2026-08-02-the-namesake-design.md` (approved at G3, 2026-08-02).

## Global Constraints

Every task's requirements implicitly include this section.

- **No new dependencies.** The workspace allowlist is `serde`, `serde_json`, `libm` (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`). No rand, no chrono, no itertools.
- **No `HashMap` / `HashSet`.** `BTreeMap` / `BTreeSet` / `Vec` only — enforced workspace-wide by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Also enforced by `clippy.toml`.
- **Float ordering uses `total_cmp`** with a deterministic tie-break, never `partial_cmp().unwrap()`.
- **Transcendentals route through `hornvale_kernel::math`** (the portable libm), never inherent `f64::powf`/`ln`/etc. `floor` and `sqrt` stay intrinsic. This matters wherever a float feeds a *draw*.
- **`#![warn(missing_docs)]` is set on every crate.** Every `pub` item, field, and variant needs a one-line doc comment or the build warns.
- **Every primitive at a `pub` boundary carries a type-audit tag.** The grammar, stated once here because a malformed tag has recurred from plan text before:
  - form is `/// type-audit: ` followed by comma-separated verdicts
  - a verdict is `bare-ok(<class>)`, `bare-ok(<class>: <field>)`, `waiver(<reason>)`, or `pending(wave-N: <field>)`
  - the `<class>` vocabulary in use: `identifier-text`, `prose`, `ratio`, `count`, `index`, `flag`
  - use the `: <field>` form whenever the item has more than one primitive; `bare-ok(count)` alone is only correct when there is exactly one
  - verify with `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
- **`cargo fmt` is the final step before every commit.** A skipped fmt gate is this project's most common review finding.
- **A domain crate may depend on `hornvale-kernel` and nothing else** — never a sibling domain. `cli/tests/architecture.rs` enforces this.
- **Determinism:** same seed → byte-identical output. Task 9 asserts this for the whole artifact set; do not defer it.

---

## Task 1: Measure whether the sound-change cascade is inert

This task exists because the spec's §5.0 withdrew the campaign's original headline claim on one seed's evidence, and §7.3 flags the underlying question as possibly an engine-level result outside this campaign. **It comes first because its answer can redirect everything after it.** It builds a metric and reports a number; it fixes nothing.

**Files:**
- Modify: `windows/lab/src/metrics.rs` (add one metric fn + registrations)
- Create: `studies/the-cascade.study.json`

**Interfaces:**
- Consumes: nothing from earlier tasks (this is the first).
- Produces: nothing later tasks depend on in code. Produces a **measurement** that Task 2 onward assume was taken.

- [ ] **Step 1: Write the failing test**

Add to `windows/lab/tests/` a new file `cascade_firing.rs`:

```rust
//! The cascade rule-firing metric: does a species' drawn sound-change
//! cascade actually change any of its words?

use hornvale_lab::metrics::registry;

#[test]
fn cascade_rules_fired_is_registered_for_both_probe_species() {
    let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
    assert!(
        names.contains(&"cascade-rules-fired-goblin"),
        "expected cascade-rules-fired-goblin in the metric registry, got {names:?}"
    );
    assert!(
        names.contains(&"cascade-rules-fired-bugbear"),
        "expected cascade-rules-fired-bugbear in the metric registry"
    );
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo nextest run -p hornvale-lab --test cascade_firing`
Expected: FAIL — `expected cascade-rules-fired-goblin in the metric registry`

- [ ] **Step 3: Write the metric**

Add to `windows/lab/src/metrics.rs`, beside `lexicon_regular` (around line 4633):

```rust
/// How many DISTINCT sound rules in `species`' drawn cascade actually fire
/// on at least one of its lexicon's `Root` entries.
///
/// The Namesake, Task 1. A `Cascade` is 2-4 drawn rules
/// (`hornvale_language::Cascade`), but `evolve` adopts a rule's proposed
/// output only when the resulting segment is already in the phonology's
/// inventory (the codomain constraint), so a rule can be drawn and then
/// rejected on every word. This metric asks how many survive that filter.
///
/// Zero means the species' whole etymological layer is inert: every word's
/// modern form equals its proto-form's nativization, and an inherited name
/// and a re-derived one are byte-identical. `Absent` if `species` is not in
/// this world's roster or its lexicon minted no `Root`.
fn cascade_rules_fired(v: &FullView, species: &str) -> MetricValue {
    if !v.components().biosphere.ids().any(|k| k.0 == species) {
        return MetricValue::Absent;
    }
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    // A BTreeSet, not a HashSet: the workspace bans hashed containers, and
    // the count must not depend on iteration order anyway.
    let mut fired: std::collections::BTreeSet<usize> = std::collections::BTreeSet::new();
    let mut any_root = false;
    // `Lexicon::entries()` yields (&str, &LexEntry) pairs — it is an
    // iterator, not a map, so there is no `.values()`.
    for (_concept, entry) in lex.entries() {
        if let hornvale_language::LexEntry::Root { derivation, .. } = entry {
            any_root = true;
            for (i, step) in derivation.steps.iter().enumerate() {
                if step.changed {
                    fired.insert(i);
                }
            }
        }
    }
    if !any_root {
        return MetricValue::Absent;
    }
    MetricValue::Number(fired.len() as f64)
}
```

Register two metrics beside `lexicon-regular-goblin` (around line 2467):

```rust
        Metric {
            name: "cascade-rules-fired-goblin",
            doc: "How many DISTINCT sound rules in the goblin cascade actually fire on \
                   at least one lexicon Root. Zero means the etymological layer is inert \
                   for this species (The Namesake §5.0); Absent if goblin is unrostered \
                   or minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0],
            },
            extract: Extractor::Full(|v: &FullView| cascade_rules_fired(v, "goblin")),
        },
        Metric {
            name: "cascade-rules-fired-bugbear",
            doc: "How many DISTINCT sound rules in the bugbear cascade actually fire on \
                   at least one lexicon Root. Zero means the etymological layer is inert \
                   for this species (The Namesake §5.0); Absent if bugbear is unrostered \
                   or minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0],
            },
            extract: Extractor::Full(|v: &FullView| cascade_rules_fired(v, "bugbear")),
        },
```

`Lexicon::entries()` is verified as `pub fn entries(&self) -> impl Iterator<Item = (&str, &LexEntry)>`; `entries` itself is a private `BTreeMap`, so the iterator is the only way in.

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo nextest run -p hornvale-lab --test cascade_firing`
Expected: PASS

- [ ] **Step 5: Prove the metric can report non-zero (anti-vacuity)**

A metric that always returns the same number is indistinguishable from a broken one. Seed 42 measured bugbear at 1 rule and goblin at 0, so the pair should *disagree* on that seed. Add:

```rust
#[test]
fn the_two_probe_species_do_not_report_the_same_inertness_on_seed_42() {
    // Not an assertion about WHICH is higher — that is the finding, not the
    // contract. Only that the metric discriminates at all: if both species
    // returned an identical value on every seed, the metric would be
    // measuring nothing and would still pass a naive smoke test.
    let goblin = metric_value_on_seed("cascade-rules-fired-goblin", 42);
    let bugbear = metric_value_on_seed("cascade-rules-fired-bugbear", 42);
    assert_ne!(
        goblin, bugbear,
        "seed 42 measured goblin 0 and bugbear 1; if these now agree, either \
         the metric is inert or the language engine changed — both are findings"
    );
}
```

Write `metric_value_on_seed` as a small local helper following the pattern in an existing lab integration test (`grep -rn "fn .*_on_seed\|build_world" windows/lab/tests/ | head`). Run it and confirm PASS.

- [ ] **Step 6: Write the study**

Create `studies/the-cascade.study.json`:

```json
{ "name": "the-cascade",
  "description": "The Namesake Task 1: how many of a species' drawn sound-change rules actually fire, over 200 unselected worlds. Answers whether the seed-42 observation (goblin 0/70, hobgoblin 0/74, kobold 0/78 words changed) is general or a one-seed artifact.",
  "seeds": { "from": 0, "count": 200 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": ["cascade-rules-fired-goblin", "cascade-rules-fired-bugbear"] }
```

- [ ] **Step 7: Run the study and capture the result to a file**

Never inline `| tail` on an expensive run — a surprise must not cost a re-run.

```bash
cargo run --release -p hornvale -- lab run studies/the-cascade.study.json 2>&1 \
  | tee /tmp/hv-cascade.txt
```

Read `/tmp/hv-cascade.txt`. Record in the task's commit message: the mean and the **fraction of worlds where goblin reports 0**.

- [ ] **Step 8: STOP and report — this is a decision point, not a step to pass through**

Report the number to Nathan with a recommendation:

- If goblin reports 0 on **most** worlds → cascade inertness is general. This is an engine finding. Update the `LANG-cascade-inertness` registry row from "ONE SEED — evidence, not yet a finding" to the measured fraction, and **ask** whether to continue The Namesake or fork a cascade campaign first. Do not decide this unilaterally.
- If goblin reports 0 on **few** worlds → seed 42 is unlucky. Update the row to say so, note that seed 42 is the artifact-bearing seed and therefore that every committed dictionary under-represents the engine, and continue to Task 2.

- [ ] **Step 9: Commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs windows/lab/tests/cascade_firing.rs studies/the-cascade.study.json
git commit -m "feat(lab): measure whether a species' sound-change cascade fires at all

The Namesake Task 1. Counts DISTINCT cascade rules that change at least one
lexicon Root. Seed 42 showed goblin 0/70, hobgoblin 0/74, kobold 0/78 words
changed by any rule, which withdrew this campaign's original headline claim
(spec 5.0); this metric answers whether that is general.

Result over 200 worlds: <FILL IN FROM /tmp/hv-cascade.txt>"
```

---

## Task 2: The pure descent arithmetic

**Files:**
- Create: `domains/history/src/descent.rs`
- Modify: `domains/history/src/lib.rs` (add `pub mod descent;`)
- Test: `domains/history/tests/descent.rs`

**Interfaces:**
- Consumes: `RoleHandle` from `domains/history/src/flesh.rs` (existing, `pub struct RoleHandle(pub u64)`).
- Produces, for Task 3 and Task 5:
  - `pub fn remove(gap_years: f64, generation_length_years: f64) -> u32`
  - `pub fn ancestor(of: RoleHandle, steps: u32, seed: Seed) -> RoleHandle`
  - `pub enum Kinship { Sibling, Ancestor(u32) }`
  - `pub fn kinship(gap_years: f64, generation_length_years: f64) -> Kinship`

- [ ] **Step 1: Write the failing tests**

Create `domains/history/tests/descent.rs`:

```rust
//! The pure descent arithmetic: turning a founding-gap into a number of
//! generations, and walking a lazy chain of implied ancestors.

use hornvale_history::descent::{Kinship, ancestor, kinship, remove};
use hornvale_kernel::Seed;
use hornvale_history::flesh::RoleHandle;

#[test]
fn a_gap_shorter_than_half_a_generation_is_the_same_generation() {
    // Seed 42 measured 13% of founded-from edges at a zero remove: a
    // daughter community founded within a generation of its mother, whose
    // founder is therefore a SIBLING, not a descendant.
    assert_eq!(remove(0.0, 21.7), 0);
    assert_eq!(remove(10.0, 21.7), 0);
}

#[test]
fn a_gap_of_one_generation_is_one_remove() {
    assert_eq!(remove(21.7, 21.7), 1);
    assert_eq!(remove(25.0, 21.7), 1);
}

#[test]
fn the_measured_median_gap_resolves_to_the_measured_median_remove() {
    // Spec 1.1: median gap 50 y; goblin generation length 21.7 y; median
    // remove 2. This pins the plan's headline arithmetic to a real number.
    assert_eq!(remove(50.0, 21.7), 2);
}

#[test]
fn the_measured_maximum_gap_stays_bounded() {
    // Spec 1.1: max gap 975 y, max remove 32. 975 / 30.9 (hobgoblin) = 31.6.
    assert_eq!(remove(975.0, 30.9), 32);
}

#[test]
fn kinship_reports_sibling_at_zero_and_ancestor_above() {
    assert_eq!(kinship(0.0, 21.7), Kinship::Sibling);
    assert_eq!(kinship(50.0, 21.7), Kinship::Ancestor(2));
}

#[test]
fn a_nonpositive_generation_length_yields_zero_rather_than_infinity() {
    // An Ametabolic kind has generation_length None; a caller that
    // substitutes 0.0 must not produce NaN, Infinity, or a panic.
    assert_eq!(remove(50.0, 0.0), 0);
    assert_eq!(remove(50.0, -3.0), 0);
}

#[test]
fn a_negative_gap_yields_zero_rather_than_underflowing_the_u32() {
    // The bake never emits a daughter founded before its mother (measured
    // 0/1759 on seed 42), but `remove` is pub and must be total.
    assert_eq!(remove(-100.0, 21.7), 0);
}

#[test]
fn ancestor_is_deterministic_and_walks_away_from_its_start() {
    let seed = Seed(7);
    let h = RoleHandle(1234);
    assert_eq!(ancestor(h, 3, seed), ancestor(h, 3, seed));
    assert_ne!(ancestor(h, 1, seed), ancestor(h, 2, seed));
    assert_ne!(ancestor(h, 1, seed), h);
}

#[test]
fn ancestor_of_zero_steps_is_the_figure_themself() {
    let seed = Seed(7);
    let h = RoleHandle(1234);
    assert_eq!(ancestor(h, 0, seed), h);
}

#[test]
fn the_deepest_measured_chain_walks_without_collision() {
    // Spec 1.1: max remove 32. Every ancestor along the deepest real chain
    // must be distinct, or two forebears would share a name.
    let seed = Seed(42);
    let h = RoleHandle(99);
    let chain: Vec<RoleHandle> = (0..=32).map(|k| ancestor(h, k, seed)).collect();
    let mut sorted: Vec<u64> = chain.iter().map(|r| r.0).collect();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(sorted.len(), 33, "ancestor walk collided within 32 steps");
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo nextest run -p hornvale-history --test descent`
Expected: FAIL to compile — `unresolved import hornvale_history::descent`

- [ ] **Step 3: Write the implementation**

Create `domains/history/src/descent.rs`:

```rust
//! The pure descent arithmetic (The Namesake, spec §3.1).
//!
//! The ledger commits a community tree — `occ-founded-from` links a daughter
//! occupation to the mother it was settled from. It does **not** commit a
//! genealogy, and the two are not the same: seed 42's founding gaps run to a
//! median of 50 years and a maximum of 975, which no lifespan in the roster
//! supports as a parent-child link. What the edge encodes is *descent at an
//! unknown remove*, and this module derives the remove.
//!
//! Everything here is a total function of its arguments — no world, no
//! ledger, no `Stream` draw. The generation length arrives as a plain `f64`
//! because this crate is kernel-only and cannot read `hornvale-species`; the
//! composition root resolves it and passes it in, the same discipline
//! `MorphOptions` follows in `domains/language`.

use crate::flesh::RoleHandle;
use hornvale_kernel::Seed;

/// How two founders on either end of one `occ-founded-from` edge are related.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kinship {
    /// The same generation: the daughter was founded within half a
    /// generation of its mother, so its founder is a sibling rather than a
    /// descendant. Measured at 13% of seed-42's edges.
    Sibling,
    /// A descendant at this many generations' remove (always `>= 1`).
    Ancestor(u32),
}

/// The number of generations between two founders, given the years between
/// their communities' foundings and the people's generation length.
///
/// Rounds to nearest, so a gap under half a generation is zero. Total by
/// construction: a non-positive or non-finite `generation_length_years`
/// (an `Ametabolic` kind has no generation length at all) and a negative
/// `gap_years` both yield `0` rather than a panic, a NaN, or a `u32`
/// underflow.
/// type-audit: bare-ok(count: gap_years), bare-ok(count: generation_length_years), bare-ok(count: return)
pub fn remove(gap_years: f64, generation_length_years: f64) -> u32 {
    if !gap_years.is_finite()
        || !generation_length_years.is_finite()
        || generation_length_years <= 0.0
        || gap_years <= 0.0
    {
        return 0;
    }
    // `floor` stays intrinsic per the kernel's math discipline; this value
    // never feeds a draw, only a count.
    let generations = (gap_years / generation_length_years + 0.5).floor();
    if generations <= 0.0 {
        0
    } else if generations >= f64::from(u32::MAX) {
        u32::MAX
    } else {
        generations as u32
    }
}

/// [`remove`], read as a relationship.
/// type-audit: bare-ok(count: gap_years), bare-ok(count: generation_length_years)
pub fn kinship(gap_years: f64, generation_length_years: f64) -> Kinship {
    match remove(gap_years, generation_length_years) {
        0 => Kinship::Sibling,
        n => Kinship::Ancestor(n),
    }
}

/// The handle of the figure `steps` generations before `of`.
///
/// A lazy walk: the intermediate ancestors a long remove implies are never
/// materialised as records, only as handles, exactly as [`RoleHandle`]'s own
/// documentation intends ("a record can reference many unnamed roles without
/// ever materializing them until something actually observes one"). Seed 42's
/// median remove is 2 and its maximum 32, so the walk is short in practice.
///
/// `steps == 0` returns `of` unchanged — a figure is their own zeroth
/// ancestor — which is what makes [`Kinship::Sibling`] resolve to a shared
/// forebear without a special case at the call site.
///
/// The mix is the same splitmix-style arithmetic [`crate::flesh::persona_of`]
/// uses, iterated: pure bit operations over the arguments, no `Stream`, so it
/// consumes no draws and touches no stream-consumption-order contract.
/// type-audit: bare-ok(count: steps)
pub fn ancestor(of: RoleHandle, steps: u32, seed: Seed) -> RoleHandle {
    let mut h = of.0;
    for _ in 0..steps {
        let mut x = h ^ seed.0;
        x = x.wrapping_mul(0x9E37_79B9_7F4A_7C15);
        x ^= x >> 29;
        x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        x ^= x >> 32;
        h = x;
    }
    RoleHandle(h)
}
```

Add to `domains/history/src/lib.rs`, beside the existing `pub mod flesh;`:

```rust
pub mod descent;
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-history --test descent`
Expected: PASS, 10 tests

- [ ] **Step 5: Verify the tests would go RED under a mutation**

The Timekeeper's lesson: eight of sixteen defects in one campaign were authored by plan text, and only a mutation step caught the alarm that could never fire. Temporarily change `+ 0.5` to `+ 0.0` in `remove` and re-run. Expected: `the_measured_median_gap_resolves_to_the_measured_median_remove` FAILS (50/21.7 = 2.30 floors to 2 either way — **if it still passes, that test is not pinning rounding**; strengthen it with `assert_eq!(remove(33.0, 21.7), 2)`, which is 1.52 and distinguishes the two). Revert the mutation.

- [ ] **Step 6: Run the type audit and fmt**

```bash
cargo fmt
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo clippy -p hornvale-history --all-targets -- -D warnings
```

Expected: all clean. If the audit rejects a tag, re-read the grammar in Global Constraints — do not invent a class.

- [ ] **Step 7: Commit**

```bash
git add domains/history/src/descent.rs domains/history/src/lib.rs domains/history/tests/descent.rs
git commit -m "feat(history): the pure descent arithmetic

occ-founded-from is a descent edge at an unknown remove, not a parent-child
link: seed 42's founding gaps run median 50 y, max 975 y. remove() derives
the number of generations from the gap and the people's generation length;
ancestor() walks the implied chain lazily. Total functions over their
arguments -- no world, no Stream draw, so no consumption-order contract."
```

---

## Task 3: The descent graph at the composition root

**Files:**
- Create: `windows/worldgen/src/descent.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `mod descent;` and re-export)
- Test: `windows/worldgen/tests/descent_graph.rs`

**Interfaces:**
- Consumes: `hornvale_history::descent::{Kinship, ancestor, kinship, remove}` and `RoleHandle` (Task 2); `hornvale_history::{OCC_FOUNDED, OCC_FOUNDED_FROM, OCC_PEOPLE}` (existing); `hornvale_species::allometry::life_history` (existing).
- Produces, for Tasks 5 and 7:
  - `pub fn founder_of(world: &World, occupation: EntityId) -> RoleHandle`
  - `pub fn forebear_of(world: &World, occupation: EntityId) -> Option<(RoleHandle, Kinship)>`
  - `pub fn clan_root_of(world: &World, occupation: EntityId) -> EntityId`
  - `pub fn generation_length_of(world: &World, species: &str) -> Option<f64>`

- [ ] **Step 1: Write the failing tests**

Create `windows/worldgen/tests/descent_graph.rs`:

```rust
//! The person-descent graph: a reprojection of the committed community tree.

use hornvale_worldgen::{build, BuildDepth, clan_root_of, forebear_of, founder_of, generation_length_of};
use hornvale_history::descent::Kinship;

fn seed42() -> hornvale_kernel::World {
    build(hornvale_kernel::Seed(42), BuildDepth::Full, &[]).expect("seed 42 builds")
}

#[test]
fn every_occupation_has_a_founder_and_the_handle_is_stable() {
    let w = seed42();
    let occs = hornvale_worldgen::occupations_at(&w);
    assert!(!occs.is_empty(), "seed 42 bakes 1776 occupations");
    for o in occs.iter().take(50) {
        assert_eq!(founder_of(&w, o.id), founder_of(&w, o.id));
    }
}

#[test]
fn two_different_occupations_have_two_different_founders() {
    let w = seed42();
    let occs = hornvale_worldgen::occupations_at(&w);
    let a = founder_of(&w, occs[0].id);
    let b = founder_of(&w, occs[1].id);
    assert_ne!(a, b, "distinct occupations must not share a founder handle");
}

#[test]
fn the_clan_walk_terminates_for_every_occupation() {
    // The committed tree is acyclic, but this walk is pub and must not
    // assume it. Seed 42's deepest chain is 29 links.
    let w = seed42();
    for o in hornvale_worldgen::occupations_at(&w) {
        let root = clan_root_of(&w, o.id);
        assert!(
            hornvale_worldgen::occupations_at(&w).iter().any(|x| x.id == root),
            "clan root {root:?} is not an occupation in this world"
        );
    }
}

#[test]
fn a_genesis_occupation_is_its_own_clan_root_and_has_no_forebear() {
    let w = seed42();
    let genesis: Vec<_> = hornvale_worldgen::occupations_at(&w)
        .into_iter()
        .filter(|o| matches!(o.founded_from, hornvale_history::Founding::Genesis(_)))
        .collect();
    assert_eq!(genesis.len(), 17, "seed 42 has 17 genesis roots");
    for o in genesis {
        assert_eq!(clan_root_of(&w, o.id), o.id);
        assert!(forebear_of(&w, o.id).is_none());
    }
}

#[test]
fn some_edges_resolve_to_siblings_and_some_to_ancestors() {
    // Seed 42: 13% zero-hop. If EVERY edge came back one way, the kinship
    // derivation would be inert and this test is the guard against that.
    let w = seed42();
    let mut siblings = 0usize;
    let mut ancestors = 0usize;
    for o in hornvale_worldgen::occupations_at(&w) {
        match forebear_of(&w, o.id) {
            Some((_, Kinship::Sibling)) => siblings += 1,
            Some((_, Kinship::Ancestor(_))) => ancestors += 1,
            None => {}
        }
    }
    assert!(siblings > 0, "expected ~13% sibling edges, got none");
    assert!(ancestors > 0, "expected a majority of ancestor edges, got none");
}

#[test]
fn generation_length_is_resolved_per_species_and_differs_across_the_roster() {
    let w = seed42();
    let goblin = generation_length_of(&w, "goblin").expect("goblin has a generation length");
    let bugbear = generation_length_of(&w, "bugbear").expect("bugbear has a generation length");
    // Spec 1.1: goblin 21.7 y, bugbear 35.6 y.
    assert!((goblin - 21.7).abs() < 0.5, "goblin generation length was {goblin}");
    assert!((bugbear - 35.6).abs() < 0.5, "bugbear generation length was {bugbear}");
}
```

If `occupations_at` has a different signature or returns a different type, check it with `grep -n "pub fn occupations_at" -A 6 windows/worldgen/src/history_emit.rs` and adapt — do not guess.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo nextest run -p hornvale-worldgen --test descent_graph`
Expected: FAIL to compile — `cannot find function founder_of`

- [ ] **Step 3: Write the implementation**

Create `windows/worldgen/src/descent.rs`:

```rust
//! The person-descent graph (The Namesake, spec §3.1): a pure reprojection
//! of the committed community tree into relations between individuals.
//!
//! This lives at the composition root and nowhere else, for the
//! constitutional reason the history bake does: it must read
//! `hornvale-history` (the occupation facts) and `hornvale-species` (the
//! allometric generation length) together, and a domain may depend on
//! neither sibling.
//!
//! **Nothing here is committed.** No fact is added, no entity is minted; a
//! founder's handle is derived from the occupation's entity id and the world
//! seed, and the chain between two founders is derived from the gap between
//! their foundings. That is what keeps this campaign free of an epoch — see
//! spec §4, and note that the freedom ends the moment a *committed* value
//! (an eponymous toponym) cites one of these names.

use hornvale_history::descent::{Kinship, kinship};
use hornvale_history::flesh::RoleHandle;
use hornvale_kernel::{EntityId, Value, World};

/// The handle of the figure who founded `occupation`.
///
/// Derived from the occupation's own entity id and the world seed, so it is
/// stable across rebuilds and independent of mint order among *other*
/// occupations. Carries no ledger write.
pub fn founder_of(world: &World, occupation: EntityId) -> RoleHandle {
    // Mix the entity id into the seed the same way `persona_of` mixes a
    // handle, so founder handles are drawn from the same space as the
    // ancestors `descent::ancestor` walks to.
    let mut x = (occupation.0 as u64) ^ world.seed.0.rotate_left(17);
    x = x.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    x ^= x >> 29;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    RoleHandle(x ^ (x >> 32))
}

/// The people occupying `occupation`, as a `KindId` label.
fn people_of(world: &World, occupation: EntityId) -> Option<String> {
    match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_PEOPLE)?
    {
        Value::Text(t) => Some(t.clone()),
        _ => None,
    }
}

/// The standard year `occupation` was founded.
///
/// Note the unit: the bake writes `BakeConfig::start_year`/`end_year`
/// straight through, so this fact is in **years**, notwithstanding the
/// "standard day" wording on `OCC_FOUNDED`'s own doc comment. The
/// inconsistency is recorded as a followup in the spec (§7.2); the arithmetic
/// throughout the history subsystem is self-consistent in years.
fn founded_year(world: &World, occupation: EntityId) -> Option<f64> {
    match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_FOUNDED)?
    {
        Value::Number(n) => Some(*n),
        _ => None,
    }
}

/// The occupation `occupation` was settled from, if it was settled from one.
fn mother_of(world: &World, occupation: EntityId) -> Option<EntityId> {
    match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_FOUNDED_FROM)?
    {
        Value::Entity(e) => Some(*e),
        // A `Number` value is `Founding::Genesis(CellId)` — a root, not a
        // parent. See `windows/almanac::history`'s decoder, which this
        // mirrors.
        _ => None,
    }
}

/// A people's generation length in years, from the shipped allometry.
///
/// `None` for an `Ametabolic` kind (a construct has no mass-derived life
/// history) or a species absent from this world's roster.
/// type-audit: bare-ok(identifier-text: species), bare-ok(count: return)
pub fn generation_length_of(world: &World, species: &str) -> Option<f64> {
    let wc = crate::WorldComponents::assemble().ok()?;
    let _ = world;
    let bio = wc.biosphere.get(&hornvale_kernel::KindId(species))?;
    hornvale_species::allometry::life_history(bio.mass, bio.metabolic_class)
        .generation_length
        .map(|y| y.get())
}

/// The figure `occupation`'s founder descends from — the founder of the
/// community it was settled from — together with how they are related.
///
/// `None` for a genesis occupation, which has no mother community.
pub fn forebear_of(world: &World, occupation: EntityId) -> Option<(RoleHandle, Kinship)> {
    let mother = mother_of(world, occupation)?;
    let child_year = founded_year(world, occupation)?;
    let mother_year = founded_year(world, mother)?;
    let species = people_of(world, occupation)?;
    // A people with no generation length (Ametabolic, or unrostered) cannot
    // have its remove derived; fall back to `Sibling` rather than inventing
    // a generation length, so the relation stays honest about what is known.
    let gl = generation_length_of(world, &species).unwrap_or(0.0);
    Some((
        founder_of(world, mother),
        kinship(child_year - mother_year, gl),
    ))
}

/// The genesis occupation at the root of `occupation`'s descent chain — the
/// clan.
///
/// Walks `occ-founded-from` to its root. The committed tree is acyclic, but
/// this function does not assume it: the walk is bounded by the number of
/// occupations in the world and returns the last node reached rather than
/// looping, so a malformed ledger degrades instead of hanging.
pub fn clan_root_of(world: &World, occupation: EntityId) -> EntityId {
    let bound = world.ledger.find(hornvale_history::IS_OCCUPATION).count() + 1;
    let mut here = occupation;
    for _ in 0..bound {
        match mother_of(world, here) {
            Some(up) => here = up,
            None => return here,
        }
    }
    here
}
```

Add to `windows/worldgen/src/lib.rs`:

```rust
mod descent;
pub use descent::{clan_root_of, forebear_of, founder_of, generation_length_of};
```

`hornvale_species::allometry::life_history(mass, class) -> LifeHistory` is verified, as is `Ledger::value_of(subject, predicate) -> Option<&Value>`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-worldgen --test descent_graph`
Expected: PASS, 6 tests. This builds a full seed-42 world per test, so allow a few minutes.

- [ ] **Step 5: Verify the sibling/ancestor split is not vacuous**

`some_edges_resolve_to_siblings_and_some_to_ancestors` is the anti-vacuity guard. Temporarily change `kinship`'s `0 => Kinship::Sibling` arm to `_ => Kinship::Sibling` and re-run. Expected: that test FAILS on `expected a majority of ancestor edges, got none`. Revert.

- [ ] **Step 6: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/src/descent.rs windows/worldgen/src/lib.rs windows/worldgen/tests/descent_graph.rs
git commit -m "feat(worldgen): the person-descent graph as a pure reprojection

Founders, forebears, and clan roots derived from the committed community
tree plus the shipped per-species generation length. No fact added, no
entity minted, no Stream drawn -- the graph is a total function of what the
ledger already carries, which is what keeps this campaign epoch-free."
```

---

## Task 4: The anthroponymic schema

**Files:**
- Create: `domains/language/src/anthroponym.rs`
- Modify: `domains/language/src/naming.rs` (add `NameKind::Person`)
- Modify: `domains/language/src/lib.rs` (`pub mod anthroponym;`, re-exports, `stream_labels()` entry)
- Test: `domains/language/tests/anthroponym.rs`

**Interfaces:**
- Consumes: nothing from Tasks 1–3 (this layer is kernel-only and knows nothing of history).
- Produces, for Tasks 5 and 6:
  - `pub struct PersonName { pub elements: Vec<NameElement> }`
  - `pub struct NameElement { pub source: ElementSource, pub author: Author, pub conferred: Option<f64> }`
  - `pub enum ElementSource { Stem, Gloss(GlossBasis), Relation(Cite), Index(IndexBasis), Deed }`
  - `pub enum Cite { Parent, Clan, Community, Place, Deity, Mentor, Child }`
  - `pub enum IndexBasis { BirthOrder, Generation }`
  - `pub enum Author { Kin, Community, Witnesses, Institution, Selfward, Outsiders, Inherent }`
  - `pub enum GlossBasis { Trade, Bearing, Origin }`
  - `pub struct NamePattern { pub elements: Vec<(ElementSource, Author)> }`
  - `NameKind::Person` variant

- [ ] **Step 1: Write the failing tests**

Create `domains/language/tests/anthroponym.rs`:

```rust
//! The anthroponymic schema: what a personal name is made of.

use hornvale_language::anthroponym::{
    Author, Cite, ElementSource, GlossBasis, IndexBasis, NameElement, NamePattern, PersonName,
};
use hornvale_language::NameKind;

#[test]
fn person_is_a_distinct_name_kind_with_its_own_seed_label() {
    // The label is a save-format contract: it is folded into the derive
    // path, so it must be "person" and must differ from every existing kind.
    let labels = [
        NameKind::Settlement.label_for_test(),
        NameKind::Deity.label_for_test(),
        NameKind::Epithet.label_for_test(),
        NameKind::Person.label_for_test(),
    ];
    assert_eq!(labels[3], "person");
    let mut sorted = labels.to_vec();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(sorted.len(), 4, "NameKind labels must be distinct");
}

#[test]
fn a_name_is_an_ordered_list_and_order_is_meaningful() {
    let given = NameElement {
        source: ElementSource::Stem,
        author: Author::Kin,
        conferred: None,
    };
    let patronymic = NameElement {
        source: ElementSource::Relation(Cite::Parent),
        author: Author::Kin,
        conferred: None,
    };
    let a = PersonName { elements: vec![given.clone(), patronymic.clone()] };
    let b = PersonName { elements: vec![patronymic, given] };
    assert_ne!(a, b, "given-first and patronymic-first are different systems");
}

#[test]
fn every_element_source_and_author_is_representable() {
    // A closed-vocabulary guard: if a variant is added without updating the
    // consumers, this forces the question at compile time rather than
    // letting a new source silently render as nothing.
    let sources = [
        ElementSource::Stem,
        ElementSource::Gloss(GlossBasis::Trade),
        ElementSource::Gloss(GlossBasis::Bearing),
        ElementSource::Gloss(GlossBasis::Origin),
        ElementSource::Relation(Cite::Parent),
        ElementSource::Relation(Cite::Clan),
        ElementSource::Relation(Cite::Community),
        ElementSource::Relation(Cite::Place),
        ElementSource::Relation(Cite::Deity),
        ElementSource::Relation(Cite::Mentor),
        ElementSource::Relation(Cite::Child),
        ElementSource::Index(IndexBasis::BirthOrder),
        ElementSource::Index(IndexBasis::Generation),
        ElementSource::Deed,
    ];
    assert_eq!(sources.len(), 14);
    let authors = [
        Author::Kin,
        Author::Community,
        Author::Witnesses,
        Author::Institution,
        Author::Selfward,
        Author::Outsiders,
        Author::Inherent,
    ];
    assert_eq!(authors.len(), 7);
}

#[test]
fn a_pattern_with_no_elements_is_a_mononym_not_an_error() {
    // A culture may name with a single given name and nothing else. That is
    // Indonesia and Ancient Egypt, not a degenerate case.
    let p = NamePattern { elements: vec![(ElementSource::Stem, Author::Kin)] };
    assert_eq!(p.elements.len(), 1);
}

#[test]
fn authorship_determines_revocability() {
    // The dimension the surface vocabulary hides: an epithet conferred by
    // outsiders cannot be revoked by its bearer; a self-assumed name can.
    assert!(!Author::Outsiders.revocable_by_bearer());
    assert!(!Author::Inherent.revocable_by_bearer());
    assert!(Author::Selfward.revocable_by_bearer());
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo nextest run -p hornvale-language --test anthroponym`
Expected: FAIL to compile — `unresolved import hornvale_language::anthroponym`

- [ ] **Step 3: Add the `Person` name kind**

In `domains/language/src/naming.rs`, add the variant to `NameKind` (around line 91):

```rust
    /// A person: the given-name element of a personal name — a bare stem,
    /// drawn like a settlement's but off its own seed path so that adding
    /// personal naming to a world reseeds nothing that already exists.
    Person,
```

and the arm to `label()` (around line 107):

```rust
            NameKind::Person => "person",
```

and to the syllable draw in `draw_stem` (around line 1130) — a given name is 2–3 syllables like a settlement, not deity-weighted:

```rust
            NameKind::Person => self.draw_syllables(stream, 2, 3, false),
```

Add a test-only label accessor so the test above can assert the contract without making `label` public:

```rust
impl NameKind {
    /// The seed-path label, exposed for the save-format-contract test in
    /// `tests/anthroponym.rs`. Not part of the ordinary API.
    #[doc(hidden)]
    pub fn label_for_test(self) -> &'static str {
        self.label()
    }
}
```

- [ ] **Step 4: Write the schema module**

Create `domains/language/src/anthroponym.rs`:

```rust
//! The anthroponymic schema (The Namesake, spec §3.2): what a personal name
//! is made of.
//!
//! A name is an **ordered list of elements**, and each element is a *source*
//! (where its material comes from) paired with an *author* (who conferred
//! it). Those two axes together span the human anthroponymic record —
//! patronymics, clan names, occupational and toponymic bynames, birth-order
//! and generation names, deed-names, teknonyms — and the ergonomic subset of
//! the speculative-fiction record, including the true name, which is simply
//! the element no one authored.
//!
//! This module is plain data and kernel-only. It never learns which people a
//! name belongs to or what its relations resolve to; the composition root
//! derives the pattern and supplies the rendered material, the same
//! discipline [`crate::MorphOptions`] and [`crate::SiteConcepts`] follow.

/// Where a name element's material comes from.
///
/// The variant order is a **save-format contract** where a pattern is
/// serialized by index; add new variants at the end.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ElementSource {
    /// Drawn phonology: the given name, a bare stem in the culture's own
    /// sound system.
    Stem,
    /// Compounded concepts from the culture's lexicon — a descriptive
    /// byname.
    Gloss(GlossBasis),
    /// A walk to another entity: the patronymic, the clan name, the
    /// toponymic.
    Relation(Cite),
    /// A position in a sequence: Roman `Quintus`, Balinese `Wayan`, a
    /// Chinese generation character.
    Index(IndexBasis),
    /// An event from the world's committed history — the deed-name.
    Deed,
}

/// What a descriptive byname describes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GlossBasis {
    /// What the bearer does: Smith, Müller, Kovács.
    Trade,
    /// What the bearer is like: Erik the Red, Æthelred the Unready.
    Bearing,
    /// Where the bearer is from, as a quality rather than a named place.
    Origin,
}

/// Which relation a `Relation` element walks to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cite {
    /// The bearer's forebear: the patronymic (Jónsdóttir, ibn, -ovich).
    Parent,
    /// The root of the bearer's descent chain: the clan name (Mac-, Ó-).
    Clan,
    /// The community the bearer belongs to.
    Community,
    /// A named place: the nisba, `da Vinci`.
    Place,
    /// The deity the bearer's community holds foremost — a theophoric name.
    Deity,
    /// The bearer's teacher rather than their parent: the transmission
    /// lineage, the anthroponymic twin of mentorship-distance drift.
    Mentor,
    /// The bearer's **child** — teknonymy, Arabic *Abu Bakr*, "father of
    /// Bakr". Assignment flows backward: the parent is named for the child,
    /// so this element cannot exist until the child does.
    Child,
}

/// Which sequence an `Index` element counts along.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexBasis {
    /// Position among siblings.
    BirthOrder,
    /// Depth in the descent chain.
    Generation,
}

/// Who conferred an element.
///
/// The load-bearing axis: authorship determines whether an element can be
/// revoked, who may confer another, and whether it can be *false*.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Author {
    /// Given at birth by family.
    Kin,
    /// Conferred by the community, typically at coming of age.
    Community,
    /// Conferred by those who saw the deed.
    Witnesses,
    /// Conferred by an office: regnal, monastic, caste.
    Institution,
    /// Assumed by the bearer — the only author that can lie.
    Selfward,
    /// Applied from outside and not revocable by the bearer: the exonym, the
    /// epithet, the slur.
    Outsiders,
    /// Conferred by nobody. The name is discovered rather than given — the
    /// true name.
    Inherent,
}

impl Author {
    /// Whether the bearer may drop an element this author conferred.
    ///
    /// Only a self-assumed element is freely revocable. A kin- or
    /// institution-given element is revocable by *that* author, not by the
    /// bearer, which is why they return `false` here.
    /// type-audit: bare-ok(flag: return)
    pub fn revocable_by_bearer(self) -> bool {
        matches!(self, Author::Selfward)
    }
}

/// One element of a personal name.
/// type-audit: bare-ok(count: conferred)
#[derive(Clone, Debug, PartialEq)]
pub struct NameElement {
    /// Where this element's material comes from.
    pub source: ElementSource,
    /// Who conferred it.
    pub author: Author,
    /// The standard year it was conferred; `None` for conferred-at-birth.
    pub conferred: Option<f64>,
}

/// A figure's full name: every element they have accrued, in cultural order.
///
/// The *full* name is rarely what is uttered — see
/// [`crate::anthroponym::render`], which returns the shortest prefix that
/// disambiguates at the scope of the utterance.
#[derive(Clone, Debug, PartialEq)]
pub struct PersonName {
    /// The elements, in the order this culture speaks them.
    pub elements: Vec<NameElement>,
}

/// A culture's naming rule: which elements, from which sources, by which
/// authors, in what order.
///
/// Derived from the culture's `SocietyVector` at the composition root, never
/// authored per-culture — the anti-lookup-table discipline (decision 0021).
#[derive(Clone, Debug, PartialEq)]
pub struct NamePattern {
    /// The elements this culture's names carry, in order.
    pub elements: Vec<(ElementSource, Author)>,
}
```

Add to `domains/language/src/lib.rs`:

```rust
pub mod anthroponym;
```

- [ ] **Step 5: Add the stream label**

`NameKind::Person` introduces a new seed path, so it must be declared in `stream_labels()` or the generated manifest goes stale. In `domains/language/src/lib.rs`, beside the `name/settlement/v3` entry (around line 559):

```rust
        (
            "language/<species>/name/person",
            "the given-name element of a personal name (The Namesake), salted by the bearer's role handle: a bare 2-3 syllable stem. No epoch suffix — this label is new, not a regeneration of an existing one (decision 0084: an epoch is declared only when a derivation moved)",
        ),
```

- [ ] **Step 6: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-language --test anthroponym`
Expected: PASS, 5 tests

- [ ] **Step 7: Regenerate the stream manifest and diff it**

A new `stream_labels()` entry changes a committed book page. This step is the one most often forgotten.

```bash
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git diff --stat book/src/reference/
```

Expected: exactly one file changed, exactly one row added. **If any other row moved, stop** — a reordering means an existing label changed, which is a save-format break.

- [ ] **Step 8: Confirm no other artifact drifted**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/laboratory/ docs/audits/
```

Expected: clean. `NameKind::Person` is additive and nothing constructs one yet, so no world's output may change. A non-empty diff here is a real finding, not something to rebaseline away.

- [ ] **Step 9: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-language --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/language/src/anthroponym.rs domains/language/src/naming.rs \
        domains/language/src/lib.rs domains/language/tests/anthroponym.rs \
        book/src/reference/stream-manifest-generated.md
git commit -m "feat(language): the anthroponymic schema and NameKind::Person

A name is an ordered list of (source, author) elements. The two axes span
the human anthroponymic record and the ergonomic half of the speculative
one -- the true name is the element no one authored. Adds the person seed
label and regenerates the stream manifest; every other artifact byte-identical."
```

---

## Task 5: Derive the pattern from `SocietyVector`

**Files:**
- Modify: `windows/worldgen/src/descent.rs` (add `name_pattern`)
- Test: `windows/worldgen/tests/name_pattern.rs`

**Interfaces:**
- Consumes: `hornvale_language::anthroponym::{Author, Cite, ElementSource, GlossBasis, NamePattern}` (Task 4); `hornvale_species::{MindVector, SocietyVector, Sociality, StatusBasis}` (existing).
- Produces, for Tasks 6 and 7: `pub fn name_pattern(mind: &MindVector, society: &SocietyVector) -> NamePattern`

- [ ] **Step 1: Write the failing tests**

Create `windows/worldgen/tests/name_pattern.rs`:

```rust
//! The per-culture naming pattern, derived from society rather than authored.

use hornvale_language::anthroponym::{Author, Cite, ElementSource};
use hornvale_species::{Sociality, SocietyVector, StatusBasis};
use hornvale_worldgen::name_pattern;

fn society(sociality: Sociality, status_basis: StatusBasis, radius: f64) -> SocietyVector {
    SocietyVector { sociality, status_basis, in_group_radius: radius }
}

fn mind() -> hornvale_species::MindVector {
    // MindVector has NO `baseline()` constructor (SocietyVector does; the
    // asymmetry is real). Construct it literally — the pattern derivation
    // does not read these values today, but an honest test supplies real
    // ones rather than zeroes.
    hornvale_species::MindVector {
        threat_response: 0.5,
        deliberation_latency: 0.5,
        time_horizon: 0.5,
    }
}

#[test]
fn every_pattern_opens_with_a_given_name() {
    for soc in [Sociality::Hierarchic, Sociality::Communal] {
        for sb in [StatusBasis::Rank, StatusBasis::Knowledge, StatusBasis::Generosity] {
            let p = name_pattern(&mind(), &society(soc, sb, 0.5));
            assert_eq!(
                p.elements.first().map(|e| e.0),
                Some(ElementSource::Stem),
                "every culture gives a given name first"
            );
        }
    }
}

#[test]
fn descent_legitimates_in_a_hierarchic_people() {
    let p = name_pattern(&mind(), &society(Sociality::Hierarchic, StatusBasis::Rank, 0.5));
    assert!(
        p.elements.iter().any(|(s, _)| matches!(
            s,
            ElementSource::Relation(Cite::Parent) | ElementSource::Relation(Cite::Clan)
        )),
        "a hierarchic people cites descent"
    );
}

#[test]
fn deeds_legitimate_in_a_communal_people() {
    let p = name_pattern(&mind(), &society(Sociality::Communal, StatusBasis::Generosity, 0.5));
    assert!(
        p.elements.iter().any(|(s, _)| matches!(
            s,
            ElementSource::Deed | ElementSource::Relation(Cite::Community)
        )),
        "a communal people cites what you did, not who you came from"
    );
}

#[test]
fn knowledge_status_cites_the_mentor_not_the_parent() {
    let p = name_pattern(&mind(), &society(Sociality::Hierarchic, StatusBasis::Knowledge, 0.5));
    assert!(
        p.elements.iter().any(|(s, _)| *s == ElementSource::Relation(Cite::Mentor)),
        "where craft earns standing, the transmission lineage is the lineage"
    );
}

#[test]
fn an_insular_people_carries_fewer_elements_than_an_expansive_one() {
    let insular = name_pattern(&mind(), &society(Sociality::Hierarchic, StatusBasis::Rank, 0.0));
    let expansive = name_pattern(&mind(), &society(Sociality::Hierarchic, StatusBasis::Rank, 1.0));
    assert!(
        insular.elements.len() < expansive.elements.len(),
        "everyone knows everyone in an insular people; a wide 'us' needs more to disambiguate"
    );
}

#[test]
fn the_roster_produces_at_least_three_distinct_signatures() {
    // Preregistered criterion 5.1(1). Measured here on the authored society
    // vectors rather than over seeds, so a regression is caught in the fast
    // gate rather than only in the study.
    let combos = [
        (Sociality::Hierarchic, StatusBasis::Rank),
        (Sociality::Hierarchic, StatusBasis::Knowledge),
        (Sociality::Communal, StatusBasis::Generosity),
        (Sociality::Communal, StatusBasis::Knowledge),
    ];
    let mut sigs: Vec<Vec<(ElementSource, Author)>> = combos
        .iter()
        .map(|(so, sb)| name_pattern(&mind(), &society(*so, *sb, 0.5)).elements)
        .collect();
    sigs.sort();
    sigs.dedup();
    assert!(sigs.len() >= 3, "expected >= 3 distinct signatures, got {}", sigs.len());
}
```

`ElementSource` and `Author` need `PartialOrd`/`Ord` for the `sort`/`dedup` above — add `PartialOrd, Ord` to their derives in Task 4's module if the compiler asks. Note in the doc comment that variant order is then a contract.

`MindVector` is `{ threat_response, deliberation_latency, time_horizon }`, all bare `f64` ratios in `[0, 1]` — verified, not guessed.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo nextest run -p hornvale-worldgen --test name_pattern`
Expected: FAIL to compile — `cannot find function name_pattern`

- [ ] **Step 3: Write the implementation**

Append to `windows/worldgen/src/descent.rs`:

```rust
/// The naming pattern a culture uses, derived from its society vector.
///
/// **Derived, never authored** (spec §3.3). A per-culture naming table would
/// be exactly the lookup table decision 0021 forecloses; the same discipline
/// already produces `morph_options`' honorific flag from
/// `StatusBasis::Rank`, and The Bane's whole threat niche from what the
/// creature already is.
///
/// The mapping:
///
/// - `Hierarchic` cites **descent** — who you came from legitimates you.
/// - `Communal` cites the **community or the deed** — what you did does.
/// - `Rank` adds a descent citation and (through `morph_options`) an
///   honorific; `Knowledge` cites the **mentor**, because where craft earns
///   standing the transmission lineage *is* the lineage; `Generosity` cites
///   the deed.
/// - `in_group_radius` sets how many elements the pattern carries: an
///   insular people needs fewer to pick someone out.
pub fn name_pattern(
    mind: &hornvale_species::MindVector,
    society: &hornvale_species::SocietyVector,
) -> hornvale_language::anthroponym::NamePattern {
    use hornvale_language::anthroponym::{Author, Cite, ElementSource, GlossBasis, NamePattern};
    let _ = mind;

    // Every culture gives a given name. It is the only universal element.
    let mut elements = vec![(ElementSource::Stem, Author::Kin)];

    // What legitimates a person here.
    match society.status_basis {
        hornvale_species::StatusBasis::Rank => {
            elements.push((ElementSource::Relation(Cite::Parent), Author::Kin));
        }
        hornvale_species::StatusBasis::Knowledge => {
            elements.push((ElementSource::Relation(Cite::Mentor), Author::Institution));
        }
        hornvale_species::StatusBasis::Generosity => {
            elements.push((ElementSource::Deed, Author::Witnesses));
        }
    }

    // How authority is shaped.
    match society.sociality {
        hornvale_species::Sociality::Hierarchic => {
            elements.push((ElementSource::Relation(Cite::Clan), Author::Kin));
        }
        hornvale_species::Sociality::Communal => {
            elements.push((ElementSource::Relation(Cite::Community), Author::Community));
        }
    }

    // How wide "us" is drawn decides how much disambiguation a name must
    // carry on its face. The threshold is the midpoint of the [0,1] axis,
    // the same place `SocietyVector::baseline` sits.
    if society.in_group_radius > 0.5 {
        elements.push((
            ElementSource::Gloss(GlossBasis::Bearing),
            Author::Outsiders,
        ));
    }
    if society.in_group_radius < 0.5 {
        // An insular people drops the outermost citation: everyone already
        // knows which clan or community you belong to.
        elements.pop();
    }

    NamePattern { elements }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-worldgen --test name_pattern`
Expected: PASS, 6 tests

- [ ] **Step 5: Verify the signature test is not vacuous**

Temporarily make `name_pattern` ignore `society` entirely and return only `vec![(ElementSource::Stem, Author::Kin)]`. Re-run. Expected: `the_roster_produces_at_least_three_distinct_signatures`, `descent_legitimates_in_a_hierarchic_people`, `deeds_legitimate_in_a_communal_people`, and `knowledge_status_cites_the_mentor_not_the_parent` all FAIL. Revert.

- [ ] **Step 6: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/src/descent.rs windows/worldgen/tests/name_pattern.rs
git commit -m "feat(worldgen): derive the naming pattern from SocietyVector

Hierarchic peoples cite descent, communal peoples cite the community or the
deed; Knowledge status cites the mentor rather than the parent, because
where craft earns standing the transmission lineage is the lineage. Derived,
never authored -- the anti-lookup-table discipline (decision 0021)."
```

---

## Task 6: Render a name, shortest prefix first

**Files:**
- Modify: `domains/language/src/anthroponym.rs` (add `render`, `Scope`)
- Test: `domains/language/tests/anthroponym_render.rs`

**Interfaces:**
- Consumes: `PersonName`, `NameElement` (Task 4).
- Produces, for Task 7:
  - `pub struct Rendered { pub parts: Vec<String> }`
  - `pub fn render(name: &Rendered, competitors: &[Rendered]) -> String`

- [ ] **Step 1: Write the failing tests**

Create `domains/language/tests/anthroponym_render.rs`:

```rust
//! The shortest-prefix render rule (decision 0024, generalized off
//! settlements): a name is uttered as the shortest element prefix that
//! disambiguates at the scope of the utterance.

use hornvale_language::anthroponym::{render, Rendered};

fn r(parts: &[&str]) -> Rendered {
    Rendered { parts: parts.iter().map(|s| s.to_string()).collect() }
}

#[test]
fn alone_in_its_scope_a_name_renders_as_its_given_name() {
    let grushak = r(&["Grushak", "Bolgson", "Ironhand"]);
    assert_eq!(render(&grushak, &[]), "Grushak");
}

#[test]
fn a_collision_on_the_given_name_extends_by_exactly_one_element() {
    let a = r(&["Grushak", "Bolgson", "Ironhand"]);
    let b = r(&["Grushak", "Nardson", "Redeye"]);
    assert_eq!(render(&a, &[b.clone()]), "Grushak Bolgson");
    assert_eq!(render(&b, &[a]), "Grushak Nardson");
}

#[test]
fn a_collision_two_deep_extends_twice() {
    let a = r(&["Grushak", "Bolgson", "Ironhand"]);
    let b = r(&["Grushak", "Bolgson", "Redeye"]);
    assert_eq!(render(&a, &[b]), "Grushak Bolgson Ironhand");
}

#[test]
fn two_identical_names_render_identically_rather_than_looping() {
    // Real anthroponymy collides and that is correct (decision 0024 accepts
    // a measured collision rate for settlements). Two figures with the same
    // full name are genuinely ambiguous; the renderer must terminate and
    // return the full stack, not spin looking for a distinguishing element.
    let a = r(&["Grushak", "Bolgson"]);
    let b = r(&["Grushak", "Bolgson"]);
    assert_eq!(render(&a, &[b]), "Grushak Bolgson");
}

#[test]
fn an_empty_name_renders_empty_rather_than_panicking() {
    assert_eq!(render(&r(&[]), &[]), "");
}

#[test]
fn a_competitor_that_shares_no_prefix_forces_no_extension() {
    let a = r(&["Grushak", "Bolgson"]);
    let b = r(&["Nard", "Vekson"]);
    assert_eq!(render(&a, &[b]), "Grushak");
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo nextest run -p hornvale-language --test anthroponym_render`
Expected: FAIL to compile — `cannot find function render`

- [ ] **Step 3: Write the implementation**

Append to `domains/language/src/anthroponym.rs`:

```rust
/// A name whose elements have been resolved to actual words by the
/// composition root — the form this module can render.
///
/// The schema above says what a name is *made of*; this says what it *reads
/// as*. Kept separate because resolving `Cite::Parent` to a word requires
/// walking the descent graph, which is a composition-root concern.
/// type-audit: bare-ok(identifier-text: parts)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rendered {
    /// The rendered words, in the culture's own order.
    pub parts: Vec<String>,
}

/// Render `name` as the shortest element prefix that distinguishes it from
/// every name in `competitors`.
///
/// This is decision 0024 — "uniqueness is a property of a reference, not of
/// a name" — generalized off settlements. Personal names collide far harder
/// than toponyms and *should*: Earth's commonest given name is borne by tens
/// of millions. Name length is therefore **computed at the point of
/// utterance**, never authored into the name. The structure is git's
/// shortest-unique-SHA prefix, DNS search-domain suffixing, and *E. coli*
/// after the first *Escherichia coli*.
///
/// `competitors` is the scope: pass the household to be addressed by a given
/// name, the settlement to be addressed by given-plus-byname, the region for
/// the full stack. A competitor identical to `name` cannot be
/// distinguished from it at any length, so the full stack is returned and
/// the ambiguity is left standing rather than papered over with invented
/// entropy — the same choice 0024 made for settlements.
/// type-audit: bare-ok(prose: return)
pub fn render(name: &Rendered, competitors: &[Rendered]) -> String {
    for take in 1..=name.parts.len() {
        let prefix = &name.parts[..take];
        let ambiguous = competitors
            .iter()
            .any(|c| c.parts.len() >= take && &c.parts[..take] == prefix);
        if !ambiguous {
            return prefix.join(" ");
        }
    }
    name.parts.join(" ")
}
```

Note the loop's exit: it tries every prefix length and falls through to the full name, so it terminates on an exact duplicate rather than searching forever.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-language --test anthroponym_render`
Expected: PASS, 6 tests

- [ ] **Step 5: Verify the rule is not vacuous**

Temporarily replace the body with `name.parts.join(" ")`. Expected: `alone_in_its_scope_a_name_renders_as_its_given_name`, `a_collision_on_the_given_name_extends_by_exactly_one_element`, and `a_competitor_that_shares_no_prefix_forces_no_extension` all FAIL. Then replace it with `name.parts.first().cloned().unwrap_or_default()` — expected: the two collision tests FAIL. Revert. Both mutations must be tried: the first proves the rule shortens, the second proves it extends.

- [ ] **Step 6: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-language --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/language/src/anthroponym.rs domains/language/tests/anthroponym_render.rs
git commit -m "feat(language): render a name as its shortest unambiguous prefix

Decision 0024 generalized off settlements: uniqueness is a property of a
reference, not of a name. Name length is computed at the point of utterance
rather than authored, so an exact duplicate returns the full stack and
leaves the ambiguity standing instead of adding entropy."
```

---

## Task 7: Measure the two preregistered claims

**Files:**
- Modify: `windows/lab/src/metrics.rs` (four metrics)
- Create: `studies/the-namesake.study.json`
- Test: `windows/lab/tests/namesake_metrics.rs`

**Interfaces:**
- Consumes: `name_pattern`, `founder_of`, `forebear_of` (Tasks 3, 5); `render` (Task 6).
- Produces: the numbers §5.1 and §5.2 are judged against.

- [ ] **Step 1: Write the failing test**

Create `windows/lab/tests/namesake_metrics.rs`:

```rust
use hornvale_lab::metrics::registry;

#[test]
fn the_four_namesake_metrics_are_registered() {
    let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
    for expected in [
        "name-pattern-signatures",
        "name-people-recoverability",
        "name-prefix-settlement-scope",
        "name-prefix-region-scope",
    ] {
        assert!(names.contains(&expected), "missing metric {expected}");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-lab --test namesake_metrics`
Expected: FAIL — `missing metric name-pattern-signatures`

- [ ] **Step 3: Write the four metrics**

Add to `windows/lab/src/metrics.rs`. Each follows `cascade_rules_fired`'s shape from Task 1 — read that function before writing these, and reuse its `v.components().biosphere` roster scan.

```rust
/// How many DISTINCT naming-pattern signatures this world's placed peoples
/// produce (The Namesake, preregistered criterion §5.1(1); target >= 3).
///
/// A signature is the ordered list of `(ElementSource, Author)` pairs
/// `name_pattern` derives from a people's society vector. If every people
/// produced the same signature the naming system would be one shape with
/// cosmetic variation, which is the failure this metric exists to catch.
fn name_pattern_signatures(v: &FullView) -> MetricValue {
    let wc = v.components();
    let mut sigs: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for kind in hornvale_worldgen::placed_peoples(v.world()) {
        let (Some(mind), Some(society)) = (
            wc.psyche.get(&hornvale_kernel::KindId(kind.0)),
            wc.society.get(&hornvale_kernel::KindId(kind.0)),
        ) else {
            continue;
        };
        let p = hornvale_worldgen::name_pattern(mind, society);
        sigs.insert(format!("{:?}", p.elements));
    }
    if sigs.is_empty() {
        return MetricValue::Absent;
    }
    MetricValue::Number(sigs.len() as f64)
}
```

Write the remaining three by the same pattern:

- `name-people-recoverability` — §5.1(2). For each placed people, derive its signature; the metric is the share of peoples whose signature is unique in this world (a directly interpretable stand-in for "recoverable above chance", and one that needs no classifier). `Absent` if fewer than two peoples are placed.
- `name-prefix-settlement-scope` — §5.2(1). Over every settlement, build the `Rendered` names of the founders of the occupations at that site, and report the share that render to exactly one element.
- `name-prefix-region-scope` — §5.2(2). The same over all founders in the world; report the **median** element count rendered, and separately assert in the study readout that fewer than 50% need the full stack.

Register all four with `SummaryKind::Numeric { bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0] }` for the two share metrics and `&[1.0, 2.0, 3.0, 4.0, 5.0]` for the two count metrics. Give each a `doc` that names its spec criterion, as the existing metrics name their spec sections.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo nextest run -p hornvale-lab --test namesake_metrics`
Expected: PASS

- [ ] **Step 5: Write the study**

Create `studies/the-namesake.study.json`:

```json
{ "name": "the-namesake",
  "description": "The Namesake's two preregistered claims (spec 5.1, 5.2) over 200 unselected worlds: whether the SocietyVector-derived naming patterns produce distinguishable cultures, and whether the shortest-prefix render rule earns its keep at settlement and region scope.",
  "seeds": { "from": 0, "count": 200 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": ["name-pattern-signatures", "name-people-recoverability",
              "name-prefix-settlement-scope", "name-prefix-region-scope"] }
```

- [ ] **Step 6: Run the study, capturing to a file**

```bash
cargo run --release -p hornvale -- lab run studies/the-namesake.study.json 2>&1 \
  | tee /tmp/hv-namesake.txt
```

- [ ] **Step 7: Judge the result against the FROZEN criteria — do not adjust the criteria**

Read `/tmp/hv-namesake.txt` and write the verdict into the commit message:

- §5.1(1): ≥3 distinct signatures — PASS / FAIL with the measured value
- §5.1(2): recoverability ≥2× chance — PASS / FAIL
- §5.2(1): ≥80% resolve in one element at settlement scope — PASS / FAIL
- §5.2(2): median ≥2 at region scope AND <50% need the full stack — PASS / FAIL

**A failed criterion is a finding, not a bug to fix.** Several campaigns have shipped the null as the headline. If a criterion fails, record it and carry it into Task 8's chronicle as the result; do not retune a constant to rescue it. If you believe a criterion was mis-specified, say so explicitly in the chronicle and label it amended-post-unblinding — never silently.

- [ ] **Step 8: fmt and commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs windows/lab/tests/namesake_metrics.rs studies/the-namesake.study.json
git commit -m "feat(lab): measure The Namesake's two preregistered claims

<VERDICT PER CRITERION, from /tmp/hv-namesake.txt>"
```

---

## Task 8: Definition of Done

**Files:**
- Create: `book/src/chronicle/the-namesake.md`
- Create: `docs/retrospectives/the-namesake.md`
- Modify: `book/SUMMARY.md` (chronicle entry — hand-authored, unlike the generated pages)
- Modify: `book/src/frontier/idea-registry.md` (flip statuses)
- Modify: `book/src/open-questions.md` (only if a Confidence Gradient bet moved)

- [ ] **Step 1: Write the chronicle entry**

`book/src/chronicle/the-namesake.md`. Book prose is technical and mathematical, comprehensible without reading the code. Lead with the two falsifications, because they are the campaign's real content: the founding-gap distribution that killed the parent-child reading, and the cascade inertness that withdrew the fossil claim before it could be preregistered. Then the design, then Task 7's verdict — including a failed criterion, stated plainly.

Do **not** cite registry IDs here: `cli/tests/docs_consistency.rs` forbids them outside `book/src/frontier/`. Name the concept instead.

- [ ] **Step 2: Add the chronicle to SUMMARY.md**

`book/SUMMARY.md` is always hand-authored — the Gallery and Reference H1s are code-generated, but this one is not.

- [ ] **Step 3: Write the retrospective**

`docs/retrospectives/the-namesake.md` — process lessons, not product (decision 0020). At minimum, carry over from the campaign scratch ledger:
- two of the spec's own proposals died to measurement taken before implementation; the pattern worth naming is *measure the edge before designing on it*
- the `occ-founded` years/days doc inconsistency
- whatever Task 1's cascade measurement returned

**Promote these from `.superpowers/sdd/` before the worktree is torn down** — that directory is git-ignored and dies with the worktree.

- [ ] **Step 4: Flip the registry statuses**

In `book/src/frontier/idea-registry.md`: `LANG-namescope` and `LANG-teknonymy` to `shipped` if Task 6 and the `Cite::Child` variant landed; update `LANG-cascade-inertness` with Task 1's battery result; leave `LANG-truename` `raw`.

- [ ] **Step 5: Re-run the docs drift check**

```bash
cargo test -p hornvale --test docs_consistency
```

Expected: PASS. It checks five-column rows, the 600-char Idea cap, the closed status vocabulary, link resolution, and the ban on registry IDs outside the frontier.

- [ ] **Step 6: Regenerate every artifact and diff**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: clean. Nothing in this campaign commits a fact, so no world's output may move. **A non-empty diff is the single most important negative result in this plan** — it would mean the epoch-free claim in spec §4 is false. Investigate; do not rebaseline it away.

Also run the checks the gate never runs:

```bash
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
git diff --exit-code book/src/reference/concept-registry-generated.md
```

Expected: clean — this campaign registers no concept. A diff here is a finding.

- [ ] **Step 7: Full gate**

```bash
make gate 2>&1 | tee /tmp/hv-gate.txt
```

Budget **at least 40 minutes** (measured 22–37 min on this Mac). **Stagger it** — three other worktrees are active and one `make ci` already uses 8.4 of 10 cores, so two concurrent gates cost about thirty minutes each and both look hung. Check `scripts/census-run.sh status` first.

- [ ] **Step 8: Commit and hand off to `superpowers:finishing-a-development-branch`**

```bash
cargo fmt
git add book/ docs/retrospectives/
git commit -m "docs(the-namesake): chronicle, retrospective, and registry sweep"
```

Then run `make preflight` from this branch before integrating, and invoke `closing-a-campaign`.

---

## Self-Review

**Spec coverage.** §1.1 measurements → Task 2's tests pin them as constants. §2 non-goals → nothing in Tasks 1–8 builds a true name, an eponymous toponym, a vessel-NPC name, or an exonym; `Cite::Child` ships as a *variant* (schema completeness) without a resolver, which is inside §2's line since no name cites it. §3.1 → Tasks 2, 3. §3.2 → Task 4. §3.3 → Task 5. §3.4 → Task 6. §4 (no epoch) → Task 4 Step 8 and Task 8 Step 6, both asserting a clean artifact diff. §5.0 → Task 1. §5.1, §5.2 → Task 7. §6 verification → the mutation steps in Tasks 2, 3, 5, 6 and the byte-identity checks. §7 flags → ①/② are Task 1's decision point and Task 8 Step 6; ③ is documented in `founded_year`'s doc comment; ④ is documented in `generation_length_of`; ⑤ is Task 8 Step 7's staggering warning. §8 capture → rows already landed in `e52fb373`, statuses flipped in Task 8 Step 4.

**Gap found and closed:** §3.2's `conferred: Option<f64>` field had no task that ever *sets* it — every element Task 5 produces is conferred at birth. Left as `None` throughout by design, and the field is documented as the seam a later deed-name or coming-of-age campaign fills. Flagged here rather than silently shipping an always-`None` field.

**Placeholder scan.** Task 7 Step 3 describes three of four metrics rather than giving full code — deliberate and bounded: each is a named variation on `name_pattern_signatures`, which is given in full, and the step states the exact denominator, numerator, and `Absent` condition for each. Two `<FILL IN>` markers exist in commit messages (Tasks 1 and 7); both are measurements that cannot be known before the run, and both steps say exactly which file to read them from.

**Type consistency.** `RoleHandle` is `hornvale_history::flesh::RoleHandle` throughout. `remove`/`kinship`/`ancestor` signatures match between Task 2's definition and Task 3's use. `NamePattern.elements` is `Vec<(ElementSource, Author)>` in both Task 4 and Task 5. `Rendered.parts` is `Vec<String>` in Tasks 6 and 7. Task 5's test needs `Ord` on `ElementSource`/`Author`, which Task 4's derives do not include — **Step 1 of Task 5 says to add it and to note that the variant order then becomes a contract.**
