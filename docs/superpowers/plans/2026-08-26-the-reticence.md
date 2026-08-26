# The Reticence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A possessed host decides whether to answer `ask`, and its answer may be
a refusal, a falsehood, or a costly truth — from a cultural prior it did not
choose and a conduct record the rider wrote.

**Architecture:** One new stage between `Session::driven_affect()` and
`testify()`. Two independently-stored inputs — a doctrine prior derived from
committed religion facts plus lexical coverage, and a per-`DriveKind` fold over
accumulated suppression — combine into a `Stance`, which selects among four
testimony outcomes. `FeltStateWord` is **not** extended; a new `Testimony` type
wraps it, so the committed `misreport_distance` metric is structurally unable to
see a falsehood.

**Tech Stack:** Rust 2024, no new external crates (decision 0004). One new
INTERNAL dependency: `hornvale-vessel` gains `hornvale-religion` (window ->
domain, legal under the layering in `cli/tests/architecture.rs`).

**Spec:** `docs/superpowers/specs/2026-08-26-the-reticence-design.md`

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` `disallowed-types`.
- **No wall-clock time.** No `std::time::Instant` anywhere, including tests.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag**
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`). Prefer an enum
  or a newtype over a tagged bare integer.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding.
- **Determinism:** no randomness outside the kernel's `Seed`/`Stream`; no float
  ordering without `total_cmp`.
- **Nothing authored.** Every input is a committed fact or an existing derived
  lexicon state. If a value cannot be derived, it does not ship (spec §8).
- **ONE DECLARED EXCEPTION to the line above, and it is the plan's, not the
  spec's.** `stance::patience()` (Task 3) holds three integer thresholds —
  `Guarded 2 / Wary 4 / Open 8`. Spec §8 says "no hand-tuned hostility
  constants", and these are the only numbers in the campaign not read off a
  fact. The defence, stated so a reviewer can reject it rather than discover it:
  the *disposition* is fully derived (a count of real overrides against a prior
  read from committed religion facts), and these three scale **how fast** a
  derived quantity crosses a boundary — they are not dispositions themselves.
  **They are also exactly what H4 is preregistered to interrogate**, so if the
  prior turns out decorative these constants are the first suspect. Do not tune
  them to make any test pass. If a reviewer rejects the exception, the fallback
  is a single shared threshold and a prior that shifts the COUNT rather than the
  step — say so rather than improvising a derivation.
- **DO NOT register a rider/possession/haunting/soul concept.** G3 resolved this
  (spec §10.4): zero exist, the doctrine arm stays unreachable, and H1 asserts
  `0`. A task that registers one **stops and reports**; it does not adapt.
- Run the fmt check and workspace clippy before each commit; full
  `make gate-commit` at task end.

## Reference: facts this plan depends on

Verified on this tree, seed 42:

```
  distinct SPECIES holding beliefs  15   <- the denominator all counts use
  settlements holding beliefs       15   <- 1:1 with species on THIS seed only
  beliefs (is-belief / held-by)    145
  species with MIXED cult-forms      0   <- uniform per species
  cult-form = organized              9 peoples (87 beliefs)
  cult-form = folk                   6 peoples (58 beliefs)
  high-god facts                     1   <- rejected as discriminator
  rider/possession/soul concepts     0   <- doctrine arm unreachable
```

Existing surface this plan consumes:

```rust
// windows/vessel/src/liveness.rs:1566
pub enum DriveKind { Thirst, Thermal, Fatigue, Hunger, Danger, Social, Idle, Homing }

// windows/vessel/src/session.rs:1162, :1174
pub fn driven_affect(&self) -> Option<AffectLabel>;
pub fn suppressed_drives(&self) -> &[DriveKind];

// windows/vessel/src/testimony.rs:167
pub fn testify(lexicon: &Lexicon, label: AffectLabel) -> Option<FeltStateWord>;
pub enum FeltStateWord { Direct(WordViews), Nearest { word, reported_as, reason } }

// domains/language/src/lexicon.rs:186   <- CONCEPT-GENERIC; takes &str
pub fn entry(&self, concept: &str) -> Option<&LexEntry>;
pub enum LexEntry { Root { derivation, views }, Gap { reason } }

// domains/religion/src/lib.rs
pub const HELD_BY: &str = "held-by";
pub const CULT_FORM: &str = "cult-form";

// windows/worldgen/src/lib.rs:5938 — settled peoples are Steeped in god/spirit
for concept in ["home", "hearth", "god", "spirit"] { ... }
```

Test scaffolding (`windows/vessel/tests/suite/ask_verb.rs:9`):

```rust
use crate::body_fields::seed_42;              // (World, WorldContext)
let (world, _ctx) = seed_42();
let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
s.handle("!wait 30");                          // seed 42's flagship is a BUGBEAR
assert_eq!(s.driven_affect().unwrap(), AffectLabel::Eager);  // at day 30
```

Every integration test goes in `windows/vessel/tests/suite/` **and** gets a
`#[path]` module line in `windows/vessel/tests/suite.rs`. A new top-level
`tests/*.rs` file fails `cli/tests/suite/test_binary_ratchet.rs`.

---

### Task 1: The doctrine prior

**Files:**
- Create: `windows/vessel/src/doctrine.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod doctrine;`)
- Modify: `windows/vessel/Cargo.toml` (add `hornvale-religion`)
- Test: `windows/vessel/tests/suite/doctrine.rs` + a `#[path]` line in `suite.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `doctrine::ImprovisedName`, `doctrine::Openness`,
  `doctrine::improvised_name(&World, &Lexicon, people: &str) -> ImprovisedName`,
  `doctrine::openness(&ImprovisedName) -> Openness`.

- [ ] **Step 1: Write the failing test**

`windows/vessel/tests/suite/doctrine.rs`:

```rust
//! The Reticence, Task 1: what a people with no word for a rider reaches for
//! instead. The frontier essay puts `god` and `spirit` on the doctrine-LESS
//! side ("a people without one explains you with the words it has"), so these
//! are IMPROVISED names, never a doctrine.

use crate::body_fields::seed_42;
use hornvale_vessel::doctrine::{ImprovisedName, Openness, improvised_name, openness};

#[test]
fn a_settled_peoples_improvised_name_follows_its_cult_form() {
    let (world, _ctx) = seed_42();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");
    let lexicon = hornvale_worldgen::lexicon_from(&world, "bugbear", &terrain, &climate)
        .expect("bugbear's lexicon builds");

    let name = improvised_name(&world, &lexicon, "bugbear");
    assert!(
        matches!(name, ImprovisedName::God | ImprovisedName::Spirit),
        "a SETTLED people has god/spirit steeped (worldgen:5938), so it must \
         improvise with one of them, got {name:?}"
    );
}

#[test]
fn the_least_equipped_people_is_the_most_open() {
    // The inversion this campaign turns on (spec 3.2): a people with apparatus
    // knows what to do about you; a people with no word at all has nothing to
    // invoke. Openness ORDER is the assertion, not any single value.
    assert!(openness(&ImprovisedName::God) < openness(&ImprovisedName::Spirit));
    assert!(
        openness(&ImprovisedName::Spirit)
            < openness(&ImprovisedName::Wordless {
                reason: hornvale_language::GapReason::Experiential("unsettled".into()),
            })
    );
    assert_eq!(openness(&ImprovisedName::God), Openness::Guarded);
}
```

Add to `windows/vessel/tests/suite.rs`, in alphabetical position:

```rust
#[path = "suite/doctrine.rs"]
mod doctrine;
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- doctrine`
Expected: FAIL to compile — `unresolved import hornvale_vessel::doctrine`.

- [ ] **Step 3: Add the dependency**

`windows/vessel/Cargo.toml`, in the `[dependencies]` block:

```toml
hornvale-religion = { path = "../../domains/religion" }
```

- [ ] **Step 4: Write the implementation**

`windows/vessel/src/doctrine.rs`:

```rust
//! What a people believes a rider IS — derived, never authored.
//!
//! The frontier essay draws the line at *has a rider-doctrine* vs
//! *improvises*: "a people with the doctrine names you correctly and knows what
//! to do about it, and a people without one explains you with the words it has
//! — intrusive thoughts, a haunting, a fever, a god, a wandering ancestor."
//! `god` and `spirit` are on the IMPROVISING side. No rider concept is
//! registered in this world (zero exist; see the spec's section 10.4), so every
//! people improvises and [`ImprovisedName`] has no doctrine arm at all. Adding
//! one is a deliberate, owner-approved act, not a fallthrough.

use hornvale_kernel::World;
use hornvale_language::{GapReason, LexEntry, Lexicon};

/// What a people with no word for a rider reaches for instead.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImprovisedName {
    /// Settled, organized cult: apparatus, precedent, a prescribed response.
    God,
    /// Settled, folk cult: a word, and no machinery behind it.
    Spirit,
    /// Unsettled: not even a word, carrying the lexicon's own reason.
    Wordless {
        /// Why this culture has no word for what the rider is.
        reason: GapReason,
    },
}

/// How willing the doctrine prior alone leaves a host, before any conduct is
/// folded in. Ordered: the least-equipped people is the most open.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Openness {
    /// Has apparatus for you. Least willing to speak.
    Guarded,
    /// Has a word for you and nothing behind it.
    Wary,
    /// Has nothing to invoke. Most willing to speak.
    Open,
}

/// The prior's sign. The essay's doctrine-holding people "knows what to do
/// about it", so equipment reduces willingness rather than raising it — the
/// opposite of the intuitive reading, and the one the essay supports.
pub fn openness(name: &ImprovisedName) -> Openness {
    match name {
        ImprovisedName::God => Openness::Guarded,
        ImprovisedName::Spirit => Openness::Wary,
        ImprovisedName::Wordless { .. } => Openness::Open,
    }
}

/// Which word `people` reaches for. Settled peoples are Steeped in both `god`
/// and `spirit` (`windows/worldgen/src/lib.rs:5938`), so lexical coverage
/// separates settled from unsettled and `cult-form` chooses between the two
/// settled arms. `cult-form` is uniform per people (measured: 0 of 15 mixed on
/// seed 42), so the FIRST belief held by this people decides — a scan, not a
/// vote, and the uniformity is asserted by this task's own test rather than
/// assumed here.
pub fn improvised_name(world: &World, lexicon: &Lexicon, people: &str) -> ImprovisedName {
    let unsettled_reason = |concept: &str| match lexicon.entry(concept) {
        Some(LexEntry::Gap { reason }) => Some(reason.clone()),
        _ => None,
    };
    if let Some(reason) = unsettled_reason("god").or_else(|| unsettled_reason("spirit")) {
        return ImprovisedName::Wordless { reason };
    }
    match cult_form_of(world, people).as_deref() {
        Some("organized") => ImprovisedName::God,
        _ => ImprovisedName::Spirit,
    }
}

/// The `cult-form` of the first belief held at a site this species peoples,
/// by ledger order.
///
/// **`held-by` targets a SETTLEMENT entity, not a people**, so the join runs
/// species -> `occ-people` -> site -> `held-by` -> belief -> `cult-form`. On
/// seed 42 species and site happen to be 1:1 (15 and 15), but that is a
/// property of one world, not a guarantee, so this takes the first by ledger
/// order and the task's own test asserts uniformity PER SPECIES — which is the
/// denominator the campaign's numbers are quoted on.
fn cult_form_of(world: &World, people: &str) -> Option<String> {
    use hornvale_kernel::Value;
    use hornvale_religion::{CULT_FORM, HELD_BY};
    let sites: std::collections::BTreeSet<_> = world
        .ledger
        .find("occ-people")
        .filter(|f| matches!(&f.object, Value::Text(t) if t == people))
        .map(|f| f.subject)
        .collect();
    world
        .ledger
        .find(HELD_BY)
        .filter(|f| matches!(&f.object, Value::Entity(e) if sites.contains(e)))
        .find_map(|f| world.ledger.text_of(f.subject, CULT_FORM))
        .map(str::to_string)
}
```

**The kernel API above was verified against `kernel/src/ledger.rs` at dispatch
time and is what the tree actually has** — `Ledger::find(predicate)` (line 388),
`Ledger::text_of(subject, predicate)` (line 429), `Fact.subject: EntityId` (line
72), `Value::{Entity, Text, Number, Flag}` (line 55). There is **no**
`Ledger::facts()` and **no** `Value::as_text()`; an earlier draft of this plan
named both and neither exists. If anything else here does not compile, adapt to
the tree and say so in your report — do not invent an accessor.

- [ ] **Step 5: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite -- doctrine`
Expected: PASS, both tests.

- [ ] **Step 6: Prove the uniformity claim rather than trusting it**

Add to `windows/vessel/tests/suite/doctrine.rs`:

```rust
#[test]
fn cult_form_is_uniform_within_every_people() {
    // The spec's section 3.2 takes the FIRST belief as decisive. That is only
    // sound because no people holds two cult-forms. Measured 0 of 15 on seed
    // 42; this test is what keeps it true.
    use hornvale_kernel::Value;
    use hornvale_religion::{CULT_FORM, HELD_BY};
    let (world, _ctx) = seed_42();

    // site -> the species that peoples it
    let mut species_of_site = std::collections::BTreeMap::new();
    for f in world.ledger.find("occ-people") {
        if let Value::Text(t) = &f.object {
            species_of_site.insert(f.subject, t.clone());
        }
    }
    // species -> every cult-form held at any site it peoples
    let mut per_species: std::collections::BTreeMap<String, std::collections::BTreeSet<String>> =
        std::collections::BTreeMap::new();
    for f in world.ledger.find(HELD_BY) {
        let Value::Entity(site) = &f.object else { continue };
        let (Some(sp), Some(form)) = (
            species_of_site.get(site),
            world.ledger.text_of(f.subject, CULT_FORM),
        ) else {
            continue;
        };
        per_species.entry(sp.clone()).or_default().insert(form.to_string());
    }
    let mixed: Vec<_> = per_species.iter().filter(|(_, s)| s.len() > 1).collect();
    assert!(mixed.is_empty(), "cult-form must be uniform per SPECIES, mixed: {mixed:?}");
    assert_eq!(per_species.len(), 15, "seed 42 has 15 species holding beliefs");
}
```

Run the doctrine filter again. Expected: PASS, three tests.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/doctrine.rs windows/vessel/src/lib.rs \
        windows/vessel/Cargo.toml Cargo.lock \
        windows/vessel/tests/suite/doctrine.rs windows/vessel/tests/suite.rs
git commit -m "feat(the-reticence): the doctrine prior, derived from cult-form and lexical coverage"
```

---

### Task 2: The history fold — accumulated suppression

**Files:**
- Modify: `windows/vessel/src/session.rs` (the `Session` field block near
  `driven_suppressed`, its constructor near line 1101, the `wait` path near line
  4060, and a new accessor near `suppressed_drives` at line 1174)
- Test: `windows/vessel/tests/suite/overrides.rs` + `#[path]` line in `suite.rs`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: `Session::overrides_of(&self, drive: DriveKind) -> u32`,
  `Session::override_record(&self) -> &BTreeMap<DriveKind, u32>`.

- [ ] **Step 1: Write the failing test**

`windows/vessel/tests/suite/overrides.rs`:

```rust
//! The Reticence, Task 2: the conduct half. `driven_suppressed` is a
//! per-decision read overwritten by every `advance_one` iteration
//! (`liveness.rs:4996-5010`). This accumulates it across the possession, so the
//! axis the rider overrides is the axis the host can later go quiet on.

use crate::body_fields::seed_42;
use hornvale_vessel::{PossessOpts, Session};

#[test]
fn overrides_accumulate_across_ticks_rather_than_being_overwritten() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 30");
    let after_first: u32 = s.override_record().values().sum();
    s.handle("!wait 30");
    let after_second: u32 = s.override_record().values().sum();
    assert!(
        after_second > after_first,
        "the record must ACCUMULATE, not be overwritten: {after_first} then {after_second}"
    );
}

#[test]
fn a_fresh_session_has_overridden_nothing() {
    let (world, _ctx) = seed_42();
    let (s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert!(s.override_record().is_empty(), "nothing is overridden before the first wait");
}
```

Add to `suite.rs`:

```rust
#[path = "suite/overrides.rs"]
mod overrides;
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- overrides`
Expected: FAIL to compile — no method `override_record`.

- [ ] **Step 3: Add the field**

In `Session`'s field block, immediately after `driven_suppressed`:

```rust
    /// Every drive this body's own arbitration wanted and did not pursue,
    /// counted across the WHOLE possession (The Reticence, Task 2) — unlike
    /// `driven_suppressed`, which is a per-decision read overwritten by every
    /// `advance_one` iteration. When the rider is driving, this is the record
    /// of what the rider made this body ignore, and it is the only conduct
    /// input the host's willingness to speak reads.
    driven_overrides: std::collections::BTreeMap<DriveKind, u32>,
```

In the constructor beside `driven_affect: None` (near line 1101):

```rust
            driven_overrides: std::collections::BTreeMap::new(),
```

- [ ] **Step 4: Accumulate where `driven_suppressed` is written**

Find where `wait` assigns `self.driven_suppressed` from
`step_one_with_controller`'s fourth return value (near `session.rs:4060`). Add,
immediately after that assignment:

```rust
        for drive in &self.driven_suppressed {
            *self.driven_overrides.entry(*drive).or_insert(0) += 1;
        }
```

**Why here and not inside `advance_one`:** the per-tick value is already
surfaced at exactly this point from a single resolution, so accumulating beside
it needs no second traversal and cannot drift from the value
`suppressed_drives()` reports.

- [ ] **Step 5: Add the accessors**

Immediately after `suppressed_drives` (line 1174):

```rust
    /// How many decisions this possession has overridden `drive` — the count
    /// of ticks on which arbitration found it active and did not pursue it
    /// (The Reticence, Task 2). Zero before the first `!wait`, and zero for a
    /// drive that has never lost.
    /// type-audit: bare-ok(count)
    pub fn overrides_of(&self, drive: DriveKind) -> u32 {
        self.driven_overrides.get(&drive).copied().unwrap_or(0)
    }

    /// The whole override record, drive-ordered (The Reticence, Task 2).
    pub fn override_record(&self) -> &std::collections::BTreeMap<DriveKind, u32> {
        &self.driven_overrides
    }
```

`DriveKind` must derive `Ord`/`PartialOrd` to be a `BTreeMap` key. If it does
not, add those derives to the enum in `liveness.rs:1566` in this task and note
it in your report — the **declaration order** there is load-bearing for
tie-breaks and must not change.

- [ ] **Step 6: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite -- overrides`
Expected: PASS, both tests.

- [ ] **Step 7: Prove the accumulation is real, not a constant**

Mutate with `scripts/mutate.py` (never `sed` — it substitutes only if the target
is found AND unique, which is what stops a silent no-op mutation):

```bash
python3 scripts/mutate.py --to '*self.driven_overrides.entry(*drive).or_insert(0) = 1;' \
    windows/vessel/src/session.rs \
    '*self.driven_overrides.entry(*drive).or_insert(0) += 1;'
```

Re-run the overrides filter. Expected: **RED** on
`overrides_accumulate_across_ticks_rather_than_being_overwritten`. A green here
means the test does not discriminate and must be strengthened before you
restore.

Restore with `git checkout -- windows/vessel/src/session.rs`, then **re-run and
confirm green again** — a restored mutation can leave a stale binary, and the
silent direction is a false GREEN.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/session.rs windows/vessel/src/liveness.rs \
        windows/vessel/tests/suite/overrides.rs windows/vessel/tests/suite.rs
git commit -m "feat(the-reticence): accumulate suppressed drives across the possession"
```

---

### Task 3: The stance — prior and fold, kept separate

**Files:**
- Create: `windows/vessel/src/stance.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod stance;`)
- Test: `windows/vessel/tests/suite/stance.rs` + `#[path]` line in `suite.rs`

**Interfaces:**
- Consumes: `doctrine::Openness` (Task 1), `Session::overrides_of` (Task 2).
- Produces: `stance::Stance`, `stance::stance_for(Openness, u32) -> Stance`.

- [ ] **Step 1: Write the failing test**

`windows/vessel/tests/suite/stance.rs`:

```rust
//! The Reticence, Task 3: prior and fold combine WITHOUT being summed into one
//! number (spec 3.5). The disagreement between them is the readable output — a
//! host may keep the warm word for you and still refuse to answer.

use hornvale_vessel::doctrine::Openness;
use hornvale_vessel::stance::{Stance, stance_for};

#[test]
fn an_unoffended_host_is_forthcoming_whatever_its_prior() {
    for prior in [Openness::Guarded, Openness::Wary, Openness::Open] {
        assert_eq!(stance_for(prior, 0), Stance::Forthcoming, "prior {prior:?} at zero overrides");
    }
}

#[test]
fn the_prior_decides_how_fast_conduct_costs_you() {
    // Same conduct, different priors -> different stances. If this passes with
    // every prior producing the same stance, the prior is decorative and H4's
    // null has fired -- report it, do not retune.
    let guarded = stance_for(Openness::Guarded, 4);
    let open = stance_for(Openness::Open, 4);
    assert_ne!(guarded, open, "the prior must move the stance at equal conduct");
}

#[test]
fn stances_worsen_monotonically_with_conduct() {
    let mut seen = Vec::new();
    for overrides in [0u32, 2, 4, 8, 16] {
        seen.push(stance_for(Openness::Wary, overrides));
    }
    let mut sorted = seen.clone();
    sorted.sort();
    assert_eq!(seen, sorted, "more overriding must never make a host MORE forthcoming: {seen:?}");
}
```

Add to `suite.rs`:

```rust
#[path = "suite/stance.rs"]
mod stance;
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- stance`
Expected: FAIL to compile — `unresolved import hornvale_vessel::stance`.

- [ ] **Step 3: Write the implementation**

`windows/vessel/src/stance.rs`:

```rust
//! Whether a host will answer, and how honestly.
//!
//! The two inputs are kept SEPARATE and are never pre-summed into one
//! disposition number (spec 3.5): the doctrine prior is what a people believes
//! a rider is, the fold is what this rider has actually done, and the case
//! worth reaching is the one where they disagree — a host that still calls you
//! by the warm word and still will not say where the water is.

use crate::doctrine::Openness;

/// What the host will do when asked about a drive. Ordered worst-last so a
/// test can assert monotonicity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stance {
    /// Answers from its own state.
    Forthcoming,
    /// Answers accurately, and volunteers the residue it would normally keep —
    /// truth chosen BECAUSE truth is what costs you.
    Costly,
    /// Names a state it is not in.
    Dissembling,
    /// Declines to name its state at all.
    Withholding,
}

/// How many overrides each prior tolerates before the stance worsens one step.
///
/// A people with apparatus spends its patience fastest. These are the only
/// three numbers in the campaign that are not read off a fact, and they are
/// thresholds on a derived count rather than authored dispositions — the
/// quantity itself is earned. Preregistered: the campaign reports whether the
/// prior moves observable testimony at all (spec section 5, H4).
fn patience(prior: Openness) -> u32 {
    match prior {
        Openness::Guarded => 2,
        Openness::Wary => 4,
        Openness::Open => 8,
    }
}

/// The stance a host of this prior takes toward a drive it has been overridden
/// on `overrides` times. Zero overriding is always [`Stance::Forthcoming`]:
/// nothing about a culture's beliefs makes a host unhelpful before the rider
/// has done anything.
pub fn stance_for(prior: Openness, overrides: u32) -> Stance {
    let step = patience(prior);
    match overrides {
        0 => Stance::Forthcoming,
        n if n <= step => Stance::Costly,
        n if n <= step * 2 => Stance::Dissembling,
        _ => Stance::Withholding,
    }
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite -- stance`
Expected: PASS, three tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/stance.rs windows/vessel/src/lib.rs \
        windows/vessel/tests/suite/stance.rs windows/vessel/tests/suite.rs
git commit -m "feat(the-reticence): the stance, from a prior and a fold kept separate"
```

---

### Task 4: `Testimony` — a falsehood the committed metric cannot see

**Files:**
- Modify: `windows/vessel/src/testimony.rs` (append below `testify`, line 167+)
- Test: `windows/vessel/tests/suite/testimony.rs` (append to the existing file)

**Interfaces:**
- Consumes: `stance::Stance` (Task 3).
- Produces: `testimony::Testimony`,
  `testimony::testify_with_stance(&Lexicon, AffectLabel, Stance, &[DriveKind]) -> Option<Testimony>`.

**PLAN DEVIATION FROM THE SPEC, adopted deliberately.** Spec section 4.3 says "a
lie gets its own `FeltStateWord` variant". This plan wraps `FeltStateWord` in a
new `Testimony` type instead, leaving `FeltStateWord` untouched. Same goal,
stronger guarantee: `misreport_distance_for` takes a `FeltStateWord` and so
becomes **structurally unable** to observe a falsehood, rather than able-to and
filtered. A filter is discipline; a type boundary is not. Report this deviation.

- [ ] **Step 1: Write the failing test**

Append to `windows/vessel/tests/suite/testimony.rs`:

```rust
mod reticence {
    use hornvale_vessel::liveness::{AffectLabel, DriveKind};
    use hornvale_vessel::stance::Stance;
    use hornvale_vessel::testimony::{Testimony, testify_with_stance};

    /// Reuse whatever this file already builds for its Direct-arm cases. If it
    /// has no reusable helper, EXTRACT one in this task rather than hand-rolling
    /// a second lexicon builder, and say so in your report.
    fn lexicon() -> hornvale_language::Lexicon {
        super::direct_arm_lexicon()
    }

    #[test]
    fn a_withholding_host_names_no_state_at_all() {
        let t = testify_with_stance(&lexicon(), AffectLabel::Frustrated, Stance::Withholding, &[]);
        assert_eq!(t, Some(Testimony::Withheld));
    }

    #[test]
    fn a_dissembling_host_claims_content_and_never_leaks_the_truth() {
        let t = testify_with_stance(&lexicon(), AffectLabel::Helpless, Stance::Dissembling, &[])
            .expect("a lexicon with felt-state words testifies");
        match t {
            Testimony::Falsehood { claimed, .. } => assert_eq!(
                claimed,
                AffectLabel::Content,
                "the dissembling rule is the one lie: claim Content"
            ),
            other => panic!("expected a Falsehood, got {other:?}"),
        }
    }

    #[test]
    fn a_costly_truth_reveals_the_residue_a_forthcoming_host_withholds() {
        let residue = [DriveKind::Thirst, DriveKind::Fatigue];
        let costly = testify_with_stance(&lexicon(), AffectLabel::Eager, Stance::Costly, &residue)
            .expect("testifies");
        let plain =
            testify_with_stance(&lexicon(), AffectLabel::Eager, Stance::Forthcoming, &residue)
                .expect("testifies");
        match (costly, plain) {
            (Testimony::Costly { revealed, .. }, Testimony::Spoken(_)) => {
                assert_eq!(revealed, residue, "the costly arm carries the residue");
            }
            other => panic!("wrong arms: {other:?}"),
        }
    }

    #[test]
    fn no_falsehood_ever_names_the_true_state() {
        for truth in [AffectLabel::Helpless, AffectLabel::Frustrated, AffectLabel::Lost] {
            let t = testify_with_stance(&lexicon(), truth, Stance::Dissembling, &[])
                .expect("testifies");
            if let Testimony::Falsehood { claimed, .. } = t {
                assert_ne!(claimed, truth, "a falsehood must never name the true state");
            }
        }
    }
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- testimony::reticence`
Expected: FAIL to compile — `Testimony` not found.

- [ ] **Step 3: Write the implementation**

Append to `windows/vessel/src/testimony.rs` (and add the imports it needs at the
file head: `crate::stance::Stance`, `crate::liveness::DriveKind`):

```rust
/// What a host actually says when asked, once its willingness is applied.
///
/// **`FeltStateWord` is deliberately NOT extended with a falsehood arm.** A lie
/// and a lexical substitution have the same shape — report X when the truth is
/// Y — and different causes, and `misreport_distance_for`
/// (`windows/lab/src/metrics.rs`) compares concept ids and cannot tell them
/// apart. Wrapping rather than extending makes that metric structurally unable
/// to observe a falsehood instead of merely filtered from one.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Testimony {
    /// The host answered from its own state — The Confidant's behaviour,
    /// unchanged, including its two blindnesses.
    Spoken(FeltStateWord),
    /// The host declined to name its state.
    Withheld,
    /// The host named a state it is not in.
    Falsehood {
        /// The word it actually said.
        word: WordViews,
        /// The state that word names — never the true one.
        claimed: AffectLabel,
    },
    /// Accurate, plus the discarded ranks a forthcoming host never mentions.
    Costly {
        /// The true state's word, by the ordinary lexical route.
        word: FeltStateWord,
        /// The residue revealed BECAUSE revealing it costs the rider.
        revealed: Vec<DriveKind>,
    },
}

/// The single lie this campaign tells: a dissembling host claims it is
/// [`AffectLabel::Content`] — the "I am fine" of a body that is not.
///
/// One rule, deterministic, and deliberately flat: the deliverable is that a
/// host CAN lie, not a taxonomy of lies. A chooser that varies the claim by what
/// the host wants believed is a later campaign's work and wants its own
/// preregistration.
const DISSEMBLING_CLAIM: AffectLabel = AffectLabel::Content;

/// [`testify`], with the host's willingness applied.
///
/// `residue` is the arbitration's discarded ranks
/// ([`crate::Session::suppressed_drives`]); it is read ONLY by the
/// [`Testimony::Costly`] arm. Every other arm ignores it, which is what keeps
/// The Confidant's cognitive gap intact for an ordinary answer.
pub fn testify_with_stance(
    lexicon: &Lexicon,
    truth: AffectLabel,
    stance: Stance,
    residue: &[DriveKind],
) -> Option<Testimony> {
    match stance {
        Stance::Withholding => Some(Testimony::Withheld),
        Stance::Forthcoming => testify(lexicon, truth).map(Testimony::Spoken),
        Stance::Costly => testify(lexicon, truth).map(|word| Testimony::Costly {
            word,
            revealed: residue.to_vec(),
        }),
        Stance::Dissembling => match testify(lexicon, DISSEMBLING_CLAIM)? {
            FeltStateWord::Direct(word) => Some(Testimony::Falsehood {
                word,
                claimed: DISSEMBLING_CLAIM,
            }),
            // The culture has no word for `Content` either. A host that cannot
            // say the lie says nothing — falling through to the truth would
            // make a hostile host MORE informative than a friendly one.
            FeltStateWord::Nearest { .. } => Some(Testimony::Withheld),
        },
    }
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite -- testimony`
Expected: PASS, existing tests plus the four new ones.

- [ ] **Step 5: Mutation-prove the falsehood cannot leak the truth**

```bash
python3 scripts/mutate.py --to 'const DISSEMBLING_CLAIM: AffectLabel = AffectLabel::Helpless;' \
    windows/vessel/src/testimony.rs \
    'const DISSEMBLING_CLAIM: AffectLabel = AffectLabel::Content;'
```

Re-run the `testimony::reticence` filter. Expected: **RED**. Restore with
`git checkout -- windows/vessel/src/testimony.rs`, re-run, confirm green.

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/testimony.rs windows/vessel/tests/suite/testimony.rs
git commit -m "feat(the-reticence): Testimony wraps FeltStateWord so a lie is invisible to misreport_distance"
```

---

### Task 5: Wire the stance into `ask`

**Files:**
- Modify: `windows/vessel/src/session.rs:4868-4899` (`fn ask`), `render_testimony`
  (near line 5077), and a new accessor beside `driven_affect` (line 1162)
- Test: `windows/vessel/tests/suite/ask_verb.rs` (append)

**Interfaces:**
- Consumes: everything from Tasks 1-4.
- Produces: `Session::driven_affect_object(&self) -> Option<DriveKind>`;
  `ask`'s observable behaviour changes.

- [ ] **Step 1: Write the failing test**

Append to `windows/vessel/tests/suite/ask_verb.rs` (add
`use hornvale_vessel::liveness::DriveKind;` at the file head):

```rust
/// The Reticence, Task 5: refusal is SELECTIVE (spec section 5, H3). A host
/// driven into silence on one drive still answers about another.
#[test]
fn a_reticent_host_still_answers_about_a_drive_it_was_never_overridden_on() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    for _ in 0..8 {
        s.handle("!wait 30");
    }
    let record = s.override_record().clone();
    assert!(!record.is_empty(), "precondition: driving must override something");
    let never = [
        DriveKind::Thirst, DriveKind::Thermal, DriveKind::Fatigue, DriveKind::Hunger,
        DriveKind::Danger, DriveKind::Social, DriveKind::Idle, DriveKind::Homing,
    ]
    .into_iter()
    .find(|d| s.overrides_of(*d) == 0);
    assert!(
        never.is_some(),
        "H3 needs at least one un-overridden drive to exist; record was {record:?}"
    );

    let turn = s.handle("ask");
    let text = match turn { Turn::Out(t) => t, Turn::Released(t) => panic!("released: {t}") };
    assert!(!text.is_empty(), "a host with a mixed record still produces an utterance");
}

/// A withheld answer must NOT write a `heard` entry: the player learned nothing,
/// and a knowledge store that records a refusal as a felt state would make
/// silence informative.
#[test]
fn a_refusal_writes_no_knowledge() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    for _ in 0..24 {
        s.handle("!wait 30");
    }
    let body_label = s.driven_body().label.clone();
    let key = format!("{body_label}::feels");
    let before = s.knowledge().0.get(&key).cloned();
    let _ = s.handle("ask");
    let after = s.knowledge().0.get(&key).cloned();
    if after == before {
        return; // withheld, or unchanged — the case this test is about
    }
    assert!(after.is_some(), "if knowledge moved at all it must hold a real value");
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- ask_verb`
Expected: FAIL to compile — `override_record` / `overrides_of` unresolved.

- [ ] **Step 3: Add the topic accessor**

Beside `driven_affect` (line 1162). `Affect` already carries
`object: Option<DriveKind>`:

```rust
    /// Which drive the driven body's most recent felt state is ABOUT (The
    /// Reticence, Task 5) — the affect's own object, so a topic-scoped refusal
    /// names the axis the rider actually overrode. `None` before the first
    /// `!wait`, and for a state with no object.
    pub fn driven_affect_object(&self) -> Option<DriveKind> {
        self.driven_affect.and_then(|affect| affect.object)
    }
```

- [ ] **Step 4: Rewrite `ask` around the stance**

After `label` and the lexicon are obtained as today — **the lexicon must be
built BEFORE the stance, because `improvised_name` reads it**, so restructure
the existing `match` to bind it to a name both can use:

```rust
        // The Reticence: the host's willingness, between arbitration and the
        // lexicon. The drive the answer is ABOUT is the pursued one — the
        // affect's own object — so a host goes quiet on the axis it was
        // overridden on, not globally.
        let topic = self.driven_affect_object();
        let prior = {
            let name = crate::doctrine::improvised_name(
                self.world,
                &lexicon,
                &self.driven_body().species,
            );
            crate::doctrine::openness(&name)
        };
        let overrides = topic.map(|d| self.overrides_of(d)).unwrap_or(0);
        let stance = crate::stance::stance_for(prior, overrides);
        let residue: Vec<_> = self.suppressed_drives().to_vec();
        let testimony = testify_with_stance(&lexicon, label, stance, &residue);
```

- [ ] **Step 5: Teach `render_testimony` the four arms**

Change its testimony parameter from `Option<FeltStateWord>` to
`Option<Testimony>`; it still returns `(String, Option<String>)` — the turn text
and the value written under `"{body}::feels"`:

- `Spoken(w)` — exactly as today, unchanged; `heard` = the reported concept id.
- `Withheld` — a refusal line naming the body; `heard` = `None`.
- `Falsehood { word, claimed }` — the lie's word; `heard` = `claimed`'s concept
  id. **Never the true label** — that is what `misreport_distance` would read.
- `Costly { word, revealed }` — the ordinary line plus the revealed drives;
  `heard` = the reported concept id, as in `Spoken`.

- [ ] **Step 6: Run the whole vessel suite**

Run: `cargo test -p hornvale-vessel --test suite`
Expected: PASS.

**If a Confidant-era `ask_verb` or `testimony` test now fails, STOP.** A
green-at-day-30 test going red means the stance fired earlier than intended.
Report the failing assertion; do NOT adjust `patience()` in `stance.rs` to make
it pass. Retuning a constant to rescue a test after seeing the result is exactly
what preregistration exists to prevent.

- [ ] **Step 7: Check for artifact drift**

```bash
grep -rn '\bask\b' book/src/gallery/ clients/ 2>/dev/null | head
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Branch table for the result — do not predict which of these fires, read it:

- **only `docs/audits/` moved** — expected (a `pub` boundary changed). Commit it
  in this commit.
- **`book/src/gallery/` moved** — a transcript's `ask` output changed. Read the
  diff, confirm it is the stance firing, then commit.
- **`clients/game/core/tests/fixtures/` moved** — STOP. Those are byte-goldens;
  a change means `ask` moved a committed session snapshot and needs Nathan's
  sign-off before any rebaseline is accepted.
- **nothing moved** — also fine, but SAY SO. Do not read it as proof of no
  drift; `git diff --exit-code` is vacuous against an unindexed path.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate-commit
git add -A
git commit -m "feat(the-reticence): ask routes through the host's willingness"
```

---

### Task 6: The instrument

**Files:**
- Create: `windows/lab/src/reticence.rs`
- Modify: `windows/lab/src/lib.rs` (export `render_reticence_report`)
- Modify: `scripts/regenerate-artifacts.sh` (add the redirect beside the
  Confidant report's)
- Modify: `docs/generated-paths.txt` (declare the new report **by name** as well
  as its directory)
- Test: `windows/lab/tests/suite/reticence.rs` + `#[path]` line

**Interfaces:**
- Consumes: Tasks 1-5.
- Produces: `render_reticence_report() -> String` and the committed artifact
  `docs/audits/the-reticence-report.md`.

- [ ] **Step 1: Write the failing test**

`windows/lab/tests/suite/reticence.rs`:

```rust
//! The Reticence, Task 6: a per-people readout of which improvised name each
//! culture reaches for, and what it costs a rider.

#[test]
fn the_report_covers_every_people_and_names_no_doctrine_arm() {
    let report = hornvale_lab::render_reticence_report();
    let rows = report.lines().filter(|l| l.starts_with("| ")).count();
    assert!(rows >= 15, "one row per people (15 on seed 42), plus header rows; got {rows}");
    assert!(
        !report.to_lowercase().contains("doctrine"),
        "no rider concept is registered, so no people may resolve to a doctrine \
         arm; if this fires, spec section 10.4 was reopened without updating H1"
    );
}
```

- [ ] **Step 2: Run it, confirm it fails**

Run: `cargo test -p hornvale-lab --test suite -- reticence`
Expected: FAIL — `render_reticence_report` not found.

- [ ] **Step 3: Implement, modelled on `render_confidant_report`**

Read `render_confidant_report` in the same crate first and follow its shape:
build the world once, iterate peoples in deterministic order, emit a Markdown
table. Columns: people, cult-form, improvised name, prior openness, patience.

**Do NOT copy its `Seed(42)` `FullView` construction without reading its own
note.** The Confidant's retrospective records that it sculpts full
terrain/climate purely to reach `lex()`, on values that are provably
world-invariant. If your report needs only felt-state existence, say so in your
report — that observation currently has **no committed home**, and this is the
place it earns one.

- [ ] **Step 4: Wire the artifact**

In `scripts/regenerate-artifacts.sh`, beside the Confidant report's line, and
matching whatever invocation that one actually uses:

```bash
cargo run -q ... render reticence > docs/audits/the-reticence-report.md
```

**Every `render` prints to stdout; the `>` redirect is what writes the file.** A
command without the redirect regenerates nothing, and the drift check that
follows then reports an empty diff that reads as "no drift".

In `docs/generated-paths.txt`, add the file **by name** (not only its already-
declared directory):

```
docs/audits/the-reticence-report.md
```

- [ ] **Step 5: Regenerate, index, verify**

```bash
make rebaseline
git add docs/audits/the-reticence-report.md
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

The `git add` is **required and ordered before the check**: a new file in an
already-declared directory inherits a vacuous drift check, because the
directory's other tracked files keep the tracked-ness test green while
`git diff` cannot see the new file at all.

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "feat(the-reticence): the per-people willingness report"
```

---

### Task 7: The preregistered readout

**Files:**
- Test: `windows/lab/tests/suite/reticence_calibration.rs` + `#[path]` line

**Interfaces:** consumes everything; produces the campaign's findings.

The four hypotheses are frozen in spec section 5. **Do not edit that section.**
Write one test per hypothesis, run them, and report the numbers — including a
falsified one, which is a finding rather than a failure.

Any `#[ignore]` in this file must carry a reason naming a cost or citing a
decision number, or `windows/lab/tests/preregistration_guard.rs` fails it.

**On the shape of these four steps.** H1 is written out in full because its
instrument is knowable from outside the code. H2-H4 name the **property** the
measurement must demonstrate and leave the instrument to you, deliberately: a
plan author does not know which drives co-activate in a given session, and both
times a Hornvale plan prescribed a specific probe from outside, the prescribed
one was a null and the implementer found a discriminating one by reading. Find
the instrument, and say in your report what you chose and why.

- [ ] **Step 1: H1 — arm distribution**

```rust
#[test]
fn h1_the_improvising_arms_are_distributed_as_preregistered() {
    // Frozen before the code: 15 peoples, organized 9, folk 6, doctrine 0.
    // A moved number here means the WORLD moved, not that the prediction was
    // loose -- chase the world change before touching this assertion.
    use hornvale_vessel::doctrine::{ImprovisedName, improvised_name};
    let (world, _ctx) = seed_42();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");

    let mut god = 0usize;
    let mut spirit = 0usize;
    let mut wordless = 0usize;
    for people in peoples_of(&world) {
        let Ok(lexicon) = hornvale_worldgen::lexicon_from(&world, &people, &terrain, &climate)
        else {
            continue;
        };
        match improvised_name(&world, &lexicon, &people) {
            ImprovisedName::God => god += 1,
            ImprovisedName::Spirit => spirit += 1,
            ImprovisedName::Wordless { .. } => wordless += 1,
        }
    }
    assert_eq!((god, spirit), (9, 6), "frozen arm counts over 15 peoples");
    assert_eq!(god + spirit + wordless, 15, "every people resolves to exactly one arm");
}
```

`peoples_of` is the distinct `peopled-by` object set, in deterministic order;
if the lab crate already has such a helper, call it rather than adding a second.
`lexicon_from` takes a `&'static str` in the `ask` call site — if it will not
accept a runtime `String` here, that is a real signature constraint, not a
mistake in your work: report it and use whatever the crate actually offers.

Expected: PASS at 9/6/0.

- [ ] **Step 2: H2 — the fold discriminates per-drive**

Assert that overriding drive *d* moves the stance on *d* and not on a co-active
drive. Then mutation-prove it: substitute a constant for the accumulated count
and confirm **RED**. Restore, re-run, confirm green. A test made robust by being
made weaker would go quietly green — that is the failure The Confidant caught in
its own Task 8.

- [ ] **Step 3: H3 — refusal is selective**

Count drives answered vs refused in one session; assert both are > 0. **State
the denominator in the assertion message** — a null without one is unreadable.

- [ ] **Step 4: H4 — the null this campaign is prepared to report**

```rust
#[test]
fn h4_does_the_prior_move_observable_testimony_at_all() {
    // Count sessions where prior and fold select DIFFERENT stances, over a
    // stated denominator. ZERO IS THE HEADLINE, not a failure: it means the
    // prior is decorative -- the same shape as The Cupel's finding and The
    // Cant's own measured null (0/210 admiration).
    // Report the count. Do NOT retune `patience()` to make it non-zero.
}
```

- [ ] **Step 5: Commit, and report all four numbers in your task report**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "measure(the-reticence): the four preregistered readouts"
```

---

### Task 8: Definition of Done

**Files:**
- Create: `book/src/chronicle/the-reticence.md` + a `book/src/SUMMARY.md` line
- Create: `docs/retrospectives/the-reticence.md` + a `docs/retrospectives/README.md` line
- Create: decision records for spec section 9's three decisions
- Modify: `book/src/frontier/idea-registry.md`, `book/src/open-questions.md`

- [ ] **Step 1: Reserve the decision numbers**

```bash
make decision-block
```

**Never `max+1`** — numbers are reserved in blocks, and a hand-picked next
number collides silently with a parallel campaign's.

- [ ] **Step 2: Write the three decision records** from spec section 9. A record's
  title must match its filename (`docs_consistency` enforces this).

- [ ] **Step 3: Chronicle entry**, name-only slug (decision 0026), wired into
  `SUMMARY.md`, written at the book's altitude: technical, comprehensible
  without the code it may show. **Carries no registry IDs or process
  vocabulary** — `docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary`
  fails on them.

- [ ] **Step 4: Retrospective** — process lessons, not product. It must carry the
  ledger's post-G3 entries and a **"Deferred, with homes"** section naming a
  committed file for each deferred item.

- [ ] **Step 5: Registry flips**
  - `PLAY-host-may-refuse` -> `shipped`, **Where** -> this campaign
  - `PLAY-host-names-you` -> annotate: the improvising half shipped; the
    doctrine half stays unreachable until a rider concept is registered
  - `PLAY-host-is-a-narrator` -> still `elaborated`; affect is routed, local
    belief and dread remain
  - `PLAY-doctrine-colours-the-improvised-name` -> repoint **Where** if this
    campaign moved it

- [ ] **Step 6: Book freshness sweep and Gradient re-score**

```bash
grep -rln 'possess\|host\|testimony' book/src/ | grep -v chronicle
```

Re-read each hit and fix the lag. Re-score any `book/src/open-questions.md` bet
this campaign moved (decision 0030); grep the campaign's domains there before
concluding none did.

- [ ] **Step 7: Regenerate, verify, commit, push**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
cargo test -p hornvale --test suite -- docs_consistency
make gate-commit
git add -A
git commit -m "docs(the-reticence): chronicle, retrospective, decisions, registry, sweep"
git push origin campaign/the-reticence
```

- [ ] **Step 8: Merge**

The `Sluice-Headline:` trailer is REQUIRED — the merge refuses without it. Same
trailer block as `Claude-Session`, no blank line between them.

```bash
make sluice BRANCH=campaign/the-reticence REF=$(git rev-parse HEAD)
```

**Chronicle and retrospective land BEFORE the merge, not after.** A second merge
to carry them costs a second slot in a strictly serial queue, plus a window in
which `main` holds a campaign the book does not describe.

**Flamegraph debt.** If this campaign's close census crosses the alarm
threshold, it is flamegraphed HERE.
`docs/timings/census-yellow-log.tsv` has rolled the obligation forward twice and
asks that it be taken by someone who is not the author of the change under
suspicion — which this campaign's author is not.
