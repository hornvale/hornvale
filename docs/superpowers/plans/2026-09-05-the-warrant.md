# The Warrant — Penstock stage 7b implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development to implement this plan
> task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give a walking creature's errand a committed, typed home — eight
registered `errand/*` predicates carrying the reason and the origin — and
retire the authored prose from every `agent-at` step's `provenance`, so the
reader-facing *why* is composed at render time from the concept registry
rather than stored once per step.

**Architecture:** One new fact per errand, committed at the mode-transition
boundary that `windows/vessel/src/liveness.rs:8056` already computes; the
per-step `agent-at` facts stay exactly as they are except that their
`provenance` becomes the producer name. A sixth `ResidentFolds` tenant indexes
errand facts per entity so a step's covering errand is a binary search.
`windows/historiography` gains an errand-aware pre-grouping pass and stays
predicate-blind for everything else.

**Tech Stack:** Rust 2024, no new dependencies (the allowlist is `serde`,
`serde_json`, `libm` — `cli/tests/suite/architecture.rs`'s `ALLOWED_EXTERNAL`).
`cargo nextest` for tests; `hornvale possess --script` for renderings.

**Spec:** `docs/superpowers/specs/2026-09-05-the-warrant-design.md` — read it
before Task 1. §1 (the measurement), §4/§4.0 (the representation and the
origin correction) and §7.2 (the instrument that goes vacuous) are the three
sections an implementer cannot work without.

## Global Constraints

- **This is an epoch.** Predicate spellings are permanent on-disk keys
  (`liveness.rs:9564`, `the_agent_at_predicate_spelling_is_a_permanent_on_disk_key`,
  whose doc says "Do not rebaseline this literal — take an epoch"). The eight
  `errand/*` keys, once committed, are as permanent as `agent-at`.
- **No fact is removed, compacted, or abstained from.** That is stage 7c.
- **No drive constant, threshold, hysteresis value or arbitration behaviour
  changes.** If a creature decides differently, the campaign has gone wrong.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only
  (`clippy.toml` `disallowed-types`).
- **No wall-clock time.** Time is `WorldTime { ticks: i64 }`.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and
  variant gets a one-line doc comment.
- `cargo fmt` is the final step before every commit; `make gate-commit` gates
  every commit that touches Rust.
- Absorb `origin/main` at every task boundary (`git merge origin/main`), and
  push the branch at every task boundary.

## File Structure

| File | Responsibility |
|---|---|
| `windows/vessel/src/liveness.rs` | the eight keys, the `Mode`→key match, the commit site, the provenance flip, the `hoist_walk_shape` literal |
| `windows/vessel/src/resident.rs` | the `Errands` tenant and its `ResidentFolds` accessor |
| `windows/historiography/src/lib.rs` | the errand-aware grouping pass and the two renderings |
| `cli/src/repl.rs`, `windows/vessel/src/session.rs` | the `--steps` flag on `why` / `!why` |
| `windows/vessel/tests/suite/tick_commit_budget.rs` | the re-pointed fear/belonging witness |
| `windows/vessel/tests/suite/the_warrant.rs` | this campaign's own battery |
| `windows/lab/src/health.rs` and the other four registration sites | registering the eight keys alongside `agent-at` |

---

### Task 1: The eight keys and the total `Mode` → key mapping

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (near `AGENT_AT`, `:34`; and the
  `match st.mode` at `:8137-8152`)
- Test: `windows/vessel/tests/suite/the_warrant.rs` (create), registered in
  `windows/vessel/tests/suite.rs`

**Interfaces:**
- Produces: `pub const ERRAND_WATER_KNOWN: &str = "errand/water-known";` and
  seven siblings (`ERRAND_WATER_BLIND`, `ERRAND_FORAGE`, `ERRAND_COMFORT`,
  `ERRAND_REST`, `ERRAND_FLIGHT`, `ERRAND_COMPANY`, `ERRAND_HOME`);
  `pub(crate) fn errand_key(mode: Mode, believed: bool) -> &'static str`;
  `pub fn errand_predicates() -> [(&'static str, &'static str); 8]` returning
  `(key, doc)` pairs for the registration sites.
- Consumes: `Mode` and `DriveKind` (`liveness.rs:2099`, `:2040`-ish).

- [ ] **Step 1: Write the failing test**

`errand_key` is `pub(crate)`, so **the mapping table is asserted from inside
`liveness.rs`'s own `#[cfg(test)] mod tests`** and only the public surface is
asserted from the integration test. Do not widen the crate's public API for a
test.

In `windows/vessel/src/liveness.rs`, inside its existing `mod tests`:

```rust
/// The eight keys are the eight arms the prose match already had, one for
/// one. Written as an explicit table rather than a loop so a NINTH mode
/// cannot be silently absorbed: adding one makes `errand_key`'s own match
/// fail to compile, and adding one WITHOUT a key here leaves this table
/// short, which the count assertion catches.
#[test]
fn every_mode_maps_to_exactly_one_errand_key() {
    let cases: [(Mode, bool, &str); 8] = [
        (Mode::Pursuing(DriveKind::Thirst), true, ERRAND_WATER_KNOWN),
        (Mode::Pursuing(DriveKind::Thirst), false, ERRAND_WATER_BLIND),
        (Mode::Pursuing(DriveKind::Hunger), false, ERRAND_FORAGE),
        (Mode::Pursuing(DriveKind::Thermal), false, ERRAND_COMFORT),
        (Mode::Pursuing(DriveKind::Fatigue), false, ERRAND_REST),
        (Mode::Pursuing(DriveKind::Danger), false, ERRAND_FLIGHT),
        (Mode::Pursuing(DriveKind::Social), false, ERRAND_COMPANY),
        (Mode::Homing, false, ERRAND_HOME),
    ];
    for (mode, believed, expected) in cases {
        assert_eq!(errand_key(mode, believed), expected, "{mode:?} believed={believed}");
    }
    assert_eq!(errand_key(Mode::Idle, false), ERRAND_HOME, "Idle shares Homing's key");
}

```

Create `windows/vessel/tests/suite/the_warrant.rs` for the public half:

```rust
//! The Warrant (Penstock 7b): the typed, compositional intention.
use hornvale_vessel::liveness::{
    errand_predicates, ERRAND_WATER_KNOWN, ERRAND_WATER_BLIND, ERRAND_FORAGE,
    ERRAND_COMFORT, ERRAND_REST, ERRAND_FLIGHT, ERRAND_COMPANY, ERRAND_HOME,
};

/// Every key is registered with a non-empty doc, and the docs are the eight
/// glosses the renderer will show. A key with an empty doc would render as
/// the bare predicate string in `recount`, which is the failure mode this
/// campaign exists to remove.
#[test]
fn every_errand_predicate_carries_a_distinct_non_empty_doc() {
    let table = errand_predicates();
    assert_eq!(table.len(), 8);
    let mut keys: Vec<&str> = table.iter().map(|(k, _)| *k).collect();
    keys.sort_unstable();
    keys.dedup();
    assert_eq!(keys.len(), 8, "keys are distinct");
    let mut docs: Vec<&str> = table.iter().map(|(_, d)| *d).collect();
    docs.sort_unstable();
    docs.dedup();
    assert_eq!(docs.len(), 8, "docs are distinct");
    for (key, doc) in table {
        assert!(!doc.is_empty(), "{key} has an empty doc");
        assert!(key.starts_with("errand/"), "{key} is not in the errand namespace");
    }
}

/// SAVE-FORMAT CONTRACT. These eight strings are permanent on-disk keys, the
/// same way `agent-at` is (`liveness.rs`'s
/// `the_agent_at_predicate_spelling_is_a_permanent_on_disk_key`). Do not
/// rebaseline this literal — take an epoch.
#[test]
fn the_errand_predicate_spellings_are_permanent_on_disk_keys() {
    assert_eq!(ERRAND_WATER_KNOWN, "errand/water-known");
    assert_eq!(ERRAND_WATER_BLIND, "errand/water-blind");
    assert_eq!(ERRAND_FORAGE, "errand/forage");
    assert_eq!(ERRAND_COMFORT, "errand/comfort");
    assert_eq!(ERRAND_REST, "errand/rest");
    assert_eq!(ERRAND_FLIGHT, "errand/flight");
    assert_eq!(ERRAND_COMPANY, "errand/company");
    assert_eq!(ERRAND_HOME, "errand/home");
}
```

Add `mod the_warrant;` to `windows/vessel/tests/suite.rs` in its existing
alphabetical position.

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(the_warrant)'`
Expected: FAIL to compile — `errand_key`, `errand_predicates` and the eight
consts are not defined. A compile failure is the correct red here **because
the test asserts over items that do not exist yet**; the behavioural red that
matters arrives in Task 2, whose assertions run against a live walk.

- [ ] **Step 3: Write minimal implementation**

In `windows/vessel/src/liveness.rs`, beside `AGENT_AT` (`:34`):

```rust
/// The errand predicates — one per arm of the drive tick's commitment
/// `Mode`, naming WHY a creature set out. **SAVE-FORMAT CONTRACT:** these
/// eight strings are permanent on-disk keys, exactly as [`AGENT_AT`] is.
/// Deliberate regeneration takes an epoch suffix (`errand/forage/v2`), never
/// a rename.
///
/// The reason lives in the predicate rather than the object because
/// `register_predicate`'s doc string is the only prose
/// `hornvale_historiography::recount` renders for a predicate — so putting it
/// here moves the reader-facing words out of the ledger and into the concept
/// registry, which is `hornvale_kernel::phenomena`'s producer rule applied to
/// facts (The Warrant spec §4.2).
pub const ERRAND_WATER_KNOWN: &str = "errand/water-known";
/// An errand toward water the creature does NOT know: exploring blind.
pub const ERRAND_WATER_BLIND: &str = "errand/water-blind";
/// An errand toward richer forage.
pub const ERRAND_FORAGE: &str = "errand/forage";
/// An errand toward a kinder temperature.
pub const ERRAND_COMFORT: &str = "errand/comfort";
/// An errand home to rest, driven by fatigue.
pub const ERRAND_REST: &str = "errand/rest";
/// An errand AWAY from frightening ground — repulsion, not attraction.
pub const ERRAND_FLIGHT: &str = "errand/flight";
/// An errand homeward, driven by loneliness.
pub const ERRAND_COMPANY: &str = "errand/company";
/// An errand home with nothing pressing: the sated walk back.
pub const ERRAND_HOME: &str = "errand/home";

/// Which errand a creature is on, from the commitment [`Mode`] it carries and
/// whether it currently believes in a water source.
///
/// **Exhaustive, with no `_` arm, deliberately.** Widening [`Mode`] or
/// [`DriveKind`] must be a compile error here rather than a silent
/// fall-through into the wrong errand — for an enum widening the compiler is
/// the enumeration, and a wildcard would void it.
pub(crate) fn errand_key(mode: Mode, believed: bool) -> &'static str {
    match mode {
        Mode::Pursuing(DriveKind::Thirst) if believed => ERRAND_WATER_KNOWN,
        Mode::Pursuing(DriveKind::Thirst) => ERRAND_WATER_BLIND,
        Mode::Pursuing(DriveKind::Hunger) => ERRAND_FORAGE,
        Mode::Pursuing(DriveKind::Thermal) => ERRAND_COMFORT,
        Mode::Pursuing(DriveKind::Fatigue) => ERRAND_REST,
        Mode::Pursuing(DriveKind::Danger) => ERRAND_FLIGHT,
        Mode::Pursuing(DriveKind::Social) => ERRAND_COMPANY,
        Mode::Homing | Mode::Idle => ERRAND_HOME,
    }
}

/// The eight errand predicates paired with the doc a registry registers them
/// under — the gloss `recount` renders. Exposed so every site that registers
/// [`AGENT_AT`] on a session's registry clone registers these beside it from
/// ONE table, rather than eight copies drifting apart.
pub fn errand_predicates() -> [(&'static str, &'static str); 8] {
    [
        (ERRAND_WATER_KNOWN, "went down to the river it knew (thirst)"),
        (ERRAND_WATER_BLIND, "wandered, having found no water yet (thirst)"),
        (ERRAND_FORAGE, "foraged toward richer ground (hunger)"),
        (ERRAND_COMFORT, "sought a kinder clime (comfort)"),
        (ERRAND_REST, "turned home, weary, to rest"),
        (ERRAND_FLIGHT, "fled the uncanny ground (fear)"),
        (ERRAND_COMPANY, "drifted homeward, missing its people (belonging)"),
        (ERRAND_HOME, "walking home (sated)"),
    ]
}
```

- [ ] **Step 4: Write the two-way agreement test**

Until Task 3 deletes it, the prose `match st.mode` at `liveness.rs:8137` and
`errand_predicates()` hold **the same eight strings in two places**. A rule
duplicated on purpose needs a test that fails when either copy moves, not one
that only checks the new copy. In `liveness.rs`'s own `mod tests`, where both
are visible:

```rust
/// TRANSITIONAL (delete with the prose match in Task 3). The eight glosses
/// live in two places until the flip: this match, and `errand_predicates()`.
/// A one-directional check would let either copy drift. Assert BOTH
/// directions — every gloss is emitted by some mode, and every mode's prose
/// is some gloss.
#[test]
fn the_registry_glosses_and_the_live_prose_match_agree_both_ways() {
    let modes: [(Mode, bool); 8] = /* the same eight rows as the mapping test */;
    let emitted: BTreeSet<&str> = modes.iter()
        .map(|&(m, b)| prose_for(m, b))   // the match, lifted to a fn if it is not one
        .collect();
    let glossed: BTreeSet<&str> = errand_predicates().iter().map(|(_, d)| *d).collect();
    assert_eq!(emitted, glossed);
}
```

If lifting the match into a `prose_for` helper is the cheapest way to make it
testable, do that — it is deleted wholesale in Task 3 either way.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(the_warrant)'`
and `cargo nextest run -p hornvale-vessel -E 'test(errand)'`
Expected: PASS, all cases.

**Prove the agreement test can fail** before moving on: change one character
of one gloss in `errand_predicates()`, confirm RED, restore it, confirm GREEN.
Assert the target text exists before substituting — a no-op mutation produces
evidence and is worse than none.

- [ ] **Step 6: Register the eight keys everywhere `agent-at` is registered**

Five sites register `AGENT_AT` on a registry clone. Add the errand keys at
each, from the one table:

```rust
for (key, doc) in hornvale_vessel::liveness::errand_predicates() {
    let _ = registry.register_predicate(key, false, doc);
}
```

**Derive the set from the observable, do not trust a list.** An earlier draft
of this step enumerated five sites; grep finds six, one of which the list
missed and two of whose line numbers had already drifted. Run:

```bash
grep -rn 'register_predicate(\s*$\|register_predicate(AGENT_AT' --include='*.rs' . -A1 | grep -B1 AGENT_AT
```

Register the errand keys at **every production and example site**. Register
them at a **test** site only where that test reads an errand back — several
test sites register `AGENT_AT` with placeholder docs (`"pos"`, `""`) and adding
eight more registrations there is noise.

**`functional` is `false`**, like `agent-at`: a creature has many errands over
its life, and a functional predicate would make the second one a
contradiction.

- [ ] **Step 7: Verify no committed artifact moved**

Run: `make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`

**Decision rule, not a prediction** (spec §10):
- clean → commit as-is.
- only `docs/audits/` moved → regenerate and commit in the same commit.
- `book/src/reference/concept-registry-generated.md` moved → **STOP and
  report.** These are session-only predicates; if they reach the genesis
  registry dump, a registration went in the wrong place.
- `book/src/gallery/` moved → **STOP and report.** Task 1 commits no fact;
  nothing in a gallery transcript may move yet.

- [ ] **Step 8: Commit**

```bash
cargo fmt
make gate-commit
git add -u && git add windows/vessel/tests/suite/the_warrant.rs
git commit   # feat(the-warrant): the eight errand predicates and the total Mode mapping
```

---

### Task 2: Commit the errand fact at the mode boundary

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — `WalkState` (`:7827`), `begin`,
  and the `Intent::Do(Action::MoveTo(n))` arm (`:8131`)
- Test: `windows/vessel/tests/suite/the_warrant.rs`

**Interfaces:**
- Consumes: `errand_key(mode, believed)` from Task 1.
- Produces: `pub(crate) fn errand_fact(entity: EntityId, origin: &Facet, day: WorldTime, key: &str) -> Fact`
  — object `Value::Text(room_to_text(origin))`, `place: None`,
  `day: Some(day)`, `provenance: "vessel/liveness"`.
- Produces: a `WalkState.errand: Option<&'static str>` field, `None` until the
  walk's first step.

- [ ] **Step 1: Write the failing test**

```rust
/// A creature that walks commits ONE errand fact per errand — a maximal run
/// of constant reason — and never one per step. The synthetic walk harness is
/// used rather than seed 42, which commits no `agent-at` at all (spec §1).
#[test]
fn an_errand_commits_once_and_its_steps_commit_under_it() {
    let run = /* the same harness `the_kerf.rs` uses to force a real walk */;
    let steps: Vec<&Fact> = run.facts.iter().filter(|f| f.predicate == AGENT_AT).collect();
    let errands: Vec<&Fact> = run.facts.iter()
        .filter(|f| f.predicate.starts_with("errand/")).collect();
    assert!(!steps.is_empty(), "the harness must walk, or this test is vacuous");
    assert!(!errands.is_empty(), "a walk commits at least one errand");
    assert!(errands.len() < steps.len(),
        "errands ({}) must be sparser than steps ({})", errands.len(), steps.len());

    // The load-bearing assertion: every step is covered — there is an errand
    // fact for the same subject at or before the step's day.
    for step in &steps {
        let covering = errands.iter()
            .filter(|e| e.subject == step.subject && e.day <= step.day)
            .last();
        assert!(covering.is_some(), "step at {:?} has no covering errand", step.day);
    }
}

/// The count of errand facts equals the count of REASON CHANGES in the trail
/// — this is the losslessness claim of spec §1, asserted mechanically rather
/// than argued. Compare against the provenance runs the steps still carry
/// (Task 2 has not flipped them yet), which is why this test must be written
/// NOW and not after Task 3.
#[test]
fn one_errand_fact_per_run_of_constant_step_provenance() {
    let run = /* same harness */;
    for entity in run.subjects() {
        let provs: Vec<&str> = run.facts.iter()
            .filter(|f| f.subject == entity && f.predicate == AGENT_AT)
            .map(|f| f.provenance.as_str()).collect();
        let runs = provs.windows(2).filter(|w| w[0] != w[1]).count()
            + usize::from(!provs.is_empty());
        let errands = run.facts.iter()
            .filter(|f| f.subject == entity && f.predicate.starts_with("errand/")).count();
        assert_eq!(errands, runs,
            "entity {entity:?}: {errands} errand facts against {runs} provenance runs");
    }
}
```

**The implementer chooses the harness.** `the_kerf.rs`, `the_roll.rs` and
`resident_folds.rs` each already force a real walk; read them and take the
cheapest one that produces multi-step errands, rather than building a new
world. Do not use seed 42 — measured, it commits no `agent-at` in 90 days.

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(the_warrant)'`
Expected: FAIL — `errands` is empty, "a walk commits at least one errand".
**This is the behavioural red** that Task 1's compile-failure red was not.

- [ ] **Step 3: Write minimal implementation**

Add to `WalkState`:

```rust
    /// The errand key this walk is currently on — `None` before its first
    /// step. Tick-local like [`WalkState::mode`] itself; the COMMITTED errand
    /// fact is what outlives the tick.
    errand: Option<&'static str>,
```

Initialise it `None` in `begin`. In the `MoveTo` arm, **before** the existing
`out.push(agent_at_fact(...))`:

```rust
                // THE ERRAND BOUNDARY. `st.mode` was assigned from this
                // tick's resolution sixty lines above; a step whose errand
                // key differs from the one this walk is carrying is the first
                // step of a new errand, and that is the discrete divergence
                // the Penstock metaplan §5.6 rules must commit. The ORIGIN
                // goes in the object, not the target: the arbitration seam
                // exposes only `Intent::Do(Action)`, so no destination exists
                // here to record (spec §4.0). The endpoint is derived.
                let key = errand_key(st.mode, st.believed.is_some());
                if st.errand != Some(key) {
                    out.push(errand_fact(npc.entity, &st.pos, st.day, key));
                    st.errand = Some(key);
                }
                out.push(agent_at_fact(npc.entity, &n, st.day, provenance));
```

`st.pos` is read **before** the assignment `st.pos = n` two lines below, which
is what makes it the origin rather than the destination.

And beside `agent_at_fact` (`:6646`):

```rust
/// A committed errand fact: `entity` set out from `origin` on `day`, for the
/// reason `key` names. The object is the ORIGIN — see spec §4.0 for why there
/// is no target to record.
pub(crate) fn errand_fact(
    entity: EntityId,
    origin: &Facet,
    day: WorldTime,
    key: &str,
) -> Fact {
    Fact {
        subject: entity,
        predicate: key.to_string(),
        object: Value::Text(room_to_text(origin)),
        place: None,
        day: Some(day),
        provenance: PRODUCER.to_string(),
    }
}
```

with `const PRODUCER: &str = "vessel/liveness";` beside it.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-vessel 2>&1 | tee /tmp/hv-t2.txt`
Expected: the two new tests PASS. **Other tests will move** — anything
asserting an exact fact-count delta. Grep the file, do not re-run to find each
one. Every such assertion must be updated **only** where the added count is an
errand fact; a moved `agent-at` count is a defect, not a rebaseline.

- [ ] **Step 5: Capture the "before" side of H1, while the prose is still live**

Task 3 flips the provenance and then has to prove nothing a reader can see was
lost. That proof needs data taken **now**, not a re-derivation of itself after
the flip — an empty diff needs a positive control, and a self-comparison is
not one.

Write, to a committed fixture under `windows/vessel/tests/fixtures/`
(`the-warrant-glosses.json` or a `.tsv`, implementer's choice), for each
entity in the walking harness: the ordered list of `(day, provenance)` pairs
at each **run start** — that is, each `agent-at` fact whose provenance differs
from its predecessor's for that subject. Declare the file by name in
`docs/generated-paths.txt` if it is regenerated, or leave it undeclared and
hand-written if it is a frozen pin; say which in the commit message, because a
new file dropped into an already-declared directory inherits that directory's
author and becomes invisible to the drift check until it is `git add`-ed.

Task 3's `every_gloss_and_its_first_day_survives_the_flip` reads this fixture.

- [ ] **Step 6: Measure H3 (spec §10)**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(tick_commit_budget)'`
Record the reported facts/agent/tick against `STEADY_STATE_CEILING = 2.5`.
Write the number into the ledger. Expected shape: seed 42 unchanged (it
commits no steps, therefore no errands); a walking seed rises by at most
`1/2.36` of its `agent-at` rate.

**Decision rule:** ceiling exceeded → STOP and report; `MIN_CONTRIBUTING_RESIDENTS`
or `NON_GROWTH_MARGIN` red → STOP and report, because this task only ADDS
facts and neither should be able to move against it.

- [ ] **Step 7: Artifact check and commit**

Same drift check and branch table as Task 1 Step 6, with one change:
`book/src/gallery/` **may** now move, because the `(N stirred)` count is a
tally of committed facts. If it moves, regenerate and commit in the same
commit, and record the before/after `stirred` numbers in the ledger.

```bash
cargo fmt && make gate-commit
git add -u && git commit   # feat(the-warrant): commit one errand fact per errand
```

---

### Task 3: The epoch — retire prose from `agent-at` provenance

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — the `match st.mode` prose block
  (`:8137-8152`), the `hoist_walk_shape` literal (`:11150-11290`), and the
  in-crate tests at `:13486`, `:13731`
- Modify: `windows/vessel/tests/suite/tick_commit_budget.rs:315`, `:527-530`
- Test: `windows/vessel/tests/suite/the_warrant.rs`

**Interfaces:**
- Consumes: `errand_fact`, `PRODUCER` from Task 2.
- Produces: nothing new; this task deletes.

- [ ] **Step 1: Re-point the vacated instrument FIRST**

This step comes before the flip on purpose. `tick_commit_budget.rs:527-530`
counts fear/belonging by grepping `provenance` for `"(fear)"` and
`"(belonging)"`. After the flip that grep matches nothing, reads 0, and
**passes** — it stops measuring inside a green gate (spec §7.2).

Rewrite it to count facts whose `predicate` is `ERRAND_FLIGHT` or
`ERRAND_COMPANY`, reading the predicate out of `session_ledger_json()` the way
the closure at `:437-447` already reads provenance. Keep
`FEAR_OR_BELONGING_CEILING = 5` and its doc, amending the doc to say what it
now keys on and why.

**Prove the re-pointed witness can still go red before flipping anything.**
Do not prescribe how: name the property — *the witness must fail if
fear/belonging errands exceed the ceiling* — and find a mutation that
demonstrates it (lowering the ceiling to 0 against a run that produces one is
the obvious candidate; the implementer verifies the run actually produces
one). Assert the target text exists before substituting it; a no-op mutation
produces evidence and is worse than none.

- [ ] **Step 2: Write the failing test**

```rust
/// THE EPOCH. No committed `agent-at` fact carries authored prose any more:
/// its provenance names the producer, like every other fact in the repo. The
/// reader-facing words live in the concept registry, on the errand
/// predicates (spec §4.2).
#[test]
fn no_agent_at_provenance_is_authored_prose() {
    let run = /* the walking harness from Task 2 */;
    for f in run.facts.iter().filter(|f| f.predicate == AGENT_AT) {
        assert_eq!(f.provenance, "vessel/liveness",
            "an agent-at provenance still carries prose: {:?}", f.provenance);
    }
}

/// H1, the losslessness claim of spec §1, asserted as EXACT EQUALITY rather
/// than approximation: the set of reason-glosses a reader can see, and the
/// day each first appears, is identical before and after the flip. The
/// "before" side is the eight strings `errand_predicates()` carries, which
/// ARE the strings the prose match emitted — so this compares the errand
/// facts' glosses against the authored set, per entity, in day order.
#[test]
fn every_gloss_and_its_first_day_survives_the_flip() {
    let run = /* the walking harness */;
    let table: BTreeMap<&str, &str> = errand_predicates().into_iter().collect();
    for entity in run.subjects() {
        let seen: Vec<(WorldTime, &str)> = run.facts.iter()
            .filter(|f| f.subject == entity && f.predicate.starts_with("errand/"))
            .map(|f| (f.day.expect("an errand is dated"), table[f.predicate.as_str()]))
            .collect();
        // Recorded in the campaign's own fixture, captured BEFORE the flip in
        // Task 2 and compared here. The implementer writes that fixture in
        // Task 2 Step 4 while the prose is still live.
        assert_eq!(seen, expected_for(entity));
    }
}
```

`expected_for(entity)` reads the fixture **Task 2 Step 5 wrote while the prose
was still live**. If that fixture does not exist, stop and go back — this test
compared against a re-derivation of itself would pass unconditionally and
prove nothing.

- [ ] **Step 3: Run test to verify it fails**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(the_warrant)'`
Expected: `no_agent_at_provenance_is_authored_prose` FAILS naming a real prose
string.

- [ ] **Step 4: Make the flip**

Delete the eight-arm `let provenance = match st.mode { ... };` block entirely
and pass `PRODUCER` to `agent_at_fact`. The `Mode` match is not moved — it is
**deleted**, because `errand_key` is now the single mapping and two copies
would drift.

Leave `session.rs`'s `WALKED_PROVENANCE` and `RETRACED_PROVENANCE` untouched
(spec §7.4: the player has no `Mode` to promote; the asymmetry is accepted and
named).

- [ ] **Step 5: Hand-rewrite the `hoist_walk_shape` literal**

`liveness.rs:11150-11290`, ~108 rows of which ~28 are `agent-at`. Its own doc
forbids machine rebaselining. **Write down the transformation before running
the test, then confirm by running it** — the same discipline that doc records
for the Wicket/Pavement compose:

- every `agent-at` row's trailing field becomes `vessel/liveness`;
- new `errand/*` rows appear at each run boundary, one per boundary, with the
  ORIGIN in the object — which for the first step of an errand is the room the
  creature was standing in, i.e. the object of the PREVIOUS `agent-at` row (or
  the creature's home, for the walk's first errand);
- no `agent-at` row's object, day or order changes; no row disappears.

If the observed diff differs from the written-down transformation in any
respect, **STOP and report** — a literal transcribed from a failure witnesses
nothing.

- [ ] **Step 6: Fix the two in-crate assertions**

`liveness.rs:13486` asserts the recount contains `"went down to the river it
knew (thirst)"`; `:13731` asserts an `agent-at` provenance contains
`thirst`/`water`/`sustenance`. Both now assert over the wrong field. Re-point
them at the errand predicate and its registry doc. **Do not delete them** — a
delete-plus-add reads as a replacement while covering a different branch;
mutate what the old assertion pinned and confirm the new one catches it.

- [ ] **Step 7: Measure H1 and H2 (spec §10)**

H1 is Step 2's test. For H2, run the §1 probe on seeds 7, 14, 23 and 42 and
compute committed provenance bytes per agent per tick before (from `main`'s
binary) and after. Record all four in the ledger.

**Decision rule:** walking-regime reduction under 50%, or seed 42 moving at
all → report as a falsified prediction in the chronicle; do not retune
anything to rescue it.

- [ ] **Step 8: Artifacts and commit**

Drift check as before. `windows/lab/tests/fixtures/affect-trace-seed-42.txt`
may move — it routes through `agent_position`. Seed 42 commits no `agent-at`,
so a move here is **surprising**: investigate before rebaselining with
`make rebaseline-goldens`, and record what moved and why in the ledger.

```bash
cargo fmt && make gate-commit
git add -u && git commit   # feat(the-warrant)!: retire authored prose from agent-at provenance
```

---

### Task 4: The `Errands` resident tenant — STRUCK (pre-flight ruling R2)

**Not implemented in this campaign.** Spec §6 justified the tenant as the
index `why?` needs to find a step's covering errand. It is not: §5.4's own
design has `recount` group by a single pass over the fact list it has already
collected, and `recount` takes `&World` — it holds no `ResidentFolds` and
cannot acquire one without a `windows/historiography` -> `windows/vessel`
dependency that `cli/tests/suite/architecture.rs` forbids. The index would
ship with no production consumer, which YAGNI forbids and which is the same
"no possible caller" defect as the origin/target correction (ledger #6).

The tenant is **owed by whichever campaign first has a consumer** — 7c's
compaction, or the metaplan §5.7 preemption counters. Both are declared
non-goals of this campaign (spec §9). Recorded in ledger entry #7, ruling R2,
and surfaced to Nathan at G6 as a deliberate scope reduction.

Task numbering below is unchanged so that ledger lines and briefs already
written keep pointing at the same work.

---

### Task 5: The renderings

**Files:**
- Modify: `windows/historiography/src/lib.rs:32-64`
- Modify: `cli/src/repl.rs:280`, `:291` (the `why` command) and `:396-413`
  (the `facts` command)
- Modify: `windows/vessel/src/session.rs:651` (help), `:9636-9669` (`!why`)
- Test: `windows/historiography/tests/` and `windows/vessel/tests/suite/the_warrant.rs`

**Interfaces:**
- Produces: `pub fn recount_steps(world: &World, entity: EntityId) -> Option<String>`
  — the per-step view (spec §5.2). `recount` keeps its signature and becomes
  the rolled-up view (spec §5.1).

- [ ] **Step 1: Write the failing test**

```rust
/// The rolled-up recount names each errand once, with its gloss, its origin,
/// its step count and its day span — and does NOT repeat the gloss per step.
#[test]
fn a_recount_names_each_errand_once() {
    let text = recount(&world, entity).expect("the walker has facts");
    let gloss = "wandered, having found no water yet (thirst)";
    assert_eq!(text.matches(gloss).count(), 1,
        "the gloss appears once per errand, not once per step:\n{text}");
    assert!(text.contains("65 steps"), "the roll-up states the step count:\n{text}");
}

/// The per-step view keeps one line per step and resolves each step's
/// parenthetical from its covering errand — today's information plus a
/// position-in-errand today cannot express.
#[test]
fn the_step_view_resolves_each_steps_covering_errand() {
    let text = recount_steps(&world, entity).expect("the walker has facts");
    assert!(text.contains("step 2 of 65"), "{text}");
    assert!(text.contains("wandered, having found no water yet (thirst)"), "{text}");
}

/// `recount` stays predicate-blind for everything else: a non-errand,
/// non-agent-at fact renders exactly as it did before.
#[test]
fn unrelated_predicates_render_unchanged() { /* pin one `drank` line verbatim */ }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo nextest run -p hornvale-historiography`
Expected: FAIL — `recount_steps` undefined; `recount` still emits one line per
step.

- [ ] **Step 3: Implement**

`recount` gains a **pre-grouping pass** over the fact list: walk the facts in
order; when one's predicate starts with `errand/`, open a group; `agent-at`
facts join the open group; any other predicate flushes the group and renders
normally. A group renders as one line:

```
- {registry doc of the errand predicate}: from {origin} — {n} steps, days {first} to {last}, ending at {last object}
```

`windows/historiography` **gains no dependency on `windows/vessel`** — it keys
on the `errand/` prefix, which is registry data, and reads the doc through
`world.registry.predicate(...)` exactly as it already does. Adding a vessel
dependency would violate the layering test in `cli/tests/suite/architecture.rs`.

`recount_steps` renders one line per step with `(gloss — step i of n, day d)`,
resolving the covering errand by scanning the same grouped structure.

Add `--steps` to the repl `why` and the session `!why`, updating
`session.rs:651`'s help line.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo nextest run -p hornvale-historiography -p hornvale-vessel -p hornvale`
Expected: PASS.

- [ ] **Step 5: Capture the before/after renderings**

Run the §1 probe on seeds 7 and 23 and save both outputs to
`.superpowers/sdd/` scratch for the chronicle. This is the campaign's headline
exhibit; capture it while the code is fresh.

- [ ] **Step 6: Artifacts and commit**

`book/src/gallery/possession-over-time-seed-42.md` runs `!why hobgoblin` and
currently gets a miss. If the miss text changed, it moves. Regenerate and
commit in the same commit.

```bash
cargo fmt && make gate-commit
git add -u && git commit   # feat(the-warrant): render an errand once, with its steps rolled up
```

---

### Task 6: Definition of Done

**Files:**
- Create: `docs/decisions/<n>-the-errand-is-the-committed-unit-of-intention.md`
- Create: `docs/decisions/<n+1>-<amends-0238s-argument>.md`
- Modify: `book/src/frontier/idea-registry.md` (`UNI-intention-is-structured`,
  `TOOL-log-bounding-divergence-restoration`, `TOOL-log-bounding-epoch-fact-lifetime`)
- Create: `book/src/chronicle/the-warrant.md`, add to `book/src/SUMMARY.md`
- Create: `docs/retrospectives/the-warrant.md`
- Modify: `docs/audits/campaign-reconciliation.tsv` (rows for the plan,
  chronicle and retrospective; flip the spec row to `shipped`)

- [ ] **Step 1: Reserve the decision numbers**

Numbers are reserved in blocks and keyed on the branch name. Check
`docs/decisions/` for the next free block before writing either record;
`cli/tests/suite/docs_consistency.rs` enforces non-overlap.

- [ ] **Step 2: Write the two decision records**

The first records the errand as the committed unit, the origin-not-target
ruling (§4.0), and the eight permanent keys. The second **amends decision
0238's argument without reversing its order**: the ordering 7b-before-7c is
right, but not for the reason 0238 states — per-step prose is not what carries
the content, as §1 measures. Cite the measurement.

- [ ] **Step 3: Amend the three registry rows**

`UNI-intention-is-structured` → `shipped`, and correct "replaces per-step
commits" to say the errand *carries the why* while 7c does the replacing.
The two `TOOL-log-bounding-*` rows lose 7b as a blocker and gain what 7c may
now assume (spec §8), **including the endpoint asymmetry**: an errand's end is
derived from its steps, so a compaction that drops them must fold the endpoint
in first.

- [ ] **Step 4: Chronicle, freshness sweep, retrospective**

The chronicle leads with the measurement, not the mechanism. The freshness
sweep covers **live** references only — chronicles are history and are not
rewritten (spec §7.3) — and re-scores any Confidence Gradient bet this moved
(`book/src/open-questions.md`), which is a **grep** over the gradient's own
axes, not a judgment about which chapters sound relevant.

- [ ] **Step 5: Absorb main, then run the full drift check**

```bash
git merge origin/main
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

A ratchet can land between your last absorption and your close; absorb **at
G6**, not before writing the DoD artifacts.

- [ ] **Step 6: Commit, push, and stop at G6**

```bash
cargo fmt && make gate-commit
git add -u && git commit && git push
```

Then **STOP.** G6 is a hard stop: present the post-G3 ledger digest, with the
epoch entries leading it, and wait. Only after Nathan's call does
`closing-a-campaign` run and the merge go through
`make sluice BRANCH=campaign/the-warrant REF=<full-sha>`.

**A census refresh is owed only if the census actually moved.** Measured at
spec time: 252 columns, none matching `fact|tick|agent|drive|walk|ledger`. If
`book/src/laboratory/generated/the-census/` moves at any task's drift check,
submit `make sluice-census BRANCH=campaign/the-warrant REF=<full-sha>` and
land the goldens through the queue before the merge.
