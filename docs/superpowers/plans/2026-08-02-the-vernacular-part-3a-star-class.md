# The Vernacular, part 3a — `star-class` becomes a concept id

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop the ledger holding Morgan–Keenan spectral classification as
prose. `star-class` and `neighbor-class` commit a registered concept id; the
author's ground-truth register still reads "a yellow-white dwarf (F)", rendered
*from* that id.

**Architecture:** `domains/astronomy` gains one table pairing each of the nine
spectral concepts with its author's-frame display string, plus lookups in both
directions. The two `facts.rs` commit sites write the id. The two renderers
(`windows/book`'s `fragment_for`, `windows/explain`) render the display from
the id, and `windows/book`'s `fact_for` — which parses rendered prose *back*
into a fact — parses the display to the id, so The Echo's round-trip stays
inverse.

**Tech Stack:** Rust edition 2024, `serde` only (decision 0004). No new
dependencies. `make gate` as the commit gate.

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only.
- **No wall-clock time.**
- Every public item documented; every pub-boundary primitive carries a
  `type-audit:` verdict tag.
- **Zero `TODO` comments in the workspace. Do not add one.**
- Layering: `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain depends on
  the kernel and nothing else.
- `cargo fmt` last.
- **Three repo Bash guards:** the raw whole-workspace nextest invocation is
  blocked (use `make gate`); bare `git stash` / `git stash pop` are blocked;
  two test runs in one Bash call are blocked — capture once and grep.
- **`make census-check` needs `duckdb`** (installed 2026-08-02). It fires on any
  commit staging a calibration-pin file.

## This plan MOVES committed facts — the only one in the campaign that does

Parts 1 and 2 gated on "zero facts moved". **This one does not.** Task 2 and
Task 3 each change a committed fact's value, deliberately, and each measures
what moved. The rule that replaces the old gate:

> **Only the fact's own value may move.** If any *downstream* fact moves — a
> name, a deity, a settlement — the class was load-bearing somewhere
> undeclared, and that is a finding to report before continuing, not a diff to
> accept.

**The baseline is already snapshotted** at
`.superpowers/sdd/2026-08-02-the-vernacular-part-3a-star-class/baseline-seed-42.json`
(26309 facts, taken at the plan's base commit). **Every** task diffs against it
— including Task 1, which must show it byte-identical. Compare fact lists, not
whole files: `World` serializes the concept registry, so the file can move while
no fact does.

## Scope

Implements §5's plan 3a and §5.1 of
`docs/superpowers/specs/2026-08-02-the-vernacular-part-3-design.md`.

**Explicitly NOT in scope:** deleting `Phenomenon.description` (3b), the colour
path (3c), `SkyReport`, and **removing** the prose→fact backward edge in
`windows/book`. This plan keeps that edge *working and inverse*; removing it is
stage 4's job.

**Verified before writing, so you need not re-check:**
`windows/scene/src/lib.rs:941` and `windows/lab/src/metrics.rs:735` read
`system.star.class_name` — the **struct field**, not the fact. No
`scene/*/v1` schema is touched and no census metric moves.

---

### Task 1: The concept ↔ display table

**Files:**
- Modify: `domains/astronomy/src/star.rs` (near `class_name`'s construction,
  around `:68-76`)
- Test: `domains/astronomy/src/star.rs` (inline `mod tests`)

**Interfaces:**
- Produces, all in `domains/astronomy`:
  - `pub fn class_concept(display: &str) -> Option<&'static str>` — display
    prose → registered concept id.
  - `pub fn class_display(concept: &str) -> Option<&'static str>` — the inverse.
  - `pub const SPECTRAL_CLASSES: [(&str, &str); 9]` — the pairs, `(concept,
    display)`.
- Consumes: nothing. This task is a pure addition; no behaviour changes.

The nine pairs, which must match **exactly** the concepts part 2 registered
(`domains/astronomy/src/lib.rs`'s `register_concepts`) and the display strings
`star.rs`/`neighborhood.rs` already produce:

| concept id | display |
|---|---|
| `orange-dwarf` | `orange dwarf (K)` |
| `yellow-dwarf` | `yellow dwarf (G)` |
| `yellow-white-dwarf` | `yellow-white dwarf (F)` |
| `red-dwarf` | `red dwarf` |
| `sun-like-star` | `sun-like star` |
| `white-dwarf` | `white dwarf` |
| `orange-giant` | `orange giant` |
| `red-giant` | `red giant` |
| `blue-giant` | `blue giant` |

- [ ] **Step 1: Write the failing test**

Add to `domains/astronomy/src/star.rs`'s inline `mod tests`:

```rust
/// The table is a bijection: every concept has exactly one display string and
/// every display string maps back to the concept it came from. The round-trip
/// matters because `windows/book` parses rendered prose back into a fact (The
/// Echo's transfer law), so render and parse must be inverse or a recovered
/// fact stops equalling the committed one.
#[test]
fn concept_and_display_round_trip_in_both_directions() {
    for (concept, display) in SPECTRAL_CLASSES {
        assert_eq!(
            class_concept(display),
            Some(concept),
            "{display:?} must parse back to {concept:?}"
        );
        assert_eq!(
            class_display(concept),
            Some(display),
            "{concept:?} must render as {display:?}"
        );
    }
    assert_eq!(class_concept("a star"), None);
    assert_eq!(class_display("not-a-class"), None);
}

/// Every display string in the table is one a producer actually emits — the
/// table cannot drift into naming classes the code never mints.
#[test]
fn every_star_class_name_is_in_the_table() {
    for mass in [0.6, 0.79, 0.8, 1.04, 1.05, 1.4] {
        let s = generate_star(Seed(1));
        let name = class_name_of_mass(mass);
        assert!(
            class_concept(name).is_some(),
            "star.rs mints {name:?}, which the table does not carry"
        );
        let _ = s;
    }
}
```

**Note on the second test:** `star.rs` currently builds `class_name` inline
inside `generate_star`. Extract that `if mass.0 < 0.8 { … }` chain into
`fn class_name_of_mass(mass: f64) -> &'static str` and have `generate_star`
call it, so the test can reach it. That extraction is behaviour-preserving —
verify by the byte-identity check in step 5.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-astronomy --lib concept_and_display_round_trip`
Expected: FAIL — `cannot find value SPECTRAL_CLASSES in this scope`.

- [ ] **Step 3: Write the table and the lookups**

In `domains/astronomy/src/star.rs`:

```rust
/// The nine spectral classes, paired with the **author's-frame** display each
/// renders as. The concept ids are what the ledger commits (part 2 registered
/// all nine with `lexeme: Absent(Void::Unnamed(..))` — real, and nameable by
/// nobody in this world); the display strings are Morgan–Keenan taxonomy, which
/// the campaign permits in the author's ground-truth register on the same
/// footing as °C or solar masses: units are the author's frame, names are the
/// world's. A creature never says these.
/// type-audit: bare-ok(identifier-text)
pub const SPECTRAL_CLASSES: [(&str, &str); 9] = [
    ("orange-dwarf", "orange dwarf (K)"),
    ("yellow-dwarf", "yellow dwarf (G)"),
    ("yellow-white-dwarf", "yellow-white dwarf (F)"),
    ("red-dwarf", "red dwarf"),
    ("sun-like-star", "sun-like star"),
    ("white-dwarf", "white dwarf"),
    ("orange-giant", "orange giant"),
    ("red-giant", "red giant"),
    ("blue-giant", "blue giant"),
];

/// The registered concept a display string names, or `None` if it names none.
/// The parse direction: `windows/book`'s `fact_for` reads rendered prose back
/// into a fact and needs this to recover the committed id.
/// type-audit: bare-ok(identifier-text: display), bare-ok(identifier-text: return)
pub fn class_concept(display: &str) -> Option<&'static str> {
    SPECTRAL_CLASSES
        .iter()
        .find(|(_, d)| *d == display)
        .map(|(c, _)| *c)
}

/// The author's-frame display for a registered concept, or `None` if the
/// concept is not a spectral class. The render direction.
/// type-audit: bare-ok(identifier-text: concept), bare-ok(identifier-text: return)
pub fn class_display(concept: &str) -> Option<&'static str> {
    SPECTRAL_CLASSES
        .iter()
        .find(|(c, _)| *c == concept)
        .map(|(_, d)| *d)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-astronomy --lib class` — capture to a file and
grep it.
Expected: PASS.

- [ ] **Step 5: Prove the extraction moved nothing**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-3a-t1.json
```

Compare its **fact list** against `$BASELINE` (see "This plan MOVES committed
facts" above). This task is a pure addition plus a behaviour-preserving
extraction, so **every fact must be identical**. If any moved, the extraction
changed the class boundaries — read `class_name_of_mass`'s comparisons against
the original.

Then refresh the drifted artifact and commit it here, in this task:

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: **`docs/audits/type-audit-report.md` only** — three new `pub` items
with `type-audit:` tags drift it, and omitting that is named in `CLAUDE.md` as
a common miss. Part 1 of this campaign hit it once and had to sweep it
afterwards. Anything under `book/src/gallery/` here means the extraction moved
a rendering, which this task must not do.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(astronomy): pair each spectral class with its author-frame display

The ledger is about to stop holding Morgan-Keenan prose and start holding the
registered concept id part 2 minted. This is the table both directions need:
render (id -> display) for the author's ground-truth register, and parse
(display -> id) for the round-trip windows/book depends on.

Pure addition. Seed 42 unchanged."
```

---

### Task 2: `star-class` commits the concept id

**Files:**
- Modify: `domains/astronomy/src/facts.rs:268` (the `STAR_CLASS` commit)
- Modify: `windows/book/src/lib.rs:236` (`fragment_for`) and `:2002-2006`
  (`fact_for`)
- Modify: `windows/explain/src/lib.rs:39` and its use of `class`
- Test: `cli/tests/` — a new round-trip test (see step 1)

**Interfaces:**
- Consumes: `hornvale_astronomy::{class_concept, class_display, SPECTRAL_CLASSES}`
  from Task 1.
- Produces: `star-class` facts whose object is a concept id.

**This task moves committed facts.** Snapshot the baseline first:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-3a-baseline.json
```

Keep that file for Tasks 2 and 3.

- [ ] **Step 1: Write the failing test**

Create `cli/tests/star_class_is_a_concept.rs`:

```rust
//! The ledger holds a registered concept id for a star's class, never
//! Morgan-Keenan prose — and the prose the author's register renders from it
//! parses back to the same id, so `windows/book`'s knowledge round-trip (The
//! Echo's transfer law) still recovers the fact it started from.

use hornvale_kernel::{Seed, Value};

fn seed_42() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"))
}

#[test]
fn the_committed_star_class_is_a_registered_concept() {
    let world = seed_42();
    let mut checked = 0;
    for fact in world.ledger.find(hornvale_astronomy::facts::STAR_CLASS) {
        let Value::Text(id) = &fact.object else {
            panic!("star-class must be Text, got {:?}", fact.object)
        };
        assert!(
            world.registry.concept(id).is_some(),
            "star-class committed {id:?}, which is not a registered concept"
        );
        assert!(
            hornvale_astronomy::class_display(id).is_some(),
            "star-class committed {id:?}, which is not a spectral class"
        );
        checked += 1;
    }
    assert!(checked > 0, "seed 42 must commit a star-class fact");
}

#[test]
fn the_rendered_display_parses_back_to_the_committed_id() {
    for (concept, display) in hornvale_astronomy::SPECTRAL_CLASSES {
        let fragment = format!("orbiting a {display}");
        let recovered = hornvale_book::fact_for_public(&fragment)
            .unwrap_or_else(|| panic!("{fragment:?} must parse"));
        assert_eq!(
            recovered,
            (
                hornvale_astronomy::facts::STAR_CLASS.to_string(),
                Value::Text(concept.to_string())
            ),
            "the round-trip must recover the committed id, not the display"
        );
    }
}
```

`fact_for` is private. Add a public wrapper beside it in
`windows/book/src/lib.rs`, exactly as part 1 did for `gloss_concept_of`:

```rust
/// The public face of the private [`fact_for`], exported so
/// `cli/tests/star_class_is_a_concept.rs` can assert render and parse are
/// inverse. Do not make `fact_for` itself public — its privacy is what keeps
/// the construction table a Book concern.
pub fn fact_for_public(fragment: &str) -> Option<(String, Value)> {
    fact_for(fragment)
}
```

**Beware the article.** `fragment_for` writes `indefinite_article(class)` — "a"
or "an" chosen from the *display*'s first letter — and `fact_for` strips
`"an "` before `"a "`. The displays include `orange dwarf (K)` and
`orange giant`, which take "an". Keep both sides reading the display, never the
id, or "an orange giant" will fail to parse.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale --test star_class_is_a_concept`
Expected: FAIL — the committed value is `"yellow dwarf (G)"`, not a registered
concept.

- [ ] **Step 3: Commit the id, render the display, parse it back**

`domains/astronomy/src/facts.rs:268` — commit the concept, not the prose:

```rust
            STAR_CLASS,
            Value::Text(
                crate::star::class_concept(&system.star.class_name)
                    .expect("every minted class name is in SPECTRAL_CLASSES")
                    .to_string(),
            ),
```

`windows/book/src/lib.rs:236` — render the display from the id:

```rust
        (STAR_CLASS, Value::Text(concept)) => {
            // The ledger holds a concept id; the author's ground-truth register
            // renders it as Morgan-Keenan taxonomy, which is the author's frame
            // and not anything a creature says. An unknown id renders nothing
            // rather than leaking a raw registry key into prose.
            let display = hornvale_astronomy::class_display(concept)?;
            Some(Fragment::Modifier(format!(
                "orbiting {} {display}",
                indefinite_article(display)
            )))
        }
```

`windows/book/src/lib.rs:2002` — parse the display back to the id:

```rust
    if let Some(rest) = fragment.strip_prefix("orbiting ") {
        let display = rest
            .strip_prefix("an ")
            .or_else(|| rest.strip_prefix("a "))?;
        let concept = hornvale_astronomy::class_concept(display)?;
        return Some((STAR_CLASS.to_string(), Value::Text(concept.to_string())));
    }
```

`windows/explain/src/lib.rs:39` — it reads `class` and puts it in narration.
Map it through `class_display` at the point of use so the narration is
unchanged; if the id is unknown, fall back to omitting the clause rather than
printing the id.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale --test star_class_is_a_concept` then
`make gate 2>&1 | tee /tmp/hv-3a-t2.log`.
Expected: both PASS. `windows/book`'s own round-trip tests are the ones most
likely to red — read the message before changing anything.

- [ ] **Step 5: Measure what moved — the point of this task**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-3a-t2-world.json
```

Compare fact lists against `/tmp/hv-3a-baseline.json`, reporting the count and
a per-predicate breakdown.

**Expected: exactly the `star-class` facts, and nothing else.** If any other
predicate moved — a name, a deity, a settlement — **stop and report**. That
would mean the class was load-bearing somewhere undeclared, which is a finding
worth more than this task.

- [ ] **Step 6: Refresh drifted artifacts**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

`book/src/gallery/the-book.md` may legitimately change **only** if a display
string differs from what was committed before; the intent is that it does
**not** — the rendered prose is unchanged, only its source. A gallery diff here
means the render/parse pair is not inverse. Commit any genuine drift.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(astronomy): the ledger holds a star's class as a concept, not prose

star-class committed Value::Text(\"yellow dwarf (G)\") — Morgan-Keenan
classification as a durable fact — while the registry declares yellow-dwarf
nameable by nobody in this world. It now commits the concept id.

The author's ground-truth register still reads 'orbiting a yellow dwarf (G)',
rendered FROM the id; windows/book's fact_for parses that display back to the
id, so The Echo's knowledge round-trip still recovers the fact it started from.

Facts moved: <N>, all star-class. Nothing downstream."
```

---

### Task 3: `neighbor-class` commits the concept id

**Files:**
- Modify: `domains/astronomy/src/facts.rs:498` (the `NEIGHBOR_CLASS` commit)
- Test: `cli/tests/star_class_is_a_concept.rs` (extend)

**Interfaces:**
- Consumes: Task 1's table; Task 2's pattern.
- Produces: `neighbor-class` facts whose object is a concept id.

`neighbor-class` has **no** prose renderer — nothing in `windows/` reads its
value to build a sentence (verified: the only readers are `facts.rs`'s own test
and the schedule's predicate listing). So this task is the fact change alone.

- [ ] **Step 1: Write the failing test**

Add to `cli/tests/star_class_is_a_concept.rs`:

```rust
#[test]
fn the_committed_neighbor_class_is_a_registered_concept() {
    let world = seed_42();
    let mut checked = 0;
    for fact in world.ledger.find(hornvale_astronomy::facts::NEIGHBOR_CLASS) {
        let Value::Text(id) = &fact.object else {
            panic!("neighbor-class must be Text, got {:?}", fact.object)
        };
        assert!(
            world.registry.concept(id).is_some(),
            "neighbor-class committed {id:?}, which is not a registered concept"
        );
        assert!(
            hornvale_astronomy::class_display(id).is_some(),
            "neighbor-class committed {id:?}, which is not a spectral class"
        );
        checked += 1;
    }
    assert!(checked > 0, "seed 42 must commit neighbor-class facts");
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale --test star_class_is_a_concept the_committed_neighbor`
Expected: FAIL — the committed value is `"red giant"`, not a registered concept.

- [ ] **Step 3: Commit the id**

`domains/astronomy/src/facts.rs:498`:

```rust
                NEIGHBOR_CLASS,
                Value::Text(
                    crate::star::class_concept(crate::neighborhood::class_name(neighbor.class))
                        .expect("every neighbour class name is in SPECTRAL_CLASSES")
                        .to_string(),
                ),
```

- [ ] **Step 4: Run the tests and the gate**

Run: `cargo test -p hornvale --test star_class_is_a_concept` then
`make gate 2>&1 | tee /tmp/hv-3a-t3.log`.
Expected: PASS.

- [ ] **Step 5: Measure what moved**

Same comparison as Task 2 step 5, against `/tmp/hv-3a-baseline.json`.
**Expected: `star-class` and `neighbor-class` facts only.** Anything else
downstream is a finding — stop and report.

- [ ] **Step 6: Refresh artifacts and commit**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
cargo fmt
git add -A
git commit -m "feat(astronomy): neighbour classes are concepts in the ledger too

Same change as star-class, and simpler: neighbor-class has no prose renderer,
so the fact's value moves and nothing renders differently.

Facts moved: <N>, all neighbor-class. Nothing downstream."
```

---

### Task 4: The epoch decision, and the readout

**Files:**
- Modify: `docs/superpowers/specs/2026-08-02-the-vernacular-part-3-design.md`
  (§6's preregistered measurement gets its result)
- Possibly create: `docs/decisions/NNNN-*.md` — **only if an epoch is owed**

**Interfaces:**
- Consumes: Tasks 2 and 3's measurements.
- Produces: the recorded readout parts 3b/3c build on.

Decision 0084 governs: **an epoch is declared only when a derivation moved.**
This task decides, with the measurements in hand, and records either outcome.

- [ ] **Step 0: Guard the mapping the commit site panics on**

Task 3's review found that nothing asserts every `NeighborClass` variant maps
into `SPECTRAL_CLASSES`. `star.rs` has `every_star_class_name_is_in_the_table`
for its mass buckets; `neighborhood.rs`'s six variants have none — and
`facts.rs` `.expect()`s on the lookup, so a drift between the two tables is a
worldgen **panic** on some seed nobody has generated. Seed 42 draws five of the
six; `BlueGiant` is correct by inspection and unguarded.

Add to `domains/astronomy/src/neighborhood.rs`'s inline `mod tests`:

```rust
/// Every neighbour class maps into the spectral table the ledger commits
/// through. `facts.rs` `.expect()`s this lookup, so a drift between the two
/// tables is a panic on whatever seed first draws the orphaned variant —
/// seed 42 draws only five of the six.
#[test]
fn every_neighbour_class_is_in_the_spectral_table() {
    for class in [
        NeighborClass::RedDwarf,
        NeighborClass::SunLike,
        NeighborClass::WhiteDwarf,
        NeighborClass::OrangeGiant,
        NeighborClass::RedGiant,
        NeighborClass::BlueGiant,
    ] {
        let display = class_name(class);
        assert!(
            crate::star::class_concept(display).is_some(),
            "{class:?} mints {display:?}, which SPECTRAL_CLASSES does not carry"
        );
    }
}
```

**Prove it can fail**: remove one pair from `SPECTRAL_CLASSES`, confirm the
test reds naming that variant, restore, prove `git status --porcelain` is
empty. Then run `cargo test -p hornvale-astronomy --lib every_neighbour`.

If `NeighborClass` does not derive `Debug`, use the display string in the
message instead of `{class:?}` rather than adding a derive.

- [ ] **Step 1: Assemble the measurement**

From Tasks 2 and 3's reports: total facts moved, the per-predicate breakdown,
and whether *anything downstream* moved. State the commit each was measured at.

- [ ] **Step 2: Decide the epoch question**

The test is 0084's: did a **derivation** move, or only a value's *spelling*?

- The class is a pure function of mass, unchanged. The same star yields the
  same class; only how the ledger *writes* it changed. That is a re-spelling of
  a derived label, not a moved derivation.
- **If nothing downstream moved**, no epoch is owed — the honest outcome, and
  the one 0084 calls for. Record it as a decision-log entry only if a reviewer
  disagrees; otherwise the spec readout suffices, exactly as 0084's own
  `room/furnishing` case did.
- **If something downstream moved**, an epoch *is* owed and the derivation that
  moved must be named. Do not declare one to be safe — 0084 calls that the
  "empty epoch" defect, and it charges a permanent manifest row for a
  discontinuity that did not occur.

- [ ] **Step 3: Write the readout into §6**

Give the frozen prediction alongside the measured result, and say plainly which
way the epoch question fell and why. If the prediction was falsified — anything
downstream moved — **that is the headline**, and it ships as one. Decision 0016
exists to make that reportable rather than tunable.

- [ ] **Step 4: Run the gate and commit**

```bash
make gate 2>&1 | tee /tmp/hv-3a-t4.log
git add -A
git commit -m "docs(the-vernacular-3): 3a readout — <N> facts moved, epoch <owed|not owed>"
```

---

## Self-review

**Spec coverage.** §5 plan 3a → Tasks 1–3. §5.1(a) (the display stays in the
author's register, rendered from the id) → Task 2 step 3. §5.1(b) (scene and
lab metric untouched) → stated under Scope, verified before writing. §5.1(c)
(the round-trip stays inverse) → Task 2's second test and its `fact_for`
change. §6's preregistered measurement → Task 4. **Not covered, by design:**
`Phenomenon.description` (3b), colour (3c), `SkyReport`, and *removing* the
backward edge (stage 4).

**Type consistency.** `class_concept(display) -> Option<&'static str>` and
`class_display(concept) -> Option<&'static str>` are used with those exact
signatures in Tasks 1, 2 and 3. `SPECTRAL_CLASSES: [(&str, &str); 9]` is
`(concept, display)` in that order everywhere. `fact_for_public` mirrors
`fact_for`'s `Option<(String, Value)>`.

**Two things stated as instructions to check rather than assertions.** Task 1's
second test needs `class_name_of_mass` extracted from `generate_star`, which
does not exist yet — the plan says to extract it and to prove the extraction
moved nothing. And Task 3 claims `neighbor-class` has no prose renderer, from a
grep of its readers; the implementer should re-grep before relying on it, since
a missed renderer would print a raw registry key into prose — the exact defect
this campaign exists to remove.
