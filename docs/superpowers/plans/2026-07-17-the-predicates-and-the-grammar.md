# The Predicates & the Grammar (C2) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The Book describes the planet in one aggregated sentence and classifies the peoples through `instance-of`: "Vebe is a planet with two moons, orbiting a yellow-white dwarf; its day lasts about 1.5 standard days." / "The Grobnar are goblins."

**Architecture:** Add a `person` concept (every culture lexicalizes it → the autonym); fix the C1 planet-identity wart (classify the world-root fact-holder, not a phantom mint); extend `clause.rs` with a modifier tail + pronoun + number words; give `windows/book` a construction table + depth-1 aggregation + referring expressions; wire the first `instance-of` facts into genesis.

**Tech Stack:** Rust 2024, std-only, `serde`/`serde_json` only. `cargo nextest`. No new crates.

## Global Constraints

- **Determinism:** same seed → byte-identical Book and world. Streams are concept-keyed, so adding `person` is an additive draw (existing lexemes unchanged). Float objects quantize at commit; text is exact.
- **No `HashMap`/`HashSet`, no wall-clock.** `BTreeMap`/`BTreeSet`/`Vec` only. `#![warn(missing_docs)]`; every `pub` item + pub-boundary primitive carries a `type-audit:` tag (CI-only gate — run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before committing pub-boundary changes).
- **Layering:** `kernel → domains/* → windows/* → cli`. `domains/language` stays domain-agnostic (frames, number words); the construction table (which domain predicate renders how) lives in `windows/book`.
- **Golden re-pins go in the drifting commit**, never deferred (rebaseline-golden-pins). **Census carve-out:** if `make gate-full` shows a census move from the `person` concept, STOP and get owner authorization (AWS regen) — never regenerate the census locally.
- **`cargo fmt` as the final step before every commit.**

---

## File structure

- `domains/language/src/packs.rs` — add the `person` universal-stratum concept (modify).
- `domains/language/src/clause.rs` — modifier tail on `Classify`, `Subject::{Name,Pronoun}`, `cardinal`, `quantity` (modify).
- `windows/book/src/lib.rs` — construction table, aggregation, referring expressions, read `instance-of` (modify).
- `windows/worldgen/src/lib.rs` — planet-identity fix (use `world_entity`); the `instance-of` species stage (modify).
- `cli/tests/fixtures/world-seed-42.json` — re-pinned (each task that drifts it).
- `book/src/gallery/the-book.md`, `book/src/reference/*` — regenerated.
- `docs/decisions/0061-the-classification-split.md` — ADR (new).

---

### Task 1: Add the `person` concept

**Files:**
- Modify: `domains/language/src/packs.rs` (`universal_stratum`)
- Test: `domains/language/src/packs.rs` (`#[cfg(test)]`), `cli/tests/fixtures/world-seed-42.json` re-pin

**Interfaces:**
- Produces: a registered `person` concept (`ConceptKind::Living`), lexicalized by every culture; `lexicon.entry("person")` resolves to a `Root`/`Compound` for each peopled race.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn person_is_a_registered_universal_concept() {
    assert!(universal_stratum().iter().any(|e| e.concept == "person"),
        "person is in the universal stratum");
}
```

- [ ] **Step 2: Run** `cargo test -p hornvale-language person_is_a_registered` → FAIL.

- [ ] **Step 3: Implement** — add to the `universal_stratum()` slice (its default `KIND` is `Substance`, so give `person` an explicit `ConceptKind::Living`):

```rust
PackEntry {
    concept: "person",
    kind: ConceptKind::Living,
    doc: "a person; a member of a people (the autonym root)",
    ladder_rank: 0,
},
```

- [ ] **Step 4: Run** the test → PASS. Then confirm a built world lexicalizes it:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -q -p hornvale -- dictionary --world /tmp/hv.json | grep -i "person" | head
```
Expected: each people has a `person` entry (a root word).

- [ ] **Step 5: Re-pin + regen (the drifting artifacts), then commit**

```bash
REBASELINE=1 cargo test -p hornvale --test lens_purity
bash scripts/regenerate-artifacts.sh    # concepts + dictionary dumps gain 'person'
git diff --stat book/src/reference/      # expect concept-registry + dictionary changes only
cargo fmt
git add domains/language/src/packs.rs cli/tests/fixtures/world-seed-42.json book/src/reference/ book/src/gallery/
git commit -m "feat(language): the person universal-stratum concept (C2 T1)"
```

- [ ] **Step 6: Census check (record, do not fix locally)** — note in the report that the census will be verified at the final `make gate-full`; if it moves, that is the owner-authorization carve-out.

---

### Task 2: The planet-identity fix

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (the `"planet"` stage, ~line 2897, inside `build_to`)
- Test: `windows/worldgen/src/lib.rs` (`#[cfg(test)]`)

**Interfaces:**
- Consumes: `world_entity` (the root fact-holder, bound at `build_to` ~line 2348).
- Produces: `is-a planet` + `name` on `world_entity` (not a fresh mint); `planet_entity(world)` returns `world_entity`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_planet_is_the_world_root_fact_holder() {
    let world = constant(1);
    let p = planet_entity(&world).expect("a planet entity");
    // the planet carries the world facts, e.g. moon-count
    assert!(world.ledger.value_of(p, "moon-count").is_some(),
        "the planet entity holds the astronomical facts");
}
```

- [ ] **Step 2: Run** → FAIL (today the planet is a separate empty mint).

- [ ] **Step 3: Implement** — in the `"planet"` stage, replace `let planet = world.ledger.mint_entity();` with `let planet = world_entity;` (the root already holds the astronomical/terrain facts). Keep the `is-a`/`name` commits on it. `world_entity` is in scope (bound ~line 2348).

- [ ] **Step 4: Run** → PASS.

- [ ] **Step 5: Re-pin + commit**

```bash
REBASELINE=1 cargo test -p hornvale --test lens_purity
cargo fmt
git add windows/worldgen/src/lib.rs cli/tests/fixtures/world-seed-42.json
git commit -m "fix(worldgen): the planet is the world-root fact-holder, not a phantom mint (C2 T2)"
```

---

### Task 3: `clause.rs` — modifier tail, pronoun, number words

**Files:**
- Modify: `domains/language/src/clause.rs`
- Test: same (`#[cfg(test)]`)

**Interfaces:**
- Produces:
  - `pub enum Subject { Name(String), Pronoun(&'static str) }` (e.g. `Pronoun("it")`, `Pronoun("its")`).
  - `ClauseSpec.subject: Subject`; a new `pub modifiers: Vec<String>` field.
  - `pub fn cardinal(n: u64) -> String` (`0→"zero" … 12→"twelve"`, else the digits).
  - `pub fn quantity(x: f64) -> String` (`1.5507 → "about 1.5"` — one decimal, "about " prefix).
  - `realize_common` appends a dry modifier tail: `"‹head›, ‹m1›, ‹m2›."` (a leading `with`/comma is the caller's phrasing, inside the modifier strings).

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn classify_with_modifier_tail() {
    let s = ClauseSpec { frame: Frame::Classify, subject: Subject::Name("Vebe".into()),
        complement: "planet".into(), number: Number::Sg, definiteness: Definiteness::Indef,
        modifiers: vec!["with two moons".into(), "orbiting a yellow-white dwarf".into()] };
    assert_eq!(realize_common(&s), "Vebe is a planet with two moons, orbiting a yellow-white dwarf.");
}
#[test]
fn cardinal_words() { assert_eq!(cardinal(2), "two"); assert_eq!(cardinal(13), "13"); }
#[test]
fn quantity_rounds() { assert_eq!(quantity(1.5507), "about 1.5"); }
```

- [ ] **Step 2: Run** `cargo test -p hornvale-language clause` → FAIL.

- [ ] **Step 3: Implement** — add `Subject`, the `modifiers` field, `cardinal`, `quantity`; in `realize_common`, render the subject (Name or Pronoun), then the head, then join `modifiers` with `", "` after the complement, before the terminal `.`. Update the C1 tests' `ClauseSpec` literals to include `subject: Subject::Name(...)` and `modifiers: vec![]`. Every new `pub` item gets a doc + `type-audit:` tag (`cardinal`/`quantity` returns: `bare-ok(prose)`; `Subject` variants: identifier-text/prose).

- [ ] **Step 4: Run** → PASS (all clause tests, including the updated C1 ones).

- [ ] **Step 5:** `type-audit -- check` clean; `cargo fmt`; commit `feat(language): clause modifier tail + pronoun subject + number words (C2 T3)`.

---

### Task 4: `windows/book` — construction table, aggregation, referring expressions

**Files:**
- Modify: `windows/book/src/lib.rs`
- Test: same

**Interfaces:**
- Consumes: Task 3 (`Subject`, `modifiers`, `cardinal`, `quantity`).
- Produces: `render_volume` now, per subject, renders one aggregated sentence: the `is-a` head + modifier fragments from the construction table, with pronoun reduction after first mention.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn planet_sentence_aggregates_moons_and_star() {
    let world = /* build seed 1 via build_world + SkyChoice::Constant, as T5-C1 */;
    let vol = render_volume(&world);
    let line = vol.lines.iter().find(|l| l.contains(" is a planet")).unwrap();
    assert!(line.contains("with two moons"), "aggregates moon-count: {line}");
    assert!(line.contains("dwarf") || line.contains("star"), "aggregates star-class: {line}");
}
```

- [ ] **Step 2: Run** → FAIL.

- [ ] **Step 3: Implement**

```rust
// construction table: predicate -> optional modifier fragment for a fact's object
fn modifier_for(pred: &str, obj: &Value) -> Option<String> {
    match (pred, obj) {
        ("moon-count", Value::Number(n)) => {
            let k = *n as u64;
            Some(format!("with {} moon{}", cardinal(k), if k == 1 { "" } else { "s" }))
        }
        ("star-class", Value::Text(c)) => Some(format!("orbiting a {c}")),
        ("day-length-std", Value::Number(d)) => Some(format!("its day lasts {} standard days", quantity(*d))),
        _ => None,
    }
}
```

In `render_volume`, for each `is-a`/`instance-of` subject: collect the subject's facts (`world.ledger.facts_about(subject)`), map each through `modifier_for`, collect the `Some` fragments in a deterministic order (facts iterate in commit order), and build one `ClauseSpec` with those `modifiers`. Track whether the subject was already named in this volume; on re-mention use `Subject::Pronoun("it"/"its")` (the day fragment already embeds "its", so referring-expression reduction here is the sentence subject only). Keep `uncovered_predicates` — moon-count/star-class/day-length now drop off the list.

- [ ] **Step 4: Run** → PASS; inspect a rendered volume by eye.

- [ ] **Step 5:** `type-audit -- check`; `cargo fmt`; commit `feat(book): construction table + depth-1 aggregation (C2 T4)`.

---

### Task 5: The `instance-of` species stage + The Book reads it

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (a new genesis stage), `windows/book/src/lib.rs` (render instance-of)
- Test: both

**Interfaces:**
- Consumes: `mint_instance_of_kind(world, wc, kind, day, provenance)` (`lib.rs:3138`), `placed_peoples(world)` (the placed peopled `KindId`s, `lib.rs:3157`), `lexicon_of(world, kind)` (`lib.rs:2026`), Task 1's `person` concept.
- Produces: per placed peopled species, a collective entity with `name = ⟨autonym⟩` (`lexicon_of(...).entry("person").roman`) and `instance-of = ⟨species KindId⟩`; The Book renders "The ⟨Autonym⟩ are ⟨species⟩."

- [ ] **Step 1: Write the failing test** (worldgen)

```rust
#[test]
fn each_placed_people_has_a_named_instance_of_collective() {
    let world = constant(1);
    // at least one entity carries instance-of a placed species kind + a name
    let has = world.ledger.find("instance-of").any(|f| {
        matches!(&f.object, Value::Text(_)) && world.ledger.text_of(f.subject, "name").is_some()
    });
    assert!(has, "a named collective per placed people");
}
```

- [ ] **Step 2: Run** → FAIL (0 instance-of facts today).

- [ ] **Step 3: Implement** — add a stage after `"planet"` in `build_to`: for each `kind` in `placed_peoples(&world)`, `let e = mint_instance_of_kind(&mut world, wc, kind.0, None, "the people as a roster kind")?;` then commit `name` from `lexicon_of(&world, kind.0)?.entry("person")` (`Root/Compound.views.roman`; `None` → leave unnamed, a coverage gap). In `windows/book`, render an `instance-of` subject as `ClauseSpec { frame: Classify, subject: Name(autonym), complement: species_label, number: Number::Pl, definiteness: Indef, modifiers: vec![] }` → "The Grobnar are goblins." (Add a `Frame`/definiteness path for the "The X are Y" plural-classify form if the C1 realizer doesn't already produce it; the plural generic already exists from C1's `classify_generic_plural`.)

- [ ] **Step 4: Run** both crates' tests → PASS; inspect the rendered volume for a people line.

- [ ] **Step 5: Re-pin + commit**

```bash
REBASELINE=1 cargo test -p hornvale --test lens_purity
type-audit check; cargo fmt
git add windows/worldgen/src/lib.rs windows/book/src/lib.rs cli/tests/fixtures/world-seed-42.json
git commit -m "feat(worldgen,book): instance-of species collectives named by autonym (C2 T5)"
```

---

### Task 6: Decision 0061, regen, and the census-clean gate

**Files:**
- Create: `docs/decisions/0061-the-classification-split.md`
- Modify: reference dumps, `book/src/gallery/the-book.md`

- [ ] **Step 1:** Write ADR `0061` (confirm it's the next free number via `ls docs/decisions/`): `is-a` = functional/immutable concept-class; `instance-of` = non-functional/mutable roster-kind; both coexist; the Book reads both. Follow an existing ADR template.

- [ ] **Step 2:** `bash scripts/regenerate-artifacts.sh`; `git diff --stat` — expect `the-book.md` (richer sentences + people lines), `concept-registry`/`dictionary` (person), no unexpected drift. Commit the regenerated artifacts + ADR.

- [ ] **Step 3: The census-clean gate** — `make gate-full` (foreground, `timeout: 3600000`). If GREEN, C2 is merge-ready. **If a census/lab metric moved (the `person` concept is the likely cause), reply BLOCKED and get owner authorization** — do not regenerate the census locally.

- [ ] **Step 4:** `cargo fmt`; commit `docs(decisions): ADR 0061; regen artifacts (C2 T6)`.

---

## Self-review notes

- **Spec coverage:** T1 (person concept) · T2 (planet-identity fix) · T3 (clause extensions) · T4 (construction table + aggregation + referring expressions) · T5 (instance-of species proof) · T6 (ADR 0061 + regen + census gate). All spec §3 components and §9 flags covered.
- **Verify-before-final** (confirm during execution): `ConceptKind::Living` is the right variant for `person`; `placed_peoples`/`lexicon_of` signatures; the plural-classify realizer path for "The X are Y"; the `dictionary` CLI subcommand name for the T1 check.
- **Determinism:** `person` is an additive concept-keyed draw; every ledger-drifting task re-pins `world-seed-42.json` in its own commit; T6 gates census-clean with the carve-out.
