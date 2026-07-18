# The First Sentence (C1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `hornvale book` renders "Elthandil is a planet" for three worlds from committed ledger facts, with zero human/LLM surface text.

**Architecture:** A kernel-core `is-a` predicate lets entities assert their class as facts. Worldgen mints the planet as an entity and commits its `is-a` and `name` facts (the name is the dominant peopled race's lexicon word for `earth`). A new `ClauseSpec` in `domains/language` (generalizing `render_line`) realizes a fact into a Common sentence; `windows/book` reads the ledger and assembles per-world volumes; the CLI emits them as a drift-checked artifact.

**Tech Stack:** Rust 2024, std-only, `serde`/`serde_json` only. `cargo nextest`. No new crates.

## Global Constraints

- **Determinism:** same seed + pins → byte-identical output. Float objects quantize at `Ledger::commit` (already handled); text is exact.
- **No `HashMap`/`HashSet`, no wall-clock time.** `BTreeMap`/`BTreeSet`/`Vec` only. `#[warn(missing_docs)]` on every crate; every `pub` item documented one line.
- **Layering:** `kernel → domains/* → windows/* → cli`. A domain depends on `hornvale-kernel` and nothing else. `windows/book` may depend on domains; it is built through `hornvale-worldgen`.
- **No new seeded draws** (metaplan decision #1): classification reads structure, the name is an existing lexicon lookup.
- **Run `cargo fmt` as the final step before every commit.** Gate before merge: `make gate`; census verification: `make gate-full`.

---

## File structure

- `kernel/src/world.rs` — register the `is-a` predicate in `World::new` (modify).
- `domains/language/src/clause.rs` — new: `ClauseSpec`, `Frame`, `Subject`, `Number`, `Definiteness`, and `realize_common`.
- `domains/language/src/lib.rs` — export the new `clause` module (modify).
- `windows/worldgen/src/lib.rs` — mint the planet entity + commit `is-a`/`name`; add `dominant_people` + `world_name` (modify).
- `windows/book/` — new crate: `Cargo.toml`, `src/lib.rs` (`render_volume`, `BookVolume`, coverage report).
- `cli/src/main.rs` — `hornvale book` subcommand + `cmd_book` (modify).
- `cli/Cargo.toml`, root `Cargo.toml` — add `hornvale-book` (modify).
- `book/src/gallery/the-book.md` — the committed, drift-checked artifact (new).
- `book/src/SUMMARY.md` — The Book primary; Chronicle + Frontier to appendices (modify).
- `.github/workflows/ci.yml`, `scripts/regenerate-artifacts.sh` — regenerate + drift-check The Book (modify).
- `docs/decisions/0058-*.md`, `0059-*.md`, `0060-*.md` — three ADRs (new).

---

### Task 1: The kernel-core `is-a` predicate

**Files:**
- Modify: `kernel/src/world.rs:11-39`
- Test: `kernel/src/world.rs` (module `#[cfg(test)]`)

**Interfaces:**
- Produces: the predicate name constant `hornvale_kernel::world::IS_A = "is-a"`, registered by `World::new`, functional (an entity has one class).

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn every_world_registers_is_a() {
    let w = World::new(Seed::new(1));
    assert!(w.registry.predicate("is-a").is_some(), "is-a must be registered");
    assert!(w.registry.predicate("is-a").unwrap().functional, "is-a is functional");
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel every_world_registers_is_a`
Expected: FAIL (predicate absent).

- [ ] **Step 3: Implement**

Add beside `NAME` in `kernel/src/world.rs`:

```rust
/// The classification predicate: `(entity, is-a, <kind-label>)`. Functional —
/// an entity has one class. Object is a `Value::Text` KindId label.
/// type-audit: bare-ok(identifier-text)
pub const IS_A: &str = "is-a";
```

In `World::new`, after the `NAME` registration:

```rust
registry
    .register_predicate(IS_A, true, "the class an entity belongs to")
    .expect("core concept registration cannot conflict in an empty registry");
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel every_world_registers_is_a`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add kernel/src/world.rs
git commit -m "feat(kernel): the is-a classification predicate (C1 T1)"
```

---

### Task 2: The dominant-people measurement + the world name

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Test: `windows/worldgen/src/lib.rs` (`#[cfg(test)]`)

**Interfaces:**
- Consumes: `WorldComponents::assemble()` (biosphere carries `mass: Mass` and `potency`; the peopled `lexicon` per `KindId`), `flagship_of(world, kind)` for population.
- Produces:
  - `pub fn dominant_people(world: &World) -> Option<KindId>` — the peopled race maximizing `Σ(population × mass)`, tie-broken by ascending `KindId`.
  - `pub fn world_name(world: &World) -> Option<String>` — that race's `lexicon.entry("earth")` romanized view (the endonym), `None` if the race lacks the concept (a coverage gap, never a silent fallback).

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn dominant_people_weights_by_mass_not_headcount() {
    let world = constant(1);
    let d = dominant_people(&world).expect("a peopled world has a dominant race");
    // deterministic across rebuilds
    assert_eq!(dominant_people(&world), Some(d));
    // the world name is that race's word for "earth", capitalized
    let name = world_name(&world).expect("dominant race names the world");
    assert!(name.chars().next().unwrap().is_uppercase(), "endonym is capitalized");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen dominant_people_weights_by_mass`
Expected: FAIL (functions absent).

- [ ] **Step 3: Implement**

`Σ(population × mass)` over peopled kinds. Population is the summed settled population; reuse the settlement accessor the almanac path uses (`flagship_of` gives one settlement — sum across a race's settlements if a total accessor exists; otherwise flagship population is the C1 stand-in, noted as a follow-up). Compare with `f64::total_cmp`, tie-break by `KindId` ascending. `world_name` looks up `lexicon.entry("earth")` and takes the `Root { views, .. } | Compound { views, .. }` `views.roman`; a `Gap` or absent entry returns `None`.

```rust
/// The peopled race with the greatest Σ(population × mass); tie-broken by
/// ascending KindId. `None` if the world has no peopled race.
pub fn dominant_people(world: &World) -> Option<KindId> {
    let wc = WorldComponents::assemble().ok()?;
    let mut best: Option<(f64, KindId)> = None;
    for (kind, bio) in wc.biosphere.iter() {
        if wc.lexicon.get(kind).is_none() {
            continue; // biosphere-only creatures have no lexicon; not candidates
        }
        let pop = settled_population(world, kind.0) as f64; // see note above
        let weight = pop * bio.mass.value();
        let better = match best {
            None => true,
            Some((w, k)) => weight.total_cmp(&w).is_gt()
                || (weight.total_cmp(&w).is_eq() && *kind < k),
        };
        if better { best = Some((weight, *kind)); }
    }
    best.map(|(_, k)| k)
}

/// The dominant race's word for `earth` (the endonym), or `None` (a coverage gap).
pub fn world_name(world: &World) -> Option<String> {
    let wc = WorldComponents::assemble().ok()?;
    let kind = dominant_people(world)?;
    let lex = wc.lexicon.get(&kind)?;
    match lex.entry("earth")? {
        LexEntry::Root { views, .. } | LexEntry::Compound { views, .. } => {
            Some(views.roman.clone())
        }
        LexEntry::Gap { .. } => None,
    }
}
```

Confirm `settled_population` / `Mass::value()` names against the existing worldgen accessors before finalizing; mirror `flagship_of`'s pattern.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen dominant_people_weights_by_mass`
Expected: PASS.

- [ ] **Step 5: Mutation check**

Temporarily halve the winning race's `mass` in a scratch assertion and confirm the chosen `KindId` changes (assert the mechanism, not the value — [[measure-dont-narrate-the-mechanism]]). Revert.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): dominant-people (Σ pop×mass) + endonym world name (C1 T2)"
```

---

### Task 3: The Common `Classify` grammar

**Files:**
- Create: `domains/language/src/clause.rs`
- Modify: `domains/language/src/lib.rs` (add `pub mod clause;` + re-exports)
- Test: `domains/language/src/clause.rs` (`#[cfg(test)]`)

**Interfaces:**
- Produces:
  - `pub enum Frame { Classify }`
  - `pub enum Number { Sg, Pl }` · `pub enum Definiteness { Indef, Def }`
  - `pub struct ClauseSpec { pub frame: Frame, pub subject: String, pub complement: String, pub number: Number, pub definiteness: Definiteness }`
  - `pub fn realize_common(spec: &ClauseSpec) -> String`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn classify_singular_indefinite() {
    let s = ClauseSpec { frame: Frame::Classify, subject: "Elthandil".into(),
        complement: "planet".into(), number: Number::Sg, definiteness: Definiteness::Indef };
    assert_eq!(realize_common(&s), "Elthandil is a planet.");
}
#[test]
fn a_becomes_an_before_vowel() {
    let s = ClauseSpec { frame: Frame::Classify, subject: "Aoth".into(),
        complement: "elemental".into(), number: Number::Sg, definiteness: Definiteness::Indef };
    assert_eq!(realize_common(&s), "Aoth is an elemental.");
}
#[test]
fn classify_generic_plural() {
    let s = ClauseSpec { frame: Frame::Classify, subject: "Goblins".into(),
        complement: "people".into(), number: Number::Pl, definiteness: Definiteness::Indef };
    assert_eq!(realize_common(&s), "Goblins are people.");
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-language clause`
Expected: FAIL (module absent).

- [ ] **Step 3: Implement**

```rust
//! The clause layer: a language-neutral ClauseSpec and the Common realizer.
//! Generalizes the render_line seam from a bespoke tenet spec to any clause.
#![allow(clippy::module_name_repetitions)]

/// The construction a clause realizes. C1 has one: classification (`isA`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Frame { /// X is a Y.
    Classify }
/// Grammatical number of the subject.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Number { /// singular
    Sg, /// plural
    Pl }
/// Whether the complement is introduced with a/the or bare.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Definiteness { /// a/an
    Indef, /// the
    Def }

/// A language-neutral clause: predicate-argument structure plus features.
/// The per-language realizer decides how (and whether) each feature surfaces.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClauseSpec {
    /// The construction.
    pub frame: Frame,
    /// The subject's surface form (already a resolved name/noun).
    pub subject: String,
    /// The complement concept's Common lexeme.
    pub complement: String,
    /// Subject number.
    pub number: Number,
    /// Complement definiteness.
    pub definiteness: Definiteness,
}

fn indefinite_article(word: &str) -> &'static str {
    match word.chars().next().map(|c| c.to_ascii_lowercase()) {
        Some('a' | 'e' | 'i' | 'o' | 'u') => "an",
        _ => "a",
    }
}

/// Realize a ClauseSpec as a Common (≈ limited English) sentence.
pub fn realize_common(spec: &ClauseSpec) -> String {
    match spec.frame {
        Frame::Classify => {
            let copula = match spec.number { Number::Sg => "is", Number::Pl => "are" };
            let det = match (spec.definiteness, spec.number) {
                (Definiteness::Def, _) => "the ".to_string(),
                (Definiteness::Indef, Number::Sg) => {
                    format!("{} ", indefinite_article(&spec.complement))
                }
                (Definiteness::Indef, Number::Pl) => String::new(), // bare generic
            };
            format!("{} {} {}{}.", spec.subject, copula, det, spec.complement)
        }
    }
}
```

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-language clause`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/language/src/clause.rs domains/language/src/lib.rs
git commit -m "feat(language): ClauseSpec + Common Classify realizer (C1 T3)"
```

---

### Task 4: Mint the planet entity and commit its facts

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (the genesis composition root)
- Test: `windows/worldgen/src/lib.rs` (`#[cfg(test)]`)

**Interfaces:**
- Consumes: Task 1 `IS_A`, Task 2 `world_name`, `Ledger::mint_entity`, `Ledger::commit`.
- Produces: `pub fn planet_entity(world: &World) -> Option<EntityId>` — the entity carrying `(is-a, "planet")`; the composition root commits `(planet, is-a, "planet")` and `(planet, name, <endonym>)` during build.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn built_world_names_and_classifies_its_planet() {
    let world = constant(1);
    let p = planet_entity(&world).expect("a built world has a planet entity");
    assert_eq!(world.ledger.text_of(p, "is-a"), Some("planet"));
    let n = world.ledger.text_of(p, "name").expect("the planet is named");
    assert_eq!(Some(n.to_string()), world_name(&world));
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen built_world_names_and_classifies`
Expected: FAIL (no planet entity/facts).

- [ ] **Step 3: Implement**

In the genesis composition root (where the world's facts are assembled — mirror the existing fact-committing sites, e.g. the deity `name-gloss` commit), after the peoples exist so `world_name` resolves:

```rust
// The planet asserts, in the ledger, what it is and what its dominant people
// call it. Classification is read from structure (no draw); the name is a
// lexicon lookup of the universal-stratum `earth` concept (no draw).
let planet = world.ledger.mint_entity();
world.ledger.commit(Fact {
    subject: planet, predicate: hornvale_kernel::world::IS_A.into(),
    object: Value::Text("planet".into()), place: None, day: None,
    provenance: "astronomy: the central body is a planet".into(),
}, &world.registry).expect("is-a on a fresh entity cannot conflict");
if let Some(name) = world_name(world) {
    world.ledger.commit(Fact {
        subject: planet, predicate: hornvale_kernel::world::NAME.into(),
        object: Value::Text(name), place: None, day: None,
        provenance: "the dominant people's word for the world".into(),
    }, &world.registry).expect("name on a fresh entity cannot conflict");
}

/// The entity carrying the planet's classification, if any.
pub fn planet_entity(world: &World) -> Option<EntityId> {
    world.ledger.find("is-a")
        .find(|f| f.object == Value::Text("planet".into()))
        .map(|f| f.subject)
}
```

Confirm the genesis entry point (the function that assembles a world's facts) and place the mint after peoples are placed. If `world_name` is `None` (a race lacks `earth`), the planet is classified but unnamed — a PROC-15 gap, not a fallback.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen built_world_names_and_classifies`
Expected: PASS.

- [ ] **Step 5: Determinism check**

```rust
#[test]
fn planet_facts_are_deterministic() {
    assert_eq!(constant(1).to_json(), constant(1).to_json());
}
```
Run it; expect PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): mint the planet entity, commit is-a + name (C1 T4)"
```

---

### Task 5: `windows/book` — render a volume from the ledger

**Files:**
- Create: `windows/book/Cargo.toml`, `windows/book/src/lib.rs`
- Modify: root `Cargo.toml` (workspace members)
- Test: `windows/book/src/lib.rs` (`#[cfg(test)]`)

**Interfaces:**
- Consumes: Task 3 `ClauseSpec`/`realize_common`, Task 4 `planet_entity`, `Ledger::text_of`.
- Produces:
  - `pub struct BookVolume { pub seed: u64, pub lines: Vec<String> }`
  - `pub fn render_volume(world: &World) -> BookVolume`
  - `pub fn uncovered_predicates(world: &World) -> Vec<String>` (Task 6)

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn volume_states_the_planet_is_a_planet() {
    let world = hornvale_worldgen::constant(1);
    let vol = render_volume(&world);
    assert!(vol.lines.iter().any(|l| l.ends_with(" is a planet.")),
        "the volume classifies the planet: {:?}", vol.lines);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-book volume_states_the_planet`
Expected: FAIL (crate/fn absent).

- [ ] **Step 3: Implement**

`Cargo.toml` mirrors `windows/almanac` (depends on `hornvale-kernel`, `hornvale-language`, `hornvale-worldgen`). `lib.rs`:

```rust
//! The Book window: render a world's committed classification facts as Common
//! sentences. Reads only the ledger; realizes via domains/language.
#![warn(missing_docs)]
use hornvale_kernel::{Value, World};
use hornvale_language::clause::{ClauseSpec, Definiteness, Frame, Number, realize_common};

/// One world's volume of The Book.
pub struct BookVolume { /// the seed
    pub seed: u64, /// the rendered lines
    pub lines: Vec<String> }

/// Render a volume: one Common sentence per `is-a` fact, subject resolved to
/// its `name` (or the entity id if unnamed).
pub fn render_volume(world: &World) -> BookVolume {
    let mut lines = Vec::new();
    for fact in world.ledger.find("is-a") {
        let Value::Text(kind) = &fact.object else { continue };
        let subject = world.ledger.text_of(fact.subject, "name")
            .map(|s| s.to_string())
            .unwrap_or_else(|| format!("Entity {}", fact.subject.0));
        lines.push(realize_common(&ClauseSpec {
            frame: Frame::Classify, subject, complement: kind.clone(),
            number: Number::Sg, definiteness: Definiteness::Indef,
        }));
    }
    BookVolume { seed: world.seed.value(), lines }
}
```

Confirm `Seed::value()`/`World` re-exports; mirror `windows/almanac/Cargo.toml`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-book volume_states_the_planet`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/book/ Cargo.toml
git commit -m "feat(book): windows/book renders a volume from is-a facts (C1 T5)"
```

---

### Task 6: PROC-15 coverage report

**Files:**
- Modify: `windows/book/src/lib.rs`
- Test: same

**Interfaces:**
- Produces: `pub fn uncovered_predicates(world: &World) -> Vec<String>` — registered predicates present in the ledger that the C1 grammar cannot yet render (everything except `is-a`), sorted, deduped.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn coverage_flags_name_as_uncovered() {
    let world = hornvale_worldgen::constant(1);
    let gaps = uncovered_predicates(&world);
    assert!(gaps.contains(&"name".to_string()), "name has no construction yet");
    assert!(!gaps.contains(&"is-a".to_string()), "is-a is covered");
}
```

- [ ] **Step 2: Run to verify it fails** — `cargo test -p hornvale-book coverage_flags_name`; Expected: FAIL.

- [ ] **Step 3: Implement**

```rust
/// Predicates present in the ledger that C1's grammar cannot yet render.
pub fn uncovered_predicates(world: &World) -> Vec<String> {
    use std::collections::BTreeSet;
    const COVERED: &[&str] = &["is-a"];
    let mut gaps: BTreeSet<String> = BTreeSet::new();
    for fact in world.ledger.find_all() { // or iterate registered predicates present
        if !COVERED.contains(&fact.predicate.as_str()) {
            gaps.insert(fact.predicate.clone());
        }
    }
    gaps.into_iter().collect()
}
```

If `find_all` does not exist, iterate `world.registry` predicate names and keep those with any `world.ledger.find(name).next().is_some()`.

- [ ] **Step 4: Run to verify it passes** — Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/book/src/lib.rs
git commit -m "feat(book): PROC-15 coverage report of unrendered predicates (C1 T6)"
```

---

### Task 7: `hornvale book` CLI command → the artifact

**Files:**
- Modify: `cli/src/main.rs` (dispatch + `cmd_book`), `cli/Cargo.toml` (dep)
- Test: manual run captured into the committed artifact

**Interfaces:**
- Consumes: Task 5/6. Builds worlds for seeds `1, 2, 3` via `hornvale_worldgen::constant`, renders volumes, prints the Book (Markdown) to stdout.

- [ ] **Step 1: Implement `cmd_book`**

```rust
fn cmd_book(_args: &[String]) -> Result<(), String> {
    let mut out = String::from("# The Book\n");
    for seed in [1u64, 2, 3] {
        let world = world_builder::constant(seed);
        let vol = hornvale_book::render_volume(&world);
        let title = world.ledger
            .text_of(hornvale_book::planet_entity(&world).unwrap(), "name")
            .unwrap_or("Untitled").to_string();
        out.push_str(&format!("\n## Volume {seed}: {title}\n\n"));
        for line in vol.lines { out.push_str(&line); out.push('\n'); }
    }
    print!("{out}");
    Ok(())
}
```

Add `Some("book") => cmd_book(&args),` to the dispatch and a usage line. Re-export `planet_entity` from `hornvale_book` (Task 4 lives in worldgen; re-export or call `world_builder::planet_entity`).

- [ ] **Step 2: Build + smoke test**

Run: `cargo run -p hornvale -- book`
Expected: three volumes, each with a "‹Name› is a planet." line.

- [ ] **Step 3: Write the committed artifact**

Run: `cargo run -p hornvale -- book > book/src/gallery/the-book.md`
Inspect it; confirm three real titles and sentences.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add cli/src/main.rs cli/Cargo.toml book/src/gallery/the-book.md
git commit -m "feat(cli): hornvale book emits three volumes (C1 T7)"
```

---

### Task 8: Drift-check wiring

**Files:**
- Modify: `scripts/regenerate-artifacts.sh`, `.github/workflows/ci.yml:57`

- [ ] **Step 1:** Add to `regenerate-artifacts.sh` (beside the almanac regen):

```bash
cargo run -p hornvale -- book > book/src/gallery/the-book.md
```

- [ ] **Step 2:** Confirm `ci.yml:57`'s `git diff --exit-code book/src/gallery/ …` already covers `book/src/gallery/the-book.md` (it globs the directory — it does).

- [ ] **Step 3:** Verify no drift:

Run: `bash scripts/regenerate-artifacts.sh && git diff --exit-code book/src/gallery/the-book.md`
Expected: clean (exit 0).

- [ ] **Step 4: Commit**

```bash
git add scripts/regenerate-artifacts.sh
git commit -m "build: regenerate + drift-check The Book (C1 T8)"
```

---

### Task 9: The restructure — The Book primary, Chronicle + Frontier to appendices

**Files:**
- Modify: `book/src/SUMMARY.md`

- [ ] **Step 1:** Add `# The Book` as the first content part after the Introduction, pointing at `./gallery/the-book.md` (with a one-line `./the-book-intro.md` preface if desired). Move the `# The Chronicle` (SUMMARY.md:32) and `# The Frontier` (SUMMARY.md:156) sections to the end under a `# Appendices` framing (a comment or a heading rename — mdBook has no nesting for `#` parts, so relabel: `# Appendix: The Chronicle`, `# Appendix: The Frontier`).

- [ ] **Step 2: Build the book**

Run: `mdbook build book`
Expected: builds; The Book appears first, Chronicle/Frontier last.

- [ ] **Step 3: Drift-check the docs graph**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS (registry-ID scoping keys off `frontier/` path, unaffected by SUMMARY order).

- [ ] **Step 4: Commit**

```bash
git add book/src/SUMMARY.md
git commit -m "docs: The Book becomes primary; Chronicle + Frontier to appendices (C1 T9)"
```

---

### Task 10: ADRs, regen, and the census-clean gate

**Files:**
- Create: `docs/decisions/0058-the-book-is-a-derived-view-grammar-is-build-state.md`, `0059-the-book-is-the-primary-artifact.md`, `0060-the-is-a-classification-predicate.md`
- Modify: `book/src/reference/` (concepts dump), any re-pinned world-hash golden

- [ ] **Step 1: Write the three ADRs** (each: context, decision, consequences), from metaplan §5 #1, #2, and the `is-a` predicate. Follow an existing `docs/decisions/00NN-*.md` as the template.

- [ ] **Step 2: Regenerate the reference dumps**

Run the `concepts`/`streams` regen (from the ci.yml "Artifacts are current" step), then:
Run: `git diff --stat book/src/reference/`
Expected: the `concepts` dump gains the `is-a` predicate; no other reference churn.

- [ ] **Step 3: Re-pin any world-hash golden** that changed (byte-identity holds; the value moved because the ledger gained facts).

- [ ] **Step 4: The census-clean gate**

Run: `make gate-full`
Expected: PASS with the census fixtures unchanged. **If any census metric moved, STOP** — that is a carve-out needing Nathan's explicit authorization before any census regen.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add docs/decisions/ book/src/reference/
git commit -m "docs(decisions): ADRs 0058-0060; regen reference dumps (C1 T10)"
```

---

## Self-review notes

- **Spec coverage:** T1 (is-a predicate) · T2 (dominance + endonym) · T3 (ClauseSpec + Common realizer) · T4 (planet entity + facts, ledger-purist) · T5 (windows/book) · T6 (PROC-15 coverage) · T7 (`hornvale book`, 3 volumes) · T8 (drift-check) · T9 (restructure) · T10 (ADRs + regen + census-clean gate). All spec §2 items and §4/§8 flags are covered.
- **Verify-before-final signatures** (confirm against the named functions during execution, do not invent): `settled_population`/`Mass::value` (T2), the genesis fact-assembly entry point (T4), `Seed::value`/re-exports (T5), `find_all` vs registry-iteration (T6), `world_builder::constant` re-export (T7).
- **Determinism:** no new seeded draws; T4 asserts byte-identity; T10 gates census-clean.
