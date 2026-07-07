# Campaign 5b: Provenance & the Year-1 Close — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the world a way to account for its own facts (`why <entity>`), measure its faiths (the Census of Faiths with two exact calibrations), and close Year 1 — the book presenting all five domains at tier 1 with the capstone exit demo.

**Architecture:** A new thin `windows/historiography` crate (kernel-only) exposes `recount(world, entity) -> Option<String>`, which replays any entity's committed facts + their provenance + the registry's predicate docs into a derivation. The REPL's `why` verb generalizes from `why <belief-number>` to `why <entity-id>` and calls it; religion's belief-specific `why` is subsumed. The Lab gains Census-of-Faiths metrics and two calibrations (verticality⇔stratification, head-deity-eternal⇔lock). The book close-out promotes religion to tier 1, adds Study 004, the Year-1 capstone chronicle, and a cascade overview showing all five domains deepened.

**Tech Stack:** Rust edition 2024; `hornvale-kernel` (ledger + registry); std only; the existing Lab study/metric/calibration framework and CI drift harness; mdbook.

## Global Constraints

- `serde` + `serde_json` only; **no new external crates** (a new *workspace* crate `windows/historiography` is fine — `members = ["windows/*"]` picks it up automatically).
- **Layering (enforced by `cli/tests/architecture.rs`):** `windows/historiography` depends on `hornvale-kernel` and **nothing else** — it reads the ledger + registry generically and interprets no domain-specific predicate. Windows may depend on the kernel/domains/other windows; a window never depends on the CLI.
- **Determinism (constitutional):** `recount` is a pure function of the world (same world → same string); no `HashMap`/`HashSet`/`std::time`; `BTreeMap`/`Vec`, `total_cmp`. It reads committed facts in ledger order (already deterministic).
- **Calibrations are exact** (spec §8): verticality is `ranked` iff `flagship-structure-size ≥ 4`; the head deity's tenet is eternal iff the world is tidally locked. Both validated as tests over the CI drift study — the 4th and 5th in the family (belief⇔lock, band-count⇔rotation, subsistence⇔biome).
- **Naming:** the provenance verb is **`why`** (generalized to any entity id). No `explain` verb is introduced.
- **Stream labels / save-format:** this plan adds no seed draws and no predicates — `recount` and the metrics only read existing committed facts. The concept registry and stream manifest are unchanged (regenerated defensively).
- Every crate `#![warn(missing_docs)]`; every public item/field/variant a `///` doc comment.
- Edition 2024; `cargo fmt` final each task. **Full gate:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the CI artifact drift check. `gen` is reserved in Rust 2024.
- **Baseline:** 403 tests pass on `main` at the start of this plan (Campaign 5a merged). Verify counts directly after each task.

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `windows/historiography/Cargo.toml` | Create | New kernel-only window crate. |
| `windows/historiography/src/lib.rs` | Create | `recount(world, entity) -> Option<String>` — generic provenance replay. |
| `cli/Cargo.toml` | Modify | Add `hornvale-historiography` dependency. |
| `cli/src/repl.rs` | Modify | `why <entity-id>` calls `recount`; `beliefs` lists each belief's entity id (so `why` is usable); help text. |
| `domains/religion/src/lib.rs` | Modify | Remove the subsumed belief-specific `why` (and its test); keep `beliefs_of`/`cult_form_of`/genesis. |
| `windows/lab/src/metrics.rs` | Modify | Census-of-Faiths metrics (pantheon size, cult form, verticality, head-deity periodicity); metric-count test → new total. |
| `windows/lab/tests/calibration.rs` | Modify | Add the verticality⇔stratification and head-deity-eternal⇔lock calibrations. |
| `studies/census-of-faiths.study.json` | Create | Author-time 10k faith census. |
| `book/src/domains/religion.md` | Modify | Promote to tier 1 with the pantheon + provenance model card. |
| `book/src/domains/overview.md` | Modify | Cascade table: religion → tier 1; a Year-1 "all five deepened" note. |
| `book/src/laboratory/study-004.md` | Create | The Census of Faiths chapter (comprehension-gated). |
| `book/src/gallery/the-gods-seed-42.md` | Create | The capstone gallery page: the two pantheons (spinning vs. locked) side by side. |
| `book/src/chronicle/campaign-5.md` | Create | The Campaign 5 + Year-1 capstone chronicle. |
| `book/src/SUMMARY.md` | Modify | Study 004, the capstone gallery page, the Campaign 5 chronicle. |
| `book/src/reference/*-generated.md`, `book/src/gallery/almanac-*.md`, `book/src/laboratory/generated/*` | Regenerate | Committed generated artifacts (census-lands-drift grows religion metrics/charts). |

Dataflow: `recount` reads the ledger+registry; the REPL and (later) other windows call it. The Lab reads religion facts via `beliefs_of`/`cult_form_of` for the Census + calibrations.

---

### Task 1: `windows/historiography` — the `recount` verb

**Files:**
- Create: `windows/historiography/Cargo.toml`
- Create: `windows/historiography/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{World, EntityId, Fact, Value, NAME}` + the registry.
- Produces: `pub fn recount(world: &World, entity: EntityId) -> Option<String>` — `None` if the entity has no facts; else a multi-line derivation: a lead naming the entity, then one line per committed fact (the predicate's registry doc, the rendered value, and the asserting provenance).

- [ ] **Step 1: Create the crate manifest**

`windows/historiography/Cargo.toml`:

```toml
[package]
name = "hornvale-historiography"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale historiography window: recount an entity's provenance."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`windows/historiography/src/lib.rs`:

```rust
//! Historiography, tier 0: recount how any entity came to be, by replaying
//! its committed facts, their provenance, and the registry's predicate docs.
//! Domain-agnostic — it interprets no domain-specific predicate, so a new
//! domain's facts are recounted the day they are committed. This is the seam
//! the Year-2 event ledger and fields-of-history deepen.
#![warn(missing_docs)]

use hornvale_kernel::{EntityId, Value, World};

/// Recount an entity from its committed facts: a lead line naming it (by its
/// `name` fact if present), then one bullet per fact — the predicate's
/// registry doc (falling back to the predicate key), the rendered value, and
/// the system that asserted it. `None` if nothing is recorded about `entity`.
pub fn recount(world: &World, entity: EntityId) -> Option<String> {
    // (implemented in Step 4)
    let _ = (world, entity);
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{ConceptRegistry, Fact, Seed};

    fn world() -> World {
        let mut w = World::new(Seed(42));
        w.registry
            .register_predicate("is-belief", true, "subject is a belief")
            .unwrap();
        w.registry
            .register_predicate("tenet", true, "the tenet text of a belief")
            .unwrap();
        w
    }

    fn fact(subject: EntityId, predicate: &str, object: Value, provenance: &str) -> Fact {
        Fact { subject, predicate: predicate.to_string(), object, place: None, day: Some(0.0), provenance: provenance.to_string() }
    }

    #[test]
    fn recount_replays_facts_with_docs_and_provenance() {
        let mut w = world();
        let e = w.ledger.mint_entity();
        w.ledger.commit(fact(e, hornvale_kernel::NAME, Value::Text("the Ever-Flame".to_string()), "religion"), &w.registry).unwrap();
        w.ledger.commit(fact(e, "is-belief", Value::Flag(true), "religion"), &w.registry).unwrap();
        w.ledger.commit(fact(e, "tenet", Value::Text("it never blinks.".to_string()), "religion"), &w.registry).unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(text.contains("the Ever-Flame"), "lead names the entity");
        assert!(text.contains("the tenet text of a belief"), "uses the registry doc");
        assert!(text.contains("it never blinks."), "renders the value");
        assert!(text.contains("religion"), "names the provenance");
    }

    #[test]
    fn recount_is_none_for_an_unknown_entity() {
        let w = world();
        assert!(recount(&w, EntityId(999)).is_none());
    }

    #[test]
    fn recount_is_deterministic() {
        let mut w = world();
        let e = w.ledger.mint_entity();
        w.ledger.commit(fact(e, "tenet", Value::Text("x".to_string()), "religion"), &w.registry).unwrap();
        let _ = ConceptRegistry::default;
        assert_eq!(recount(&w, e), recount(&w, e));
    }
}
```

- [ ] **Step 3: Run to see it fail**

Run: `cargo test -p hornvale-historiography 2>&1 | tail -6`
Expected: FAIL (`unimplemented!`).

- [ ] **Step 4: Implement `recount`**

Replace the stub body:

```rust
/// Render a value for a recount line.
fn render_value(value: &Value) -> String {
    match value {
        Value::Text(t) => t.clone(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
        Value::Entity(e) => format!("entity {}", e.0),
    }
}

pub fn recount(world: &World, entity: EntityId) -> Option<String> {
    let facts: Vec<&hornvale_kernel::Fact> = world.ledger.facts_about(entity).collect();
    if facts.is_empty() {
        return None;
    }
    let name = world
        .ledger
        .text_of(entity, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("entity {}", entity.0));
    let mut out = format!("{name}:\n");
    for f in facts {
        let label = world
            .registry
            .predicate(&f.predicate)
            .map(|p| p.doc.clone())
            .unwrap_or_else(|| f.predicate.clone());
        out.push_str(&format!(
            "- {label}: {} (asserted by {})\n",
            render_value(&f.object),
            f.provenance
        ));
    }
    Some(out)
}
```

- [ ] **Step 5: Run + fmt + clippy + commit**

Run: `cargo test -p hornvale-historiography && cargo clippy -p hornvale-historiography --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`. (Workspace still compiles — nothing depends on the new crate yet.)

```bash
git add windows/historiography/Cargo.toml windows/historiography/src/lib.rs Cargo.lock
git commit -m "feat(historiography): recount — generic provenance replay for any entity"
```

---

### Task 2: The REPL `why <entity>` verb

**Files:**
- Modify: `cli/Cargo.toml`
- Modify: `cli/src/repl.rs`
- Modify: `domains/religion/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_historiography::recount`.
- Produces: REPL `why <entity-id>` (recounts any entity); `beliefs` lists each belief's entity id; religion's belief-specific `why` removed.

- [ ] **Step 1: Add the dependency**

In `cli/Cargo.toml` `[dependencies]`, add:

```toml
hornvale-historiography = { path = "../windows/historiography" }
```

- [ ] **Step 2: Write the failing REPL test**

In `cli/src/repl.rs` tests, add:

```rust
#[test]
fn why_recounts_any_entity_by_id() {
    let world = build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated, &hornvale_terrain::TerrainPins::default(), &SettlementPins::default()).unwrap();
    // The flagship belief entities are listed with ids by `beliefs`.
    let listing = { let mut o = Vec::new(); run(&world, "beliefs\nquit\n".as_bytes(), &mut o).unwrap(); String::from_utf8(o).unwrap() };
    assert!(listing.contains('['), "beliefs lists entity ids in brackets");
    // Recount the flagship settlement (entity from village_info).
    let id = hornvale_settlement::village_info(&world).unwrap().id.0;
    let mut out = Vec::new();
    run(&world, format!("why {id}\nquit\n").as_bytes(), &mut out).unwrap();
    let out = String::from_utf8(out).unwrap();
    assert!(out.contains("asserted by"), "recount names provenance");
    // An unknown entity is reported, not fatal.
    let mut out2 = Vec::new();
    run(&world, "why 99999\nquit\n".as_bytes(), &mut out2).unwrap();
    assert!(String::from_utf8(out2).unwrap().contains("nothing is recorded"));
}
```

- [ ] **Step 3: Run to see it fail**

Run: `cargo test -p hornvale why_recounts_any_entity 2>&1 | tail -6`
Expected: FAIL (why still takes a belief number; `beliefs` shows no ids).

- [ ] **Step 4: Generalize the REPL**

In `cli/src/repl.rs`, replace the `"why"` arm:

```rust
            "why" => match argument.and_then(|a| a.parse::<u64>().ok()) {
                Some(id) => match hornvale_historiography::recount(world, EntityId(id)) {
                    Some(text) => write!(output, "{text}")?,
                    None => writeln!(output, "nothing is recorded about entity {id}")?,
                },
                None => writeln!(output, "usage: why <entity-id> (ids from `beliefs`, `places`)")?,
            },
```

Update the `"beliefs"` arm to show each belief's entity id so `why` is usable:

```rust
            "beliefs" => {
                let beliefs = hornvale_religion::beliefs_of(world);
                if beliefs.is_empty() {
                    writeln!(output, "no beliefs are recorded")?;
                }
                for (i, belief) in beliefs.iter().enumerate() {
                    writeln!(output, "{}. [{}] {}", i + 1, belief.id.0, belief.tenet)?;
                }
            }
```

Update the `HELP` string's `why` line to `why <id>        recount how an entity came to be (ids from beliefs/places)`. Ensure `EntityId` is imported in `repl.rs` (it already is — `use hornvale_kernel::{EntityId, ...}`).

- [ ] **Step 5: Subsume religion's `why`**

In `domains/religion/src/lib.rs`, remove `pub fn why(...)` and its test `why_names_the_source_phenomenon` (the REPL no longer calls it; `recount` supersedes it). Grep `hornvale_religion::why` to confirm no other caller remains.

- [ ] **Step 6: Full gate + commit**

Run: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add cli/Cargo.toml cli/src/repl.rs domains/religion/src/lib.rs Cargo.lock
git commit -m "feat(cli): generalize why to recount any entity; subsume religion::why"
```

---

### Task 3: The Census of Faiths

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Modify: `windows/lab/tests/calibration.rs`
- Create: `studies/census-of-faiths.study.json`

**Interfaces:**
- Consumes: `hornvale_religion::{beliefs_of, cult_form_of}`, the flagship (`village_info`), the existing `flagship-structure-size` and `tidally-locked` metrics.
- Produces: four religion metrics + two calibrations.

New metrics (append to `registry()`; update the count test to the new total):
- `pantheon-size` (Numeric, edges `&[1.0,2.0,3.0,4.0,5.0,6.0]`) — `beliefs_of(&v.world).len()` as f64; `Absent` if no beliefs.
- `cult-form` (Categorical) — `cult_form_of(&v.world)`; `Absent` if none.
- `pantheon-verticality` (Categorical) — `"ranked"` if any belief `high_god`, else `"flat"`; `Absent` if no beliefs.
- `head-deity-periodicity` (Categorical) — the head deity (`beliefs_of[0]`, the most salient) is `"eternal"` if its tenet contains `"never"`, else `"cyclic"`; `Absent` if no beliefs. (Uses the same eternal/cyclic signal the belief-kind metric uses at tier 0.)

- [ ] **Step 1: Write failing tests** (metric extraction for seed 42; count test → new total). Confirm the new registry length by counting (28 + 4 = 32) and assert it.

- [ ] **Step 2: Run to fail.** `cargo test -p hornvale-lab registry_has 2>&1 | tail -6`

- [ ] **Step 3: Implement the four metrics** in `registry()` (mirror the existing religion/land metric style; `Absent` guards; no `HashMap`). Example for verticality:

```rust
        Metric {
            name: "pantheon-verticality",
            doc: "Whether the flagship pantheon is ranked (a high god presides) or flat; \
                   Absent if there are no beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let beliefs = hornvale_religion::beliefs_of(&v.world);
                if beliefs.is_empty() {
                    MetricValue::Absent
                } else if beliefs.iter().any(|b| b.high_god) {
                    MetricValue::Text("ranked".to_string())
                } else {
                    MetricValue::Text("flat".to_string())
                }
            },
        },
```

- [ ] **Step 4: Add the two calibrations** to `windows/lab/tests/calibration.rs`, loading `census-lands-drift.study.json` (mirror the existing three). For each non-refusal row:
  - **verticality ⇔ stratification:** `pantheon-verticality` is `"ranked"` iff the `flagship-structure-size` column ≥ 4.0 (the `RANKED_STRATA` threshold); skip rows where verticality is `Absent`.
  - **head-deity-eternal ⇔ lock:** `head-deity-periodicity` is `"eternal"` iff the `tidally-locked` column is `Flag(true)`; skip `Absent`.

```rust
#[test]
fn pantheon_verticality_matches_stratification() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (vert_i, size_i) = (idx("pantheon-verticality"), idx("flagship-structure-size"));
    for row in &result.rows {
        if row.refusal.is_some() { continue; }
        let ranked = matches!(&row.values[vert_i], MetricValue::Text(t) if t == "ranked");
        let stratified = matches!(&row.values[size_i], MetricValue::Number(n) if *n >= 4.0);
        if matches!(row.values[vert_i], MetricValue::Absent) { continue; }
        assert_eq!(ranked, stratified, "seed {}: verticality calibration violated", row.seed);
    }
}

#[test]
fn head_deity_is_eternal_exactly_when_tidally_locked() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (head_i, lock_i) = (idx("head-deity-periodicity"), idx("tidally-locked"));
    for row in &result.rows {
        if row.refusal.is_some() { continue; }
        if matches!(row.values[head_i], MetricValue::Absent) { continue; }
        let eternal = matches!(&row.values[head_i], MetricValue::Text(t) if t == "eternal");
        let locked = matches!(row.values[lock_i], MetricValue::Flag(true));
        assert_eq!(eternal, locked, "seed {}: head-deity calibration violated", row.seed);
    }
}
```

- [ ] **Step 5: Create the study.** `studies/census-of-faiths.study.json`:

```json
{ "name": "census-of-faiths",
  "description": "The full Census of Faiths: pantheon size, cult form, verticality, and head-deity periodicity over 10,000 worlds — the verticality and head-deity calibrations at author-time scale.",
  "seeds": { "from": 0, "count": 10000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all" }
```

- [ ] **Step 6: Gate + commit.**

Run: `cargo test -p hornvale-lab && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS (all five calibrations green). `cargo fmt`.

```bash
git add windows/lab/src/metrics.rs windows/lab/tests/calibration.rs studies/census-of-faiths.study.json
git commit -m "feat(lab): Census of Faiths metrics + verticality & head-deity calibrations"
```

If a calibration FAILS, do NOT weaken it — report the seed + columns; it may reveal a real threshold mismatch (e.g. `RANKED_STRATA` vs. the structure-size metric).

---

### Task 4: The Year-1 close-out (book + CI + regeneration)

**Files:**
- Modify: `book/src/domains/religion.md`, `book/src/domains/overview.md`, `book/src/SUMMARY.md`
- Create: `book/src/laboratory/study-004.md`, `book/src/gallery/the-gods-seed-42.md`, `book/src/chronicle/campaign-5.md`
- Modify: `.github/workflows/ci.yml` (confirm `census-lands-drift` + drift net cover the new metrics; no new render needed)
- Regenerate: `book/src/gallery/almanac-*.md`, `book/src/reference/*-generated.md`, `book/src/laboratory/generated/census-lands-drift/*`

- [ ] **Step 1: Promote the religion chapter to tier 1** with a model card (spec §4–§6): the pantheon (deity per phenomenon above the floor, no cap, esoterica; eternal/cyclic tenets); structure (verticality from strata, priesthood from the shaman caste); provenance-by-replay (`why`/`recount`); declared approximations (one flagship's pantheon; celestial+ambient only; static theology; single species). Keep the tier-0 section. Match the `campaign-3c`/tier-1-chapter voice.
- [ ] **Step 2: Update the cascade overview** (`overview.md`): religion row → tier 1; add a short "Year 1: all five domains deepened" note framing the astronomy→theology cascade.
- [ ] **Step 3: Write Study 004 (Census of Faiths).** Run the 10k census for real numbers: `cargo run -q --release -p hornvale -- lab run studies/census-of-faiths.study.json 2>/dev/null`, read `lab-out/census-of-faiths/rows.csv`. Comprehension-gated prose: the two calibrations (exact), and the pantheon-size / cult-form / verticality distributions (the unknown numbers). Reference the CI-checked `census-lands-drift` charts; do not commit `census-of-faiths` artifacts (author-time — state this).
- [ ] **Step 4: Write the capstone gallery page** `book/src/gallery/the-gods-seed-42.md`: prose framing the exit demo, quoting the two "The Gods" sections (spinning Wheel-Turner vs. locked Still Crown) by `{{#include}}`-ing the relevant committed almanac files or quoting them — the visual capstone of Year 1. (Use `{{#include ../gallery/almanac-seed-42-sky.md}}`-style includes only if you want the whole almanac; otherwise quote the two Gods sections directly as fenced blocks — quoting is safer for a focused page. Prefer quoting.)
- [ ] **Step 5: Write the Campaign 5 + Year-1 chronicle** `book/src/chronicle/campaign-5.md`: religion becomes tier 1 (the pantheon shaped by sky and society); provenance-by-replay; the Census of Faiths; and a **Year-1 retrospective** — the five campaigns, the enrichment thesis proven end to end (astronomy → … → theology), the calibration family, what Year 2 might hold. Verify the numbers you cite against the regenerated artifacts.
- [ ] **Step 6: SUMMARY + regenerate.** Add to `book/src/SUMMARY.md`: `- [Study 004: The Census of Faiths](./laboratory/study-004.md)`, `- [The Gods of Seed 42](./gallery/the-gods-seed-42.md)`, `- [Campaign 5: The Gods](./chronicle/campaign-5.md)`. Regenerate every committed artifact **stdout-only** (the `census-lands-drift` summary/charts gain the four religion metrics; the almanacs/registry are unchanged by 5b but regenerate defensively). Confirm `grep -rlE "Compiling|Finished \`|Running \`target" book/src` is empty and a second full CI-block regeneration leaves `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` clean.
- [ ] **Step 7: Full gate + book build + commit.**

Run: `mdbook build book && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, then the drift check.

```bash
git add book/ .github/workflows/ci.yml
git commit -m "docs(book): Year-1 close-out — religion tier 1, Census of Faiths, capstone chronicle"
```

---

## Self-Review Notes

**Spec coverage (against §6, §8, §12):**
- §6 (`windows/historiography` + `recount`; `why <belief-number>` → `why <entity-id>`; subsume religion's `why`) → Tasks 1–2. §8 (Census of Faiths metrics + verticality⇔stratification and head-deity-eternal⇔lock calibrations; author-time census-of-faiths) → Task 3. §12 (religion chapter tier 1, Study 004, Year-1 capstone chronicle, cascade overview all-5-tier-1, exit-demo gallery) → Task 4.

**This completes Campaign 5 and Year 1.** After 5b merges, all five domains are tier 1 and the Year-1 exit criteria (spec §15) are met.

**Type consistency:** `recount(world, entity) -> Option<String>` (Task 1) is consumed with that exact signature by the REPL (Task 2). The Census metrics (Task 3) read `beliefs_of`/`cult_form_of`/`Belief.high_god` (5a) and the existing `flagship-structure-size`/`tidally-locked` columns; the calibrations reference those exact metric names. Registry total moves 28 → 32 (Task 3 count test).

**Watch items (carried lessons):** verify test counts directly (baseline 403); `windows/historiography` must stay kernel-only (architecture test enforces); removing `religion::why` (Task 2) — grep for stray callers first; regenerate artifacts stdout-only (Task 4); the two new calibrations must pass over the real 500-seed drift study (if one fails, it's a real threshold bug, not a test to weaken); `gen` reserved in Rust 2024; the capstone gallery page should quote the two Gods sections rather than risk a fragile transclusion.
