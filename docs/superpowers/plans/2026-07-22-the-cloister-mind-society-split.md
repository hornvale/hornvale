# The Cloister — MindVector / SocietyVector split · Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cleave `PsychVector` into `MindVector` (the individual mind, carried by every minded kind) and `SocietyVector` (the community mind, carried only by `Settled` kinds), so a solitary dragon carries a mind but asserts no society shape.

**Architecture:** Two atomic steps. First a pure, byte-identical rename `PsychVector` → `MindVector` (all six fields intact — compiler-verified, easy to review). Then the real extraction: move the three society dims into a new `SocietyVector` component store carried only by `Settled` kinds, add a `SocietyVector::baseline()` that mixed consumers resolve for a `Solitary`, and delete the dragons' authored society dims. A final task verifies byte-identity (regen + diff) and updates the book.

**Tech Stack:** Rust (edition 2024), the Hornvale workspace (`hornvale-species`, `hornvale-worldgen`, `hornvale-vessel`, `cli`). `cargo nextest`, `cargo test --doc`, the type-audit tool.

## Global Constraints

- **Not an epoch. No new seeded draws. No stream label touched. No census regen.** (Spec §4.) Placement of new draws is the only epoch trigger; this campaign adds none.
- **Worlds and the reference catalogue must be byte-identical** — a *prediction* verified in Task 3 by regen+diff, never asserted. If a byte moves, stop and investigate (a missed consumer).
- **A shared-signature change must compile workspace-wide in ONE commit.** The pre-commit hook runs `cargo clippy --workspace --all-targets`; a half-renamed tree fails it. `cargo build --workspace` misses test-only call sites — gate with `cargo clippy --workspace --all-targets` (The Solitary Tongue close lesson). Tasks 1 and 2 are each a single atomic green commit.
- **type-audit:** `MindVector` and `SocietyVector` are `bare-ok(ratio)` structs; the verdict tag goes on the **struct doc comment**, not per-field (per-field tags are silently ignored — The Solitary Tongue close lesson).
- Every public item/field/variant gets a one-line doc comment (`#![warn(missing_docs)]`). Run `cargo fmt` as the final step before every commit.
- Commit gate: `make gate` (fmt + clippy + nextest + doctests). Iterate scoped (`cargo test -p <crate>`); `--workspace` at the gate.

---

### Task 1: Pure rename `PsychVector` → `MindVector`

A mechanical, byte-identical rename. The struct keeps all six fields; only the type name changes. The compiler is the completeness check — a missed occurrence fails to build. No behavior changes, so no new tests; existing tests must stay green unchanged (except the type name).

**Files (every occurrence of the identifier `PsychVector`):**
- Modify: `domains/species/src/lib.rs` — struct def (`145`), `impl Component for PsychVector` (`742`), the 7 authored instances in `psyche_registry` (`948`–`1024`), any test refs.
- Modify: `windows/worldgen/src/components.rs` — store type (`28`, `98`, `209`), imports.
- Modify: `windows/worldgen/src/lib.rs` — fn params/imports (`658`, `3683`, `3695`), and any local refs.
- Modify: `windows/worldgen/src/chorus.rs` — import + `beta_of`/`doctrine_beta_of` params (`34`, `194`, `774`).
- Modify: `cli/src/phonology.rs` — param (`104`), import.
- Modify: `cli/src/audio.rs` — any type ref.
- Modify: `windows/vessel/src/liveness.rs` — doc-comment references to `PsychVector` (`43`, `48`, `69`, `1816`, `2069`).
- Modify: any test files naming the type: `windows/worldgen/tests/*.rs`, `windows/worldgen/src/lib.rs` test module.

**Interfaces:**
- Produces: the type `hornvale_species::MindVector` with the same six public fields (`threat_response`, `deliberation_latency`, `in_group_radius`, `time_horizon`, `sociality`, `status_basis`) and `impl Component`. `psyche_registry() -> ComponentStore<KindId, MindVector>` (name unchanged).

- [ ] **Step 1: Rename the type and its doc.** In `domains/species/src/lib.rs`, change `pub struct PsychVector {` (line 145) to `pub struct MindVector {`. Update the struct doc comment's first line to name the new type; keep the `type-audit: bare-ok(ratio)` line. Change `impl Component for PsychVector {}` (742) to `impl Component for MindVector {}`.

- [ ] **Step 2: Rename every remaining occurrence.** Find them all:

```bash
cd <worktree>
grep -rln '\bPsychVector\b' --include=*.rs
```

Replace `PsychVector` → `MindVector` in every file listed (identifier-boundary match; there is no other symbol containing the substring). Include doc comments (e.g. `vessel/liveness.rs` mentions `PsychVector.threat_response` — becomes `MindVector.threat_response`). Leave the store field name `psyche` and `psyche_registry()` unchanged (decision: keep `psyche` holding `MindVector`).

- [ ] **Step 3: Confirm the rename is total and byte-identical.**

```bash
grep -rn '\bPsychVector\b' --include=*.rs   # expect: no matches
cargo clippy --workspace --all-targets -- -D warnings
```

Expected: clippy clean, zero `PsychVector` matches.

- [ ] **Step 4: Run the affected suites — all green, unchanged.**

```bash
cargo nextest run -p hornvale-species -p hornvale-worldgen -p hornvale
```

Expected: PASS (a pure rename changes no behavior).

- [ ] **Step 5: fmt + commit.**

```bash
cargo fmt
git add -A
git commit -m "refactor(the-cloister): rename PsychVector -> MindVector (byte-identical)

Pure rename, all six fields intact; the store field stays `psyche`.
No behavior change. Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ"
```

---

### Task 2: Extract `SocietyVector` (the load-bearing split)

Move the three society dims out of `MindVector` into a new `SocietyVector` carried only by `Settled` kinds; add `baseline()`; drop the dragons' society dims; update every consumer to resolve society (with baseline fallback for a `Solitary`). Atomic: the tree only compiles once every site is updated, so this is one green commit.

**Files:**
- Modify: `domains/species/src/lib.rs` — shrink `MindVector`; add `SocietyVector` + `baseline()` + `impl Component`; split authoring; add `society_registry()`.
- Modify: `windows/worldgen/src/components.rs` — add `society` store to `WorldComponents`, `assemble`, `from_parts`; add the `society == Settled` integrity clause.
- Modify: `windows/worldgen/src/lib.rs` — `species_carrying_input`, `voice_params`, `morph_options` signatures + their call sites; the two `PsychSummary` projections; the `voice_params` belief call site.
- Modify: `windows/worldgen/src/chorus.rs` — `beta_of`, `doctrine_beta_of`, `account_params_from` stance.
- Modify: `cli/src/phonology.rs` — `sample_names_for` signature + its callers; `morph_options` call sites.
- Modify: `cli/src/audio.rs` — the `sample_names_for` caller.
- Modify: tests naming the moved fields (`windows/worldgen/tests/explanations.rs`, `doctrine.rs`, `confluence.rs`, `demesne.rs`, `beta_calibration_sweep.rs`; `windows/lab/src/metrics.rs`).
- Test: unit tests in `domains/species/src/lib.rs` and `windows/worldgen/src/components.rs`.

**Interfaces:**
- Consumes: `MindVector` from Task 1.
- Produces:
  - `MindVector { threat_response: f64, deliberation_latency: f64, time_horizon: f64 }` (3 fields).
  - `SocietyVector { sociality: Sociality, status_basis: StatusBasis, in_group_radius: f64 }` with `pub const fn baseline() -> Self` and `impl Component`.
  - `society_registry() -> ComponentStore<KindId, SocietyVector>` (the four peoples only).
  - `WorldComponents.society: ComponentStore<KindId, SocietyVector>`.
  - Consumer signatures: `species_carrying_input(base, &MindVector)`; `voice_params(&MindVector, &SocietyVector)`; `morph_options(&SocietyVector)`; `beta_of(&SocietyVector)`; `doctrine_beta_of(&SocietyVector)`; `sample_names_for(world, &str, &SocietyVector)`.

- [ ] **Step 1: Write the failing unit tests** in `domains/species/src/lib.rs` test module:

```rust
#[test]
fn society_baseline_equals_the_goblin_authored_society() {
    let goblin = society_registry().get(&KindId("goblin")).copied().unwrap();
    assert_eq!(goblin, SocietyVector::baseline());
}

#[test]
fn society_registry_holds_exactly_the_settled_peoples() {
    let society: Vec<_> = society_registry().ids().map(|k| k.0).collect();
    assert_eq!(society, vec!["bugbear", "goblin", "hobgoblin", "kobold"]);
    // dragons are minded (psyche) but not Settled — no society vector
    assert!(society_registry().get(&KindId("red-dragon")).is_none());
    assert!(psyche_registry().get(&KindId("red-dragon")).is_some());
}
```

And in `windows/worldgen/src/components.rs` test module:

```rust
#[test]
fn integrity_rejects_a_non_settled_kind_carrying_society() {
    // a Solitary kind (dragon) must not carry a society vector
    // (constructed via from_parts with a bogus society entry → MalformedKind)
    // — see Step 5 for the clause under test.
}
```

- [ ] **Step 2: Run them — expect failure** (`SocietyVector`, `baseline`, `society_registry` undefined):

```bash
cargo test -p hornvale-species society_baseline_equals 2>&1 | tail -5
```

Expected: FAIL to compile (`cannot find ... SocietyVector`).

- [ ] **Step 3: Shrink `MindVector` and add `SocietyVector`** in `domains/species/src/lib.rs`. Replace the six-field struct with:

```rust
/// The individual-mind vector (spec: The Cloister): the psychology every
/// minded kind carries, whether or not it belongs to a society. Scalars are
/// bare ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening requires
/// its own campaign.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MindVector {
    /// How this creature answers threat: flee 0 ↔ stand 1.
    pub threat_response: f64,
    /// How slowly decisions are made (banked; read by the vessel).
    pub deliberation_latency: f64,
    /// How far ahead works are planned: immediate 0 ↔ generational 1.
    pub time_horizon: f64,
}

/// The community-mind vector (spec: The Cloister): the psychology only a
/// society has, carried solely by `Settled` kinds. A `Solitary` creature
/// carries none; consumers needing a society reading for one resolve
/// [`SocietyVector::baseline`]. `in_group_radius` is a bare ratio in `[0, 1]`.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SocietyVector {
    /// Authority shape.
    pub sociality: Sociality,
    /// What earns standing.
    pub status_basis: StatusBasis,
    /// How wide "us" is drawn: insular 0 ↔ expansive 1.
    pub in_group_radius: f64,
}

impl SocietyVector {
    /// The goblin-baseline society reading — the value a mixed consumer
    /// resolves for a `Solitary` kind that carries no society vector. Equal to
    /// the goblin's authored society dims (`Hierarchic`, `Rank`, 0.5).
    pub const fn baseline() -> Self {
        Self {
            sociality: Sociality::Hierarchic,
            status_basis: StatusBasis::Rank,
            in_group_radius: 0.5,
        }
    }
}
```

Update `impl Component for MindVector {}` and add `impl Component for SocietyVector {}`.

- [ ] **Step 4: Split the authoring.** `psyche_registry()` now emits `MindVector` (3 fields) for all seven minded kinds; add `society_registry()` for the four peoples. For each people, keep the same numbers — e.g. goblin:

```rust
// in psyche_registry(): the four peoples + three dragons, MindVector only
(KindId("goblin"), MindVector { threat_response: 0.5, deliberation_latency: 0.5, time_horizon: 0.5 }),
// kobold {0.8, 0.7, 0.8}; hobgoblin {0.7, 0.6, 0.5}; bugbear {0.8, 0.4, 0.3};
// white/red/black-dragon each {0.95, 0.5, 0.90}
```

```rust
/// The community-mind component — authored directly, present only for the
/// four settling peoples (goblin is the baseline). A Solitary minded kind
/// (a dragon) carries a MindVector but no SocietyVector.
/// type-audit: bare-ok(identifier-text)
pub fn society_registry() -> ComponentStore<KindId, SocietyVector> {
    [
        (KindId("goblin"),    SocietyVector { sociality: Sociality::Hierarchic, status_basis: StatusBasis::Rank,      in_group_radius: 0.5 }),
        (KindId("kobold"),    SocietyVector { sociality: Sociality::Communal,   status_basis: StatusBasis::Knowledge, in_group_radius: 0.2 }),
        (KindId("hobgoblin"), SocietyVector { sociality: Sociality::Hierarchic, status_basis: StatusBasis::Rank,      in_group_radius: 0.3 }),
        (KindId("bugbear"),   SocietyVector { sociality: Sociality::Communal,   status_basis: StatusBasis::Rank,      in_group_radius: 0.3 }),
    ]
    .into_iter()
    .collect()
}
```

The three dragon entries in `psyche_registry` lose their `in_group_radius`/`sociality`/`status_basis` lines (and the now-irrelevant `// relates by dominance` comments); they get **no** `society_registry` row. Update the stale `psyche_registry` doc comment (it still says "four settling peoples") to "every minded kind (the four peoples and the three solitary dragons)".

- [ ] **Step 5: Add the `society` store and the integrity clause** in `windows/worldgen/src/components.rs`. Add `pub society: ComponentStore<KindId, SocietyVector>` to `WorldComponents` (after `psyche`); build it in `assemble()` via `hornvale_species::society_registry()`; thread it through `from_parts`. Pass it to `check_integrity` and add:

```rust
// The Cloister: society ⟺ Settled. Every Settled kind carries a society
// vector; no non-Settled kind does (a Solitary carries a mind but no society).
let settled: std::collections::BTreeSet<KindId> = biosphere
    .iter()
    .filter(|(_, b)| b.social_form == hornvale_species::SocialForm::Settled)
    .map(|(k, _)| *k)
    .collect();
if !society.ids().copied().eq(settled.iter().copied()) {
    return Err(BuildError::MalformedKind(
        "society vector key-set must equal the Settled key-set".into(),
    ));
}
```

(`BTreeSet` iterates in `KindId` order; `society.ids()` is already ascending.)

- [ ] **Step 6: Update the pure-society consumers** in `windows/worldgen/src/chorus.rs`:

```rust
pub fn beta_of(society: &SocietyVector) -> f64 {
    let base = match society.status_basis {
        StatusBasis::Knowledge => 1.0,
        StatusBasis::Rank => 2.0,
        StatusBasis::Generosity => 1.5,
    };
    let modifier = if society.sociality == Sociality::Hierarchic { 0.5 } else { 0.0 };
    base + modifier
}
pub fn doctrine_beta_of(society: &SocietyVector) -> f64 { beta_of(society) + 0.5 }
```

Update the `chorus.rs` import to `SocietyVector`. At `beta_of`/`doctrine_beta_of` call sites (`702`, `1074`, `1513`, `2279`), the caller holds a peopled psyche today; look up the society vector for that placed people instead (these are `Settled`, so the vector is always present — `wc.society.get_by_label(species)` / `wc.society.get(kind)`, mirroring the adjacent psyche lookup).

Also update **`account_params_from`** (`~1204`): its stance logic (`1239`–`1241`) reads `psyche.in_group_radius` (now society) and `psyche.threat_response` (still mind). Add a society lookup beside the existing `wc.psyche.get_by_label(species)` (`1216`) — `let society = wc.society.get_by_label(species).ok_or_else(...)?;` (a peopled species always carries one) — and read `society.in_group_radius` for the `Neighbors` branch, keeping `psyche.threat_response` for the `Rivals` branch.

- [ ] **Step 7: Update the mixed + individual consumers** in `windows/worldgen/src/lib.rs`:

```rust
pub fn species_carrying_input(
    base: hornvale_demography::CarryingInput,
    mind: &hornvale_species::MindVector,
) -> hornvale_demography::CarryingInput {
    let freshwater_factor = 0.5 + mind.time_horizon;
    let hostility_factor = (1.5 - mind.threat_response).max(0.0);
    /* unchanged body */
}

pub fn voice_params(
    mind: &hornvale_species::MindVector,
    society: &hornvale_species::SocietyVector,
) -> hornvale_language::VoiceParams {
    hornvale_language::VoiceParams {
        formality: (status_register(society.status_basis) + mind.deliberation_latency) / 2.0,
        repetition: sociality_register(society.sociality),
        epithet_density: status_register(society.status_basis),
    }
}

pub fn morph_options(society: &hornvale_species::SocietyVector) -> hornvale_language::MorphOptions {
    hornvale_language::MorphOptions {
        honorifics: society.status_basis == hornvale_species::StatusBasis::Rank,
    }
}
```

The two `PsychSummary` projections (`~4475`, `~7732`) read `threat_response`/`time_horizon` from the `MindVector` (`wc.psyche.get`) and `sociality`/`status_basis` from the society vector (`wc.society.get`). These are peopled passes (`Settled`), so `wc.society.get(&KindId(name)).expect("peopled pass over a fauna kind")` mirrors the existing psyche `.expect`. The belief `voice_params` call (`~5588`) is likewise peopled — pass both the mind and the society vector looked up by label.

- [ ] **Step 8: Update `sample_names_for` and the baseline-resolving CLI callers** in `cli/src/phonology.rs`. `sample_names_for` needs only the society vector now:

```rust
pub(crate) fn sample_names_for(
    world: &World,
    species: &str,
    society: &hornvale_species::SocietyVector,
) -> Vec<(&'static str, GeneratedName)> {
    let phonology = world_builder::language_of(world, species);
    let namer = Namer::new(&world.seed, species, &phonology);
    let morph = world_builder::morph_options(society);
    /* unchanged body */
}
```

The callers (`phonology.rs:82`, `phonology.rs:281`, `cli/src/audio.rs:34`) iterate `psyche_registry()` (all minded, **including dragons**) — this is the one place a `Solitary` is reached, so resolve the baseline:

```rust
let society = hornvale_species::society_registry()
    .get(kind)
    .copied()
    .unwrap_or(hornvale_species::SocietyVector::baseline());
// ... sample_names_for(&world, species, &society)
```

The `morph_options` call at `phonology.rs:108` is now inside `sample_names_for` (fed the society vector); the standalone `morph_options(psych)` at `phonology.rs:253` (a test) resolves society the same way.

- [ ] **Step 9: Update the remaining test call sites.** `explanations.rs` (`35`–`38`, `beta_of(goblin)` etc. → pass a `SocietyVector`), `doctrine.rs` (`129`–`130`), `confluence.rs:253` / `demesne.rs:339` / `beta_calibration_sweep.rs:134` (`species_carrying_input(..., psych)` → a `MindVector`), `lab/src/metrics.rs:1423`, and the `voice_params` test at `worldgen/src/lib.rs:8087` (now takes mind + society). Fill the society-baseline integrity test stubbed in Step 1 using `from_parts` with a bogus non-Settled society row.

- [ ] **Step 10: Compile workspace-wide, then run the suites.**

```bash
cargo clippy --workspace --all-targets -- -D warnings
cargo nextest run -p hornvale-species -p hornvale-worldgen -p hornvale-lab -p hornvale
```

Expected: clippy clean; tests PASS (the new unit tests included). If a calibration/belief test's *value* changed, STOP — the split was supposed to preserve every peopled reading; a value drift means a field was mis-split.

- [ ] **Step 11: type-audit + fmt + commit.**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
git add -A
git commit -m "feat(the-cloister): split MindVector / SocietyVector; Solitary carries no society

SocietyVector (sociality, status_basis, in_group_radius) is carried only by
Settled kinds; MindVector shrinks to the individual dims. Dragons drop their
authored society dims; mixed consumers resolve SocietyVector::baseline() for a
Solitary. check_integrity gains society == Settled. Peopled readings unchanged.
Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ"
```

---

### Task 3: Verify byte-identity and update the book

The load-bearing claim (§4): worlds AND the reference catalogue are byte-identical. Verify by regeneration and diff — do not assert.

**Files:**
- Modify: `book/src/` species/psychology model-card chapter (§5 — split into a *mind* card and a *society* card; note the dragon's baseline resolution).
- Regenerate (must diff clean): `book/src/reference/` dictionary + phonology pages, `book/src/audio/` (no new clips expected), the three seed-42 almanacs under `book/src/gallery/`.
- Modify: `docs/audits/type-audit-report.md` (regenerated).

- [ ] **Step 1: Regenerate the reference catalogue and almanacs, then diff.** Run the artifact commands from `.github/workflows/ci.yml`'s "Artifacts are current" step (the dictionary/phonology/audio + almanac regen), then:

```bash
git diff --stat book/src/reference/ book/src/gallery/ book/src/audio/
```

Expected: **empty** (byte-identical). If any file changed, STOP and investigate — it is a consumer the §4 map missed (do not re-pin a drift as if expected).

- [ ] **Step 2: Confirm no census touched.** The census goldens (`book/src/laboratory/generated/*/rows.csv`) must be untouched; do **not** run `HV_CENSUS=1`. Confirm:

```bash
git diff --exit-code book/src/laboratory/
```

Expected: clean.

- [ ] **Step 3: Update the model card** (§5): split the psychology card into mind (three individual dims, every minded kind) and society (three community dims, `Settled` only); add the one-line dragon note (a solitary carries a mind but no society vector; its society reading resolves to the baseline).

- [ ] **Step 4: Regenerate the type-audit report.**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat docs/audits/
```

- [ ] **Step 5: Full gate.**

```bash
make gate
```

Expected: PASS.

- [ ] **Step 6: fmt + commit.**

```bash
cargo fmt
git add -A
git commit -m "docs(the-cloister): split the psychology model card; verify byte-identity

Reference catalogue + seed-42 almanacs regenerated: no drift (the split is
byte-identical). Census untouched. Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ"
```

---

## Notes for the executor

- **Not my campaign's bug:** the stale comments in `phonology.rs:40` and `audio.rs:25` claim the loop "skips dragons" — dragons became speakers in The Solitary Tongue, so it no longer does. Leave them (orthogonal to the split).
- The seven `.expect("peopled pass over a fauna kind")` sites stay as-is — they guard peopled passes and dragons are unplaced. Fixing them with fallbacks is the Placement campaign's job (spec §7).
- Close with `closing-a-campaign` (chronicle entry, retrospective, decision 00XX, Confidence-Gradient sweep if any bet moved — none expected).
