# The Hallmark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ratify the kernel-membership criterion, promote six forced-duplicate types into the kernel (spellings preserved), and ship `tools/placement-audit` — a shape-twin detector with a tagged, fingerprinted novelty ratchet.

**Architecture:** Kernel gains small roster/vocabulary modules on the `band.rs` model ("the kernel holds the type, the domain holds the meaning"); each donating domain keeps behavior and re-exports the kernel type (`pub use`) so downstream paths stay source-compatible. The tool is a standalone syn-based source walker outside the workspace, modeled on `tools/type-audit` (walk/extract) and `tools/seam-guard` (tag grammar).

**Tech Stack:** Rust edition 2024; workspace deps serde/serde_json/libm only; the tool crate (outside the workspace) uses `syn = 2` + `proc-macro2` with `span-locations`, exactly like type-audit.

**Spec:** `docs/superpowers/specs/2026-09-01-the-hallmark-design.md`
**Ledger:** `docs/superpowers/ledgers/2026-09-01-the-hallmark.md` (append rulings as they occur)

## Global Constraints

- Every committed spelling is preserved exactly: `"eternal"/"cyclic"/"ambient"` (religion's ledger tags), `"karst"/"lava-tube"/"fracture"` (CaveKind's scene legend), `"karst-cave"/"lava-tube"/"fracture-cave"` (the corpus genera), variant names serialized by serde (`"Ordinary"` etc.). Under decision 0216 these moves are not save-format epochs *because* spellings survive; any committed byte moving is the STOP branch of that task.
- Branch table for every promotion task's `make rebaseline` check: only `docs/audits/` and `docs/digest/` moved → regenerate and commit in the same commit (pub-boundary changes always drift the type-audit report — never treat that as failure); anything under `book/src/laboratory/generated/`, census CSVs, SVG filenames, or `clients/game/core/tests/fixtures/` moved → STOP, report, do not commit.
- No new workspace dependencies (`cli/tests/architecture.rs` `ALLOWED_EXTERNAL`); the tool crate is outside the workspace and carries its own.
- `BTreeMap`/`BTreeSet`/`Vec` only; no `HashMap`. No wall-clock time. Every pub item gets a one-line doc comment (`#![warn(missing_docs)]`).
- `cargo fmt` as the final step before every commit; every commit passes `make gate-commit` (the pre-commit hook runs it when Rust paths are staged).
- Each task's commit regenerates `docs/audits/type-audit-report.md` when pub boundaries changed: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md` — the gate's `type-audit-report` freshness check fails otherwise.
- Kernel `pub use` re-exports name items explicitly (the `kernel/src/lib.rs` house style), never `pub use module::*`.
- Work in the campaign worktree `/Users/nathan/Projects/hornvale/.claude/worktrees/the-hallmark` on branch `campaign/the-hallmark`.

**Stage boundaries:** after Task 5 and after Task 8, submit a stage gate (`make sluice-stage BRANCH=campaign/the-hallmark REF=<full-sha>` from the controller, not from a task).

---

### Task 1: The criterion decision (docs only)

**Files:**
- Create: `docs/decisions/0517-the-kernel-membership-criterion.md`
- Modify: `docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md` (§4.7, add pointer)
- Modify: `docs/digest/decisions-in-force.md` (regenerated)

**Interfaces:**
- Consumes: spec §2 of `docs/superpowers/specs/2026-09-01-the-hallmark-design.md` (the criterion text).
- Produces: decision number **0517**, cited by every later task's tags and doc comments.

- [ ] **Step 1: Write the decision record.** Header format matches `docs/decisions/0516-*.md`:

```markdown
# 0517. The kernel-membership criterion

**Status:** Accepted (2026-09-01) · **Decider:** Nathan · **Campaign:** The Hallmark

## Context

Decision 0044 states the only ratified placement test and scopes it to
coherent physical quantities. Decision 0216 extended it by analogy to one
non-unit roster (`Band`) without generalizing. No record says what belongs
in the kernel as such, and the Entity-Component program (metaplan
2026-07-14 §4.7) will force the question at scale: §4.7 places mechanisms
in the kernel and component registries in domains but is silent on the
types those registries' schemas are made of.

## Decision

A type belongs in the kernel when any clause holds:

- **(a)** More than one domain speaks it today — 0044's clause generalized
  from quantities to any type, qualified by 0216's forced-vs-deliberate
  test: a *forced* duplicate (one side exists only because layering forbids
  the import) qualifies; a *deliberate* projection (deleting one side would
  remove an independent answer, or the two carry different information)
  does not.
- **(b)** It originates in a kernel type — 0044's clause, unchanged.
- **(c)** It appears in the wire schema of a component or relationship
  registered for cross-domain query in the Entity-Component component
  catalog. Dormant until the catalog exists; ratified now so the EC
  campaigns inherit a settled rule. Its enforcement (an architecture test
  asserting every type in a registered schema resolves to
  `hornvale_kernel`) is those campaigns' deliverable.

**Modifier, all clauses — stability graduation:** a volatile type iterates
domain-side; its identity may be reserved kernel-side early; the type moves
when its shape settles. Kernel churn is the most expensive rebuild tier and
promotion is save-format-adjacent, so promotion is deliberate and
occasional, never a reflex.

**The split (0216's formula):** the kernel holds the type, the roster, and
the ordering; the domain holds the meaning — derivations, valences,
behavior — via local traits or free functions (orphan-rule-legal), with
`pub use` keeping call sites source-compatible.

**Refused:** placement by anticipation; closed kernel enums enumerating
per-domain content (adding a domain must never edit the kernel — open
registries carry growing vocabularies); any promotion that changes a
committed spelling without an epoch.

## Consequences

Amends 0044's scope (its mechanics stand). Amends metaplan §4.7 (mechanism
placement stands; wire types gain a rule). Resolves idea-registry row
`DOM-kernel-owns-vocabulary` (raw → ratified). Enforced forward by
`tools/placement-audit` (The Hallmark spec §3): a novelty ratchet on shape
twins, with reconsider-on-touch via shape fingerprints. The tool detects
and demands; it never decides — shape identity is not semantic identity.
```

- [ ] **Step 2: Amend the metaplan.** In `docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md` §4.7, append this paragraph after the existing one (do not edit the existing text):

```markdown
**Amended by decision 0517 (The Hallmark, 2026-09-01):** the placement rule
above is silent on the *types a registered component's schema is made of*.
0517 clause (c) closes that: a type appearing in the wire schema of a
component registered for cross-domain query belongs in the kernel. Domains
still own their registries; the kernel additionally owns the wire
vocabulary those registries speak.
```

- [ ] **Step 3: Regenerate the digest's decision index** (it drifts on any decision addition):

Run: `cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md`

Then check what moved: `git diff --stat docs/digest/`. Branch: only `decisions-in-force.md` (and possibly `intent-vs-reality.md`) moved → proceed. Anything else → STOP and report.

- [ ] **Step 4: Commit** (docs-only; the pre-commit hook will skip the gate):

```bash
git add docs/decisions/0517-the-kernel-membership-criterion.md \
        docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md \
        docs/digest/
git commit -m "decision(0517): the kernel-membership criterion (The Hallmark)"
```

---

### Task 2: UnitError — one definition

**Files:**
- Modify: `domains/astronomy/src/units.rs` (delete lines ~8-29: the struct + Display + Error impls; add import)
- Modify: `domains/paleoclimate/src/units.rs` (delete lines ~4-14: the struct; add import)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: `hornvale_kernel::UnitError` (already re-exported from `kernel/src/lib.rs:78-81`; fields `unit: &'static str`, `value: f64`, `reason: &'static str`; has `Display` + `std::error::Error`).
- Produces: `hornvale_astronomy::UnitError` and `hornvale_paleoclimate::UnitError` are now re-exports of the kernel type. Existing `pub use` lines in each crate's `lib.rs` (`domains/astronomy/src/lib.rs:65`, `domains/paleoclimate/src/lib.rs:20`) keep working unchanged because they re-export from the `units` module, which now re-exports the kernel's.

Working precedent: `domains/species/src/lib.rs:22` already imports `UnitError` from the kernel and constructs it — this task makes astronomy and paleoclimate do the same.

- [ ] **Step 1: Capture the current behavior (the red that proves the swap is observable).** In `domains/astronomy/src/units.rs`, the existing test `errors_name_the_unit_and_reason` (~line 347) exercises `Display` via `e.to_string()`. Run it and confirm green before touching anything:

Run: `cargo test -p hornvale-astronomy errors_name_the_unit_and_reason`
Expected: PASS (this is the behavior that must survive the swap).

- [ ] **Step 2: Replace astronomy's definition.** In `domains/astronomy/src/units.rs`, delete the `UnitError` struct, its `Display` impl, and its `impl std::error::Error` block (the contiguous block from `/// Why a quantity constructor refused a value.` through `impl std::error::Error for UnitError {}`). In their place add:

```rust
pub use hornvale_kernel::UnitError;
```

The `use std::fmt;` at the top may now be unused — remove it only if `cargo check` says so (other items in the file may use it).

- [ ] **Step 3: Replace paleoclimate's definition.** In `domains/paleoclimate/src/units.rs`, delete the `UnitError` struct (it has no impls). In its place add:

```rust
pub use hornvale_kernel::UnitError;
```

Note: paleoclimate's copy had no `Display`/`Error` impls, so adopting the kernel's *adds* capability; nothing is removed.

- [ ] **Step 4: Check the workspace compiles and scoped tests pass:**

Run: `cargo check -p hornvale-astronomy -p hornvale-paleoclimate && cargo test -p hornvale-astronomy -p hornvale-paleoclimate`
Expected: PASS, including `errors_name_the_unit_and_reason` (the `quantity!` macro refers to `UnitError` unqualified, so the re-export satisfies it).

- [ ] **Step 5: Regenerate the type-audit report** (two pub structs vanished):

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Record the deferred minors in the ledger.** Append to `docs/superpowers/ledgers/2026-09-01-the-hallmark.md`: `terrain/src/crust.rs`'s `Result<CrustKm, String>` and `windows/worldgen/src/harvest.rs`'s `LatError` are error-convention outliers deferred (the latter dissolves with the queued angle family). Neither is a shape twin the detector sees.

- [ ] **Step 7: fmt + commit:**

```bash
cargo fmt
git add domains/astronomy/src/units.rs domains/paleoclimate/src/units.rs \
        docs/audits/type-audit-report.md docs/superpowers/ledgers/2026-09-01-the-hallmark.md
git commit -m "refactor(kernel): one UnitError — astronomy and paleoclimate adopt the kernel's (0517 clause a)"
```

---

### Task 3: kernel `genesis` module — GenesisError and GenesisOutcome<T>

**Files:**
- Create: `kernel/src/genesis.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod genesis;` to the module block, alphabetical; add `pub use genesis::{GenesisError, GenesisOutcome};`)
- Modify: `domains/astronomy/src/pins.rs` (delete `GenesisError` + Display + Error impls; add `pub use`)
- Modify: `domains/astronomy/src/system.rs` (delete `GenesisOutcome`; adopt `GenesisOutcome<StarSystem>`; field `system` → `value`)
- Modify: `domains/astronomy/src/provider.rs`, `domains/astronomy/src/facts.rs`, `domains/astronomy/src/lib.rs` (field-name and re-export follow-through)
- Modify: `domains/terrain/src/pins.rs`, `domains/terrain/src/globe.rs`, `domains/terrain/src/provider.rs`, `domains/terrain/src/facts.rs`, `domains/terrain/src/lib.rs` (same shape; field `globe` → `value`)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: nothing new.
- Produces: `hornvale_kernel::genesis::{GenesisError, GenesisOutcome}`, re-exported at the kernel root. `GenesisError` moves **verbatim** (same two variants, same field names, same Display strings `"invalid pin '{pin}': {reason}"` / `"unsatisfiable pin '{pin}': {reason}"`). `GenesisOutcome<T> { pub value: T, pub notes: Vec<String> }`. Astronomy's `generate` returns `GenesisOutcome<StarSystem>`; terrain's returns `GenesisOutcome<TectonicGlobe>`. Downstream field accesses change: `outcome.system` → `outcome.value` (astronomy), `outcome.globe` → `outcome.value` (terrain). Each domain's `pins`/`system`/`globe` modules re-export the kernel items so every existing `hornvale_astronomy::GenesisError` / `hornvale_terrain::GenesisOutcome` path keeps compiling.

- [ ] **Step 1: Write the kernel module with its own tests first.** Create `kernel/src/genesis.rs`:

```rust
//! Shared genesis vocabulary (decision 0517 clause (a)): the pin-refusal
//! error and the outcome envelope that every pin-driven genesis speaks.
//! `GenesisError` was defined character-for-character identically in
//! `domains/astronomy` and `domains/terrain` — a forced duplicate, since
//! neither may import the other (decision 0002). The kernel holds the type;
//! each domain keeps its own pins, validation, and generation (the
//! roster/meaning split, decision 0216).
//!
//! Genesis itself is deliberately NOT a `Domain` trait member — its inputs
//! are domain-specific composition work (Constitution §2.6; see
//! `kernel/src/domain.rs`). These are shared *types*, not a shared
//! mechanism.

/// Why genesis refused to produce its artifact.
/// type-audit: bare-ok(identifier-text: InvalidPin.pin), bare-ok(prose: InvalidPin.reason), bare-ok(identifier-text: UnsatisfiablePin.pin), bare-ok(prose: UnsatisfiablePin.reason)
#[derive(Debug, Clone, PartialEq)]
pub enum GenesisError {
    /// A pin's value is outside its legal range.
    InvalidPin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The rule it violates.
        reason: String,
    },
    /// A legal pin has no physically consistent solution under the model.
    UnsatisfiablePin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The physical conflict.
        reason: String,
    },
}

impl std::fmt::Display for GenesisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenesisError::InvalidPin { pin, reason } => {
                write!(f, "invalid pin '{pin}': {reason}")
            }
            GenesisError::UnsatisfiablePin { pin, reason } => {
                write!(f, "unsatisfiable pin '{pin}': {reason}")
            }
        }
    }
}

impl std::error::Error for GenesisError {}

/// What genesis produced: the artifact plus the degradation notes it
/// recorded along the way (empty when genesis was untroubled). The notes
/// become genesis-note facts at the composition root.
/// type-audit: bare-ok(prose: notes)
#[derive(Debug, Clone, PartialEq)]
pub struct GenesisOutcome<T> {
    /// The generated artifact (a star system, a tectonic globe, ...).
    pub value: T,
    /// Human-readable degradation records.
    pub notes: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The Display strings are a CLI-facing spelling contract — both domains
    /// printed exactly these before the move.
    #[test]
    fn display_spellings_are_preserved() {
        let invalid = GenesisError::InvalidPin {
            pin: "moons".to_string(),
            reason: "must be small".to_string(),
        };
        assert_eq!(invalid.to_string(), "invalid pin 'moons': must be small");
        let unsat = GenesisError::UnsatisfiablePin {
            pin: "sky".to_string(),
            reason: "no such star".to_string(),
        };
        assert_eq!(unsat.to_string(), "unsatisfiable pin 'sky': no such star");
    }
}
```

- [ ] **Step 2: Wire the kernel module and run its test:**

In `kernel/src/lib.rs`, add `pub mod genesis;` after `pub mod geosphere;`... — no: keep the block alphabetical, so between `pub mod fold;` and `pub mod geosphere;`. Add `pub use genesis::{GenesisError, GenesisOutcome};` in the re-export section (after the `pub use fold::…` line if present, keeping that section's existing order style).

Run: `cargo test -p hornvale-kernel display_spellings_are_preserved`
Expected: PASS

- [ ] **Step 3: Adopt in astronomy.** In `domains/astronomy/src/pins.rs`, delete the `GenesisError` enum, its `Display` impl, and `impl std::error::Error` (the block at ~120-153). Add in its place:

```rust
pub use hornvale_kernel::genesis::GenesisError;
```

In `domains/astronomy/src/system.rs`, delete the `GenesisOutcome` struct (~29-37) and add:

```rust
pub use hornvale_kernel::genesis::GenesisOutcome;
```

Then update astronomy's uses of the payload field: in `system.rs` the `Ok(GenesisOutcome { system, notes })` construction becomes `Ok(GenesisOutcome { value: system, notes })` and the return type `Result<GenesisOutcome, GenesisError>` becomes `Result<GenesisOutcome<StarSystem>, GenesisError>`. In `provider.rs` (~:371, :432, :498, :697, :745, :1546) and `facts.rs` (~:271, :697), change `GenesisOutcome` type mentions to `GenesisOutcome<StarSystem>` and every `.system` field access / `system:` initializer on an outcome to `.value` / `value:`. `domains/astronomy/src/lib.rs:62`'s `pub use system::{GenesisOutcome, StarSystem, generate};` keeps working (it re-exports the module's re-export).

Run: `cargo check -p hornvale-astronomy 2>&1 | head -30` and fix every site the compiler names — the compiler, not this list, is the authority on the complete site set.

- [ ] **Step 4: Adopt in terrain, same shape.** `domains/terrain/src/pins.rs`: delete the enum + impls (~30-63), add `pub use hornvale_kernel::genesis::GenesisError;`. `domains/terrain/src/globe.rs`: delete `GenesisOutcome` (~215-223), add `pub use hornvale_kernel::genesis::GenesisOutcome;`, change `generate`'s return type to `Result<GenesisOutcome<TectonicGlobe>, GenesisError>`, `Ok(GenesisOutcome { globe, notes })` → `Ok(GenesisOutcome { value: globe, notes })`. Follow the compiler through `provider.rs` (`:83`) and `facts.rs` (`:62`) — `.globe` → `.value`.

Run: `cargo check -p hornvale-terrain 2>&1 | head -30`, fix, then `cargo check --workspace --all-targets 2>&1 | tail -5` — `windows/worldgen`'s `BuildError::Genesis(GenesisError)` and `BuildError::TerrainGenesis(hornvale_terrain::GenesisError)` now wrap the *same* kernel type through two re-export paths; both variants stay (they distinguish which genesis failed), and the code compiles unchanged.
Expected: clean check.

- [ ] **Step 5: Run the save-format-sensitive property suites** (stream consumption order must be untouched — this task moves types, never draws):

Run: `cargo test -p hornvale-astronomy --test suite -- genesis_properties && cargo test -p hornvale-terrain --test suite -- tectonic_properties`
Expected: PASS

- [ ] **Step 6: Regenerate the type-audit report, fmt, commit:**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add kernel/src/genesis.rs kernel/src/lib.rs domains/astronomy domains/terrain docs/audits/type-audit-report.md
git commit -m "refactor(kernel): genesis module — GenesisError verbatim, GenesisOutcome<T> (0517 clause a)"
```

Note: this is a kernel-layer edit — expect the commit gate to take minutes, not seconds.

---

### Task 4: locale adopts climate's GroundKind; Substrate deleted

**Files:**
- Modify: `domains/climate/src/variants.rs` (add `Serialize` derive to `GroundKind`)
- Modify: `windows/locale/src/regime.rs` (delete `Substrate`; `Negations.substrate` re-typed)
- Modify: `windows/locale/src/substrate.rs`, `windows/locale/src/budget.rs`, `windows/locale/src/grammar.rs` (rename sites; delete `ground_of`)
- Modify: `windows/locale/src/lib.rs` (re-export swap at :15, literal at :723)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: `hornvale_climate::GroundKind` (five variants `Ordinary/Sand/Evaporite/Basaltic/Ashen`; locale already depends on `hornvale-climate` — `windows/locale/Cargo.toml:11`).
- Produces: `Negations.substrate: hornvale_climate::GroundKind`. `hornvale_locale::Substrate` no longer exists; `windows/locale/src/lib.rs:15` re-exports `GroundKind` in its place. No other crate consumed `hornvale_locale::Substrate` (verified by workspace grep in the dossier), so no downstream breakage.

**Hazard, stated up front:** `hornvale_climate::Substrate` already exists as an unrelated *trait* (`domains/climate/src/substrate.rs:38`). Do not alias `GroundKind as Substrate` in locale — the collision is exactly why the rename must be spelled out. Locale also has its own module `windows/locale/src/substrate.rs` (the `substrate_at` derivation) — the module keeps its name; only the *type* renames.

- [ ] **Step 1: Capture the current serialization (mutation-proved).** `Substrate` derives `Serialize` and is a field of `Negations` (also `Serialize`). Before changing anything, add a pinning test at the bottom of `windows/locale/src/regime.rs`'s existing `mod tests`:

```rust
    /// Pins the serialized spelling of the substrate slot across the
    /// Substrate -> GroundKind swap (The Hallmark): serde derives the
    /// variant name, and the two enums' variant names are identical, so
    /// this string must not move.
    #[test]
    fn substrate_slot_serializes_by_variant_name() {
        let n = mundane();
        let json = serde_json::to_string(&n).expect("Negations serializes");
        assert!(
            json.contains("\"substrate\":\"Ordinary\""),
            "substrate slot spelling moved: {json}"
        );
    }
```

Run: `cargo test -p hornvale-locale substrate_slot_serializes_by_variant_name`
Expected: PASS on the *old* type first — that green is the baseline the swap must preserve. (If the assertion text is not found, STOP: the serialization shape is not what this plan assumed; report what `json` actually contains.)

- [ ] **Step 2: Give `GroundKind` the `Serialize` derive.** In `domains/climate/src/variants.rs:333`, change:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
```
to
```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
```

and ensure the file imports it (`use serde::Serialize;` at the top if not present). If `domains/climate/Cargo.toml` lacks a serde dependency, add `serde = { workspace = true }` — check first: `grep serde domains/climate/Cargo.toml`.

- [ ] **Step 3: The swap.** In `windows/locale/`:
  - `regime.rs`: delete the `Substrate` enum (~11-24). Add `use hornvale_climate::GroundKind;` and re-type the field: `pub substrate: GroundKind,`. Update `Negations::strangeness`'s `Substrate::Ordinary` → `GroundKind::Ordinary` and the three test fixtures (`:130, :145, :169`).
  - `substrate.rs`: `use crate::regime::Substrate;` → `use hornvale_climate::GroundKind;`; return type and the six `Substrate::X` sites → `GroundKind::X`.
  - `budget.rs`: import list drops `Substrate`, gains `use hornvale_climate::GroundKind;`; `:205` renames.
  - `grammar.rs`: import swap; **delete `ground_of` entirely** (~265-277) and its two call sites: `:291` becomes `let pool = hornvale_climate::variant_pool(formation, stratum, substrate);` and the test at `:519` passes `GroundKind::Ordinary` directly. `derived_regime`'s and `draw_variety`'s and `substrate_pool`'s parameter types rename; all test fixtures rename.
  - `lib.rs`: `:15` re-export becomes `pub use hornvale_climate::GroundKind;` alongside the remaining `regime::{…}` names (drop `Substrate` from that list); `:723` renames.

- [ ] **Step 4: Verify — the pinning test must pass on the new type unchanged:**

Run: `cargo test -p hornvale-locale && cargo check --workspace --all-targets 2>&1 | tail -3`
Expected: PASS, including `substrate_slot_serializes_by_variant_name` — same assertion, new type. Clean workspace check.

- [ ] **Step 5: Regenerate, fmt, commit:**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add domains/climate/src/variants.rs domains/climate/Cargo.toml windows/locale docs/audits/type-audit-report.md
git commit -m "refactor(locale): adopt climate's GroundKind, delete Substrate and its identity shim"
```

---

### Task 5: paleoclimate days — diagnose, then migrate or STOP

**Files:**
- Possibly modify: `domains/paleoclimate/src/strata.rs`, `domains/paleoclimate/src/ice.rs`, `domains/paleoclimate/src/facts.rs`, `windows/worldgen/src/lib.rs`, `windows/worldgen/src/history_bake.rs`, `cli/src/main.rs`, plus test fixtures
- Always modify: `docs/superpowers/ledgers/2026-09-01-the-hallmark.md` (the diagnosis is recorded either way)

**Interfaces:**
- Consumes: `hornvale_kernel::WorldTime` (`from_std_days(f64) -> Result<WorldTime, UnitError>`, `as_std_days() -> f64`, ticks are `i64`, negative legal, derives `Ord`) and `TickSpan` (`from_std_days`, `as_std_days`).
- Produces: either `EraClimate.day: WorldTime`, `IceState.day: WorldTime`, `PaleoRecord.glacial_maximum_day: WorldTime` (the migration branch), or a ledger entry + idea-registry row descoping it (the STOP branch).

**Why this task is gated on a diagnosis, not a prediction.** The composition root conflates two axes in this field: `windows/worldgen/src/lib.rs:3876` writes `day: cfg.start_year` (a *year* into a *day* slot) and `windows/worldgen/src/history_bake.rs:1651` compares `e.day <= year`. A retype to `WorldTime` forces that conflation into the open — which may be a semantic repair beyond this campaign's scope. Additionally `domains/paleoclimate/src/facts.rs:88` commits `Value::Number(record.glacial_maximum_day)` to the ledger — a committed-byte surface.

- [ ] **Step 1: Diagnose the conflation.** Read `windows/worldgen/src/lib.rs` around :3733-3765 and :3876-3924, and `windows/worldgen/src/history_bake.rs:1640-1660`. Answer in writing (ledger entry): is `EraClimate.day` on the constant-sky/bake path a *standard day* (same axis as the deep-time path, merely small numbers) or a *year* (a genuinely different unit sharing the slot)? The deciding observable: `history_bake.rs`'s `era_index_for(&self, eras, year: f64)` — trace what its `year` argument actually is at its call sites.

- [ ] **Step 2: Branch on the diagnosis.**
  - **Same axis (days throughout, just misleadingly named `year` variables):** proceed to Step 3.
  - **Two axes sharing the slot (a year genuinely compared to a day):** STOP the migration. Append the ledger entry with the evidence (file:line, the trace from Step 1), add an idea-registry row (`DOM-` prefix, status `raw`) naming the conflation as the prerequisite the migration is blocked on, cite it from the `pending(wave-2: day)` tags' doc lines (do not remove the tags), and commit just those docs. The task is then complete — a null is a result.

- [ ] **Step 3 (migration branch only): capture the committed-byte baseline.** Before any retype:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hallmark-w42-before.json
```

- [ ] **Step 4 (migration branch only): retype.** `EraClimate.day`, `IceState.day`, `PaleoRecord.glacial_maximum_day` become `WorldTime`; `integrate_ice`'s `samples: &[(f64, f64)]` becomes `&[(WorldTime, f64)]`. Mechanical rules:
  - `b.day.total_cmp(&a.day)` (both `strata.rs:122` and the byte-for-byte duplicate at `windows/worldgen/src/lib.rs:3764` — the comment there says they must match; change both identically) → `b.day.cmp(&a.day)`.
  - `(day - p) / DAYS_PER_KYR` in `ice.rs` → `(day - p).as_std_days() / DAYS_PER_KYR` (the `Sub` impl yields `TickSpan`).
  - Every `day: <f64 expr>` construction site converts once, at the crossing: `WorldTime::from_std_days(expr).expect("era day within tick range")` — the rounding rule named at the call, per the kernel's hatch doc.
  - `facts.rs:88` commits `Value::Number(record.glacial_maximum_day.as_std_days())` — the same f64 back out.
  - Remove the `pending(wave-2: day)` / `pending(wave-2: glacial_maximum_day)` / `pending(wave-2: samples)` tag entries for the migrated positions (the primitive is gone; type-audit will fail on a stale position tag otherwise).
  - Follow `cargo check --workspace --all-targets` through the worldgen and test-fixture sites; the compiler is the site list.

- [ ] **Step 5 (migration branch only): the byte-identity verdict.**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hallmark-w42-after.json
diff /tmp/hallmark-w42-before.json /tmp/hallmark-w42-after.json && echo IDENTICAL
```

Branch: IDENTICAL → proceed. Any diff → STOP; do not commit code; record the differing lines in the ledger and take the Step 2 STOP branch instead (the diff is the evidence the retype moves committed bytes, i.e. an epoch question).

- [ ] **Step 6: scoped tests, report, fmt, commit** (wording per branch):

```bash
cargo test -p hornvale-paleoclimate && cargo test -p hornvale-worldgen 2>&1 | tail -3
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add -A domains/paleoclimate windows/worldgen cli/src docs/audits docs/superpowers/ledgers
git commit -m "refactor(paleoclimate): day fields carry WorldTime (0517 clause b residue)"
```

---

### Task 6: Sentiment — kernel phenomena vocabulary

**Files:**
- Modify: `kernel/src/phenomena.rs` (add `Sentiment` + `Sentiment::of`)
- Modify: `kernel/src/lib.rs` (add `Sentiment` to the `pub use phenomena::{…}` list)
- Modify: `domains/religion/src/lib.rs` (delete the enum; re-export; keep spellings as functions)
- Modify: `domains/language/src/register.rs` + `domains/language/src/lib.rs` (delete `LineSentiment`; adopt kernel `Sentiment`)
- Modify: `windows/worldgen/src/lib.rs` (delete `line_sentiment_of`, ~9946-9956; its one call site at :9973)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: `kernel::phenomena::{Phenomenon, Venue}` (already kernel-resident; `Sentiment::of` reads `phenomenon.venue` and `phenomenon.period_days`).
- Produces: `hornvale_kernel::Sentiment` with variants `Eternal/Cyclic/Ambient` and `pub fn of(&Phenomenon) -> Sentiment`. Religion re-exports it (`pub use hornvale_kernel::Sentiment;`) so every `hornvale_religion::Sentiment` path downstream (worldgen `:5407/:7431/:7454`, chorus `:256`, almanac tests) compiles unchanged. Religion keeps the ledger spellings as free functions: `pub fn sentiment_tag(Sentiment) -> &'static str` (was `as_str`) and private `fn parse_sentiment(&str) -> Option<Sentiment>` (was `parse`). Language's `LineContent.sentiment` is re-typed to `Sentiment`; `LineSentiment` no longer exists; `domains/language/src/lib.rs:161` re-exports `Sentiment` from the kernel in its place.

**Save-format surface:** the strings `"eternal"/"cyclic"/"ambient"` are committed ledger facts and appear thousands of times in the committed census CSVs (`book/src/laboratory/generated/the-census/rows.csv` carries 4692× `cyclic`). They live in `sentiment_tag`'s match arms and must move character-for-character.

- [ ] **Step 1: The kernel type, with a test.** In `kernel/src/phenomena.rs`, after the `Phenomenon` struct, add:

```rust
/// A consumer's felt relationship to a phenomenon — watched (eternal),
/// mourned-and-feasted (cyclic), or felt through the ambient world rather
/// than watched (ambient). A pure function of a phenomenon's venue and
/// periodicity, so it lives beside [`Phenomenon`] (decision 0517 clause
/// (b): it originates in a kernel type). Each domain keeps its own meaning
/// of it: religion's ledger spelling (`sentiment_tag`), language's render
/// registers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sentiment {
    /// An unchanging presence in the day or night sky: always watched.
    Eternal,
    /// A presence that departs and returns: mourned in absence, feasted on
    /// return.
    Cyclic,
    /// Felt through the ambient world (air, seasons) rather than watched.
    Ambient,
}

impl Sentiment {
    /// Derive a sentiment from a phenomenon's venue and periodicity:
    /// `Venue::Ambient` is always `Ambient`; otherwise an aperiodic
    /// phenomenon (`period_days: None`) is `Eternal` and a periodic one is
    /// `Cyclic`.
    pub fn of(phenomenon: &Phenomenon) -> Self {
        if phenomenon.venue == Venue::Ambient {
            Sentiment::Ambient
        } else if phenomenon.period_days.is_none() {
            Sentiment::Eternal
        } else {
            Sentiment::Cyclic
        }
    }
}
```

(The derivation body is religion's, moved verbatim.) Add a kernel test in `phenomena.rs`'s test module pinning all three arms of `of` — build a `Phenomenon` per arm and assert the variant.

Add `Sentiment` to `kernel/src/lib.rs`'s `pub use phenomena::{…}` list (alphabetical within the braces).

Run: `cargo test -p hornvale-kernel sentiment`
Expected: PASS

- [ ] **Step 2: Religion adopts.** In `domains/religion/src/lib.rs`, delete the `Sentiment` enum and its whole `impl Sentiment` block (~178-229). Add:

```rust
pub use hornvale_kernel::Sentiment;

/// The lowercase tag committed to the ledger's `sentiment` fact — a
/// save-format spelling contract (religion's meaning of the kernel's
/// [`Sentiment`], per decision 0517's roster/meaning split).
/// type-audit: bare-ok(identifier-text: return)
pub fn sentiment_tag(sentiment: Sentiment) -> &'static str {
    match sentiment {
        Sentiment::Eternal => "eternal",
        Sentiment::Cyclic => "cyclic",
        Sentiment::Ambient => "ambient",
    }
}

/// Parse the lowercase tag committed to the ledger's `sentiment` fact.
/// `None` for anything else (a legacy save with no `sentiment` fact).
fn parse_sentiment(tag: &str) -> Option<Sentiment> {
    match tag {
        "eternal" => Some(Sentiment::Eternal),
        "cyclic" => Some(Sentiment::Cyclic),
        "ambient" => Some(Sentiment::Ambient),
        _ => None,
    }
}
```

Update the in-crate call sites: `:347` `sentiment.as_str()` → `sentiment_tag(sentiment)`; `:401` `.and_then(Sentiment::parse)` → `.and_then(parse_sentiment)`. (`Sentiment::of` at `:307` compiles unchanged — `of` moved with the type.)

Run: `cargo test -p hornvale-religion`
Expected: PASS — the tests at `:597/:602/:626` assert variants through the re-export.

- [ ] **Step 3: Language adopts.** In `domains/language/src/register.rs`: delete `LineSentiment` (~33-44) and the module-doc sentence claiming a copy; add `use hornvale_kernel::Sentiment;`; re-type `LineContent.sentiment: Sentiment`; the three match arms `LineSentiment::X` → `Sentiment::X`; the test fixture at `:175`. In `domains/language/src/lib.rs:161`, replace `LineSentiment` in the re-export list with a separate `pub use hornvale_kernel::Sentiment;` line.

- [ ] **Step 4: Delete the shim.** In `windows/worldgen/src/lib.rs`, delete `line_sentiment_of` (~9946-9956); at `:9973`, `sentiment: line_sentiment_of(belief.sentiment),` → `sentiment: belief.sentiment,` (both sides are now the kernel type).

Run: `cargo check --workspace --all-targets 2>&1 | tail -3 && cargo test -p hornvale-language && cargo test -p hornvale-worldgen 2>&1 | tail -3`
Expected: clean + PASS.

- [ ] **Step 5: Committed-artifact verdict.** The census CSVs are pure committed data this task must not touch:

```bash
git status --porcelain book/src/laboratory/generated/ | head
```
Branch: empty → proceed. Anything listed → STOP, report (nothing in this task regenerates a census; drift here means something unexpected ran).

- [ ] **Step 6: report, fmt, commit** (kernel-layer edit — slow gate expected):

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add kernel domains/religion domains/language windows/worldgen docs/audits/type-audit-report.md
git commit -m "refactor(kernel): Sentiment lives beside Phenomenon (0517 clause b); LineSentiment and its shim deleted"
```

---

### Task 7: Horizon — kernel roster; climate's Stratum embeds it

**Files:**
- Create: `kernel/src/horizon.rs`
- Modify: `kernel/src/lib.rs` (`pub mod horizon;` + `pub use horizon::Horizon;`)
- Modify: `domains/terrain/src/strata.rs` (delete the enum; re-export)
- Modify: `domains/climate/src/facets.rs` (`Stratum`'s five rock variants → `Rock(Horizon)`), `domains/climate/src/variants.rs`, `domains/climate/src/biome.rs`, `domains/climate/src/provider.rs` (match-arm follow-through)
- Modify: `domains/climate/tests/suite/facets.rs` (the correspondence test — now structural)
- Modify: `windows/worldgen/src/chamber.rs` (delete `stratum_of_band`; call sites construct `Stratum::Rock(…)`)
- Modify: `book/src/reference/lexicon-of-place.md` (the roster table rows for `Stratum`/`Horizon` — hand-written prose, updated to name the kernel as the roster's home)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: nothing new.
- Produces: `hornvale_kernel::Horizon` — the five rock bands, verbatim variants `Regolith/Cover/Basement/Roots/Underneath`, now deriving `Ord` (shallow → deep, the `Band` precedent: "further down" becomes a comparison). Terrain re-exports it from `strata.rs` (`pub use hornvale_kernel::Horizon;`), so `hornvale_terrain::Horizon` paths (worldgen tests, `features.rs`, the doc-test differential pair) compile unchanged. Climate's `Stratum` becomes `{ Surface, Epipelagic, Mesopelagic, Bathypelagic, Abyssal, Hadal, Rock(Horizon) }` — the mirror is now structural, and `stratum_of_band` has nothing left to do.

**Two hazards, named:**
1. `domains/terrain/src/features.rs:59-118` holds a doc-test **differential pair** (`compile_fail` blocks) that `domains/terrain/tests/cave_construction_proof.rs` re-reads and asserts differ on exactly one line. The blocks mention `Horizon::Regolith`. The re-export keeps the path valid — do not reword those blocks.
2. `Stratum` does not derive `Serialize` and is not committed anywhere (verified in the dossier); the shape change is pure code. `domains/terrain/src/provider.rs:992-996`'s name table (`"Regolith"…"Underneath"`) is a readout spelling contract — it moves nowhere and must not be edited.

- [ ] **Step 1: The kernel module.** Create `kernel/src/horizon.rs` on the `band.rs` model:

```rust
//! The stratigraphic rock ladder, shared by every domain that names it
//! (decision 0517 clause (a)). Before this module the five bands carried
//! two names: `Horizon` in `domains/terrain` (which derives them from a
//! column) and a mirrored rock half inside `domains/climate::Stratum`,
//! bridged by `windows/worldgen`'s `stratum_of_band` identity match.
//! Climate does not compute a horizon — it cannot import terrain (decision
//! 0002) — so the mirror was a forced duplicate, the same filing error
//! `kernel/src/band.rs` corrected for the delve ladder.
//!
//! **This module holds the roster and the ordering, and nothing else.**
//! Which band a depth falls in (`band_at_depth`), the column that stamps
//! them, and every readout spelling stay in `hornvale_terrain`.

/// A named band of the rock column, top → bottom; resolution coarsens
/// downward. Ordered shallow → deep, so a **greater** horizon is a
/// **deeper** one — the derived `Ord` replaces the ad-hoc ordinal tables
/// callers kept for themselves.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Horizon {
    /// The living skin: soil / weathered regolith.
    Regolith,
    /// Deposited / volcanic surface rock — the legible archive.
    Cover,
    /// Crystalline craton (terrain's inherited `Basement`).
    Basement,
    /// Deep crust: hot, high-pressure.
    Roots,
    /// The primordial substrate / threshold to the not-here.
    Underneath,
}

/// Every horizon, shallowest to deepest.
const ALL: [Horizon; 5] = [
    Horizon::Regolith,
    Horizon::Cover,
    Horizon::Basement,
    Horizon::Roots,
    Horizon::Underneath,
];

impl Horizon {
    /// Every horizon of the ladder in order, shallowest first.
    pub fn all() -> &'static [Horizon] {
        &ALL
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The derived Ord runs shallow → deep, so depth comparisons read the
    /// way the rock does.
    #[test]
    fn the_ladder_is_ordered_shallow_first() {
        let mut sorted = ALL;
        sorted.sort();
        assert_eq!(sorted, ALL);
        assert!(Horizon::Regolith < Horizon::Underneath);
    }
}
```

Wire into `kernel/src/lib.rs` (module block alphabetical; `pub use horizon::Horizon;`).

Run: `cargo test -p hornvale-kernel horizon`
Expected: PASS

- [ ] **Step 2: Terrain re-exports.** In `domains/terrain/src/strata.rs`, delete the `Horizon` enum (~89-102) and add `pub use hornvale_kernel::Horizon;`. Nothing else in terrain changes — `column()`, `band_at_depth`, `deposit_depth`, the `cave_depth.rs` ordinal table, the readout name table, and the differential pair all compile against the re-export.

Run: `cargo test -p hornvale-terrain 2>&1 | tail -3 && cargo test -p hornvale-terrain --test cave_construction_proof 2>&1 | tail -3`
Expected: PASS both (the differential-pair harness is the one most likely to object if anything in `features.rs` was touched — it must not have been).

- [ ] **Step 3: Climate embeds.** In `domains/climate/src/facets.rs`: add `use hornvale_kernel::Horizon;`; replace the five rock variants of `Stratum` (`Regolith`, `Cover`, `Basement`, `Roots`, `Underneath`, with their long mirror doc comments) with one:

```rust
    /// A band of the rock column (the kernel's shared roster — decision
    /// 0517; terrain derives which band a depth falls in). A rock depth
    /// *register*, explicitly not something a chamber moves between (The
    /// Stratum §3). Measured note kept from the mirror era: `Underneath`
    /// was empty in 55,947 caves (Task 0) but stays representable — rule 1a
    /// makes `ChamberAddr.band` index the delve ladder, and the open
    /// `MAP-cave-depth-weld` fix may make it occur.
    Rock(Horizon),
```

Follow the compiler through climate: `Realm::strata()`'s rock list becomes `&ROCK_STRATA` where `const ROCK_STRATA: [Stratum; 5]` maps `Horizon::all()`'s order (a `const` array of `Stratum::Rock(Horizon::Regolith)` … in ladder order — write it out; `Horizon::all()` is not const-callable in an array literal); the `unreachable!` arms in `BiomeExpr::biome()` (facets ~301-305) and `variants.rs` (~656-727) collapse to `Stratum::Rock(_) => unreachable!(…)` with the same message; `for_legacy`'s arms and any `Stratum::Regolith`-style site become `Stratum::Rock(Horizon::Regolith)`. In `domains/climate/tests/suite/facets.rs`, the test asserting `"Stratum's rock bands must mirror Horizon's names, in order"` is now true by construction — replace it with one asserting `Realm`'s rock strata list is `Horizon::all()` wrapped in `Rock`, in order (the ordering claim survives; the spelling claim is structural).

Run: `cargo test -p hornvale-climate 2>&1 | tail -3`
Expected: PASS

- [ ] **Step 4: Delete the shim.** In `windows/worldgen/src/chamber.rs`: delete `stratum_of_band` (~426-439). Call sites (`:1411`, `:1908`, `:1923`, `:1928`): `stratum_of_band(stratum_at(…))` → `hornvale_climate::Stratum::Rock(stratum_at(…))`.

Run: `cargo check --workspace --all-targets 2>&1 | tail -3 && cargo test -p hornvale-worldgen 2>&1 | tail -3`
Expected: clean + PASS.

- [ ] **Step 5: Update the book's roster table.** In `book/src/reference/lexicon-of-place.md` (~lines 74-92), the `Stratum` row's variant list and the prose about the mirrored roster change to name the kernel as the shared roster's home (`Rock(Horizon)`; `Horizon` now `hornvale_kernel`, re-exported by terrain). Hand-written prose — edit directly, keep the table shape.

- [ ] **Step 6: report, fmt, commit** (kernel-layer edit):

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add kernel domains/terrain domains/climate windows/worldgen book/src/reference/lexicon-of-place.md docs/audits/type-audit-report.md
git commit -m "refactor(kernel): Horizon is the shared rock roster; Stratum embeds it, stratum_of_band deleted (0517 clause a)"
```

---

### Task 8: CaveKind — kernel roster; Formation adjudicated by branch

**Files:**
- Create: `kernel/src/cave.rs`
- Modify: `kernel/src/lib.rs` (`pub mod cave;` + `pub use cave::CaveKind;`)
- Modify: `domains/terrain/src/features.rs` (delete the enum + impl; re-export)
- Possibly modify: `domains/climate/src/facets.rs` (`Formation`'s three cave variants — see the branch)
- Modify: `cli/tests/suite/cave_kind_correspondence.rs` (per branch outcome)
- Modify: `book/src/reference/lexicon-of-place.md` (if the Formation branch changes its rows)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: nothing new.
- Produces: `hornvale_kernel::CaveKind` with variants `Karst/LavaTube/Fracture`, `LEGEND: [&'static str; 3] = ["karst", "lava-tube", "fracture"]`, and `name()` — all moved verbatim (`name()`'s strings are the scene-emission spelling contract; `windows/locale/src/lib.rs:348-351` serializes by it). Terrain re-exports (`pub use hornvale_kernel::CaveKind;` in `features.rs`), so `hornvale_terrain::CaveKind` (worldgen, vessel, locale's own re-export at `lib.rs:37`) compiles unchanged. `genus_of` in `windows/worldgen/src/delve_seating.rs` **stays** — the corpus genera (`"karst-cave"` etc.) are genuinely different words from `name()`'s legend, and that mapping is climate-corpus meaning, which the criterion leaves domain-side.

- [ ] **Step 1: The kernel module.** Create `kernel/src/cave.rs`:

```rust
//! The cave-kind roster, shared by every domain and window that names a
//! void by the lithologic process that opened it (decision 0517 clause
//! (a)). Terrain derives which kind a site carries and how deep it
//! reaches; climate's corpus names communities *of* these formations under
//! its own genus spellings; this module holds only the roster and the
//! scene-emission legend.

/// A cave type, by the lithologic process that opened the void.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaveKind {
    /// Carbonate dissolution (wet limestone).
    Karst,
    /// Drained basaltic/volcanic tube.
    LavaTube,
    /// Fault/fracture void in tectonically active rock.
    Fracture,
}

impl CaveKind {
    /// The three cave-kind names in stable order — the self-describing
    /// legend for scene emission (mirrors `WaterKind::LEGEND`).
    pub const LEGEND: [&'static str; 3] = ["karst", "lava-tube", "fracture"];

    /// Stable name, for scene emission.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(self) -> &'static str {
        match self {
            CaveKind::Karst => "karst",
            CaveKind::LavaTube => "lava-tube",
            CaveKind::Fracture => "fracture",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// LEGEND and name() are the same spelling contract stated twice; they
    /// must agree in order and content.
    #[test]
    fn legend_and_name_agree() {
        let named = [
            CaveKind::Karst.name(),
            CaveKind::LavaTube.name(),
            CaveKind::Fracture.name(),
        ];
        assert_eq!(named, CaveKind::LEGEND);
    }
}
```

Wire into `kernel/src/lib.rs`. Run: `cargo test -p hornvale-kernel legend_and_name_agree` — PASS.

- [ ] **Step 2: Terrain re-exports.** In `domains/terrain/src/features.rs`, delete the `CaveKind` enum and its `impl` block (~12-37) and add `pub use hornvale_kernel::CaveKind;`. `Cave`, `from_reach`, `cave_process`, `render.rs`'s color table, `cave_depth.rs` — all compile against the re-export.

Run: `cargo test -p hornvale-terrain 2>&1 | tail -3 && cargo test -p hornvale-terrain --test cave_construction_proof 2>&1 | tail -3`
Expected: PASS both.

- [ ] **Step 3: The Formation branch.** `Formation`'s `KarstCave`/`LavaTube`/`FractureCave` variants mirror `CaveKind` per their 0094 doc comments, but their corpus spellings differ (`"karst-cave"` vs `"karst"`) and `Formation` is a 21-variant vocabulary whose names reach the committed audit corpus. Decide by evidence, not preference:

Run: `grep -rn 'KarstCave\|FractureCave' domains/climate/src/ | wc -l` and read `domains/climate/src/axes.rs` + `underworld.rs` for where Formation names become corpus strings.

  - **If** the three variants are matched only as enum values and their corpus strings are produced by an explicit spelling table (so an embed `Formation::Cave(CaveKind)` can keep every emitted string identical): perform the embed, update the spelling table to match on `Cave(CaveKind::Karst) => "karst-cave"` etc., update `cli/tests/suite/cave_kind_correspondence.rs` to the structural form, and verify with the Task's Step 4 check.
  - **Otherwise** (names derived from variant identifiers, or the blast radius reaches committed corpus files): keep `Formation`'s three variants as they are, update their doc comments to cite the kernel type (`Mirrors hornvale_kernel::CaveKind::Karst — kept as climate's own corpus vocabulary; the spellings genuinely differ (see windows/worldgen's genus_of), so this is a deliberate projection under 0517 clause (a), not a forced duplicate`), keep `cave_kind_correspondence.rs` exactly as the exhaustive-match guard it already is, and ledger the adjudication.

Either branch is a valid completion; what is not valid is an embed that moves a committed corpus byte.

- [ ] **Step 4: Committed-artifact verdict** (same as Task 6 Step 5):

```bash
git status --porcelain book/src/laboratory/generated/ docs/audits/system-coverage-wolverson-2021.md | head
```
Branch: empty → proceed; anything listed → STOP, revert the Formation embed, take the other branch.

- [ ] **Step 5: workspace check, report, fmt, commit:**

```bash
cargo check --workspace --all-targets 2>&1 | tail -3
cargo test -p hornvale-worldgen 2>&1 | tail -3 && cargo test -p hornvale-vessel 2>&1 | tail -3
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add -A kernel domains/terrain domains/climate cli/tests windows book/src/reference docs/audits docs/superpowers/ledgers
git commit -m "refactor(kernel): CaveKind is the shared cave roster (0517 clause a); Formation adjudicated per branch"
```

---

### Task 9: tools/placement-audit — crate, walker, detector

**Files:**
- Create: `tools/placement-audit/Cargo.toml`, `src/main.rs`, `src/lib.rs`, `src/args.rs`, `src/walk.rs`, `src/extract.rs`, `src/detect.rs`
- Create: `tools/placement-audit/tests/fixtures/twins/domain_a.rs`, `tools/placement-audit/tests/fixtures/twins/domain_b.rs`, `tools/placement-audit/tests/fixtures/twins/unrelated.rs`, `tools/placement-audit/tests/detector.rs`
- Modify: root `Cargo.toml` (`exclude` list gains `"tools/placement-audit"`)

**Interfaces:**
- Consumes: nothing from the workspace (standalone crate).
- Produces, for Tasks 10-11: `walk::scan(roots: &[PathBuf]) -> Result<Vec<CrateTypes>, String>` where `CrateTypes { crate_name: String, types: Vec<TypeShape> }` and `TypeShape { name: String, kind: ShapeKind /* Enum | Struct */, members: Vec<String> /* sorted */, doc: String, file: PathBuf, line: usize }`; `detect::twins(&[CrateTypes]) -> Vec<TwinGroup>` where `TwinGroup { members: Vec<TypeShape> /* ≥2, different crates, same kind + same sorted member set */ }`. Exit-code convention 0/1/2 (green / findings / usage-or-scan error).

- [ ] **Step 1: Cargo.toml** — copy type-audit's shape including the load-bearing empty `[workspace]` table (without it, a build from this worktree binds to the outer workspace):

```toml
[package]
name = "placement-audit"
version = "0.1.0"
edition = "2024"
license = "MIT"
publish = false

# Empty table: deliberately outside the main workspace, and it stops cargo's
# ancestor search unconditionally — required for builds from a worktree under
# .claude/worktrees/ (same rationale as tools/type-audit's).
[workspace]

[lib]
name = "placement_audit"
path = "src/lib.rs"

[[bin]]
name = "placement-audit"
path = "src/main.rs"

[dependencies]
syn = { version = "2", features = ["full", "visit", "extra-traits"] }
# span-locations makes Span::start().line return real line numbers when
# parsing files; without it diagnostics report :0:.
proc-macro2 = { version = "1", features = ["span-locations"] }
```

Add `"tools/placement-audit"` to the root `Cargo.toml`'s `exclude` list (after `"tools/type-audit"`), matching the crate it is modeled on rather than the two accidental omissions (`tools/board`, `tools/seam-guard`).

- [ ] **Step 2: main.rs and args.rs** — type-audit's shape verbatim, two subcommands:

`src/main.rs`:
```rust
//! Binary entry point; delegates to [`placement_audit::run`].

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    std::process::exit(placement_audit::run(&args));
}
```

`src/args.rs` — `Command::Check { paths: Vec<PathBuf> }` and `Command::Report`, parsed exactly like type-audit's `parse_args` (hand-rolled, std-only, unknown command → `Err`), with the same unit test shape (`parses_check_report_and_rejects_unknown`).

- [ ] **Step 3: walk.rs** — type-audit's walker with two deliberate differences:

```rust
/// The roots scanned when `check` is given no explicit paths. Kernel and
/// domains only: a window may import a domain, so a window-side duplicate
/// is an ordinary refactor, never a forced one (spec §3, The Hallmark).
pub const SCAN_ROOTS: &[&str] = &["kernel", "domains"];
```

Copy `collect_rs_files` **including** the explicit-file-root-is-trusted asymmetry (an explicitly-named file is pushed even under a `tests/` path; a directory sweep prunes `tests`/`examples`/`benches`) — that asymmetry is what lets this tool's own fixtures live under `tests/fixtures/` — and `crate_name_of` (domains/<name> → name; kernel → "kernel"; keep the "cli" fallback arm for explicitly-passed fixture paths, naming it `"other"` instead).

- [ ] **Step 4: extract.rs** — hand-rolled recursion over `&[syn::Item]` (type-audit's pattern, not `Visit`): collect every `syn::Item::Enum` and `syn::Item::Struct` with bare-`pub` visibility (copy `is_bare_pub` and `doc_text` from type-audit verbatim; recurse into non-`#[cfg(test)]` inline modules via `has_cfg_test`). For an enum, `members` = sorted variant identifier strings; for a struct, sorted named-field identifier strings (skip tuple/unit structs — a shape with no named members matches too easily to mean anything). Record `name`, `doc`, file, `ident.span().start().line`.

- [ ] **Step 5: detect.rs** — group all collected `TypeShape`s by `(kind, members)` key; a group whose members span ≥2 distinct `crate_name`s is a `TwinGroup`. Sort groups and members deterministically (by first member's crate then type name).

- [ ] **Step 6: The fixture pair and the detector test.** `tests/fixtures/twins/domain_a.rs`:

```rust
/// A three-valued mood.
pub enum Mood {
    /// Up.
    Bright,
    /// Flat.
    Level,
    /// Down.
    Dim,
}
```

`tests/fixtures/twins/domain_b.rs` — same three variants under a different type name (name equality is not the match key):

```rust
/// The same three-valued mood under another name.
pub enum Temper {
    /// Up.
    Bright,
    /// Flat.
    Level,
    /// Down.
    Dim,
}
```

`tests/fixtures/twins/unrelated.rs` — an enum sharing two of three variant names (must NOT match):

```rust
/// A near-miss: overlapping but not identical member set.
pub enum Slope {
    /// Up.
    Bright,
    /// Flat.
    Level,
    /// Sideways.
    Askew,
}
```

`tests/detector.rs` (fixtures resolved via `env!("CARGO_MANIFEST_DIR")`, never cwd):

```rust
//! Integration: the detector finds an exact member-set twin across two
//! files and refuses the near-miss.

use std::path::PathBuf;
use placement_audit::{detect, walk};

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/twins").join(name)
}

#[test]
fn exact_member_sets_twin_and_near_misses_do_not() {
    let crates = walk::scan(&[
        fixture("domain_a.rs"),
        fixture("domain_b.rs"),
        fixture("unrelated.rs"),
    ])
    .unwrap();
    let twins = detect::twins(&crates);
    assert_eq!(twins.len(), 1, "exactly one twin group: {twins:?}");
    let names: Vec<&str> = twins[0].members.iter().map(|t| t.name.as_str()).collect();
    assert_eq!(names, ["Mood", "Temper"]);
}
```

Wait — the three fixture files resolve to the same `crate_name` (`"other"`), and a twin requires ≥2 distinct crates. Place the fixtures under `tests/fixtures/twins/domains/a/src/lib.rs` and `tests/fixtures/twins/domains/b/src/lib.rs` and `tests/fixtures/twins/domains/c/src/lib.rs` instead, so `crate_name_of` derives crates `a`, `b`, `c` from the `domains/<name>` anchor, and pass those paths. Keep the file contents as above (Mood in `a`, Temper in `b`, Slope in `c`). This is the step's real content — the detector's cross-crate requirement must be exercised, not bypassed.

- [ ] **Step 7: Run red → implement → green:**

Run: `cargo test --manifest-path tools/placement-audit/Cargo.toml`
Expected: FAIL first (unimplemented), then PASS once Steps 3-5 are filled in. Also run the unit tests in `args.rs`/`walk.rs`/`extract.rs`/`detect.rs` (each module carries a `#[cfg(test)] mod tests`, house style).

- [ ] **Step 8: fmt + commit:**

```bash
cargo fmt --manifest-path tools/placement-audit/Cargo.toml
git add tools/placement-audit Cargo.toml
git commit -m "feat(placement-audit): crate scaffold, source walker, shape-twin detector"
```

---

### Task 10: placement-audit — tag grammar, fingerprints, verdicts

**Files:**
- Create: `tools/placement-audit/src/tag.rs`, `src/fingerprint.rs`, `src/verdict.rs`
- Create: `tools/placement-audit/tests/verdicts.rs` + fixture files under `tests/fixtures/verdicts/domains/{a,b}/src/lib.rs`
- Modify: `tools/placement-audit/src/lib.rs` (module wiring; `run`'s `check` arm)

**Interfaces:**
- Consumes: Task 9's `TypeShape` / `TwinGroup`.
- Produces: `tag::parse(doc: &str) -> Result<Option<PlacementTag>, TagError>` where `PlacementTag { verdict: TagVerdict /* Promote(String) | Deliberate(String) | Deferred(String) */, shape: Option<String> }`; `fingerprint::of(members: &[String]) -> String` (6 lowercase hex chars); `verdict::judge(&[TwinGroup]) -> Vec<Finding>` with finding kinds `UNTAGGED` / `STALE` / (tag-parse error). Tagged-and-current twins produce no finding. Exit 1 when findings exist.

**The tag grammar** (seam-guard's model: one paragraph ending at the first blank `///` line; reasonless is a parse error; duplicate clause is an error):

```
/// placement: deferred(<reason>) shape(a1b2c3)
/// placement: deliberate(<reason>) shape(a1b2c3)
/// placement: promote(<decision or spec anchor>) shape(a1b2c3)
```

- [ ] **Step 1: fingerprint.rs.** The fingerprint must be stable across Rust releases and platforms, so it is a hand-rolled FNV-1a 64 (std's `DefaultHasher` is explicitly unstable across releases — never use it for a committed spelling):

```rust
//! The shape fingerprint: FNV-1a 64 over the sorted member names joined
//! with '\n', rendered as the first 6 lowercase hex digits. Hand-rolled
//! because the fingerprint is committed inside `placement:` tags — std's
//! DefaultHasher is documented as unstable across releases and cannot back
//! a committed spelling.

/// Fingerprint a sorted member-name list.
pub fn of(members: &[String]) -> String {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    let mut feed = |byte: u8| {
        h ^= u64::from(byte);
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    };
    for (i, m) in members.iter().enumerate() {
        if i > 0 {
            feed(b'\n');
        }
        for b in m.bytes() {
            feed(b);
        }
    }
    format!("{h:016x}")[..6].to_string()
}
```

Unit test: two different member lists get different fingerprints; the same list twice gets the same one; the function is order-insensitive **only because callers pass sorted members** — assert `of(&[a,b]) == of(&[a,b])` and `of(&[a,b]) != of(&[a,c])`.

- [ ] **Step 2: tag.rs.** Seam-guard's parser shape: find `placement:` in the doc text, cut the body at the first `\n\n` (the one-paragraph rule — `doc_text` joins doc lines with `\n`, so a blank `///` line is `\n\n`), lex `name(arg)` clauses with nesting-aware paren matching, then:
  - exactly one of `promote`/`deliberate`/`deferred` (zero → `MissingVerdict`; two → `DuplicateClause`);
  - the argument is the reason/anchor; empty → `EmptyReason` (the reasonless-is-an-error rule, with seam-guard's rationale in the error's doc comment: an unexplained acknowledgement silences the finding while recording nothing about why);
  - optional `shape(<6 hex>)`; malformed hex → `BadShape`.
  Unit tests: the three seam-guard paragraph tests adapted (prose after a blank line cannot overwrite the tag; a wrapped paragraph still parses; a reasonless verdict is an error), plus duplicate-verdict rejection.

- [ ] **Step 3: verdict.rs.** For each `TwinGroup`, for each member `TypeShape`:
  - no `placement:` tag → `Finding { kind: Untagged, … }` naming the twin's other members (crate:type@file:line) so the message is actionable;
  - tag parse error → a finding carrying the error message;
  - tag present, `shape` missing → `Stale` with message "tag has no shape(…) — add shape(<fp>)" (printing the computed fingerprint);
  - tag present, `shape` ≠ `fingerprint::of(&member.members)` → `Stale` ("shape moved: tag says X, type is Y — re-adjudicate and re-fingerprint");
  - tag present and current → no finding.
  Findings sort deterministically (crate, file, line).

- [ ] **Step 4: Wire `run`'s check arm** (type-audit's shape: print findings one per line `crate:line: message (Type)`, `eprintln!` a count, exit 1 on any; scan error → exit 2).

- [ ] **Step 5: The bidirectional integration test.** Fixtures under `tests/fixtures/verdicts/domains/a/src/lib.rs` (a twin tagged current — compute its fingerprint by hand for the fixture's member set and write it into the fixture), `…/domains/b/src/lib.rs` (the twin's other side: one type tagged with a WRONG fingerprint, plus a second untagged twin pair member). `tests/verdicts.rs` asserts: the current tag yields no finding; the wrong fingerprint yields exactly one `Stale`; the untagged member yields exactly one `Untagged`; **and** — the mutation-proved rule — assert the fixture text actually contains the tag being tested before asserting on the verdict (`assert!(src.contains("placement: deliberate"), "FIXTURE TARGET NOT FOUND")`), so a fixture edit cannot silently vacate the test.

Run: `cargo test --manifest-path tools/placement-audit/Cargo.toml`
Expected: PASS.

- [ ] **Step 6: fmt + commit:**

```bash
cargo fmt --manifest-path tools/placement-audit/Cargo.toml
git add tools/placement-audit
git commit -m "feat(placement-audit): placement tags, FNV shape fingerprints, three-valued verdicts"
```

---

### Task 11: placement-audit — report artifact, gate wiring, baseline

**Files:**
- Create: `tools/placement-audit/src/report.rs`, `tools/placement-audit/tests/report_determinism.rs`
- Create: `docs/audits/placement-audit-roster.md` (generated, committed)
- Modify: `Makefile` (two new targets; `.PHONY` line 36; `quick-run` AND `style-run` prerequisite lists; `prewarm` build line)
- Modify: `scripts/hooks/pre-commit` (Rust-relevant staged-path filter gains `tools/placement-audit/`)
- Modify: `scripts/lane-outboard.sh` (run the tool's own test suite in the `outboard` set)
- Modify: `scripts/regenerate-artifacts.sh` (Group C report line, literal path)
- Modify: `docs/generated-paths.txt` (the report declared **by file name**)
- Modify: `docs/timings.md` is NOT hand-edited — `scripts/timed.sh` ledgers it

**Interfaces:**
- Consumes: Tasks 9-10's `scan`/`twins`/`judge`/`render`.
- Produces: the committed roster artifact and the live gate check. After this task, an untagged new shape twin in kernel/domains fails every commit.

- [ ] **Step 1: report.rs** — type-audit's report style: header `# Placement Audit Roster` + the house preamble `_Generated by \`tools/placement-audit\`. Do not edit by hand; regenerate with\n\`cargo run --manifest-path tools/placement-audit/Cargo.toml -- report\`._`, then one section per twin group (members as a table: crate, type, file:line, verdict, shape), all ordering from sorted collections, **no timestamps**. `tests/report_determinism.rs` pins byte-identical double render and `assert!(!a.contains("2026"))`, exactly like type-audit's.

- [ ] **Step 2: Baseline run.** Build and run the check against the real tree:

```bash
cargo run --manifest-path tools/placement-audit/Cargo.toml -- check
```

Branch on the finding list: after Tasks 2-8 the six §1 twins are gone, so the expected shape is *zero or few* findings. Any twin it does report gets adjudicated NOW: read both sides, decide `deliberate(<why>)` or `deferred(<why>)` per the 0216 forced-vs-deliberate test, add the tag with the fingerprint the tool printed, and ledger each adjudication. Do not tag anything `promote` at baseline — a promote-worthy finding at this stage means a Batch task missed something: STOP and report instead.

- [ ] **Step 3: Measure, then wire per the spec's decision rule.**

```bash
time cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- check
time cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- check
```

(Second run is the warm number.) The comparator, measured 2026-09-01 on this Mac: type-audit's warm check is **~6.3-6.4 s** (the Makefile comment's "1.2s" is stale — do not use it). Rule: warm check ≤ ~12.8 s (2× type-audit) → wire into the commit gate (this step's remaining bullets). Over → wire the check into `scripts/lane-outboard.sh` only (stage rung) and skip the `quick-run`/`style-run` edits; ledger the measured number either way. Expected side of the branch: this tool scans `kernel` + `domains` only — a strict subset of type-audit's four roots — but the expectation decides nothing; the measurement does.

Gate wiring (the commit-gate branch), modeled on the type-audit lines at Makefile ~486-511:

```make
# In the gate for the same reason type-audit is: a lint, not an artifact —
# default-deny on undeclared shape twins across kernel/domains (decision
# 0517; The Hallmark spec §3).
placement-audit: ## Verify shape-twin placement tags (default-deny; decision 0517)
	cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- check

placement-audit-report: ## Fail if the committed placement roster is stale (regen cmd in the message)
	@tmp="$$(mktemp /tmp/hv-placement-audit-report.XXXXXX)"; \
	trap 'rm -f "$$tmp"' EXIT; \
	cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- report > "$$tmp"; \
	if ! diff -q "$$tmp" docs/audits/placement-audit-roster.md >/dev/null 2>&1; then \
		echo "placement-audit-report: docs/audits/placement-audit-roster.md is stale. Regenerate it with:" >&2; \
		echo "  cargo run --manifest-path tools/placement-audit/Cargo.toml -- report > docs/audits/placement-audit-roster.md" >&2; \
		exit 1; \
	fi
```

Then: add both names to `.PHONY` (line ~36); append both to **both** `quick-run:` and `style-run:` prerequisite lists (they are the same four names today; they must stay in lockstep); add `cargo build --manifest-path tools/placement-audit/Cargo.toml` beside type-audit's in the `prewarm` target (~line 584).

- [ ] **Step 4: The three wiring points outside the Makefile** (each is a recorded past failure mode):
  - `scripts/hooks/pre-commit`: add `tools/placement-audit/` to the Rust-relevant staged-path filter beside `tools/type-audit/` — without it, edits to the tool itself skip `make quick` entirely.
  - `scripts/lane-outboard.sh`: add `run "tools/placement-audit" cargo test --manifest-path tools/placement-audit/Cargo.toml` beside type-audit's line — the `outboard` set is the only thing that runs a tool crate's own suite (the seam-guard 31-hidden-tests lesson).
  - `scripts/regenerate-artifacts.sh`: in Group C, directly below the seam-guard roster line (the closest structural precedent), add — with the output path **literal**, never a variable (`every_declared_generated_path_is_written_by_its_author` greps the source for the path string):

```bash
spawn run --manifest-path tools/placement-audit/Cargo.toml -- report > docs/audits/placement-audit-roster.md
```

- [ ] **Step 5: Generate, declare, and add the artifact in one commit.** Generate the roster (`cargo run --manifest-path tools/placement-audit/Cargo.toml -- report > docs/audits/placement-audit-roster.md`). In `docs/generated-paths.txt`, add beside the other individually-declared `docs/audits/` files (tab-separated, and the by-name declaration is the point — a new file inside the already-declared `docs/audits/` directory is invisible to `git diff` until it has an index entry):

```
docs/audits/placement-audit-roster.md	artifacts
```

`git add` the roster file in this same commit — `cli/tests/suite/generated_paths.rs` refuses an untracked declared path, which is exactly the refusal this ordering exists to trigger if forgotten.

- [ ] **Step 6: Verify the whole gate end to end, both directions.**

Run: `make quick`
Expected: green, including both new targets.

Then the red direction (the ratchet must be provably non-vacuous): add a scratch pub enum to `kernel/src/color.rs` duplicating an existing domain enum's member set — the cheapest is a copy of the verdicts fixture pair: instead, add to a scratch file `domains/alchemy/src/lib.rs`… **do not touch real crates.** The red direction is already pinned by `tests/verdicts.rs` (Task 10) against fixtures; for the wiring itself, verify the exit code propagates: `cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- check tools/placement-audit/tests/fixtures/verdicts; echo "exit: $?"` — Expected: findings printed, `exit: 1`. That proves the binary the Makefile calls fails when findings exist; `make placement-audit`'s recipe is a bare cargo-run, so a non-zero exit fails the target by make's own semantics.

Run the enforcement suite that watches these files: `cargo test -p hornvale --test suite -- generated_paths lane_sets 2>&1 | tail -3`
Expected: PASS (no roster row was added, so the chamber phase lists are untouched; the new generated-path row is tracked).

- [ ] **Step 7: fmt + commit:**

```bash
cargo fmt --manifest-path tools/placement-audit/Cargo.toml
git add tools/placement-audit Makefile scripts/hooks/pre-commit scripts/lane-outboard.sh \
        scripts/regenerate-artifacts.sh docs/generated-paths.txt docs/audits/placement-audit-roster.md \
        docs/superpowers/ledgers/2026-09-01-the-hallmark.md
git commit -m "feat(placement-audit): committed roster + commit-gate wiring — the placement ratchet is live"
```

---

### Task 12: capture close-out (docs only)

**Files:**
- Modify: `book/src/frontier/idea-registry.md` (re-score `DOM-kernel-owns-vocabulary`; add Batch C rows)
- Modify: `docs/superpowers/ledgers/2026-09-01-the-hallmark.md` (final entries)

**Interfaces:**
- Consumes: outcomes of Tasks 5 and 8 (which branches were taken).
- Produces: the registry state the spec's §5 promises. (The chronicle entry, book freshness sweep, and retrospective are campaign-close work under the `closing-a-campaign` skill — not this task.)

- [ ] **Step 1: Re-score `DOM-kernel-owns-vocabulary`** (idea-registry ~line 1776): status `raw` → `ratified`, refs gain decision 0517 and this campaign's spec. Do not rewrite the row's text — append to its refs and flip the status, matching how `PROC-kernel-units` shows its 0044 ratification.

- [ ] **Step 2: Add the Batch C rows**, one per deferred item, status `raw`, each citing The Hallmark spec §5 and the survey evidence: (a) the two parallel environment-axis bases (kernel `environment_v1_basis` vs the hard-coded temperature/moisture/insolation/elevation quartet in `species::ConditionNiche` + worldgen's `Substrate`); (b) the manikin psychology vectors (`species::MindVector`/`SocietyVector` shadowed by `culture::PsychSummary` scalars); (c) `ObjectProperty`'s altitude (a window-owned vocabulary hand-mirrored by two domains); (d) the biome-name roster (`species::BiomeAffinity` string-keys against `climate::Biome::name()`). Use the registry's existing row format and ID conventions (check neighboring `DOM-*` rows; duplicate IDs redden `cli/tests/docs_consistency.rs`).

- [ ] **Step 3: If Task 5 or Task 8 took a STOP/fallback branch**, ensure each has its registry row (Task 5's conflation row; Task 8's Formation adjudication is ledger-only unless it descoped).

- [ ] **Step 4: Verify the registry checks pass:**

Run: `cargo test -p hornvale --test suite -- docs_consistency 2>&1 | tail -3`
Expected: PASS.

- [ ] **Step 5: Commit** (docs-only):

```bash
git add book/src/frontier/idea-registry.md docs/superpowers/ledgers/2026-09-01-the-hallmark.md
git commit -m "docs(the-hallmark): registry capture — DOM-kernel-owns-vocabulary ratified, Batch C rows recorded"
```

---

## Plan self-review record

- **Spec coverage:** §2 → Task 1; §3 → Tasks 9-11; §4 Batch A → Tasks 2-5; §4 Batch B → Tasks 6-8; §5 → Task 12 (+ per-task ledger notes); §6.1 → Task 1, §6.2 → Tasks 10-11, §6.3 → Tasks 2-8, §6.4 → Task 11 Step 5, §6.5 → campaign close (out of plan scope, per Task 12's note).
- **Known deviations from the survey, deliberate:** `genus_of` is kept (spec's "Formation cave half" gets a decision-rule branch in Task 8 because the corpus spellings genuinely differ); the paleoclimate migration is diagnosis-gated (the day/year conflation found at plan time); the tool lands after the promotions so the baseline is clean (ledger #5).
- **Type consistency:** `GenesisOutcome<T>.value` (Tasks 3), `Sentiment`/`sentiment_tag` (Task 6), `Stratum::Rock(Horizon)` (Task 7), `TypeShape`/`TwinGroup`/`PlacementTag` (Tasks 9-11) — names match across their producing and consuming tasks.

---

## Addendum (2026-09-02): Nathan's G6 rulings — Tasks 13-15

Nathan overrode two execution-time adjudications at the merge stop (ledger
#14): the `EraClimate.day` two-axes defect is fixed in this campaign and the
`WorldTime` migration then proceeds; `Formation`'s cave half is structurally
unified with the kernel's `CaveKind`. Committed spellings and bytes remain
inviolate: frozen corpora keep `"karst-cave"`-style strings, and any
committed-byte movement is escalated to Nathan as a diff, never landed
silently.

### Task 13: one axis in the slot — the bake path stops writing years into EraClimate.day

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (bake-path `EraClimate` construction, ~:3876, :3921-3924; possibly the second `era_day` region :3909-3913)
- Modify: `windows/worldgen/src/history_bake.rs` (`era_index_for` ~:1651 and its callers; the `.min_by` at ~:4288; test fixtures)
- Possibly create: a named, UNQUANTIZED year-to-day crossing beside `ledger_day_of_bake_year` (see constraints)
- Modify: `docs/superpowers/ledgers/2026-09-01-the-hallmark.md` (entry #15: the design taken)

**Interfaces:**
- Consumes: the Task 5 diagnosis (task-5-report.md; registry row `DOM-era-day-axis`): `paleoclimate_from` writes deep-time DAYS (`lib.rs:3733`, `-DEEP_TIME_WINDOW_DAYS + ...`); `bake_eras` writes bake YEARS (`day: cfg.start_year` at :3876; `bake_day` from the start/end-year linspace at :3921-3924); `era_index_for(eras, year)` compares `e.day <= year` (`history_bake.rs:1651`).
- Produces: `EraClimate.day` holds standard days on EVERY producer path; `DOM-era-day-axis`'s blocker is discharged; Task 15 can retype the field.

**Design constraints (the property is fixed; the edit is the implementer's):**
1. **The Ell's design stands**: the bake reasons in years internally (`BakeConfig::start_year`/`end_year`, `Occupation` values stay years). What changes is only the foreign-type boundary — a bake year may not be stored in `EraClimate.day`, a paleoclimate type whose contract is days.
2. **Quantize-at-emit only**: `ledger_day_of_bake_year` QUANTIZES (decision 0033) and is the LEDGER crossing — do not reuse it for an in-memory field. If a year-to-day conversion is needed for the field, it is a new named, unquantized crossing (one function, doc'd as the sibling of `ledger_day_of_bake_year` with the quantization difference stated), or the bake path carries its own era container and converts where it hands data to paleoclimate-typed consumers. Choose by reading who consumes bake-path `EraClimate` values and in which unit — enumerate those consumers in the report before editing.
3. **Selection semantics preserved**: `era_index_for`'s era choice for every input must be provably unchanged (convert both sides of a comparison identically, or keep the comparison in years inside the bake and convert at the container boundary). State in the report why the transform preserves every `<=` outcome, including exact-equality grid alignments.
4. **The comparator twins stay twins**: if `strata::extract`'s peak comparator or worldgen's `:3763-3764` twin is touched, both change identically.

- [ ] **Step 1: scope.** Enumerate every reader of a bake-path-produced `EraClimate` (grep `bake_eras` callers and what they do with `.day`; `era_index_for` call sites with the unit of each `year` argument; whether bake-path eras ever reach `hornvale_paleoclimate::extract` or `facts.rs`). Write the table into the report FIRST.
- [ ] **Step 2: capture baselines.** Generate the before-world (`cargo run -p hornvale -- new --seed 42 --out /tmp/hallmark-t13-before.json`) and capture the worldgen history-bake suites' green state to a log file (one run, grep the log).
- [ ] **Step 3: implement** per the constraints; smallest change that makes the field single-axis.
- [ ] **Step 4: verdicts.** Regenerate `/tmp/hallmark-t13-after.json`; `diff` must be IDENTICAL. Re-run the worldgen suite once to a log and grep the result line; same for paleoclimate. `git status --porcelain book/ clients/ | head` must be empty. ANY committed byte or fixture moved: STOP, do not commit, report the diff verbatim (Nathan adjudicates).
- [ ] **Step 5: docs.** Update the `EraClimate.day` field doc and the `DOM-era-day-axis` registry row (blocker discharged — re-word the row to record the fix and point at this commit; run the docs_consistency suite). Ledger entry #15 records the design chosen and the consumer table's conclusion.
- [ ] **Step 6: fmt, type-audit report if pub surfaces changed, commit.** Kernel-adjacent worldgen edit: slow gate, foreground.

### Task 14: Formation's cave half embeds the kernel CaveKind

**Files:**
- Modify: `domains/climate/src/facets.rs` (replace `KarstCave`/`LavaTube`/`FractureCave` (~:215-223) with one `Cave(CaveKind)` variant; the grouped arm at :314)
- Modify: `domains/climate/src/variants.rs:744` (grouped arm)
- Modify: `windows/worldgen/src/lib.rs:677-679`, `windows/locale/src/surface.rs:148-150` (grouped arms become `Formation::Cave(_)`)
- Modify: `cli/tests/suite/cave_kind_correspondence.rs` (the exhaustive map becomes structural — see Step 3)
- Modify: `book/src/reference/lexicon-of-place.md` if it names the three variants
- Modify: `docs/superpowers/ledgers/2026-09-01-the-hallmark.md` (entry #16)

**Controller-verified blast radius:** NO production code constructs the three variants (workspace grep: only the two unreachable/empty grouped arms, the two downstream grouped arms, and the correspondence test). The corpus strings (`axes.rs:288-290`, `underworld.rs:194-198`) are freestanding literals with zero linkage to the enum — they do not move.

- [ ] **Step 1:** Replace the three variants with one `Cave(CaveKind)` variant whose doc cites decision 0517 and Nathan's G6 unification ruling (ledger #14) and states the corpus spellings ("karst-cave"...) stay climate's own. Add `use hornvale_kernel::CaveKind;`.
- [ ] **Step 2:** Follow the compiler: grouped arms become `Formation::Cave(_)`; any exhaustive `Formation` match the compiler names gets the one new arm. The compiler is the site list.
- [ ] **Step 3:** The correspondence test: the CaveKind-to-Formation variant map is now structural, so the cli test's exhaustive match is obsolete. Verify worldgen's `every_cave_kind_matches_a_corpus_genus` (delve_seating.rs tests) still pins the corpus-spelling map in both directions; if it does, delete the cli test with a pointer to it in the commit message; if it does not, extend it first.
- [ ] **Step 4: verdicts.** `git status --porcelain book/src/laboratory/generated/ docs/audits/system-coverage-wolverson-2021.md clients/ | head` empty; workspace check clean; climate/worldgen/locale/vessel scoped suites green (one run each, logged and grepped); the placement-audit check stays green (Formation and CaveKind are still not shape twins — member sets differ).
- [ ] **Step 5:** lexicon page + docs_consistency if touched; ledger #16; type-audit report regen (pub enum variants changed); fmt; commit (kernel-consumer edit: slow gate).

### Task 15: the WorldTime migration, unblocked (original Task 5 migration branch)

Precondition: Task 13 landed. Scope: `EraClimate.day`, `IceState.day`, `PaleoRecord.glacial_maximum_day` become `WorldTime`; `integrate_ice` samples `&[(WorldTime, f64)]`; both peak comparators become `b.day.cmp(&a.day)` (identically, both files); `(day - p).as_std_days() / DAYS_PER_KYR`; every construction site converts once at the crossing with the rounding rule named (`WorldTime::from_std_days(...).expect("era day within tick range")`); `facts.rs:88` commits `Value::Number(record.glacial_maximum_day.as_std_days())`; remove the migrated `pending(wave-2: ...)` tag positions; person's waivered DTO stays out of scope. Verdicts: the seed-42 before/after byte diff is non-negotiable (IDENTICAL or STOP-and-report); worldgen + paleoclimate suites green; porcelain check on committed artifact paths empty; type-audit report regen; docs: field docs and ledger #17. If Task 13 chose a bake-local era container (so `EraClimate` only ever has days-producers), say so in the report and migrate accordingly.
