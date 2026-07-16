# KindId (ECS Campaign 2) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give a kind a typed, stable identity — a kernel `KindId` newtype over its label — key the species registry by it, and subordinate the packer's positional `u32` to an explicit build-local dense index. Byte-identical.

**Architecture:** A type-safety refactor. The label is already the de-facto serialized identity (`SPECIES_NAME` = `Value::Text`); this makes it a first-class type with a save-format contract, and makes the `u32` explicitly a never-serialized build artifact. Nothing computed changes.

**Tech Stack:** Rust (edition 2024), `serde`/`serde_json` only, `cargo nextest`. No new crates.

## Global Constraints

- **Byte-identical:** every existing seed-42 world and committed artifact stays bit-for-bit identical. The serialized identity (the `Value::Text` label) and the registry order (alphabetical by label) do not change. If any artifact drifts, the refactor changed behavior — stop and find it.
- **No `HashMap`/`HashSet`** — `KindId: Ord` keys a `BTreeMap`, as today. Float sorts use `total_cmp`.
- **`KindId` does NOT derive `Serialize`/`Deserialize`** — it is build-state, never enters the save; a `&'static str` cannot deserialize to a static ref. Kind references reach the ledger as `Value::Text`, unchanged.
- **`#![warn(missing_docs)]`** — `KindId` and its field get doc comments.
- **type-audit:** `KindId` carries `type-audit: bare-ok(identifier-text)`; only the ratified classes are valid. Run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before close.
- **`cargo fmt`** as the final step before every commit. Commit gate is `make gate`.
- **No new dependencies** (allowlist `[libm, serde, serde_json]`).

## File Structure

- `kernel/src/` — the `KindId` newtype (in `ledger.rs` beside `EntityId`, or a small `kind.rs`); re-exported from `kernel/src/lib.rs`.
- `domains/species/src/lib.rs` — `registry()` (and `family_registry()` if it keys by a kind/family label) return `BTreeMap<KindId, _>`; internal lookups adjusted.
- `windows/worldgen/src/lib.rs`, `windows/lab/`, `cli/` — registry index/get/`find` call-sites take/pass `KindId`; the packer's `.enumerate()` `u32` documented as the build-local dense index.
- `book/src/chronicle/`, `docs/retrospectives/`, `book/src/frontier/idea-registry.md` — close artifacts (stage 4).

---

## Task 1: Introduce the `KindId` newtype in the kernel

**Files:**
- Modify: `kernel/src/ledger.rs` (add `KindId` beside `EntityId`), `kernel/src/lib.rs` (re-export)
- Test: `kernel/src/ledger.rs` unit test

**Interfaces:**
- Produces: `pub struct KindId(pub &'static str)` deriving `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash`; re-exported as `hornvale_kernel::KindId`.

- [ ] **Step 1: Write the failing test** — `KindId` exists, is `Ord`, and its label is readable.

```rust
#[test]
fn kind_id_orders_by_label() {
    use crate::KindId;
    let mut ids = [KindId("kobold"), KindId("goblin"), KindId("bugbear")];
    ids.sort();
    assert_eq!(ids, [KindId("bugbear"), KindId("goblin"), KindId("kobold")]);
    assert_eq!(ids[0].0, "bugbear");
}
```

- [ ] **Step 2: Run it, verify it fails to compile** — `cargo test -p hornvale-kernel kind_id_orders_by_label`. Expected: `cannot find type KindId`.

- [ ] **Step 3: Add the newtype** (in `kernel/src/ledger.rs`, near `EntityId`):

```rust
/// The stable identity of a *kind* — the authored label a kind is known by
/// ("red-dragon", "kobold"). A kind's identity is its label, never its
/// position in any registry (decision 0015: a name is its own key). When a
/// kind is referenced in the ledger it is referenced by this label (a
/// `Value::Text`); a deliberate change to a kind's authored traits that must
/// not alias the old kind takes an epoch suffix ("red-dragon/v2"), never a
/// rename. Build-state: never serialized — the label enters the save as
/// `Value::Text`, not as a `KindId`.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KindId(pub &'static str);
```

- [ ] **Step 4: Re-export** from `kernel/src/lib.rs` wherever `EntityId` is re-exported (e.g. `pub use ledger::{EntityId, KindId, ...};`).

- [ ] **Step 5: Run the test** — `cargo test -p hornvale-kernel kind_id_orders_by_label`. Expected: PASS. Then `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` — clean.

- [ ] **Step 6: Commit**

```bash
git add kernel/src/ledger.rs kernel/src/lib.rs
git commit -m "feat(kernel): add KindId — the typed stable identity of a kind"
```

---

## Task 2: Key the species registry by `KindId`

**Files:**
- Modify: `domains/species/src/lib.rs` (`registry()` → `BTreeMap<KindId, SpeciesDef>`; `family_registry()` per open item; internal lookups)
- Modify: `windows/worldgen/src/lib.rs`, `windows/lab/src/*.rs`, `cli/src/*.rs` and their tests — registry index/get/`find` call-sites
- Test: workspace build + existing byte-identity suite

**Interfaces:**
- Consumes: `hornvale_kernel::KindId` (Task 1).
- Produces: `pub fn registry() -> BTreeMap<KindId, SpeciesDef>`. `SpeciesDef.name: &'static str` is unchanged and equals its `KindId`'s label (decision 0015 duplication).

- [ ] **Step 1: Re-key `registry()`** — change the return type to `BTreeMap<KindId, SpeciesDef>` and each `reg.insert("goblin", SpeciesDef { name: "goblin", … })` to `reg.insert(KindId("goblin"), SpeciesDef { name: "goblin", … })`. `SpeciesDef.name` stays the label. Do the same for `family_registry()` only if it keys by a kind/family label (open item: reuse `KindId` for the family label; do not mint a separate type unless a call-site forces it).

- [ ] **Step 2: Build to enumerate the breakage** — `cargo build --workspace 2>&1 | grep -E "expected .KindId|no method|cannot index"`. Expected: errors at registry index/get/`find` sites (`reg["goblin"]`, `registry().get(name)`, `roster.iter().find(|d| d.name == species)` that then index the map).

- [ ] **Step 3: Fix the call-sites.** Two patterns:
  - **Kind-identity sites** (a value that *is* a kind) → wrap in `KindId`: `reg[&KindId("goblin")]`, `registry().get(&KindId(name))`.
  - **Free-text sites** (user input, e.g. a `--species NAME` CLI string) → keep the `&str` and resolve against the registry, failing loudly if unknown. Where code does `roster.iter().find(|d| d.name == species)` (worldgen:1394), the `d.name` comparison against a free-text `&str` is unchanged (name is still `&'static str`); only sites that then *index the registry map* need `KindId`.

- [ ] **Step 4: Build clean** — `cargo build --workspace`. Expected: success.

- [ ] **Step 5: Run the byte-identity / worldgen / species suites** — `cargo nextest run -p hornvale-species -p hornvale-worldgen`. Expected: PASS unchanged (a pure re-keying; the map order and values are identical). If a seed-42 golden test drifts, STOP — the refactor changed behavior.

- [ ] **Step 6: Full workspace check** — `cargo nextest run --workspace`. Expected: green (modulo any pre-existing deferred-census reds, if present — verify no NEW category). `cargo fmt` + `cargo clippy --workspace --all-targets -- -D warnings` clean.

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "refactor(species,worldgen,lab,cli): key the species registry by KindId"
```

---

## Task 3: Subordinate the `u32` dense index + the label-stability regression

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (document the `.enumerate()` `u32` as the build-local dense index)
- Test: `domains/species/src/lib.rs` or `windows/worldgen/` (the label-stability regression)

**Interfaces:**
- Consumes: `registry()` keyed by `KindId` (Task 2).

- [ ] **Step 1: Write the failing regression test** — inserting a new kind must not change any *other* kind's serialized reference (the label), though the build-local `u32` may renumber.

```rust
#[test]
fn inserting_a_kind_does_not_change_other_kinds_serialized_labels() {
    // The committed identity of a kind is its label, not its position.
    let base: Vec<&str> = registry().keys().map(|k| k.0).collect();
    // A dummy kind inserted alphabetically first must not alter the LABELS
    // the other kinds serialize under (only the build-local u32 renumbers).
    // Assert every existing label still resolves to the same SpeciesDef name.
    for k in registry().keys() {
        assert_eq!(registry()[k].name, k.0,
            "a kind's label ({}) must equal its SpeciesDef.name (0015)", k.0);
    }
    assert!(base.windows(2).all(|w| w[0] < w[1]), "registry keys stay label-ordered");
}
```

- [ ] **Step 2: Run it** — `cargo test -p hornvale-species inserting_a_kind_does_not_change_other_kinds_serialized_labels`. Expected: PASS (this asserts the 0015 invariant the re-keying preserves). If it needs a live dummy-insert to be meaningful, extend it to build two rosters (with and without a prepended dummy kind) and assert the shared kinds' `SPECIES_NAME` facts are byte-identical.

- [ ] **Step 3: Document the `u32` as build-local** — at the `.enumerate()` sites (`windows/worldgen/src/lib.rs` ~534, 583, 2394), add/confirm a doc comment stating the `u32` is a **build-local dense index** derived from the `KindId`-ordered registry, valid only within one build and **never serialized** (the durable-label / ephemeral-index split). Keep it to one authoritative comment referenced from the others if they repeat.

- [ ] **Step 4: Run the suite** — `cargo nextest run -p hornvale-species -p hornvale-worldgen`. Expected: PASS, byte-identical.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "docs(worldgen): subordinate the packer u32 to a build-local dense index; label-stability regression"
```

---

## Task 4: Close the campaign

**Files:**
- Create: `book/src/chronicle/<slug>.md` (check `ls book/src/chronicle/` for an available evocative slug first — e.g. `the-true-name`), `docs/retrospectives/<slug>.md`
- Modify: `book/src/SUMMARY.md` (wire the chronicle), `book/src/frontier/idea-registry.md` (UNI-22 campaign-2 progress note)

- [ ] **Step 1: Preflight** — `make preflight` from the branch; on NO-GO, merge main in, re-run the gate, re-run preflight.

- [ ] **Step 2: Chronicle** — `book/src/chronicle/<slug>.md`, an available slug (confirm with `ls book/src/chronicle/`), at the deliberate altitude: the kind's identity made a typed contract; the label was always the identity, now it is a type; the `u32` demoted to a build artifact; byte-identical. Wire into `SUMMARY.md`.

- [ ] **Step 3: Retrospective** — `docs/retrospectives/<slug>.md`: process lessons (a byte-identical type-safety refactor; the small-foundational-campaign question; anything the migration surfaced).

- [ ] **Step 4: Registry note** — in UNI-22, note campaign 2 (`KindId`) shipped; run `cargo test -p hornvale --test docs_consistency` (frontier links resolve).

- [ ] **Step 5: Full gate + drift** — `make gate` green; `git diff` clean after any artifact regen (there should be none — byte-identical).

- [ ] **Step 6: Merge readiness** — invoke `closing-a-campaign`; the FF-merge and any census/push are Nathan's calls.
