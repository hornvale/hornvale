# Dissolve SpeciesDef (ECS Campaign 3) Implementation Plan

> **COMPLETE — SHIPPED 2026-07-16** (merged to local main; chronicle *The
> Dissolution*). Executed in 10 byte-identical commits + a final-review fix.
> The revised endgame (Tasks 5–10) followed ledger #60/#61: the roster is a
> component-set, and `SpeciesDef` was deleted this campaign (not phased).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Dissolve the `SpeciesDef` god-struct into per-concern component registries keyed by `KindId`, each owned by a domain (body+mind → `species`, speech → `language`), composed only at `worldgen` — byte-identical.

**Architecture:** Introduce `kernel::ComponentStore<K,C>` (a `BTreeMap`-backed typed component store, the generalization of `CellMap`). Split `SpeciesDef`'s fields into per-concern `ComponentStore<KindId, C>` registries published by their owning domain. `worldgen` (composition root) joins them by `KindId` into a `WorldComponents` and runs a referential-integrity check. Strangler-fig: registries coexist with the god-struct until the final task deletes it. Every stage is byte-identical and independently testable.

**Tech Stack:** Rust (edition 2024), std-only, `serde`/`serde_json` only. `cargo nextest` gate. No new dependencies.

## Global Constraints

- **Byte-identical.** Same seed + pins → bit-identical worlds and artifacts. The committed-artifact drift-check is the master oracle; every task must leave it green. `cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json` before/after a task must be identical.
- **Determinism:** no `HashMap`/`HashSet` (clippy-enforced); `BTreeMap`/`BTreeSet`/`Vec` only. `ComponentStore` iterates ascending-by-key.
- **Layering (enforced by `cli/tests/architecture.rs`):** a domain depends on `hornvale-kernel` and **no other domain**. `species` must not depend on `language` and vice versa. Cross-crate equivalence tests live in `windows/worldgen` (which depends on both), never in a domain.
- **Dependencies:** `serde` + `serde_json` only; allowlist enforced. No new crates.
- **Docs:** every crate is `#![warn(missing_docs)]`; every `pub` item gets a one-line doc. Every primitive at a `pub` boundary carries a `type-audit:` verdict tag; the audit must stay clean (`cargo run --manifest-path tools/type-audit/Cargo.toml -- check`).
- **`cargo fmt` is the final step before every commit.** Cost-order each task: fmt + clippy first, then scoped tests, then the gate.
- **Census:** never regenerate locally. The standing deferred-AWS census reds are pre-existing; a byte-identical refactor does not change them. "Green modulo the census reds."

---

### Task 1: `kernel::ComponentStore<K,C>` + `Component` marker

**Files:**
- Create: `kernel/src/component.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod component;` and re-export)
- Test: `kernel/src/component.rs` (`#[cfg(test)]` module)

**Interfaces:**
- Produces: `hornvale_kernel::ComponentStore<K: Ord, C>` with `new`, `get(&self,&K)->Option<&C>`, `insert(&mut self,K,C)->Option<C>`, `contains(&self,&K)->bool`, `iter()->impl Iterator<Item=(&K,&C)>` (ascending key), `ids()->impl Iterator<Item=&K>`, `len`, `is_empty`, `FromIterator<(K,C)>`, `Default`. Produces the open marker trait `hornvale_kernel::Component`.

- [ ] **Step 1: Write the failing test** in `kernel/src/component.rs`:

```rust
#![allow(dead_code)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stores_and_retrieves_by_key_in_ascending_order() {
        let s: ComponentStore<u32, &str> =
            [(3, "c"), (1, "a"), (2, "b")].into_iter().collect();
        assert_eq!(s.len(), 3);
        assert_eq!(s.get(&2), Some(&"b"));
        assert_eq!(s.get(&9), None);
        assert!(s.contains(&1));
        // deterministic ascending-by-key iteration (the no-HashMap dividend)
        let order: Vec<u32> = s.ids().copied().collect();
        assert_eq!(order, vec![1, 2, 3]);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-kernel component:: 2>&1 | tail -20`
Expected: FAIL to compile — `ComponentStore` not found.

- [ ] **Step 3: Write the implementation** (top of `kernel/src/component.rs`, above the test module):

```rust
//! The entity-space component store: a typed table of component `C` keyed by
//! an identity `K`. The generalization of [`crate::CellMap`] from a fixed
//! `CellId` key to any ordered key — the storage substrate an ECS component
//! registry is built on. Deterministic ascending-by-key iteration (the
//! no-`HashMap` rule's dividend); `BTreeMap`-backed. The dense-`Vec` backend
//! for dense keys and the permutation indexes are the query engine's work
//! (metaplan §4.5, campaign 4), not here.
#![warn(missing_docs)]

use std::collections::BTreeMap;

/// A typed store of component `C` keyed by identity `K`.
/// type-audit: bare-ok(container)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ComponentStore<K: Ord, C> {
    map: BTreeMap<K, C>,
}

impl<K: Ord, C> ComponentStore<K, C> {
    /// An empty store.
    pub fn new() -> Self {
        Self { map: BTreeMap::new() }
    }
    /// The component for `k`, if present.
    pub fn get(&self, k: &K) -> Option<&C> {
        self.map.get(k)
    }
    /// Insert `c` under `k`, returning any previous value.
    pub fn insert(&mut self, k: K, c: C) -> Option<C> {
        self.map.insert(k, c)
    }
    /// Whether `k` has a component.
    pub fn contains(&self, k: &K) -> bool {
        self.map.contains_key(k)
    }
    /// `(key, component)` pairs in ascending key order.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &C)> {
        self.map.iter()
    }
    /// The keys, ascending.
    pub fn ids(&self) -> impl Iterator<Item = &K> {
        self.map.keys()
    }
    /// The number of components.
    pub fn len(&self) -> usize {
        self.map.len()
    }
    /// Whether the store is empty.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<K: Ord, C> FromIterator<(K, C)> for ComponentStore<K, C> {
    fn from_iter<I: IntoIterator<Item = (K, C)>>(iter: I) -> Self {
        Self { map: iter.into_iter().collect() }
    }
}

/// Open marker for a component type. **Not sealed** — sealing would break the
/// constitutional open-extension rule (any domain may declare a component).
/// Invariant safety comes from each component's validating constructor, not
/// from restricting who may implement this trait.
pub trait Component {}
```

- [ ] **Step 4: Wire the module** — in `kernel/src/lib.rs`, add `pub mod component;` (alphabetical, after `pub mod climate`/before `pub mod domain` as ordering allows — place it after line 8's block consistently) and add to the re-export block:

```rust
pub use component::{Component, ComponentStore};
```

- [ ] **Step 5: Run to verify it passes + gate the crate**

Run: `cargo test -p hornvale-kernel component:: 2>&1 | tail -20`
Expected: PASS. Then `cargo clippy -p hornvale-kernel --all-targets -- -D warnings` clean.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add kernel/src/component.rs kernel/src/lib.rs
git commit -m "feat(kernel): ComponentStore<K,C> + open Component marker (ECS c3 substrate)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 2: Body + mind registries in `species` (derived from `registry()`)

**Files:**
- Modify: `domains/species/src/lib.rs` (add registry accessors + `Component` impls; keep `registry()`/`SpeciesDef` intact)
- Test: `domains/species/src/lib.rs` (`#[cfg(test)]`)

**Interfaces:**
- Consumes: `hornvale_kernel::{ComponentStore, Component, KindId}`; the existing `registry() -> BTreeMap<KindId, SpeciesDef>`.
- Produces: `species::biosphere_registry() -> ComponentStore<KindId, BiosphereTraits>` (16 kinds), `species::psyche_registry() -> ComponentStore<KindId, PsychVector>` (4 peoples), `species::perception_registry() -> ComponentStore<KindId, PerceptionVector>` (4 peoples), `species::family_of() -> ComponentStore<KindId, &'static str>` (16 kinds). `impl Component` for `BiosphereTraits`, `PsychVector`, `PerceptionVector`.

- [ ] **Step 1: Write the failing equivalence test** (same-crate; in `domains/species/src/lib.rs` tests):

```rust
#[test]
fn component_registries_equal_the_god_struct_fields() {
    let god = registry();
    let bio = biosphere_registry();
    let psy = psyche_registry();
    let per = perception_registry();
    let fam = family_of();
    // biosphere + family: present for ALL kinds, field-equal.
    assert_eq!(bio.len(), god.len());
    assert_eq!(fam.len(), god.len());
    for (kind, def) in god.iter() {
        assert_eq!(bio.get(kind), Some(&def.biosphere), "biosphere {kind:?}");
        assert_eq!(fam.get(kind), Some(&def.family), "family {kind:?}");
        match &def.peopled {
            Some(p) => {
                assert_eq!(psy.get(kind), Some(&p.psych), "psych {kind:?}");
                assert_eq!(per.get(kind), Some(&p.perception), "perception {kind:?}");
            }
            None => {
                assert!(!psy.contains(kind), "fauna {kind:?} must have no psyche");
                assert!(!per.contains(kind), "fauna {kind:?} must have no perception");
            }
        }
    }
    // the peopled cluster: psyche and perception share one key-set (the 4 peoples).
    let psy_ids: Vec<_> = psy.ids().collect();
    let per_ids: Vec<_> = per.ids().collect();
    assert_eq!(psy_ids, per_ids);
    assert_eq!(psy.len(), 4);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-species component_registries 2>&1 | tail -20`
Expected: FAIL — `biosphere_registry` not found.

- [ ] **Step 3: Add the accessors + `Component` impls** in `domains/species/src/lib.rs` (after `registry()`):

```rust
use hornvale_kernel::{Component, ComponentStore};

impl Component for BiosphereTraits {}
impl Component for PsychVector {}
impl Component for PerceptionVector {}

/// The universal biosphere component, one per kind — the canonical entity set
/// (every kind that competes for space has a biosphere row). Derived from the
/// authored `registry()` during the campaign-3 migration; becomes the direct
/// authoring home when the god-struct is deleted (final task).
/// type-audit: bare-ok(identifier-text)
pub fn biosphere_registry() -> ComponentStore<KindId, BiosphereTraits> {
    registry().into_iter().map(|(k, d)| (k, d.biosphere)).collect()
}

/// The peopled psychology component — present only for settling, speaking kinds.
/// type-audit: bare-ok(identifier-text)
pub fn psyche_registry() -> ComponentStore<KindId, PsychVector> {
    registry()
        .into_iter()
        .filter_map(|(k, d)| d.peopled.map(|p| (k, p.psych)))
        .collect()
}

/// The peopled perception component — present only for peoples.
/// type-audit: bare-ok(identifier-text)
pub fn perception_registry() -> ComponentStore<KindId, PerceptionVector> {
    registry()
        .into_iter()
        .filter_map(|(k, d)| d.peopled.map(|p| (k, p.perception)))
        .collect()
}

/// The universal taxonomy lookup: a kind's family label, one per kind. Read by
/// worldgen to resolve a kind's proto vector against language's family_proto.
/// type-audit: bare-ok(identifier-text)
pub fn family_of() -> ComponentStore<KindId, &'static str> {
    registry().into_iter().map(|(k, d)| (k, d.family)).collect()
}
```

Note: `PsychVector`/`PerceptionVector` derive `Clone, Copy`, so `d.peopled.map(|p| p.psych)` copies; `BiosphereTraits` is `Clone` (not `Copy`) so `d.biosphere` moves out of the owned `d` — fine (owned map from `into_iter`).

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-species component_registries 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 5: fmt + clippy + type-audit + commit**

```bash
cargo clippy -p hornvale-species --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
git add domains/species/src/lib.rs
git commit -m "feat(species): body+mind component registries derived from registry() (ECS c3)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 3: Worldgen reads the body/mind registries via `WorldComponents` (partial)

**Files:**
- Create: `windows/worldgen/src/components.rs` (the `WorldComponents` composition + assembly)
- Modify: `windows/worldgen/src/lib.rs` (declare the module; switch biosphere + psyche + perception consumers)
- Test: `windows/worldgen/tests/dissolve_equivalence.rs` (new; cross-crate equivalence lives in a window)

**Interfaces:**
- Consumes: `species::{biosphere_registry, psyche_registry, perception_registry, family_of}`; `hornvale_kernel::ComponentStore`.
- Produces: `worldgen::components::WorldComponents` (fields per spec §3.3, populated for the species-owned components this task; language-owned fields added in Task 5). `WorldComponents::assemble() -> Result<WorldComponents, BuildError>` performing the referential-integrity check for the species-owned components.

- [ ] **Step 1: Write the failing test** in `windows/worldgen/tests/dissolve_equivalence.rs`:

```rust
//! Cross-crate equivalence guardrail for the SpeciesDef dissolution: the
//! component registries (possibly in different crates) reassemble the same
//! kind data the god-struct held. Lives in worldgen because only a window may
//! depend on more than one domain.
use hornvale_worldgen::components::WorldComponents;

#[test]
fn assemble_holds_every_kind_and_passes_integrity() {
    let wc = WorldComponents::assemble().expect("well-formed roster");
    // biosphere = the canonical entity set (all 16 kinds today).
    assert_eq!(wc.biosphere.len(), 16);
    // peopled cluster is coherent: psyche and perception share one key-set,
    // a subset of biosphere.
    let psy: Vec<_> = wc.psyche.ids().collect();
    let per: Vec<_> = wc.perception.ids().collect();
    assert_eq!(psy, per);
    assert_eq!(wc.psyche.len(), 4);
    for k in wc.psyche.ids() {
        assert!(wc.biosphere.contains(k), "peopled kind {k:?} lacks a biosphere row");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen --test dissolve_equivalence 2>&1 | tail -20`
Expected: FAIL — `components` module not found.

- [ ] **Step 3: Create `windows/worldgen/src/components.rs`:**

```rust
//! The composition root's component set: the per-domain component registries
//! joined by `KindId`. A kind is the set of components carrying its key; "the
//! goblin" exists only as this assembled view. `assemble()` gathers each
//! domain's registry and enforces referential integrity (the load-time
//! invariant that replaces `Option<PeopledTraits>`).
#![warn(missing_docs)]

use hornvale_kernel::{ComponentStore, KindId};
use hornvale_species::{BiosphereTraits, PerceptionVector, PsychVector};

use crate::BuildError;

/// The joined component registries of a world's kinds. (Language-owned speech
/// components are added in the speech-relocation task.)
pub struct WorldComponents {
    /// Universal body component — the canonical entity set.
    pub biosphere: ComponentStore<KindId, BiosphereTraits>,
    /// Peopled psychology.
    pub psyche: ComponentStore<KindId, PsychVector>,
    /// Peopled perception.
    pub perception: ComponentStore<KindId, PerceptionVector>,
    /// Universal taxonomy: a kind's family label.
    pub family_of: ComponentStore<KindId, &'static str>,
}

impl WorldComponents {
    /// Gather every domain's registry and enforce referential integrity: the
    /// peopled cluster shares one key-set, and every peopled kind has a
    /// biosphere row. Fails loudly with the physical reason.
    pub fn assemble() -> Result<Self, BuildError> {
        let biosphere = hornvale_species::biosphere_registry();
        let psyche = hornvale_species::psyche_registry();
        let perception = hornvale_species::perception_registry();
        let family_of = hornvale_species::family_of();

        let psy_ids: Vec<&KindId> = psyche.ids().collect();
        let per_ids: Vec<&KindId> = perception.ids().collect();
        if psy_ids != per_ids {
            return Err(BuildError::MalformedKind(
                "psyche and perception registries must share one key-set".into(),
            ));
        }
        for k in psyche.ids() {
            if !biosphere.contains(k) {
                return Err(BuildError::MalformedKind(format!(
                    "peopled kind {k:?} has no biosphere component"
                )));
            }
            if !family_of.contains(k) {
                return Err(BuildError::MalformedKind(format!(
                    "kind {k:?} has no family"
                )));
            }
        }
        Ok(Self { biosphere, psyche, perception, family_of })
    }
}
```

- [ ] **Step 4: Add the `BuildError` variant** — in `windows/worldgen/src/lib.rs`, find `enum BuildError` and add:

```rust
    /// A kind's component-set is malformed (referential-integrity failure at
    /// assembly): e.g. a peopled kind missing a biosphere or a speech row.
    MalformedKind(String),
```

Add a matching `Display` arm if `BuildError` implements `Display` by hand (mirror the existing arms). Declare the module: add `pub mod components;` near the top of `windows/worldgen/src/lib.rs`.

- [ ] **Step 5: Switch the biosphere/psyche/perception consumers.** Replace the reads that go through `def.biosphere` / `peopled(def).psych` / `peopled(def).perception` with registry lookups. Reference sites (from the code sweep): `perception_lens` (`:1359`), the perception passes (`:1509`, `:1846`), `status_register`/psych reads (`:2027`), and any `.biosphere` read in the packer path. Pattern, per site:

```rust
// before: let p = &peopled(def).perception;
// after:  let p = wc.perception.get(&kind_id).expect("peopled pass over a fauna kind");
```

Thread a `&WorldComponents` (or the specific registry) into these functions where they currently take `def: &SpeciesDef`. **Do not** touch articulation/lexicon reads yet (they stay on `peopled(def)` until Task 5). The `peopled()` helper stays for the still-god-struct speech reads. Keep biosphere reads in the packer identical (it already reads `.biosphere`; point it at `wc.biosphere.get(kind)` returning the same value).

- [ ] **Step 6: Run the guardrail + byte-identity check**

Run: `cargo test -p hornvale-worldgen --test dissolve_equivalence 2>&1 | tail -20` → PASS.
Byte-identity:
```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-after.json
cargo run -p hornvale-kernel --example first_light > /tmp/fl-after.txt
git stash list >/dev/null # (do not stash; compare against committed artifacts instead)
git diff --exit-code book/src/gallery/ book/src/reference/ || echo "REGEN NEEDED — investigate before proceeding"
```
Expected: no artifact diff (byte-identical). If the world JSON changed, STOP and diff — a value moved.

- [ ] **Step 7: fmt + clippy + commit**

```bash
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo fmt
git add windows/worldgen/src/components.rs windows/worldgen/src/lib.rs windows/worldgen/tests/dissolve_equivalence.rs
git commit -m "feat(worldgen): WorldComponents + integrity check; read body/mind registries (ECS c3)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 4: Relocate speech types + authoring into `language`

**Files:**
- Modify: `domains/language/src/lib.rs` (add `ArticulationVector`, `ExoticManner`, `Lexicon`, the three registries; author the byte-identical values)
- Test: `windows/worldgen/tests/dissolve_equivalence.rs` (extend with the cross-crate speech equivalence test)

**Interfaces:**
- Consumes: `hornvale_kernel::{ComponentStore, Component, KindId}`.
- Produces: `language::ArticulationVector` (7 fields, identical to `species::ArticulationVector`), `language::ExoticManner`, `language::Lexicon { noun, worker_override, warrior, artisan, shaman, top }`, `language::articulation_registry() -> ComponentStore<KindId, ArticulationVector>` (4 peoples), `language::lexicon_registry() -> ComponentStore<KindId, Lexicon>` (4 peoples), `language::family_proto() -> ComponentStore<KindId, ArticulationVector>` (3 families). `impl Component` for each.

- [ ] **Step 1: Write the failing cross-crate equivalence test** (append to `dissolve_equivalence.rs`):

```rust
#[test]
fn language_speech_registries_equal_the_god_struct_speech() {
    use hornvale_kernel::KindId;
    let god = hornvale_species::registry();
    let art = hornvale_language::articulation_registry();
    let lex = hornvale_language::lexicon_registry();
    for (kind, def) in god.iter() {
        match &def.peopled {
            Some(p) => {
                let a = art.get(kind).expect("peopled kind has articulation");
                // field-by-field equality across the crate boundary
                assert_eq!(a.labiality, p.articulation.labiality, "labiality {kind:?}");
                assert_eq!(a.vowel_space, p.articulation.vowel_space);
                assert_eq!(a.voicing, p.articulation.voicing);
                assert_eq!(a.sibilance, p.articulation.sibilance);
                assert_eq!(a.voice_loudness, p.articulation.voice_loudness);
                assert_eq!(a.tonality, p.articulation.tonality);
                assert_eq!(a.exotic as u8, p.articulation.exotic as u8);
                let l = lex.get(kind).expect("peopled kind has lexicon");
                assert_eq!(l.noun, p.noun);
                assert_eq!(l.warrior, p.warrior);
                assert_eq!(l.top, p.top);
            }
            None => assert!(art.get(kind).is_none(), "fauna {kind:?} has no articulation"),
        }
    }
    // family protos: the 3 multi-member families.
    let proto = hornvale_language::family_proto();
    assert_eq!(proto.get(&KindId("goblinoid")).map(|a| a.voicing), Some(0.55));
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen --test dissolve_equivalence language_speech 2>&1 | tail -20`
Expected: FAIL — `articulation_registry` not found in `hornvale_language`.

- [ ] **Step 3: Add the types + registries to `domains/language/src/lib.rs`.** Copy `ArticulationVector` and `ExoticManner` verbatim from `domains/species/src/lib.rs` (same fields, same derives, same doc + `type-audit` tags), add `Lexicon`, and author the three registries with the exact values from the four peopled `SpeciesDef` literals and the `family_registry()` in species. Author directly (language cannot read `species`). Skeleton:

```rust
use hornvale_kernel::{Component, ComponentStore, KindId};

/// An exotic manner of articulation found in a kind's phonology.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExoticManner { /* None, Trill, Click, Ejective — copy from species */ }

/// The closed seven-dimension articulation vector (phonology). Moved here from
/// `species` (ECS c3): the phonology component's owner is language.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ArticulationVector { /* labiality, vowel_space, voicing, sibilance,
    voice_loudness, tonality, exotic — copy fields + docs from species */ }

/// The peopled social lexicon (stopgap vocabulary The Tongues will generate).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct Lexicon {
    /// The settlement noun ("village", "warren").
    pub noun: &'static str,
    /// Worker-role override; `None` = the subsistence word.
    pub worker_override: Option<&'static str>,
    /// The warrior-rung word.
    pub warrior: &'static str,
    /// The artisan-rung word.
    pub artisan: &'static str,
    /// The shaman-rung word.
    pub shaman: &'static str,
    /// The top-rung word.
    pub top: &'static str,
}

impl Component for ArticulationVector {}
impl Component for Lexicon {}

/// Peopled phonology, one per speaking kind. Values are the byte-identical
/// articulation vectors formerly on `species::PeopledTraits`.
/// type-audit: bare-ok(identifier-text)
pub fn articulation_registry() -> ComponentStore<KindId, ArticulationVector> {
    [
        (KindId("goblin"), ArticulationVector { /* goblin's exact values */ }),
        (KindId("kobold"), ArticulationVector { /* ... */ }),
        (KindId("hobgoblin"), ArticulationVector { /* ... */ }),
        (KindId("bugbear"), ArticulationVector { /* ... */ }),
    ].into_iter().collect()
}

/// Peopled lexicon, one per speaking kind. Byte-identical to the former
/// `species::PeopledTraits` noun + rung words.
/// type-audit: bare-ok(identifier-text)
pub fn lexicon_registry() -> ComponentStore<KindId, Lexicon> { /* the 4 peoples */ }

/// Proto ancestral articulation vectors keyed by family (goblinoid/draconic/
/// plant) — moved from `species::family_registry`.
/// type-audit: bare-ok(identifier-text)
pub fn family_proto() -> ComponentStore<KindId, ArticulationVector> { /* the 3 */ }
```

Fill every `/* ... */` with the exact numeric values from `domains/species/src/lib.rs` (the four peopled literals + `family_registry()`). The equivalence test in Step 1 is the proof they match. **Do not yet remove anything from `species`** — the two type copies coexist this task (they are distinct crate-local types).

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen --test dissolve_equivalence 2>&1 | tail -20`
Expected: PASS (both equivalence tests).

- [ ] **Step 5: fmt + clippy + type-audit + commit**

```bash
cargo clippy -p hornvale-language --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
git add domains/language/src/lib.rs windows/worldgen/tests/dissolve_equivalence.rs
git commit -m "feat(language): own the speech cluster — articulation/lexicon/proto registries (ECS c3)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 5: Worldgen switches speech consumers to `language`; delete speech from `species`

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (name-gen `envelope_of`, phonology, `culture::Vocabulary` assembly); `windows/worldgen/src/components.rs` (add language-owned fields to `WorldComponents`)
- Modify: `domains/species/src/lib.rs` (remove `ArticulationVector`, `ExoticManner`, articulation/lexicon fields from `PeopledTraits`, `family_registry`)

**Interfaces:**
- Consumes: `language::{articulation_registry, lexicon_registry, family_proto, ArticulationVector, ExoticManner, Lexicon}`; the equivalence guardrails from Tasks 3–4.
- Produces: `WorldComponents` extended with `articulation`, `lexicon`, `family_proto` fields (populated + integrity-checked). `species::PeopledTraits` reduced to `{ psych, perception }`.

- [ ] **Step 1: Extend `WorldComponents`** (in `components.rs`) with the language-owned fields and integrity checks:

```rust
    /// Peopled phonology (language-owned).
    pub articulation: ComponentStore<KindId, hornvale_language::ArticulationVector>,
    /// Peopled lexicon (language-owned).
    pub lexicon: ComponentStore<KindId, hornvale_language::Lexicon>,
    /// Family proto vectors (language-owned).
    pub family_proto: ComponentStore<KindId, hornvale_language::ArticulationVector>,
```

In `assemble()`, populate them and assert the peopled cluster now spans all four peopled registries (psyche/perception/articulation/lexicon share one key-set):

```rust
let articulation = hornvale_language::articulation_registry();
let lexicon = hornvale_language::lexicon_registry();
let family_proto = hornvale_language::family_proto();
let art_ids: Vec<&KindId> = articulation.ids().collect();
if art_ids != psy_ids || lexicon.ids().collect::<Vec<_>>() != psy_ids {
    return Err(BuildError::MalformedKind(
        "the peopled cluster (psyche/perception/articulation/lexicon) must share one key-set".into(),
    ));
}
```

- [ ] **Step 2: Switch worldgen's speech consumers.** Replace, at each site:
  - `envelope_of(&peopled(def).articulation)` → `envelope_of(wc.articulation.get(&kind).expect("peopled"))`, where `envelope_of` now takes `&hornvale_language::ArticulationVector` (`:1593`, `:1626`).
  - `envelope_of(&hornvale_species::family_registry()[&KindId(family)])` → read `wc.family_proto.get(&KindId(family))` (`:1650`, `:2006`).
  - The `culture::Vocabulary` assembly (`:2715–2719`, `:4441–4445`, `:3392`, `:3427`) reads `peopled_def.warrior`/`.noun`/… → read `wc.lexicon.get(&kind)` fields.
  - `has_priesthood: … == peopled_def.shaman` (`:2731`) → `== wc.lexicon.get(&kind).unwrap().shaman`.
  - `envelope_of`'s body: its `hornvale_species::ExoticManner` match arms become `hornvale_language::ExoticManner` (or the mapping moves into language — see plan open item; keep the mapping in worldgen this task, just retype it).

- [ ] **Step 3: Delete speech from `species`.** In `domains/species/src/lib.rs`: remove the `ArticulationVector` struct, `ExoticManner` enum, the `articulation`/`noun`/`worker_override`/`warrior`/`artisan`/`shaman`/`top` fields from `PeopledTraits` (leaving `{ psych, perception }`), the `family_registry()` fn, the four peopled literals' articulation/lexicon fields, and the `SPECIES_LABIALITY`…`SPECIES_TONALITY`/`SPECIES_EXOTIC_MANNER` genesis commits **only if** they are re-sourced — **check:** `genesis_in` commits articulation facts from `peopled.articulation`. Those facts are part of the byte-identical ledger. Re-source them in `genesis_in` from `hornvale_language::articulation_registry()`? **No** — `species` cannot depend on `language`. Move the articulation-fact commits out of `species::genesis_in` into a worldgen genesis pass that reads `wc.articulation`. Verify the committed fact stream is byte-identical (same predicates, same order). This is the one subtle site: the articulation *facts* must still be committed, now sourced from language via worldgen.

- [ ] **Step 4: Run the guardrails + byte-identity**

Run: `cargo test -p hornvale-worldgen --test dissolve_equivalence 2>&1 | tail -20` → PASS.
```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
git diff --exit-code cli/tests/fixtures/world-seed-42.json book/src/gallery/ book/src/reference/
```
Expected: no diff. If the articulation facts moved order/predicate, STOP and fix the genesis pass until byte-identical.

- [ ] **Step 5: fmt + clippy (workspace) + type-audit + commit**

```bash
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
git add -A
git commit -m "refactor(worldgen,species): speech owned by language; species holds body+mind (ECS c3)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 6: Delete the god-struct; author registries directly; genesis over the entity set

**Files:**
- Modify: `domains/species/src/lib.rs` (invert: author `biosphere_registry`/`psyche_registry`/`perception_registry`/`family_of` directly; delete `SpeciesDef`, `PeopledTraits`, `registry()`)
- Modify: `windows/worldgen/src/lib.rs` (roster/genesis paths iterate `wc.biosphere.ids()`; drop `SpeciesDef`/`peopled()` helper)

**Interfaces:**
- Consumes: the registries from Tasks 2–5.
- Produces: `species` with no god-struct — the registries are the authoring home. Worldgen's `default_roster()`/genesis rebuilt on `WorldComponents`.

- [ ] **Step 1: Invert the species authoring.** Move the 16 kinds' authored values out of the (about-to-be-deleted) `SpeciesDef` literals directly into `biosphere_registry()` / `psyche_registry()` / `perception_registry()` / `family_of()` as direct `[(KindId(...), …), …].into_iter().collect()` bodies. The same-crate equivalence test (`component_registries_equal_the_god_struct_fields`) can no longer run against `registry()` — **replace it** with per-registry invariant tests (goblin biosphere mass == 18.1, the 4 peopled key-set, 16 biosphere keys) that assert the same facts without the god-struct.

- [ ] **Step 2: Delete `SpeciesDef`, `PeopledTraits`, `registry()`** from `species`. Fix the remaining species-internal consumers (`genesis`, `genesis_in`, `default_roster`) — genesis mints one entity per `biosphere_registry().ids()` (the entity set), committing `SPECIES_NAME` from the `KindId` label and mind/species facts from `psyche_registry`/`perception_registry`. The articulation facts are already committed by the worldgen pass (Task 5). Keep the mint order = ascending `KindId` (byte-identical to today's alphabetical roster).

- [ ] **Step 3: Rebuild worldgen's roster/genesis.** `default_roster()` (`:1420`) and `build_world_with_roster` (`:575`, `:634`) took `Vec<SpeciesDef>`; switch them to drive off `WorldComponents` (iterate `wc.biosphere.ids()`; look up components per kind). Delete the `peopled()` helper (`:1446`) — its callers now use `wc.psyche.get(k)` / `wc.perception.get(k)` behind the same `is_some()` gates. The `species_carrying_input`/pin paths resolve `&str` names to `KindId` against `wc.biosphere`, failing loudly on an unknown name (unchanged behavior).

- [ ] **Step 4: Run to verify byte-identity across the workspace**

```bash
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c3.txt
git diff --exit-code cli/tests/fixtures/world-seed-42.json book/src/gallery/ book/src/reference/ book/src/laboratory/
```
Expected: PASS except the pre-existing deferred-AWS census reds (unchanged count). No artifact diff. Inspect `/tmp/hv-c3.txt`: the only failures must be the standing census-schema reds — any other red means a value moved; STOP and debug.

- [ ] **Step 5: fmt + clippy + type-audit + commit**

```bash
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
git add -A
git commit -m "refactor(species,worldgen): delete SpeciesDef; registries are the authoring home (ECS c3)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 7: Verify, document, close

**Files:**
- Create: `book/src/chronicle/<slug>.md` (chronicle entry)
- Create: `docs/retrospectives/2026-07-16-ecs-campaign-3-<slug>.md`
- Modify: `book/src/frontier/idea-registry.md` (UNI-22 campaign-3 progress note); any stale chapter naming `SpeciesDef` (freshness sweep)

**Interfaces:** none (documentation + gate).

- [ ] **Step 1: Full gate.** Run `make gate` (fmt + clippy + nextest + doctests). Expected: green modulo the standing deferred-AWS census reds. Confirm the census red *count* is unchanged from main (a byte-identical refactor adds none).

- [ ] **Step 2: Byte-identity attestation.** Regenerate the committed artifacts per the CI "Artifacts are current" step and confirm no diff:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- lab run studies/the-census.study.json
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

- [ ] **Step 3: Book freshness sweep.** Grep the book for `SpeciesDef` / "god-struct" / peopled-struct language and update any chapter that describes the old shape to the component-registry reality (`grep -rn 'SpeciesDef' book/src`). The species chapter's model card and any ECS chronicle cross-refs.

- [ ] **Step 4: Chronicle + retrospective.** Write the chronicle entry (the dissolution as story: a kind became a join, not a record; speech moved to its owning domain; byte-identical) and the one-page retrospective (process lessons — include the ELL5-labels lesson from ledger #57, and the strangler-fig type-relocation ordering). Add the UNI-22 campaign-3 progress note to the idea registry.

- [ ] **Step 5: Update the decision ledger + memory, commit the close.**

```bash
cargo fmt
git add -A
git commit -m "docs(ecs-c3): close — chronicle, retrospective, UNI-22 note, freshness sweep

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

- [ ] **Step 6: Present the G6 package to Nathan** (merge/close hard stop): the post-G3 ledger digest (save-format/determinism entries lead), the byte-identity attestation, the census-red status, and the merge mechanics. Then run `closing-a-campaign`.

---

## Self-Review

**Spec coverage:** ComponentStore substrate (Task 1 ← §3.1); seven registries (Tasks 2, 4 ← §3.2); WorldComponents + referential integrity (Tasks 3, 5 ← §3.3, §2); speech relocation incl. type move (Tasks 4–5 ← §3.2); god-struct deletion + genesis over entity set (Task 6 ← §3.3, §6 stage 5); byte-identity + equivalence-shadow + coherence tests (every task ← §4, §5); close (Task 7 ← §6 stage 5). The five spec stages (§6) map to Tasks 1 / 2–3 / (part of 3) / 4–5 / 6–7. All covered.

**Placeholder scan:** the `/* copy from species */` fills in Task 4 are deliberate — the exact numeric values are in `domains/species/src/lib.rs` (four peopled literals + `family_registry`) and reproducing 40+ constants inline would invite transcription error; the Step-1 equivalence test is the mechanical proof they were copied correctly. Every other step has concrete code or an exact command.

**Type consistency:** `ComponentStore` ops (`get`/`insert`/`contains`/`iter`/`ids`/`len`) used identically across Tasks 1–6. `WorldComponents` field names (`biosphere`/`psyche`/`perception`/`family_of`/`articulation`/`lexicon`/`family_proto`) consistent Tasks 3→5→6. `BuildError::MalformedKind(String)` defined Task 3, used Tasks 3/5. `envelope_of` retyped to `&hornvale_language::ArticulationVector` in Task 5, consistent with the Task-4 type move.

**Known risk flagged inline:** the articulation-*fact* commits (Task 5 Step 3) are the one non-mechanical site — genesis facts must stay byte-identical while their source crate moves. The byte-identity check gates it.
