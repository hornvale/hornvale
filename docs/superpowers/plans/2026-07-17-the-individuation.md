# The Individuation (ECS Campaign 5) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the instance half of the entity-component substrate: `NonZeroU64` `EntityId`s, canonical `instance-of` facts with a latest-wins `kind_of` read, the per-call `instance_biosphere` prototype-inheritance lens, the first four non-species kinds, and the keystone kind⋈instance query — with every shipped world's ledger byte-identical.

**Architecture:** Kernel gets the domain-agnostic mechanisms (the predicate, mint/change/read APIs, the by-label store read); domains own the new component types; worldgen composes (union roster, validated mint). No shipped world mints an instance: the only serialized drift is additive concept-registry rows, regenerated in the same commits that register them. Spec: `docs/superpowers/specs/2026-07-17-instance-join-ecs-campaign-5-design.md` (approved at G3).

**Tech Stack:** Rust edition 2024, std + serde/serde_json only. `cargo nextest` for tests.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (clippy-enforced).
- **No new dependencies** — serde + serde_json workspace allowlist.
- **No wall-clock time**; no new stream draws (minting consumes no randomness — do not touch any `streams` module).
- **Every public item gets a one-line doc comment** (`#![warn(missing_docs)]`) **and a `type-audit:` verdict tag on every primitive at a pub boundary** — type-audit now runs in `make gate`.
- **`cargo fmt` as the final step before every commit**; clippy `-D warnings`.
- **Never run the live census locally.** Local artifact freshness is `scripts/regenerate-artifacts.sh` (census-skipped by default) + `scripts/freeze-fixture.sh` for the living byte-goldens. Any step naming `the-census.study.json` for a local run is a plan bug.
- **Byte-identity contract (spec §3):** shipped-world ledgers unchanged; only registry rows drift; re-baseline fixtures **in the drifting commit**, never at close.
- **Mutation-verify every new test** before counting it done: break the code the test guards (swap override precedence, drop a sort, etc.), see RED, restore, see GREEN. Say so in the task report.
- Worktree: `.claude/worktrees/the-individuation`, branch `worktree-the-individuation`. All commands run from the worktree root.

---

### Task 0: Baseline gate in the cold worktree

**Files:** none modified.

**Interfaces:** Produces: a pinned GREEN baseline (`/tmp/hv-c5-baseline.txt`) and confirmation the branch base is current with main.

- [ ] **Step 1: Preflight against main**

Run: `make preflight`
Expected: GO (base `37ae360` or later; if main moved, merge main INTO `worktree-the-individuation`, re-run, and note the absorption in the task report).

- [ ] **Step 2: Full baseline suite (cold build ~10 min)**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-baseline.txt`
Expected: exit 0, all tests pass. If anything is red, STOP and report — do not proceed on a red baseline; diff against main's known-green state.

- [ ] **Step 3: fmt + clippy baseline**

Run: `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: clean. No commit (nothing changed).

---

### Task 1: `EntityId` becomes `NonZeroU64`

**Files:**
- Modify: `kernel/src/ledger.rs` (the `EntityId` struct, `mint_entity`, `max_entity_id`, tests)
- Modify: `kernel/src/fact_index.rs` (range sentinels)
- Modify: `domains/astronomy/src/provider.rs` (two test observer dummies)
- Modify: any other of the 40 `EntityId(` sites the compiler flags (14 files total; let `cargo check --workspace` enumerate them)

**Interfaces:**
- Produces: `EntityId(pub std::num::NonZeroU64)`; `EntityId::new(raw: u64) -> Option<EntityId>` (const); `EntityId::get(self) -> u64` (const); `pub(crate) EntityId::MIN` / `EntityId::MAX` (crate-internal sentinels). Serialized form unchanged (a bare JSON number ≥ 1).

- [ ] **Step 1: Write the failing tests** (in `kernel/src/ledger.rs` `tests`)

```rust
#[test]
fn option_entity_id_is_niche_packed() {
    // The c4-deferred perf contract: the NonZeroU64 niche halves Option<EntityId>.
    assert_eq!(std::mem::size_of::<Option<EntityId>>(), 8);
}

#[test]
fn entity_id_zero_is_unrepresentable() {
    assert!(EntityId::new(0).is_none());
    assert_eq!(EntityId::new(7).unwrap().get(), 7);
    // A forged 0 in a save now fails loudly at deserialize.
    assert!(serde_json::from_str::<EntityId>("0").is_err());
    assert_eq!(
        serde_json::from_str::<EntityId>("7").unwrap(),
        EntityId::new(7).unwrap()
    );
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-kernel option_entity_id_is_niche_packed`
Expected: FAIL — `size_of` is 16 today (and `EntityId::new` does not exist → compile error first; that counts as the red).

- [ ] **Step 3: Change the type and constructors**

In `kernel/src/ledger.rs`:

```rust
use std::num::NonZeroU64;

/// Opaque entity handle. Minted by the ledger, never reused. `NonZeroU64`:
/// 0 has always been reserved as "never valid", so the niche is free and
/// `Option<EntityId>` is 8 bytes. Serializes as the bare number.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct EntityId(pub NonZeroU64);

impl EntityId {
    /// Construct from a raw id; `None` for the reserved 0.
    /// type-audit: bare-ok(constructor-edge)
    pub const fn new(raw: u64) -> Option<EntityId> {
        match NonZeroU64::new(raw) {
            Some(n) => Some(EntityId(n)),
            None => None,
        }
    }
    /// The raw id value.
    /// type-audit: bare-ok(constructor-edge: return)
    pub const fn get(self) -> u64 {
        self.0.get()
    }
    /// Smallest valid id (1) — index range sentinel.
    pub(crate) const MIN: EntityId = EntityId(NonZeroU64::MIN);
    /// Largest valid id — index range sentinel.
    pub(crate) const MAX: EntityId = EntityId(NonZeroU64::MAX);
}
```

`mint_entity` becomes:

```rust
    /// Mint a fresh entity id. Ids start at 1; 0 is reserved as "never valid".
    pub fn mint_entity(&mut self) -> EntityId {
        self.next_entity += 1;
        EntityId(NonZeroU64::new(self.next_entity).expect("next_entity starts at 0 and only increments"))
    }
```

`max_entity_id` swaps `.0` for `.get()`:

```rust
        self.facts
            .iter()
            .flat_map(|f| {
                let object_id = match f.object {
                    Value::Entity(e) => Some(e.get()),
                    _ => None,
                };
                [Some(f.subject.get()), object_id, f.place.map(|p| p.get())]
            })
            .flatten()
            .max()
            .unwrap_or(0)
```

(`next_entity: u64` stays as-is — same serialized field.)

- [ ] **Step 4: Rework the index sentinels**

In `kernel/src/fact_index.rs`: `ObjKey::MIN` becomes
`ObjKey(Value::Entity(EntityId::MIN))`; the two `range(...)` calls replace
`EntityId(0)` → `EntityId::MIN` and `EntityId(u64::MAX)` → `EntityId::MAX`.
All ranges are inclusive (`..=`), so MIN = 1 excludes no real id.

- [ ] **Step 5: Chase the compiler through the remaining sites**

Run: `cargo check --workspace 2>&1 | tee /tmp/hv-c5-t1-check.txt`
Fix every error: `EntityId(n)` literals in tests → `EntityId::new(n).unwrap()`; the two astronomy `EntityId(0)` observer dummies → `EntityId::new(1).unwrap()`; `.0` reads used as `u64` → `.get()` (Display sites like `subject.0` may stay — `NonZeroU64` implements `Display`). Do NOT change any serialized struct's shape.

- [ ] **Step 6: Full test pass + byte-identity spot check**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-t1.txt`
Expected: exit 0. `cli/tests/lens_purity.rs` and `cli/tests/branches_identity.rs` passing IS the byte-identity evidence (both deserialize committed world fixtures and re-derive).

- [ ] **Step 7: fmt, clippy, commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "refactor(kernel): EntityId is NonZeroU64 — Option<EntityId> 16->8B (c4 followup, ecs-c5 T1)"
```

---

### Task 2: `instance-of` — mint, change, read

**Files:**
- Modify: `kernel/src/world.rs` (the predicate const + registration in `World::new`)
- Modify: `kernel/src/ledger.rs` (`mint_instance`, `change_kind`, `kind_of`, `latest_value_of` + tests)
- Modify: `kernel/src/lib.rs` (re-export `INSTANCE_OF` beside `NAME`)
- Modify: `windows/worldgen/tests/species_worlds.rs` (the genesis zero-instances pin)
- Modify (regen): `cli/tests/fixtures/world-seed-42.json`, `book/src/reference/concept-registry-generated.md` (+ anything else the regen scripts touch)

**Interfaces:**
- Consumes: Task 1's `EntityId`.
- Produces: `hornvale_kernel::INSTANCE_OF: &str = "instance-of"` (non-functional, registered in `World::new`); `Ledger::mint_instance(&mut self, kind_label: &str, day: Option<f64>, provenance: &str, registry: &ConceptRegistry) -> Result<EntityId, LedgerError>`; `Ledger::change_kind(&mut self, e: EntityId, kind_label: &str, day: Option<f64>, provenance: &str, registry: &ConceptRegistry) -> Result<(), LedgerError>`; `Ledger::kind_of(&self, e: EntityId) -> Option<&str>` (latest-wins); `Ledger::latest_value_of(&self, e: EntityId, predicate: &str) -> Option<&Value>` (latest-wins general read).

- [ ] **Step 1: Write the failing kernel tests** (in `kernel/src/ledger.rs` `tests`)

```rust
#[test]
fn mint_instance_commits_an_instance_of_fact() {
    let mut w = crate::World::new(crate::Seed(1));
    let e = w
        .ledger
        .mint_instance("owlbear", Some(0.0), "test", &w.registry)
        .unwrap();
    assert_eq!(w.ledger.kind_of(e), Some("owlbear"));
    assert_eq!(w.ledger.find(crate::INSTANCE_OF).count(), 1);
}

#[test]
fn kind_change_is_a_fact_and_kind_of_is_latest_wins() {
    let mut w = crate::World::new(crate::Seed(1));
    let e = w
        .ledger
        .mint_instance("owlbear", Some(0.0), "test", &w.registry)
        .unwrap();
    w.ledger
        .change_kind(e, "awakened-owlbear", Some(12.5), "test: the awakening", &w.registry)
        .unwrap();
    // Current kind is the LATEST fact (contrast value_of's first-wins).
    assert_eq!(w.ledger.kind_of(e), Some("awakened-owlbear"));
    // The history is the ledger's native state machine: both transitions
    // survive, in commit order, day-stamped.
    let history: Vec<&Value> = w
        .ledger
        .facts_about(e)
        .filter(|f| f.predicate == crate::INSTANCE_OF)
        .map(|f| &f.object)
        .collect();
    assert_eq!(
        history,
        vec![
            &Value::Text("owlbear".to_string()),
            &Value::Text("awakened-owlbear".to_string())
        ]
    );
}

#[test]
fn latest_value_of_returns_the_last_committed_value() {
    let mut w = crate::World::new(crate::Seed(1));
    let e = w
        .ledger
        .mint_instance("owlbear", None, "test", &w.registry)
        .unwrap();
    w.ledger
        .change_kind(e, "corpse", None, "test", &w.registry)
        .unwrap();
    assert_eq!(
        w.ledger.latest_value_of(e, crate::INSTANCE_OF),
        Some(&Value::Text("corpse".to_string()))
    );
    assert_eq!(w.ledger.latest_value_of(e, "name"), None);
}

#[test]
fn kind_of_survives_serialization_roundtrip() {
    let mut w = crate::World::new(crate::Seed(1));
    let e = w
        .ledger
        .mint_instance("granite", None, "test", &w.registry)
        .unwrap();
    let json = serde_json::to_string(&w.ledger).unwrap();
    let l2: Ledger = serde_json::from_str(&json).unwrap();
    // Exercises the lazy index rebuild path on a fresh deserialize.
    assert_eq!(l2.kind_of(e), Some("granite"));
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-kernel mint_instance`
Expected: compile error — `INSTANCE_OF` / `mint_instance` not found. That is the red.

- [ ] **Step 3: Implement**

`kernel/src/world.rs`, beside `NAME`:

```rust
/// The kind an entity is an instance of (object: `Value::Text` kind label).
/// NON-functional: a kind can change over sim time (awakened beast, corpse,
/// lich); each change is a new day-stamped fact and the CURRENT kind is the
/// latest one (`Ledger::kind_of`). Kind references serialize as labels,
/// never positions (metaplan §7).
pub const INSTANCE_OF: &str = "instance-of";
```

In `World::new`, after the `NAME` registration:

```rust
        registry
            .register_predicate(
                INSTANCE_OF,
                false,
                "the kind an entity is an instance of; the latest fact is its current kind",
            )
            .expect("core concept registration cannot conflict in an empty registry");
```

`kernel/src/ledger.rs`:

```rust
    /// Mint a fresh entity and commit its `instance-of` fact in one
    /// operation — the sole writer of the predicate (single-writer by
    /// construction). The kernel is roster-blind: label validation is the
    /// composition root's job (worldgen).
    /// type-audit: bare-ok(identifier-text: kind_label), waiver(decision-0014: day), bare-ok(prose: provenance)
    pub fn mint_instance(
        &mut self,
        kind_label: &str,
        day: Option<f64>,
        provenance: &str,
        registry: &ConceptRegistry,
    ) -> Result<EntityId, LedgerError> {
        let e = self.mint_entity();
        self.commit(
            Fact {
                subject: e,
                predicate: crate::world::INSTANCE_OF.to_string(),
                object: Value::Text(kind_label.to_string()),
                place: None,
                day,
                provenance: provenance.to_string(),
            },
            registry,
        )?;
        Ok(e)
    }

    /// Commit a kind-change fact for an existing entity (owlbear ->
    /// awakened-owlbear). Appends; never edits. No transition constraints
    /// here — guards ride the c6 capability schema.
    /// type-audit: bare-ok(identifier-text: kind_label), waiver(decision-0014: day), bare-ok(prose: provenance)
    pub fn change_kind(
        &mut self,
        e: EntityId,
        kind_label: &str,
        day: Option<f64>,
        provenance: &str,
        registry: &ConceptRegistry,
    ) -> Result<(), LedgerError> {
        self.commit(
            Fact {
                subject: e,
                predicate: crate::world::INSTANCE_OF.to_string(),
                object: Value::Text(kind_label.to_string()),
                place: None,
                day,
                provenance: provenance.to_string(),
            },
            registry,
        )
        .map(|_| ())
    }

    /// The LAST committed object for (subject, predicate) — the read for
    /// sim-mutable non-functional predicates, where commit order is time
    /// order. Contrast `value_of` (first object; the functional read).
    /// type-audit: bare-ok(identifier-text: predicate)
    pub fn latest_value_of(&self, e: EntityId, predicate: &str) -> Option<&Value> {
        self.facts_about(e)
            .filter(|f| f.predicate == predicate)
            .last()
            .map(|f| &f.object)
    }

    /// The entity's current kind label: the latest `instance-of` fact.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn kind_of(&self, e: EntityId) -> Option<&str> {
        match self.latest_value_of(e, crate::world::INSTANCE_OF) {
            Some(Value::Text(t)) => Some(t.as_str()),
            _ => None,
        }
    }
```

`kernel/src/lib.rs`: add `INSTANCE_OF` to the `world` re-export (beside `NAME` / `World`).

- [ ] **Step 4: Run kernel tests**

Run: `cargo test -p hornvale-kernel`
Expected: PASS (all, including the four new ones).

- [ ] **Step 5: The genesis zero-instances pin** (append to `windows/worldgen/tests/species_worlds.rs`)

```rust
#[test]
fn genesis_commits_no_instance_of_facts() {
    // Spec §3 (the shadow contract): no shipped world mints an instance this
    // campaign. The mechanism exists; the cutover is a later campaign's
    // deliberate, drift-owning decision. If this test reddens, someone wired
    // instance minting into genesis — that is a save-format event, not a bug
    // fix. Talk to Nathan first.
    let w = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    assert_eq!(w.ledger.find(hornvale_kernel::INSTANCE_OF).count(), 0);
}
```

(Match the file's existing imports; `exposure.rs` shows the exact `build_world` idiom.)

- [ ] **Step 6: Run it**

Run: `cargo test -p hornvale-worldgen --test species_worlds genesis_commits_no_instance_of_facts`
Expected: PASS. Mutation-verify: temporarily commit an `instance-of` fact in genesis (any `mint_instance` call), see RED, revert, see GREEN.

- [ ] **Step 7: Re-baseline the registry-bearing artifacts (in THIS commit)**

The new predicate row changes every serialized `World`. Regenerate:

```bash
bash scripts/regenerate-artifacts.sh
bash scripts/freeze-fixture.sh
git status --porcelain
```

Expected drift: `book/src/reference/concept-registry-generated.md` (+ the
`instance-of` row) and `cli/tests/fixtures/world-seed-42.json` (registry
block only). **The three seed-42 almanacs and every gallery/laboratory
artifact must NOT drift** — they are ledger-derived, and the ledger is
unchanged. If an almanac or census file moves, STOP: that is a byte-identity
violation, not regen noise. Inspect the `world-seed-42.json` diff and
confirm it touches only the registry section.

- [ ] **Step 8: Full suite, fmt, clippy, commit**

```bash
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-t2.txt
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(kernel): canonical instance-of facts — mint_instance/change_kind/kind_of, latest-wins reads (ecs-c5 T2)"
```

---

### Task 3: The prototype join — override predicates + `instance_biosphere` + JOIN≡SCAN

**Files:**
- Modify: `kernel/src/component.rs` (`get_by_label` on `ComponentStore<KindId, C>`)
- Modify: `domains/species/src/lib.rs` (two predicate consts, registration, the lens)
- Create: `domains/species/tests/instance_lens.rs` (the JOIN≡SCAN battery)
- Modify (regen): the same two registry-bearing artifacts as Task 2

**Interfaces:**
- Consumes: Task 2's `kind_of` / `latest_value_of` / `mint_instance` / `change_kind`.
- Produces: `hornvale_species::SPECIES_MASS_KG: &str = "species-mass-kg"`; `hornvale_species::SPECIES_POTENCY: &str = "species-potency"` (both registered NON-functional — sim-mutable, latest-wins, consistent with `instance-of`'s temporality); `hornvale_species::instance_biosphere(&Ledger, EntityId, &ComponentStore<KindId, BiosphereTraits>) -> Option<BiosphereTraits>`; `ComponentStore::<KindId, C>::get_by_label(&self, label: &str) -> Option<&C>`.

- [ ] **Step 1: Write the failing unit tests** (in `domains/species/tests/instance_lens.rs`)

```rust
//! The prototype-inheritance join: an instance's effective trait is its own
//! (latest) override fact, else its kind's authored registry default. The
//! keystone property is JOIN == SCAN: the lens over the index equals a naive
//! linear recomputation, over seeded random ledgers (generator coverage is
//! part of the review — the c4 signed-zero lesson).

use hornvale_kernel::{EntityId, KindId, Seed, Value, World};
use hornvale_species::{
    BiosphereTraits, SPECIES_MASS_KG, SPECIES_POTENCY, biosphere_registry, instance_biosphere,
};

fn world() -> World {
    let mut w = World::new(Seed(7));
    hornvale_species::register_concepts(&mut w.registry).unwrap();
    w
}

fn override_fact(e: EntityId, pred: &str, n: f64) -> hornvale_kernel::Fact {
    hornvale_kernel::Fact {
        subject: e,
        predicate: pred.to_string(),
        object: Value::Number(n),
        place: None,
        day: Some(1.0),
        provenance: "test".to_string(),
    }
}

#[test]
fn lens_returns_kind_defaults_when_no_overrides() {
    let mut w = world();
    let reg = biosphere_registry();
    let e = w.ledger.mint_instance("owlbear", None, "test", &w.registry).unwrap();
    let eff = instance_biosphere(&w.ledger, e, &reg).unwrap();
    let authored = reg.get_by_label("owlbear").unwrap();
    assert_eq!(eff.mass.kilograms(), authored.mass.kilograms());
    assert_eq!(eff.potency, authored.potency);
}

#[test]
fn override_beats_default_and_latest_override_wins() {
    let mut w = world();
    let reg = biosphere_registry();
    let e = w.ledger.mint_instance("owlbear", None, "test", &w.registry).unwrap();
    w.ledger.commit(override_fact(e, SPECIES_MASS_KG, 900.0), &w.registry).unwrap();
    w.ledger.commit(override_fact(e, SPECIES_MASS_KG, 950.0), &w.registry).unwrap();
    let eff = instance_biosphere(&w.ledger, e, &reg).unwrap();
    assert_eq!(eff.mass.kilograms(), 950.0, "latest override wins");
    // Untouched fields keep the authored default.
    assert_eq!(eff.potency, reg.get_by_label("owlbear").unwrap().potency);
}

#[test]
fn overrides_survive_kind_change() {
    // The spec §4.3 contract: overrides are per-instance, not
    // per-(instance, kind) — the unusually large owlbear stays large when
    // its kind changes.
    let mut w = world();
    let reg = biosphere_registry();
    let e = w.ledger.mint_instance("owlbear", None, "test", &w.registry).unwrap();
    w.ledger.commit(override_fact(e, SPECIES_MASS_KG, 900.0), &w.registry).unwrap();
    w.ledger.change_kind(e, "woolly-mammoth", Some(2.0), "test", &w.registry).unwrap();
    let eff = instance_biosphere(&w.ledger, e, &reg).unwrap();
    assert_eq!(eff.mass.kilograms(), 900.0, "override outlives the transition");
    // Non-overridden fields come from the NEW kind.
    assert_eq!(
        eff.potency,
        reg.get_by_label("woolly-mammoth").unwrap().potency
    );
}

#[test]
fn lens_is_total_never_panicking() {
    let mut w = world();
    let reg = biosphere_registry();
    // No instance-of fact at all -> None.
    let bare = w.ledger.mint_entity();
    assert!(instance_biosphere(&w.ledger, bare, &reg).is_none());
    // Dangling label -> None.
    let dangling = w.ledger.mint_instance("no-such-kind", None, "test", &w.registry).unwrap();
    assert!(instance_biosphere(&w.ledger, dangling, &reg).is_none());
    // Physically invalid override (negative mass) -> None, loudly absent
    // rather than silently defaulted.
    let bad = w.ledger.mint_instance("owlbear", None, "test", &w.registry).unwrap();
    w.ledger.commit(override_fact(bad, SPECIES_MASS_KG, -5.0), &w.registry).unwrap();
    assert!(instance_biosphere(&w.ledger, bad, &reg).is_none());
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-species --test instance_lens`
Expected: compile error (`SPECIES_MASS_KG`, `instance_biosphere`, `get_by_label` not found).

- [ ] **Step 3: Implement**

`kernel/src/component.rs` (new impl block; `KindId` is `&'static str`-backed, so a runtime label cannot construct a key — compare by content; O(n) over small rosters is fine):

```rust
impl<C> ComponentStore<KindId, C> {
    /// The component of the kind whose label equals `label`, if present.
    /// Runtime labels (e.g. from ledger text) cannot construct a `KindId`
    /// key, so this compares label content; rosters are small.
    /// type-audit: bare-ok(identifier-text: label)
    pub fn get_by_label(&self, label: &str) -> Option<&C> {
        self.iter().find(|(k, _)| k.0 == label).map(|(_, c)| c)
    }
}
```

`domains/species/src/lib.rs` — the consts beside `SPECIES_NAME`:

```rust
/// Body mass in kilograms — a level-agnostic trait predicate: the subject
/// may be a kind-representative entity or an instance (the instance fact is
/// the prototype-inheritance override). Non-functional: sim-mutable, the
/// latest fact wins (`Ledger::latest_value_of`).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_MASS_KG: &str = "species-mass-kg";
/// Magical potency override — level-agnostic, non-functional, latest-wins
/// (see `SPECIES_MASS_KG`).
/// type-audit: bare-ok(identifier-text)
pub const SPECIES_POTENCY: &str = "species-potency";
```

In `register_concepts`, with the existing registrations:

```rust
    registry.register_predicate(SPECIES_MASS_KG, false, "body mass in kilograms (latest wins)")?;
    registry.register_predicate(SPECIES_POTENCY, false, "magical potency (latest wins)")?;
```

The lens, near `species_entity`:

```rust
/// The instance-component lens (spec §4.3): the effective `BiosphereTraits`
/// of an instance — its (latest) numeric override facts applied over its
/// current kind's authored registry default. Materialized per call; derived,
/// never serialized, never cached (the tick cache is c6). Total: `None` for
/// a kindless entity, a dangling label, or a physically invalid override.
pub fn instance_biosphere(
    ledger: &Ledger,
    e: EntityId,
    biosphere: &ComponentStore<KindId, BiosphereTraits>,
) -> Option<BiosphereTraits> {
    let label = ledger.kind_of(e)?;
    let mut traits = biosphere.get_by_label(label)?.clone();
    if let Some(Value::Number(m)) = ledger.latest_value_of(e, SPECIES_MASS_KG) {
        traits.mass = Mass::new(*m).ok()?;
    }
    if let Some(Value::Number(p)) = ledger.latest_value_of(e, SPECIES_POTENCY) {
        if !p.is_finite() || *p < 0.0 {
            return None;
        }
        traits.potency = *p;
    }
    Some(traits)
}
```

(Adjust imports: `Ledger`, `ComponentStore`, `Mass` are already in scope in this crate; add whatever the compiler asks for.)

- [ ] **Step 4: Run the unit tests**

Run: `cargo test -p hornvale-species --test instance_lens`
Expected: PASS (all four). Mutation-verify: swap the override/default precedence in the lens (apply defaults last), see `override_beats_default_and_latest_override_wins` go RED; restore.

- [ ] **Step 5: Add the JOIN≡SCAN property battery** (append to `instance_lens.rs`)

```rust
// Tiny deterministic PRNG — splitmix64, the kernel's own test idiom.
fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E3779B97F4A7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
    z ^ (z >> 31)
}

/// The naive oracle: recompute the effective traits by a LINEAR scan of the
/// whole ledger (no index, no kernel reads) + the registry — the definition
/// the lens must refine.
fn naive_effective(
    w: &World,
    e: EntityId,
    reg: &hornvale_kernel::ComponentStore<KindId, BiosphereTraits>,
) -> Option<BiosphereTraits> {
    let mut kind: Option<String> = None;
    let mut mass: Option<f64> = None;
    let mut potency: Option<f64> = None;
    for f in w.ledger.iter() {
        if f.subject != e {
            continue;
        }
        match (f.predicate.as_str(), &f.object) {
            ("instance-of", Value::Text(t)) => kind = Some(t.clone()),
            (p, Value::Number(n)) if p == SPECIES_MASS_KG => mass = Some(*n),
            (p, Value::Number(n)) if p == SPECIES_POTENCY => potency = Some(*n),
            _ => {}
        }
    }
    let mut t = reg.get_by_label(kind.as_deref()?)?.clone();
    if let Some(m) = mass {
        t.mass = hornvale_kernel::Mass::new(m).ok()?;
    }
    if let Some(p) = potency {
        if !p.is_finite() || p < 0.0 {
            return None;
        }
        t.potency = p;
    }
    Some(t)
}

#[test]
fn join_equals_scan_over_random_ledgers() {
    // Generator coverage (spec §5.2, the c4 lesson): kind changes 0/1/2+,
    // overrides before AND after changes, both zero spellings, dangling
    // labels, kindless entities.
    let reg = biosphere_registry();
    let labels = ["owlbear", "woolly-mammoth", "giant-elk", "no-such-kind"];
    for seed in 0..64u64 {
        let mut w = world();
        let mut st = seed.wrapping_add(1);
        let mut entities: Vec<EntityId> = vec![w.ledger.mint_entity()]; // kindless
        for _ in 0..12 {
            let label = labels[(splitmix(&mut st) as usize) % labels.len()];
            entities.push(w.ledger.mint_instance(label, None, "gen", &w.registry).unwrap());
        }
        for _ in 0..120 {
            let e = entities[(splitmix(&mut st) as usize) % entities.len()];
            match splitmix(&mut st) % 4 {
                0 => {
                    let label = labels[(splitmix(&mut st) as usize) % labels.len()];
                    w.ledger.change_kind(e, label, None, "gen", &w.registry).unwrap();
                }
                1 => {
                    let m = match splitmix(&mut st) % 4 {
                        0 => 0.0,
                        1 => -0.0,
                        2 => -3.0, // invalid: lens and scan must agree on None
                        _ => (splitmix(&mut st) % 5000) as f64,
                    };
                    let _ = w.ledger.commit(override_fact(e, SPECIES_MASS_KG, m), &w.registry);
                }
                _ => {
                    let p = match splitmix(&mut st) % 3 {
                        0 => 0.0,
                        1 => -0.0,
                        _ => (splitmix(&mut st) % 100) as f64 / 100.0,
                    };
                    let _ = w.ledger.commit(override_fact(e, SPECIES_POTENCY, p), &w.registry);
                }
            }
        }
        for &e in &entities {
            let join = instance_biosphere(&w.ledger, e, &reg);
            let scan = naive_effective(&w, e, &reg);
            assert_eq!(join, scan, "seed {seed} entity {e:?}");
        }
    }
}
```

(If `BiosphereTraits` lacks `PartialEq`, derive it — it is plain data. If
`units::Mass` is not exported under that path, use the path the compiler
suggests; `hornvale_species` already imports it.)

- [ ] **Step 5b: The deterministic lens budget** (spec §6 — a drift-checked count, not wall time; append to `instance_lens.rs`)

```rust
#[test]
fn lens_demo_world_fact_count_is_pinned() {
    // The deterministic perf budget: the lens demonstration's fixed
    // construction commits exactly this many facts. Drift here means the
    // mint/override paths changed shape — re-derive the number deliberately.
    let mut w = world();
    let e = w.ledger.mint_instance("owlbear", None, "budget", &w.registry).unwrap();
    w.ledger.commit(override_fact(e, SPECIES_MASS_KG, 900.0), &w.registry).unwrap();
    w.ledger.change_kind(e, "woolly-mammoth", Some(2.0), "budget", &w.registry).unwrap();
    assert_eq!(w.ledger.len(), 3); // instance-of, override, kind-change
}
```

- [ ] **Step 6: Run the battery; mutation-verify the keystone**

Run: `cargo test -p hornvale-species --test instance_lens join_equals_scan`
Expected: PASS. Then three mutations, each must redden it: (a) in `kind_of`, replace `.last()` with `.next()` (first-wins) — RED; (b) in the lens, skip the override application — RED; (c) in the lens, apply the mass override without the `Mass::new` validity gate — RED (the −3.0 arm disagrees with the scan… verify it actually fires; if it does not, the generator is under-covering — fix the generator, not the assertion). Restore GREEN after each.

- [ ] **Step 7: Regen registry-bearing artifacts (this commit registers two predicates)**

```bash
bash scripts/regenerate-artifacts.sh && bash scripts/freeze-fixture.sh && git status --porcelain
```

Expected drift: the concepts page + `world-seed-42.json` registry block only, exactly as Task 2. Almanacs/census untouched or STOP.

- [ ] **Step 8: Full suite, fmt, clippy, commit**

```bash
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-t3.txt
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(species): prototype-inheritance join — instance_biosphere lens + level-agnostic override predicates + JOIN==SCAN battery (ecs-c5 T3)"
```

---

### Task 4: The non-species kinds — components, union roster, validated mint

**Files:**
- Modify: `domains/religion/src/lib.rs` (`DeityTraits` + `deity_registry`)
- Modify: `domains/culture/src/lib.rs` (`Transmission`, `CultureTraits` + `culture_registry`)
- Modify: `domains/terrain/src/lib.rs` (`MaterialTraits` + `material_registry`; put them in the crate root or a new `materials` module matching the crate's layout)
- Modify: `windows/worldgen/src/components.rs` (three stores, `ComponentTag` variants, `kinds()`, doc update)
- Modify: `windows/worldgen/src/lib.rs` (`mint_instance_of_kind`)
- Modify: `windows/lab/src/roster.rs` (the `from_stores` call sites gain three empty stores)

**Interfaces:**
- Consumes: Tasks 2–3.
- Produces: `hornvale_religion::{DeityTraits { pub manifest: bool }, deity_registry() -> ComponentStore<KindId, DeityTraits>}`; `hornvale_culture::{Transmission::{Oral, Written}, CultureTraits { pub transmission: Transmission }, culture_registry()}`; `hornvale_terrain::{MaterialTraits { pub hardness_mohs: f64, pub sedimentary: bool }, material_registry()}` (rows: `granite` {6.5, false}, `limestone` {3.0, true}); `WorldComponents { pub deity, pub culture, pub material, … }`; `ComponentTag::{Deity, Culture, Material}`; `WorldComponents::kinds() -> Vec<KindId>` (union of all stores' key-sets, ascending); `hornvale_worldgen::mint_instance_of_kind(&mut World, &WorldComponents, kind: &str, day: Option<f64>, provenance: &str) -> Result<EntityId, BuildError>`; `WorldComponents::from_stores` now takes ten stores (…existing seven…, deity, culture, material).

- [ ] **Step 1: Write the failing tests** (append to `windows/worldgen/src/components.rs` `tests` and `windows/worldgen/tests/species_worlds.rs`)

In `components.rs` tests:

```rust
#[test]
fn the_kind_roster_is_the_union_of_all_stores() {
    // Spec §4.4: "the biosphere store is the canonical entity set" is
    // retired — deity/culture/material kinds carry no biosphere row.
    let wc = WorldComponents::assemble().unwrap();
    let kinds = wc.kinds();
    for label in ["deity", "culture", "granite", "limestone", "owlbear", "goblin"] {
        assert!(
            kinds.iter().any(|k| k.0 == label),
            "union roster must contain {label}"
        );
    }
    // Non-species kinds are NOT in the biosphere store (the genesis /
    // placement constraint): genesis must not mint them as species.
    for label in ["deity", "culture", "granite", "limestone"] {
        assert!(wc.biosphere.get_by_label(label).is_none());
    }
    // Ascending order (BTree-backed union).
    let mut sorted = kinds.clone();
    sorted.sort();
    assert_eq!(kinds, sorted);
}

#[test]
fn kinds_with_covers_the_new_component_tags() {
    let wc = WorldComponents::assemble().unwrap();
    assert_eq!(wc.kinds_with(ComponentTag::Deity), vec![KindId("deity")]);
    assert_eq!(wc.kinds_with(ComponentTag::Culture), vec![KindId("culture")]);
    assert_eq!(
        wc.kinds_with(ComponentTag::Material),
        vec![KindId("granite"), KindId("limestone")]
    );
}
```

In `species_worlds.rs` (validated mint — an integration concern):

```rust
#[test]
fn minting_validates_the_kind_against_the_union_roster() {
    let wc = hornvale_worldgen::WorldComponents::assemble().unwrap();
    let mut w = hornvale_kernel::World::new(hornvale_kernel::Seed(9));
    // A canonical non-species kind mints fine…
    let d = hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "deity", Some(0.0), "test").unwrap();
    assert_eq!(w.ledger.kind_of(d), Some("deity"));
    // …an unknown kind fails loudly with the physical reason.
    let err = hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "tarrasque", None, "test");
    assert!(err.is_err(), "unknown kind must be rejected: {err:?}");
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen the_kind_roster 2>&1 | head -20`
Expected: compile errors (`kinds`, `ComponentTag::Deity`, registries missing).

- [ ] **Step 3: Implement the three domain components**

`domains/religion/src/lib.rs`:

```rust
/// Authored kind-level traits of deity kinds — the religion-owned component.
/// Deliberately thin (spec §4.4): enrichment is content work for the
/// campaign that ships deity instances in worlds.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DeityTraits {
    /// Whether the kind takes physical form in the world.
    /// type-audit: bare-ok(flag)
    pub manifest: bool,
}

/// The canonical deity-kind registry. One kind today: `deity`, unmanifest.
pub fn deity_registry() -> hornvale_kernel::ComponentStore<hornvale_kernel::KindId, DeityTraits> {
    [(hornvale_kernel::KindId("deity"), DeityTraits { manifest: false })]
        .into_iter()
        .collect()
}
```

`domains/culture/src/lib.rs`:

```rust
/// How a culture kind transmits itself between generations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Transmission {
    /// Spoken/performed transmission (the pre-writing default).
    Oral,
    /// Written transmission.
    Written,
}

/// Authored kind-level traits of culture kinds — the culture-owned
/// component (an aggregate kind: no body, no psyche). Deliberately thin
/// (spec §4.4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CultureTraits {
    /// The kind's transmission mode.
    pub transmission: Transmission,
}

/// The canonical culture-kind registry. One kind today: `culture`, oral.
pub fn culture_registry() -> hornvale_kernel::ComponentStore<hornvale_kernel::KindId, CultureTraits>
{
    [(
        hornvale_kernel::KindId("culture"),
        CultureTraits { transmission: Transmission::Oral },
    )]
    .into_iter()
    .collect()
}
```

`domains/terrain/src/lib.rs` (or its idiomatic module — follow the crate's layout, re-export at the root):

```rust
/// Authored traits of a material kind — body-stuff without agency, the
/// terrain-owned component. Fields are real, coarse physical scales
/// (spec §4.4: thin and honest).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MaterialTraits {
    /// Scratch hardness on the Mohs scale (1–10).
    /// type-audit: bare-ok(ratio: hardness_mohs)
    pub hardness_mohs: f64,
    /// Whether the material is sedimentary in origin.
    /// type-audit: bare-ok(flag)
    pub sedimentary: bool,
}

/// The canonical material-kind registry, mirroring the lithology classes
/// worlds already carry: granite (igneous, ~6.5 Mohs) and limestone
/// (sedimentary, ~3 Mohs).
pub fn material_registry() -> hornvale_kernel::ComponentStore<hornvale_kernel::KindId, MaterialTraits>
{
    [
        (hornvale_kernel::KindId("granite"), MaterialTraits { hardness_mohs: 6.5, sedimentary: false }),
        (hornvale_kernel::KindId("limestone"), MaterialTraits { hardness_mohs: 3.0, sedimentary: true }),
    ]
    .into_iter()
    .collect()
}
```

(`ComponentStore` implements `FromIterator` if `from_stores`-era code collects it; if not, build with `new()` + `insert` — match whatever `roster.rs` compiles with today.)

- [ ] **Step 4: Grow `WorldComponents`**

In `windows/worldgen/src/components.rs`: add the three fields (`pub deity: ComponentStore<KindId, hornvale_religion::DeityTraits>`, `pub culture: ComponentStore<KindId, hornvale_culture::CultureTraits>`, `pub material: ComponentStore<KindId, hornvale_terrain::MaterialTraits>`), populate them in `assemble()` from the three new registries, add the three parameters to `from_stores` (and thread them through `check_integrity`'s signature only if it needs them — it does not: no new cross-invariants), add the three `ComponentTag` variants + `kinds_with` arms, and:

```rust
    /// Every kind in the world: the union of all component stores' key-sets,
    /// ascending. The kind roster — NOT the biosphere store, which is only
    /// the set of kinds with bodies (genesis iterates biosphere for species
    /// entities; deity/culture/material kinds have no biosphere row).
    pub fn kinds(&self) -> Vec<KindId> {
        let mut all: std::collections::BTreeSet<KindId> = std::collections::BTreeSet::new();
        all.extend(self.biosphere.ids().copied());
        all.extend(self.psyche.ids().copied());
        all.extend(self.perception.ids().copied());
        all.extend(self.articulation.ids().copied());
        all.extend(self.lexicon.ids().copied());
        all.extend(self.family_proto.ids().copied());
        all.extend(self.family_of.ids().copied());
        all.extend(self.deity.ids().copied());
        all.extend(self.culture.ids().copied());
        all.extend(self.material.ids().copied());
        all.into_iter().collect()
    }
```

Update the module doc: "the biosphere store is the canonical entity set" →
"the biosphere store is the set of kinds with bodies; the kind roster is
`kinds()`, the union of every store's key-set."

Update the `from_stores` call sites in `windows/lab/src/roster.rs`: pass
`hornvale_kernel::ComponentStore::new()` (or `Default::default()`) for the
three new stores in every existing roster function.

- [ ] **Step 5: The validated mint** (in `windows/worldgen/src/lib.rs`, near `build_world`)

```rust
/// Mint an instance of a known kind: the composition root's validated entry
/// to `Ledger::mint_instance` (the kernel is roster-blind; spec §4.2). Fails
/// loudly when the label is not in the union kind roster.
pub fn mint_instance_of_kind(
    world: &mut World,
    wc: &WorldComponents,
    kind: &str,
    day: Option<f64>,
    provenance: &str,
) -> Result<EntityId, BuildError> {
    if !wc.kinds().iter().any(|k| k.0 == kind) {
        return Err(BuildError::MalformedKind(format!(
            "cannot mint an instance of unknown kind {kind:?} (not in the union roster)"
        )));
    }
    world
        .ledger
        .mint_instance(kind, day, provenance, &world.registry)
        .map_err(BuildError::Ledger)
}
```

(`BuildError::MalformedKind` exists; `BuildError::Ledger(LedgerError)` exists per the enum. `type-audit:` tags on the `&str`/`Option<f64>` params: `bare-ok(identifier-text: kind)`, `waiver(decision-0014: day)`, `bare-ok(prose: provenance)`.)

- [ ] **Step 6: Run the tests**

Run: `cargo test -p hornvale-worldgen 2>&1 | tail -20` then `cargo test -p hornvale-lab 2>&1 | tail -5`
Expected: PASS (including the pre-existing integrity suite — the new stores add no invariants — and lab's rosters with the widened `from_stores`).

- [ ] **Step 7: Genesis is untouched — verify**

Run: `cargo test -p hornvale-worldgen --test species_worlds`
Expected: PASS, including `genesis_commits_no_instance_of_facts` (nothing in this task runs at genesis; the registries are build-state and never serialized — no artifact regen needed this commit).

- [ ] **Step 8: fmt, clippy, full suite, commit**

```bash
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-t4.txt
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(worldgen,religion,culture,terrain): non-species kinds — DeityTraits/CultureTraits/MaterialTraits, union kind roster, validated mint (ecs-c5 T4)"
```

---

### Task 5: The awakened-owlbear roster and the transition arc

**Files:**
- Modify: `windows/lab/src/roster.rs` (`awakened_owlbear_components`)
- Create: `windows/lab/tests/individuation.rs` (transition + survival tests)

**Interfaces:**
- Consumes: everything above.
- Produces: `hornvale_lab::roster::awakened_owlbear_components() -> WorldComponents` — a two-kind test roster: `owlbear` (canonical biosphere row only) and `awakened-owlbear` (owlbear's biosphere with `potency: 0.6`, plus the full goblin-derived peopled cluster — check_integrity's psyche⇒perception/articulation/lexicon/family invariant is kept, not relaxed; singleton families `"owlbear"` / `"awakened-owlbear"` need no proto).

- [ ] **Step 1: Write the failing tests** (`windows/lab/tests/individuation.rs`)

```rust
//! The Individuation's composed demonstrations: a kind reached BY TRANSITION
//! (owlbear -> awakened-owlbear, the UNI-7/BIO-11 arc), with per-instance
//! overrides surviving the change — over a from_stores test roster, never
//! canonical (spec §4.4: genesis/placement must not see these kinds).

use hornvale_kernel::{Seed, Value, World};
use hornvale_lab::roster::awakened_owlbear_components;
use hornvale_species::{SPECIES_MASS_KG, instance_biosphere};

fn world() -> World {
    let mut w = World::new(Seed(11));
    hornvale_species::register_concepts(&mut w.registry).unwrap();
    w
}

#[test]
fn the_roster_passes_integrity_and_stays_out_of_the_canon() {
    let wc = awakened_owlbear_components();
    // from_stores ran check_integrity: the awakened kind carries the FULL
    // peopled cluster (it speaks).
    assert!(wc.psyche.get_by_label("awakened-owlbear").is_some());
    assert!(wc.biosphere.get_by_label("owlbear").is_some());
    // The canonical world knows neither the awakened kind…
    let canon = hornvale_worldgen::WorldComponents::assemble().unwrap();
    assert!(canon.biosphere.get_by_label("awakened-owlbear").is_none());
    // …and the awakened kind is mighty (potency > 0) while the beast is not.
    let beast = wc.biosphere.get_by_label("owlbear").unwrap();
    let awakened = wc.biosphere.get_by_label("awakened-owlbear").unwrap();
    assert_eq!(beast.potency, 0.0);
    assert!(awakened.potency > 0.0);
}

#[test]
fn awakening_is_a_fact_and_the_large_owlbear_stays_large() {
    let wc = awakened_owlbear_components();
    let mut w = world();
    let e = hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "owlbear", Some(0.0), "lab").unwrap();
    // An unusually large individual: a per-instance override.
    w.ledger
        .commit(
            hornvale_kernel::Fact {
                subject: e,
                predicate: SPECIES_MASS_KG.to_string(),
                object: Value::Number(900.0),
                place: None,
                day: Some(0.0),
                provenance: "lab".to_string(),
            },
            &w.registry,
        )
        .unwrap();
    // The awakening: a day-stamped kind-change fact, never a struct edit.
    w.ledger
        .change_kind(e, "awakened-owlbear", Some(40.0), "lab: the awakening", &w.registry)
        .unwrap();
    assert_eq!(w.ledger.kind_of(e), Some("awakened-owlbear"));
    let eff = instance_biosphere(&w.ledger, e, &wc.biosphere).unwrap();
    // Defaults now come from the NEW kind (mighty)…
    assert!(eff.potency > 0.0);
    // …and the instance override survives the transition (spec §4.3).
    assert_eq!(eff.mass.kilograms(), 900.0);
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-lab --test individuation`
Expected: compile error — `awakened_owlbear_components` not found.

- [ ] **Step 3: Implement the roster** (in `windows/lab/src/roster.rs`, following `goblin_derived`'s cloning idiom)

```rust
/// The Individuation's two-kind test roster: `owlbear` (the canonical beast,
/// biosphere only) and `awakened-owlbear` (the same body, `potency: 0.6`,
/// plus the full goblin-derived peopled cluster — an awakened beast speaks,
/// and check_integrity's peopled invariant is kept, not relaxed). Test/lab
/// only: canonical registries must never carry these rows (genesis would
/// mint and place them; spec §4.4).
pub fn awakened_owlbear_components() -> WorldComponents {
    let g = KindId("goblin");
    let beast = KindId("owlbear");
    let awakened = KindId("awakened-owlbear");
    let canon_bio = hornvale_species::biosphere_registry();
    let beast_traits = canon_bio.get(&beast).expect("the shipped owlbear has a biosphere row").clone();
    let mut awakened_traits = beast_traits.clone();
    awakened_traits.potency = 0.6;

    let mut biosphere: ComponentStore<KindId, _> = ComponentStore::new();
    biosphere.insert(beast, beast_traits);
    biosphere.insert(awakened, awakened_traits);

    let psyche: ComponentStore<KindId, _> = [(
        awakened,
        *hornvale_species::psyche_registry().get(&g).expect("the shipped goblin has a psyche row"),
    )]
    .into_iter()
    .collect();
    let perception: ComponentStore<KindId, _> = [(
        awakened,
        *hornvale_species::perception_registry().get(&g).expect("the shipped goblin has a perception row"),
    )]
    .into_iter()
    .collect();
    let articulation: ComponentStore<KindId, _> = [(
        awakened,
        *hornvale_language::articulation_registry().get(&g).expect("the shipped goblin has an articulation row"),
    )]
    .into_iter()
    .collect();
    let lexicon: ComponentStore<KindId, _> = [(
        awakened,
        hornvale_language::lexicon_registry().get(&g).expect("the shipped goblin has a lexicon row").clone(),
    )]
    .into_iter()
    .collect();
    // Singleton families need no family_proto entry.
    let family_of: ComponentStore<KindId, &'static str> =
        [(beast, "owlbear"), (awakened, "awakened-owlbear")].into_iter().collect();

    WorldComponents::from_stores(
        biosphere,
        psyche,
        perception,
        articulation,
        lexicon,
        hornvale_language::family_proto(),
        family_of,
        ComponentStore::new(), // deity
        ComponentStore::new(), // culture
        ComponentStore::new(), // material
    )
    .expect("the awakened-owlbear roster is well-formed")
}
```

(Match the file's existing imports and whether `BiosphereTraits` is `Copy` — use `.clone()` where the compiler demands. If the canonical owlbear's psyche-cluster types are `Copy` in the goblin rows, the `*` derefs mirror `goblin_derived`.)

- [ ] **Step 4: Run, mutation-verify, commit**

Run: `cargo test -p hornvale-lab --test individuation`
Expected: PASS. Mutation-verify: drop the `awakened_traits.potency = 0.6` line — the roster test's mighty assertion goes RED; restore.

```bash
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-t5.txt
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(lab): awakened-owlbear test roster — the transition arc, overrides survive kind change (ecs-c5 T5)"
```

---

### Task 6: The keystone query, artifacts, book, retro

**Files:**
- Create: the keystone test (append to `windows/lab/tests/individuation.rs`)
- Create: `book/src/chronicle/the-individuation.md` (+ `book/src/SUMMARY.md` entry, matching the chronicle part's list)
- Modify: `book/src/frontier/idea-registry.md` (UNI-22 row: campaign 5 shipped, campaign 6 next)
- Modify: any stale book chapter naming "biosphere is the entity set" (grep `book/src` for the phrase; the components/ECS chapter)
- Create: `docs/retrospectives/the-individuation.md`
- Modify: `docs/superpowers/specs/2026-07-17-instance-join-ecs-campaign-5-design.md` (STATUS header line, as c4 did at close)

**Interfaces:** Consumes everything; produces the campaign's Definition-of-Done surface (G6 inputs).

- [ ] **Step 1: Write the keystone test** (append to `windows/lab/tests/individuation.rs`)

```rust
#[test]
fn mighty_things_in_the_cold_north() {
    // Drill 4, the program keystone: filter instances by their KIND's
    // authored components (potency, via the registry) crossed with their OWN
    // ledger facts (location), answered by the c4 query engine (OSP) — the
    // kind ⋈ instance join this campaign exists to land.
    let wc = awakened_owlbear_components();
    let mut w = world();
    // Register a location predicate for the demonstration (non-functional).
    w.registry
        .register_predicate("located-in", false, "spatial containment")
        .unwrap();
    let north = w.ledger.mint_entity();
    let south = w.ledger.mint_entity();
    let place = |w: &mut World, e, region| {
        w.ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: e,
                    predicate: "located-in".to_string(),
                    object: Value::Entity(region),
                    place: None,
                    day: Some(0.0),
                    provenance: "lab".to_string(),
                },
                &w.registry,
            )
            .unwrap();
    };
    // Three instances: a mighty northerner, a mundane northerner, a mighty southerner.
    let mighty_north =
        hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "awakened-owlbear", None, "lab").unwrap();
    let mundane_north =
        hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "owlbear", None, "lab").unwrap();
    let mighty_south =
        hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "awakened-owlbear", None, "lab").unwrap();
    place(&mut w, mighty_north, north);
    place(&mut w, mundane_north, north);
    place(&mut w, mighty_south, south);

    // The join: things in the north (OSP object query) whose current kind's
    // effective potency > 0 (instance_of -> registry via the lens).
    let answer: Vec<_> = w
        .ledger
        .query_by_object(&Value::Entity(north))
        .filter(|f| f.predicate == "located-in")
        .map(|f| f.subject)
        .filter(|&e| {
            instance_biosphere(&w.ledger, e, &wc.biosphere)
                .map(|t| t.potency > 0.0)
                .unwrap_or(false)
        })
        .collect();
    assert_eq!(answer, vec![mighty_north], "exactly the mighty northerner");
}
```

- [ ] **Step 2: Run it; mutation-verify**

Run: `cargo test -p hornvale-lab --test individuation mighty_things`
Expected: PASS. Mutation-verify: filter on the OLD kind (replace `instance_biosphere`'s registry read with the beast row) — RED because the awakened northerner's might comes from its kind's components; restore.

- [ ] **Step 3: Final artifact freshness sweep**

```bash
bash scripts/regenerate-artifacts.sh && bash scripts/freeze-fixture.sh
git status --porcelain
```

Expected: **empty** (Tasks 2/3 already re-baselined in their own commits). Any drift here means a task skipped its regen — investigate before proceeding. Then diff the census fixtures against the merge base to prove untouched:

```bash
git diff $(git merge-base HEAD main)..HEAD --stat -- book/src/laboratory/generated/ | cat
```

Expected: no output.

- [ ] **Step 4: Chronicle entry** (`book/src/chronicle/the-individuation.md`)

Write at the book's altitude (technical, comprehensible without the code): the campaign's story — instances individuating from kinds; the `instance-of` state machine (the awakening as a day-stamped fact); the prototype join (override-else-default, the large owlbear that stays large); the four component-set shapes; the shadow posture (mechanism landed, no world changed — the ledger bytes are the proof); the keystone query. Add the SUMMARY.md line in the chronicle section, matching neighbors. Run `mdbook build book` — expected: clean build.

- [ ] **Step 5: Freshness sweep + UNI-22 re-score**

- `grep -rn "canonical entity set" book/src/ docs/` — update any chapter that still says biosphere is the entity roster (point at the union `kinds()`).
- `book/src/frontier/idea-registry.md` UNI-22 row: append campaign-5 shipped
  (instance⋈ledger: instance-of facts, the lens, the join, non-species kinds;
  shadow posture — no world changed) and that campaign 6 (systems & schedule)
  is next. **The main checkout has uncommitted registry edits from a parallel
  session — edit ONLY this worktree's copy; a merge conflict at close is
  normal and resolved additively.**
- Check `book/src/open-questions.md` for any bet this campaign moves (per
  the c4 precedent: substrate milestones usually do not move a taste bet —
  grep the ledger/ECS terms and say so in the close report if unchanged).
- Run `cargo test -p hornvale --test docs_consistency` — expected: PASS.

- [ ] **Step 6: Retrospective** (`docs/retrospectives/the-individuation.md`)

One page, process not product. Must include: the scratch-ledger-lost-with-worktree lesson (#79 — promote or archive `.superpowers/sdd/` before a worktree teardown); the followup register's six rows (copy from `.superpowers/sdd/followups.md`); whatever execution taught. Update the spec's header with a STATUS line (as c4's spec carries).

- [ ] **Step 7: The full pre-merge gate**

```bash
make gate 2>&1 | tail -20
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c5-final.txt
```

Expected: gate green (fmt, clippy, nextest, doctests, type-audit); full suite exit 0. Diff any red against `/tmp/hv-c5-baseline.txt` before blaming the campaign.

- [ ] **Step 8: Commit**

```bash
git add -A && git commit -m "docs(the-individuation): close — chronicle, UNI-22 re-score, freshness sweep, retrospective (ecs-c5 T6)"
```

**STOP after this task: G6 is a hard stop.** Present the post-G3 ledger digest to Nathan; the FF to main (`git -C <main-checkout> merge --ff-only worktree-the-individuation` after clean-checking the main checkout), any push, and worktree teardown are his calls, run under the `closing-a-campaign` skill.

---

## Execution notes for the dispatcher

- Subagent-driven (dispatching-hornvale-subagents): sonnet floor, dispatch
  preamble verbatim, `cd` into the worktree in every dispatch, challenge-
  response on return. Task reports to `.superpowers/sdd/task-N-report.md`.
- `make preflight` at every task boundary; absorb main INTO the branch on
  ancestry NO-GO (merge, re-gate). Exception rules per CLAUDE.md.
- Tasks 2 and 3 are the only artifact-regenerating tasks; Task 6 verifies
  the sweep is a no-op.
