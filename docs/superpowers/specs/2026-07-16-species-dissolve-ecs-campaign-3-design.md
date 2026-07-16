# Dissolve `SpeciesDef` — ECS Program, Campaign 3: Components by Domain — Design

**Status:** Draft for G3 review (2026-07-16) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan · **Program:**
[ECS metaplan](2026-07-14-ecs-program-metaplan-design.md) §4.2, §4.3, §5 (c3), §7

> Campaign 3 of the entity-component program, and the one the first two were
> foundations for. It dissolves the `SpeciesDef` god-struct into per-concern
> component registries, each keyed by `KindId`, each owned by a domain,
> composed only at worldgen — so "an entity is its component-set"
> (metaplan §2) stops being a description of the data model and becomes the
> actual shape of the code. Byte-identical: this moves *where* traits live,
> never *what* they compute.

## 1. What this delivers

After campaign 2, a kind is one `SpeciesDef` value in one map:

```rust
SpeciesDef { name, family, biosphere: BiosphereTraits, peopled: Option<PeopledTraits> }
registry() -> BTreeMap<KindId, SpeciesDef>          // all authoring, one crate (species)
```

That struct still fuses two unrelated ideas the exploration named: **what
competes for space and energy** (biosphere) and **what forms societies and
speaks** (the peopled cluster). A dragon and a fungus are `peopled: None`, but
the *shape* that forces them to be able to carry a psychology and a vocabulary
still exists, and all of it — body, mind, and speech — is authored in one
crate.

Campaign 3 replaces the god-struct with **component registries dispersed to
their owning domains**:

- **`species`** authors the **body** and the **mind**: the universal
  `BiosphereRegistry` (every kind) and the peopled-only `PsycheRegistry` and
  `PerceptionRegistry`.
- **`language`** authors the **speech**: the peopled-only `ArticulationRegistry`
  (phonology) and `LexiconRegistry` (the stopgap social vocabulary), plus the
  family proto vectors — *the types move too* (`ArticulationVector`,
  `ExoticManner`, and the proto registry migrate from `species` into
  `language`).
- **`worldgen`** (the composition root) holds the struct-of-registries and is
  the only place they meet. It joins the components of a kind by `KindId` and
  runs a referential-integrity check over the join.

`SpeciesDef` and `PeopledTraits` are deleted. This is the first campaign where
a domain owning exactly one slice of a kind is **enforced by construction** —
the constitutional `kernel → domains → windows` layering (a domain depends on
kernel and no other domain) means no domain *can* read another's component
directly; only worldgen composes. (Scope chosen at G1 (#57): both the
normalization *and* the first cross-crate ownership move land here — the
meatier refactor — not the normalize-only path.)

## 2. The reframe: a kind is a join, not a record

The load-bearing idea, and the one that answers "who authors the goblin?":
**no one does.** A kind stops being a record you can hold and becomes an
*informational join key*. `species` authors goblin's biosphere; `species`
authors goblin's psyche and perception; `language` authors goblin's
articulation and lexicon; the label `KindId("goblin")` is the only thing they
share. "The goblin" exists only as the view worldgen assembles by looking up
that key in every registry. This is relational normalization — one wide
denormalized table (`SpeciesDef`) partitioned vertically into per-concern
tables sharing a primary key — and it is the metaplan's orchestral-score
picture made literal: each domain a staff, each kind a measure, silence where a
component is absent (a fauna kind has no row in the mind or speech registries).

Two consequences fix the open design questions:

- **Roster authority.** There is no roster struct and no central kind list. The
  key-space is the *union* of `KindId`s across the registries, and the
  **universal `BiosphereRegistry` is the canonical entity set** — the trunk
  every kind hangs from (every kind that competes for space has a biosphere
  row). "Which kinds exist" is answered by iterating the biosphere registry.
- **Cluster coherence replaces `Option`.** Today `peopled: Option<PeopledTraits>`
  makes "all peopled traits or none" true by construction. Split across four
  registries, that becomes a **worldgen referential-integrity invariant**:
  every kind present in `PsycheRegistry` is present in `PerceptionRegistry`,
  `ArticulationRegistry`, `LexiconRegistry`, and `BiosphereRegistry`, and no
  fauna kind appears in a peopled registry. This is the same shape as today's
  `every_multi_member_family_has_a_proto` guard — a load-time check that fails
  loudly, not a type.

## 3. The design

### 3.1 The component-storage substrate: `kernel::ComponentStore<K, C>`

The kernel gains the domain-agnostic component-storage type the metaplan
names (§4.2) — the generalization of `CellMap<T>` from a fixed `CellId` key to
any ordered key:

```rust
/// A typed store of component `C` keyed by identity `K` — the entity-space
/// analogue of `geosphere::CellMap`. Deterministic iteration (ordered key).
pub struct ComponentStore<K: Ord, C> { /* BTreeMap<K, C> for now */ }

impl<K: Ord, C> ComponentStore<K, C> {
    pub fn get(&self, k: &K) -> Option<&C>;
    pub fn insert(&mut self, k: K, c: C);
    pub fn contains(&self, k: &K) -> bool;
    pub fn iter(&self) -> impl Iterator<Item = (&K, &C)>;   // ascending key
    pub fn ids(&self) -> impl Iterator<Item = &K>;
    pub fn len(&self) -> usize;
}

/// Open marker (NOT sealed — sealing would break open-extension): anyone may
/// declare a component; per-component validating constructors keep invariants.
pub trait Component {}
```

**Scope discipline.** In this campaign `ComponentStore` is a thin,
`BTreeMap`-backed wrapper — its iteration order is exactly today's
`BTreeMap<KindId, _>` order (ascending by label), so the substrate is
byte-identical on arrival. The metaplan's dense-`Vec`/`CellMap` backend for
dense keys, and the `SPO`/`PSO`/`OSP` permutation indexes, are the query
engine's perf work (**campaign 4**, §4.5) and are explicitly *out* here. This
campaign delivers the *typed component API*, not the index.

### 3.2 The components and their owners

Each existing authored vector-struct becomes one component registry, grouped by
owning crate. The struct contents are unchanged — only their container and
crate move.

```
registry (owner)                      | key    | present for      | authored values
--------------------------------------+--------+------------------+-----------------
species::BiosphereRegistry            | KindId | ALL 16 kinds     | BiosphereTraits (unchanged)
species::PsycheRegistry               | KindId | 4 peoples        | PsychVector (unchanged)
species::PerceptionRegistry           | KindId | 4 peoples        | PerceptionVector (unchanged)
language::ArticulationRegistry        | KindId | 4 peoples        | ArticulationVector (TYPE MOVES)
language::LexiconRegistry             | KindId | 4 peoples        | Lexicon (noun + rung words)
language::family_proto (registry)     | KindId | 3 families       | ArticulationVector (MOVES)
species::family_of (taxonomy)         | KindId | ALL 16 kinds     | family label (&'static str)
```

- **`SpeciesDef.name`** is deleted: the label *is* the `KindId` key (decision
  0015). **`SpeciesDef.family`** survives as a small universal species-owned
  taxonomy lookup (`family_of(KindId) -> &'static str`), read by worldgen to
  resolve a kind's proto vector against `language`'s `family_proto` registry.
- **`Lexicon`** is a new `language` struct holding the fields that are loose in
  `PeopledTraits` today (`noun`, `worker_override`, `warrior`, `artisan`,
  `shaman`, `top`). Its downstream consumer is unchanged: worldgen assembles
  `culture`'s `Vocabulary` from it exactly as it does now from
  `PeopledTraits` — culture stays a consumer, language becomes the owner (the
  stopgap vocabulary The Tongues will eventually generate; matrix + the "deleted
  by The Tongues" rationale).
- **`ArticulationVector` + `ExoticManner`** move from `species` to `language`.
  Worldgen's `envelope_of` (the `species::ArticulationVector` →
  `language::Envelope` translation, `worldgen:1593`) collapses: `language` reads
  its own type. No type is promoted to the kernel (a component owns its types).

Ownership by **construction**: `species` cannot read `language`'s articulation
(no `species → language` dependency) and vice versa; each domain sees only what
it authors, and only worldgen — which depends on both — composes across them.
The packer/habitat layer reads **only** `BiosphereRegistry`, unchanged.

### 3.3 Worldgen: the composition root

Worldgen holds the composed set and joins by `KindId`:

```rust
pub struct WorldComponents {
    pub biosphere:   ComponentStore<KindId, BiosphereTraits>,       // from species
    pub psyche:      ComponentStore<KindId, PsychVector>,           // from species
    pub perception:  ComponentStore<KindId, PerceptionVector>,      // from species
    pub family_of:   ComponentStore<KindId, &'static str>,          // from species
    pub articulation: ComponentStore<KindId, ArticulationVector>,   // from language
    pub lexicon:     ComponentStore<KindId, Lexicon>,               // from language
    pub family_proto: ComponentStore<KindId, ArticulationVector>,   // from language
}
```

- `WorldComponents::assemble()` gathers each domain's registry and runs the
  **referential-integrity check** (§2): peopled-cluster coherence + biosphere
  totality. It fails loudly (a `BuildError` with the physical reason) if a kind
  is malformed — the load-time invariant that replaces `Option`.
- The roster / genesis path changes from `roster: &[SpeciesDef]` to iterating
  `biosphere.ids()` (the entity set) and looking up each kind's optional
  components. `peopled(def)` (`worldgen:1446`, the `.unwrap()`-behind-a-filter
  helper) becomes a `psyche.get(kind)` / `articulation.get(kind)` lookup, gated
  by the same `is_some()` filters at the same pass boundaries the campaign-1
  seam established (settlement genesis, phonology, perception lensing).

Because the values, their `KindId` keys, and worldgen's assembly order are all
unchanged, every downstream computation receives byte-identical inputs.

## 4. Determinism

**Byte-identical (the master oracle).** No serialized value changes: biosphere,
psyche, perception, articulation, lexicon, and proto values are the same
constants; every registry keys by `KindId` and iterates ascending-by-label
exactly as `BTreeMap<KindId, _>` does today; genesis mints entities and commits
facts in the same order over the same roster (the biosphere entity set is the
same 16 kinds in the same order). So every committed world and artifact is
bit-for-bit identical. CI's artifact drift-check certifies it for free — a
behavior-preserving refactor is byte-identical (metaplan §6). This holds across
all five stages (§6); each is independently byte-identical.

**Determinism contracts (lead the G3 flagged section, per metaplan §7):**

- **Serialized kind references stay the label.** Unchanged from campaign 2 — a
  kind reaches the ledger as a `Value::Text` label; the dissolution adds no new
  serialized surface and moves no value. The epoch-suffix rule (`red-dragon/v2`,
  never a rename) is untouched.
- **New load-time invariant: peopled-cluster coherence.** The referential-
  integrity check (§2, §3.3) is a new build-time guard (like the orphan-
  predicate check), not a save-format change. It makes "a kind with a mind but
  no speech" *unrepresentable at load* rather than latent.

**Type relocations are mechanical + drift-checked.** `ArticulationVector` /
`ExoticManner` / `family_proto` moving from `species` to `language` shifts their
`type-audit` verdicts to `language`'s public boundary; the audit stays clean
workspace-wide (the tags travel with the types). No new workspace dependency:
the moved types use only `&'static str`, `f64`, and their own enum; `language`
already depends on the kernel (`KindId`).

**No `HashMap`/`HashSet`** — every registry is `KindId`-keyed `BTreeMap` under
`ComponentStore`, as today.

## 5. Tests

- **Byte-identity:** the committed seed-42 worlds and every drift-checked
  artifact are unchanged (the master oracle). Asserted at each stage.
- **Equivalence shadow (the per-step guarantee, metaplan §6.3):** for every
  kind, `biosphere.get(kind)` equals the old `SpeciesDef.biosphere`,
  `psyche.get(kind)` equals the old `peopled.psych`, etc. — a direct field-for-
  field equality between the pre-dissolution `registry()` and the assembled
  components, over every kind. This is the strongest correctness test and it
  guards every migration step.
- **Referential integrity:** the peopled cluster is coherent (the four peopled
  registries have identical key-sets; every peopled kind has a biosphere row; no
  fauna kind appears in a peopled registry) — with a negative test that a
  deliberately malformed assembly (mind without speech) fails loudly.
- **`KindId`-label stability (the metaplan regression):** inserting a dummy kind
  does not change any other kind's serialized reference — carried forward from
  campaign 2, now exercised across the split registries.
- **Ownership by construction (compile-level):** a note/guard that `species`
  does not depend on `language` and vice versa (the architecture test
  `cli/tests/architecture.rs` already enforces the no-domain→domain rule; the
  type moves must keep it green — the real proof that ownership is enforced, not
  conventional).
- **type-audit** clean workspace-wide; `cargo fmt` / clippy clean; the full
  commit gate green (modulo the standing deferred-AWS census reds, unchanged by
  this refactor).

## 6. Stages (strangler-fig; each byte-identical, independently testable)

1. **The substrate.** Add `kernel::ComponentStore<K, C>` + the open `Component`
   marker trait, `BTreeMap`-backed, with the small operation set. Nothing
   consumes it yet. Additive, byte-identical.
2. **Body + mind registries (in `species`).** Publish
   `species::biosphere_registry()`, `psyche_registry()`,
   `perception_registry()`, and `family_of()` as `ComponentStore`s, *derived
   from the existing `registry()`* so both forms coexist. The equivalence-shadow
   test (§5) goes green here. Byte-identical.
3. **Worldgen reads the body/mind registries.** Switch worldgen's biosphere and
   psyche/perception consumers off `def.biosphere` / `peopled(def).psych` /
   `.perception` and onto the registries + `WorldComponents` (partial, mind side
   only). Byte-identical.
4. **Relocate speech to `language`.** Move `ArticulationVector` / `ExoticManner`
   and the proto registry into `language`; add `language::Lexicon`,
   `articulation_registry()`, `lexicon_registry()`, `family_proto()`. Collapse
   `envelope_of` into language. Switch worldgen's phonology/lexicon consumers
   (name-gen, `culture::Vocabulary` assembly) onto language's registries.
   Byte-identical.
5. **Delete the god-struct + close.** Remove `SpeciesDef`, `PeopledTraits`, and
   the old `registry()`; worldgen assembles `WorldComponents` and iterates the
   biosphere entity set for genesis; the referential-integrity check goes live.
   Then: byte-identity across the suite; chronicle + retrospective + the UNI-22
   campaign-3 registry note; the gate.

Stages 1–4 leave the world building through a working (if transitional) path at
every commit; stage 5 removes the scaffolding.

## 7. Boundary

**In:** `kernel::ComponentStore<K, C>` + the `Component` marker; the dissolution
of `SpeciesDef` into the seven registries of §3.2; the relocation of the speech
cluster (types + authoring) into `language`; the `WorldComponents` composition
root + its referential-integrity check; the equivalence-shadow and coherence
tests.

**Out (later campaigns):** `ComponentStore`'s dense-`Vec`/`CellMap` backend and
the `SPO`/`PSO`/`OSP` permutation indexes + predicate interning (campaign 4);
instance-keyed components, `instance_of` facts, and the kind⋈instance prototype
join (campaign 5); non-species entities — deity, culture-as-entity, material
(campaign 5); domains-as-systems and the capability-schema schedule (campaign
6); the spatial partition (campaign 7). Moving the lexicon's *consumer* off
culture, or generating it (The Tongues), is out — this campaign only moves its
*owner*.

## 8. Open items (plan-level calls, not design gaps)

- **Component granularity.** Whether `species` publishes three registries
  (biosphere / psyche / perception) or bundles psyche+perception into one
  `Mind` component, and whether `language` bundles articulation+lexicon, is a
  plan-stage call. The spec's primary decomposition is one registry per existing
  vector-struct (cleanest "component-set", lowest churn); bundling by owner is
  an equivalent, coarser option the plan may pick.
- **`family_of` home.** The universal family-label lookup could be a standalone
  species taxonomy registry or a field carried on `BiosphereTraits` (both are
  universal, 16 entries). The minimal choice is a standalone lookup so
  `BiosphereTraits` stays purely ecological; a stage-2 detail.
- **Where `envelope_of` lands in `language`.** Whether the articulation→envelope
  mapping becomes a `language` free function or `ArticulationVector` gains an
  `fn envelope(&self)` is a stage-4 detail; behavior (and bytes) are unchanged.
