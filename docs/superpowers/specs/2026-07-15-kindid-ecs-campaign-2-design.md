# KindId — ECS Program, Campaign 2: Kinds Get Identity — Design

**Status:** Draft for G3 review (2026-07-15) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan · **Program:**
[ECS metaplan](2026-07-14-ecs-program-metaplan-design.md) §4.1, §5, §7

> Campaign 2 of the entity-component program. It gives a *kind* a stable,
> typed identity — a `KindId` — so that the label a kind is known by is a
> first-class type with a save-format contract, not a bare `&'static str` that
> happens to double as a map key and a positional array tag. Byte-identical:
> the serialized identity does not change, only its type does.

## 1. What this delivers

Today a species kind is identified two ways at once, both implicit:

- as a **bare `&'static str`** — the key of `registry() -> BTreeMap<&'static
  str, SpeciesDef>` and the value of `SpeciesDef.name`, and the text of the
  `SPECIES_NAME` fact committed to the ledger (`Value::Text("kobold")`);
- as a **positional `u32`** — `.enumerate()` over that registry in the packer
  and habitat layer (`windows/worldgen/src/lib.rs:534, 583, 2394`), used for
  array-indexed math.

The label is already the **de-facto serialized identity** — kind references in
the save are the `SPECIES_NAME` text, never the `u32`, which is recomputed each
build. So there is no active corruption bug. What is missing is that the
identity is *implicit and untyped*: nothing at the type level distinguishes a
kind label from any other string, nothing documents that the label is a
save-format contract, and nothing states that the `u32` is a subordinate,
never-serialized build artifact.

Campaign 2 makes all three explicit:

1. A **`KindId` newtype** (in the kernel, beside `EntityId`) is the stable
   identity of a kind — its label, typed.
2. The species registry is **keyed by `KindId`**: `registry() -> BTreeMap<
   KindId, SpeciesDef>`. `SpeciesDef` itself is unchanged otherwise (its
   dissolution into domain-owned registries is campaign 3).
3. The packer's `u32` becomes an explicit **build-local dense index**, derived
   from the `KindId`-keyed registry each build and documented as never
   serialized — the durable-label / ephemeral-index split made real.

This is byte-identical (§3), and it is the foundation campaign 3 (domain-owned
component registries keyed by `KindId`) builds on.

## 2. The design

### 2.1 `KindId` — a typed label in the kernel

`KindId` is a newtype over the stable label, placed in the kernel as an
identity primitive beside `EntityId` (the metaplan placement rule: the kernel
holds the domain-agnostic identity mechanisms; domains author the values).

```rust
/// The stable identity of a *kind* — the authored label a kind is known by
/// ("red-dragon", "kobold"). A kind's identity is its label, never its
/// position in any registry (decision 0015: a name is its own key). When a
/// kind is referenced in the ledger it is referenced by this label; a
/// deliberate change to a kind's authored traits that must not alias the old
/// kind takes an epoch suffix ("red-dragon/v2"), never a rename.
/// type-audit: bare-ok(identifier-text)
pub struct KindId(pub &'static str);
```

`&'static str` (not `String`) because kinds are **build-state**: authored,
never deserialized into new kinds (the rate model — the registry is re-derived
from authored data, not loaded from the save). A kind *reference* arriving from
the ledger as `Value::Text` is resolved against the registry by label match; it
does not need to construct an owned `KindId`. (Should a future campaign need a
runtime-constructed `KindId`, that is an additive change then, not now — YAGNI.)

`KindId` and `EntityId` are **distinct newtypes**, so indexing a kind registry
with an entity id — or vice versa — is a compile error at zero cost. The
metaplan's `Id<Kind>`/`Id<Entity>` phantom-unification is *not* adopted: two
plain newtypes already give the compile-time safety, and unifying them is an
optional refinement with no benefit here.

### 2.2 The registry, keyed by `KindId`

`registry()` returns `BTreeMap<KindId, SpeciesDef>`. `KindId`'s inner label
equals `SpeciesDef.name`, exactly as decision 0015 has `PredicateDef.name`
duplicate its registry key — the duplication is deliberate and the name is the
identity. `BTreeMap<KindId, _>` orders by the label (a `KindId: Ord` derived
over `&'static str`), so iteration order is unchanged from today's
`BTreeMap<&str, _>`: alphabetical by label.

Lookups that take a bare `&str` species name today (e.g. `def_in`,
`worldgen:1394 `roster.iter().find(|d| d.name == species)`) either take a
`KindId` or continue matching on the label; the choice per call-site is to use
`KindId` where the value *is* a kind identity and keep `&str` where it is
free-text user input (a `--species NAME` CLI argument is a string until it is
resolved to a `KindId` against the registry, failing loudly if unknown).

### 2.3 The `u32` dense index, subordinated

The packer and habitat layer keep their `u32` tag for array-indexed math — it
is a real performance handle, not identity. Campaign 2 makes its status
explicit: the `u32` is a **build-local dense index** assigned by enumerating
the `KindId`-keyed registry (or the roster slice) in order, valid only within
one build, and **never serialized**. A short doc comment and a single helper
(the enumerate site) localize the label→index derivation, so the invariant
"the `u32` is derived from the `KindId` order and never escapes a build" is
stated in one place rather than assumed across three.

### 2.4 The epoch contract, made explicit

The determinism contract the metaplan flagged (#32) becomes real here:
serialized kind references are the label, and a deliberate change to a kind
that must not alias the old one takes an **epoch suffix** (`red-dragon/v2`),
never a rename — the discipline of decision 0006 and the deity-name /v2 epoch
(0050/0051). This lives as `KindId`'s doc comment (§2.1) and as a stability
regression test (§4).

## 3. Determinism

**Byte-identical.** The serialized identity of a kind is its label, and the
label text is unchanged — `SPECIES_NAME` still commits `Value::Text("kobold")`,
the registry still orders alphabetically by label, and the packer's `u32` is
still assigned in the same order. So every committed world and artifact is
bit-for-bit identical across this campaign; the change is to *types and
documentation*, not to any value. The existing artifact drift-check certifies
this for free — a behavior-preserving refactor is byte-identical.

**No `HashMap`/`HashSet`** — `KindId: Ord` keys a `BTreeMap`, as today.

## 4. Tests

- **Byte-identity:** every existing seed-42 world and committed artifact is
  unchanged (the drift-check; assert `SPECIES_NAME` facts and the packed stack
  are bit-identical to the pre-campaign build).
- **`KindId`-label stability (the metaplan's regression):** inserting a new
  kind into the registry must not change any *other* kind's serialized
  reference. A test that adds a dummy kind and asserts every existing kind's
  `SPECIES_NAME` text (and its habitat-stack presence) is byte-identical — the
  positional `u32` may renumber (build-local), the label may not.
- **Type safety:** a compile-level assurance (by construction) that a `KindId`
  cannot be passed where an `EntityId` is expected; a unit test that a
  `--species` pin with an unknown label fails loudly with the physical reason
  (unchanged behavior, now typed at the resolution boundary).
- **type-audit:** `KindId`'s `identifier-text` verdict; the audit stays clean
  workspace-wide.
- `cargo fmt` / clippy clean; the full commit gate green.

## 5. Stages

1. **Introduce `KindId`** — the kernel newtype deriving `Clone, Copy, Debug,
   PartialEq, Eq, PartialOrd, Ord, Hash` (like `EntityId`), with doc +
   type-audit tag. It does **not** derive `Serialize`/`Deserialize`: a `KindId`
   is build-state and never enters the save — a `&'static str` cannot
   deserialize to a static reference, and a kind reference reaches the ledger
   as a `Value::Text` label, not as a serialized `KindId`. Nothing consumes it
   yet. Compiles, gate green.
2. **Re-key the registry** — `registry() -> BTreeMap<KindId, SpeciesDef>`;
   `family_registry` likewise if it keys by a kind/family label. Fix the
   consuming call-sites to construct/pass `KindId`. Byte-identical.
3. **Subordinate the `u32`** — document and localize the build-local dense
   index derivation from the `KindId` order; the epoch-contract doc on
   `KindId`; the label-stability regression test.
4. **Verify + close** — byte-identity across the suite; chronicle + retro +
   registry note (UNI-22 campaign-2 progress); the gate.

Stages 1–3 are each byte-identical and independently testable; stage 4 closes.

## 6. Boundary

**In:** the `KindId` newtype, the registry re-keyed by it, the `u32`
subordinated and documented, the epoch contract made explicit, the
label-stability regression. **Out (later campaigns):** dissolving `SpeciesDef`
into domain-owned component registries (campaign 3); predicate/label interning
into a `Symbol(u32)` (campaign 4); the phantom `Id<T>` unification (optional,
unneeded); any change to `EntityId` minting (the alphabetical-mint renumbering
is an instance-id concern, already benign because references are label-based).

## 7. Open items

- **`family_registry` keying** — whether the family label also becomes a
  `KindId` (families are a kind-like taxonomy) or stays a distinct `FamilyId`
  is a stage-2 call; the minimal choice is to reuse `KindId` for any authored
  kind/family label and only split if a family proves to need a distinct type.
- **CLI `--species` resolution boundary** — the exact point a free-text
  species argument becomes a `KindId` (resolved against the registry, failing
  loudly if unknown) is a stage-2 detail; the behavior (loud failure on an
  unknown name) is unchanged.
