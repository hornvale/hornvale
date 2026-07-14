# The Domain Trait & Registry — design

**Date:** 2026-07-12
**Status:** SHIPPED 2026-07-14 (complete 2026-07-12; see `book/src/chronicle/the-domain-trait.md` and `docs/retrospectives/the-domain-trait.md`)
**Scope:** Refactoring proposal **A1** from the 2026-07-12 design examination. Introduce a
kernel `Domain` trait and a single composition-root roster, replacing the two
independently-maintained per-domain lists (`register_all` in worldgen; the streams
manifest in the CLI). Declarative registration only — genesis, metrics, and reference
metadata are explicitly out of scope.

---

## 1. Motivation

Nine domains were built to a shared but un-formalized template. Two of that template's
members — concept registration and seed-derivation stream labels — are aggregated by
**hand-maintained, per-domain lists in two different crates**:

- `windows/worldgen/src/lib.rs` `register_all()` calls `register_concepts` on 9 domains
  in cascade order.
- `cli/src/streams.rs` lists 8 domains to build the drift-checked streams manifest. It
  renders a section for every listed domain — including the four streamless ones
  (`climate`, `culture`, `religion`, `species`), which show `*(no seed-derivation
  streams)*`. paleoclimate is the lone omission, and not by any rule: it was simply never
  added to the list. A1 makes the manifest iterate the roster uniformly, which restores
  paleoclimate to it (see §4).

Nothing enforces that these two lists agree. Their membership already differs, and their
*ordering* differs. This is latent drift: a new domain added to one list but not the
other registers concepts but vanishes from the manifest (or vice versa), silently. The
constitutional goal "adding a domain must never require editing an existing one" is not
met — a new domain today requires editing worldgen **and** the CLI.

A1 routes both consumers through one roster so they cannot disagree, and names the
pattern so it is discoverable.

### What A1 does *not* do

`generate()` signatures are irreducibly heterogeneous — `climate(inputs)`,
`astronomy(seed, pins)`, `terrain(seed, geo, pins)`, `language(seed, species, envelope)`,
`settlement::genesis(...)`. Wiring those inputs together *is* the composition root's
defining job (Constitution §2.6); it is not accidental duplication. Genesis therefore
stays as explicit worldgen wiring. A1 fuses only the two **declarative** members
(registration, stream labels). This boundary is principled, not a compromise: the
lifecycle of "adding a domain" has two declarative transitions that today are separable
(and so can drift) and one compositional transition (`generate`) that carries real work;
A1 fuses the first two and leaves the third alone.

## 2. The trait

Lives in `kernel` (every domain already depends on it; no other crate may host a type all
domains implement without breaking layering).

```rust
/// A generative domain's declarative registration surface — the members the
/// composition root aggregates uniformly across every domain. Genesis is NOT
/// here (its inputs are domain-specific composition work); neither are metrics
/// or reference metadata (separate traits, if ever). See the non-goals.
pub trait Domain {
    /// This domain's crate name, the manifest key (e.g. "hornvale-astronomy").
    /// Implementations return `env!("CARGO_PKG_NAME")` so the key is compiled
    /// from the crate itself and cannot drift or be mistyped.
    fn crate_name(&self) -> &'static str;

    /// Register this domain's predicates and concepts into the shared registry.
    fn register_concepts(&self, registry: &mut ConceptRegistry) -> Result<(), RegistryError>;

    /// This domain's seed-derivation stream labels (permanent save-format
    /// contracts). Defaults to empty for domains that draw no seed streams
    /// (e.g. paleoclimate, whose forcing comes from astronomy).
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        Vec::new()
    }
}
```

Object-safe (`&dyn Domain`): all methods take `&self`, return owned or `'static` data.

### Per-domain implementation

Each domain crate defines a zero-sized unit struct next to its existing registration
code and implements the trait by **delegating to the existing free functions** — which
remain in place as shims for the duration of the migration and beyond (they are also the
per-domain unit tests' entry points).

```rust
// domains/astronomy/src/lib.rs
pub struct Astronomy;
impl hornvale_kernel::Domain for Astronomy {
    fn crate_name(&self) -> &'static str { env!("CARGO_PKG_NAME") }
    fn register_concepts(&self, reg: &mut ConceptRegistry) -> Result<(), RegistryError> {
        register_concepts(reg) // existing free fn, unchanged
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        stream_labels() // existing free fn, unchanged
    }
}
```

Domains with no `stream_labels()` free function (e.g. paleoclimate) simply take the
trait default.

## 3. The roster

`worldgen` — the only crate constitutionally permitted to know all domains — owns the
roster, `pub` so the CLI consumes it through its existing worldgen re-export.

```rust
// windows/worldgen/src/lib.rs
// Order is REGISTRATION order, not alphabetical: `language::register_concepts`
// references concepts it does not own (terrain's `stone`, religion's `god`, …)
// and must run AFTER their owners, or it claims them under domain "language"
// and conflicts. This is the pre-existing cascade order that made that work.
// (The deeper fix — un-conflating register vs. reference — is a captured
// follow-up; see §4. Adding a domain that lends/borrows stratum concepts must
// respect this order; every other domain is order-free.)
pub const DOMAINS: &[&dyn Domain] = &[
    &hornvale_astronomy::Astronomy,
    &hornvale_climate::Climate,
    &hornvale_terrain::Terrain,
    &hornvale_settlement::Settlement,
    &hornvale_species::Species,
    &hornvale_culture::Culture,
    &hornvale_religion::Religion,
    &hornvale_language::Language,
    &hornvale_paleoclimate::Paleoclimate,
];
```

`DOMAINS` is the single membership list — the score. Each consumer imposes its own
ordering on it: `register_all` iterates it as stored (registration order), the streams
manifest sorts it by crate name. Neither authors its own domain list; the roster is the
one place membership lives.

### Consumers

```rust
// register_all: iterate the roster, then worldgen's own NAME_GLOSS predicate.
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    for d in DOMAINS {
        d.register_concepts(registry)?;
    }
    registry.register_predicate(NAME_GLOSS, true, "...")
}

// streams manifest (cli/src/streams.rs, rewritten to iterate DOMAINS):
//   sort DOMAINS by crate_name FIRST (the manifest is alphabetical), then
//   for each domain: header `### {crate_name}`, then either the label table
//   or `*(no seed-derivation streams)*` when stream_labels() is empty.
//   The trailing hardcoded `hornvale-kernel (internal)` section is appended after.
```

**On error context:** `register_all` keeps its `Result<(), RegistryError>` signature.
The notation pass proposed wrapping the loop error with the failing domain's
`crate_name()`, but `RegistryError` carries only `ConflictingDefinition { name }` (no
context slot) and `register_all` has ~8 callers — a return-type change is out of
proportion to A1, and the existing error already names the conflicting *concept*, which
(registered by exactly one domain) already locates the culprit. Refinement dropped.

## 4. The contracts (one deliberate regeneration; everything else byte-identical)

The **concepts dump stays byte-identical** — that is the primary drift proof. The
**streams manifest gains exactly one section** (`hornvale-paleoclimate`, rendered
`*(no seed-derivation streams)*`) as a deliberate, reviewed regeneration; the committed
page is regenerated and committed in the same change. No stream label is added, removed,
or renamed, so no save-format contract is touched.

1. **Registration order IS observable — the roster is stored in registration order.**
   The original spec assumed registration order was unobservable (pure `BTreeMap`
   set-insertion). That is **false**: `hornvale_language::register_concepts` registers
   concepts it *references but does not own* (terrain's `stone`, religion's `god`,
   settlement's `home`, climate's `snow`/biomes, species' `<kind>`s) under domain
   `"language"` **only if absent** — so if `language` runs before an owner, it claims that
   owner's concept and the owner's later registration conflicts (`ConflictingDefinition`).
   The old hand-ordered `register_all` worked because `language` ran last; an alphabetical
   roster runs it fourth and breaks. Therefore `DOMAINS` is stored in the pre-existing
   **cascade order** (owners before `language`); `register_all` iterates it as stored,
   reproducing today's ownership exactly, so the concepts dump is byte-identical. The
   drift check on the concepts dump proves it. The **streams manifest sorts `DOMAINS` by
   crate name** at render time, so it stays alphabetical and byte-identical independent of
   storage order.

2. **The manifest iterates the roster uniformly.** Every registered domain gets a
   section; streamless domains (`climate`, `culture`, `religion`, `species`, and now
   `paleoclimate`) render `*(no seed-derivation streams)*`. This removes the arbitrary
   paleoclimate omission — the *only* intended artifact change in A1. The trait needs no
   opt-out flag; emptiness is shown, not hidden. (The trailing hardcoded
   `hornvale-kernel (internal)` section is not a domain and stays appended after the
   loop.)

3. **`NAME_GLOSS` stays a post-roster registration** inside `register_all`. worldgen is
   the composition root, not a domain; it does not join the roster.

### Deferred: un-conflating `register` vs. `reference` (the root fix)

The order dependency above is a symptom: `ConceptRegistry::register_concept` conflates two
acts — *claiming ownership* of a concept and *ensuring a concept is present*. `language`
uses the ownership verb to do what is semantically a *reference*, so whoever runs first
"owns" a shared concept. This also hides a latent bug unrelated to A1: if `language` lists
a concept **no** domain owns, it silently becomes the owner under `"language"`, masking a
missing registration. The principled fix is a distinct `reference_concept(name)` primitive
(assert-present, claim nothing; dangling reference → loud error), leaving `register_concept`
as the ownership claim. That is out of A1's scope (a kernel-API change with its own review
surface); A1 takes the minimal registration-order fix above and **captures the root fix as
a frontier idea-registry row + a follow-up**. This spec does not depend on that follow-up.

## 5. `crate_name()` via `CARGO_PKG_NAME`

Each impl returns `env!("CARGO_PKG_NAME")` rather than a hand-typed literal. The manifest
key is then compiled from the crate itself — it cannot be mistyped and cannot drift from
the crate it names. A unit test asserts every `crate_name()` in `DOMAINS` is unique and
non-empty (guarding against a duplicated or missing roster entry).

## 6. Migration stages (green at every step)

1. **kernel** — add the `Domain` trait + trait-level tests (default `stream_labels`
   returns empty; object-safety compiles). No consumers yet.
2. **domains/\*** — one commit per domain (or small batches): add the ZST + `impl Domain`
   delegating to the existing free functions. Free functions stay. Nothing downstream
   changes; every per-domain test keeps passing.
3. **worldgen** — introduce `pub const DOMAINS`; rewrite `register_all` to iterate it
   (with the error-wrapping) then register `NAME_GLOSS`. Add the worldgen unit tests
   (§7). Run the artifact drift check — registry/manifest must be byte-identical.
4. **cli** — rewrite `cli/src/streams.rs` (or its manifest builder) to iterate
   `hornvale_worldgen::DOMAINS`; delete the local list. Drift check confirms the manifest
   is unchanged.
5. **Definition of Done** — chronicle entry (`book/src/chronicle/`), one-page
   retrospective (`docs/retrospectives/`), and a freshness sweep: the architecture
   chapter's "adding a domain" story now reads "implement `Domain`, add one line to
   `DOMAINS`" — add that checklist. Re-score any Confidence-Gradient bet this moves.

## 7. Testing

- **Characterization guard (primary):** the CI artifact drift-check proves the
  **concepts dump** is byte-identical, and that the **streams manifest** matches its
  regenerated form (the one that now includes the paleoclimate `no streams` section). The
  regeneration is committed as part of stage 4.
- **worldgen unit tests (new):**
  - `DOMAINS` crate-names are unique and non-empty.
  - Iterating `DOMAINS` registers the same sorted predicate/concept name-set as a
    snapshot captured before the change.
  - The manifest built from `DOMAINS` contains a section for every roster domain,
    including `hornvale-paleoclimate` rendered as `*(no seed-derivation streams)*`.
- **Per-domain tests (unchanged):** `register_concepts` idempotent; `stream_labels`
  declares every derivation — all keep passing through the shims.
- **Architecture test (unchanged):** `cli/tests/architecture.rs` continues to enforce
  the dependency-edge allowlist; A1 adds no new edges.

## 8. Non-goals (the anti-god-interface clause)

`Domain` covers **declarative registration only**. It must not grow. Future per-domain
behaviors get their **own** traits, never new `Domain` members:

- Genesis / provider reconstruction → a `Reconstruct` trait (proposal A4).
- Metric contribution → a `MetricSource` trait (proposal B2).
- Reference/concept-dump metadata → read from the populated registry, as today.

The polarity risk is concrete: bolting each future want onto `Domain` would rebuild the
worldgen monolith as an interface. This clause is the guardrail.

## 9. Known limitation — one edit-site, not zero

A1 collapses the roster from **N hand-maintained edit-sites to 1** (`DOMAINS`), but not to
0. The constitution forbids the external crates (`inventory`, `linkme`) or build scripts
that would make the `domains/` directory itself the source of truth. Consequently the
drift check catches a *removed* domain (its concepts vanish from the dump) but cannot
catch a brand-new domain that nobody added to `DOMAINS` — there is nothing yet to miss.
The mitigation is exactly what A1 delivers: a single, obvious edit line, plus the "how to
add a domain" checklist added to the architecture chapter in step 5. Reducing this to
zero is a non-goal here and would require revisiting the no-external-crates rule.
