# The Hallmark — the kernel-membership criterion, the first consolidation, and the placement ratchet

**Status:** draft for G3 review
**Campaign ledger:** `docs/superpowers/ledgers/2026-09-01-the-hallmark.md`

A hallmark is the stamp an assay office strikes into metal: it certifies
what the thing is and where it may circulate, and striking it is a deliberate
act by an authority, never an automatic one. This campaign gives type
placement the same apparatus: a ratified criterion for what belongs in the
kernel, a first batch of promotions the criterion licenses, and a tool that
demands every questionable placement carry a stamped verdict — without ever
striking the stamp itself.

## 1. Problem

Three facts, established by survey against `origin/main` at `18f63ebfa`
(2026-09-01) and verified in source where this spec relies on them:

1. **Forced duplicates exist and cost real shims.** `UnitError` is defined
   three times byte-for-byte (`kernel/src/units.rs:27`,
   `domains/astronomy/src/units.rs:10`, `domains/paleoclimate/src/units.rs:7`).
   `GenesisError` is defined verbatim in `domains/astronomy/src/pins.rs:123`
   and `domains/terrain/src/pins.rs:33`. `climate::GroundKind`
   (`domains/climate/src/variants.rs:334`) and `locale::Substrate`
   (`windows/locale/src/regime.rs:13`) carry five identical variants with
   word-identical docs, bridged by a five-arm identity match
   (`windows/locale/src/grammar.rs:268`). `religion::Sentiment`
   (`domains/religion/src/lib.rs:183`) and `language::LineSentiment`
   (`domains/language/src/register.rs:37`) are the same three-valued
   vocabulary, bridged by `line_sentiment_of`
   (`windows/worldgen/src/lib.rs:9950`). `terrain::Horizon`
   (`domains/terrain/src/strata.rs:91`) is mirrored inside
   `climate::Stratum` (`domains/climate/src/facets.rs:116`) and
   `terrain::CaveKind` (`domains/terrain/src/features.rs:14`) inside
   `climate::Formation`, bridged by `stratum_of_band`
   (`windows/worldgen/src/chamber.rs:431`).

2. **No general criterion for kernel membership exists.** Decision 0044's
   placement test is scoped to coherent physical quantities; decision 0216
   extended it to one non-unit roster (`Band`) by analogy without
   generalizing; `ecology.rs`, `color.rs`, `room.rs`, `provenance.rs` were
   placed by campaign-spec rulings never promoted to decision records.
   `kernel/CLAUDE.md` documents contracts, never scope.

3. **The Entity-Component program will force the question at scale.** The
   ECS metaplan (`docs/superpowers/specs/2026-07-14-ecs-program-metaplan-design.md`
   §4.7) puts the domain-agnostic mechanisms in the kernel and component
   registries in domains, but is silent on the types those registries'
   schemas are made of — and a cross-domain query that returns a
   `terrain::Horizon` to a non-terrain consumer has no legal home for that
   type outside the kernel.

## 2. Decision to ratify: the kernel-membership criterion

One decision record, superseding 0044's *scope* (its mechanics stand) and
amending metaplan §4.7 (which is otherwise unchanged). A type belongs in the
kernel when any clause holds:

- **(a)** more than one domain speaks it today — 0044's clause, generalized
  from quantities to any type, with 0216's forced-vs-deliberate test as the
  qualifier: a *forced* duplicate (one side exists only because layering
  forbids the import) qualifies; a *deliberate* projection (deleting one
  side would remove an independent answer, or the two carry different
  information) does not.
- **(b)** it originates in a kernel type — 0044's clause, unchanged.
- **(c)** it appears in the wire schema of a component or relationship
  **registered for cross-domain query** in the EC component catalog. This
  clause is dormant until the catalog exists; it is ratified now so the EC
  campaigns inherit a settled rule instead of relitigating placement per
  component.

**Modifier, all clauses: stability graduation.** A volatile type iterates
domain-side (the cheap rebuild tier); its *identity* (name, registry ID)
may be reserved kernel-side early; the type moves when its shape settles.
Kernel churn is the most expensive rebuild tier and a promotion is also a
save-format-adjacent act, so promotion is a deliberate, occasional event —
never a reflex.

**The split, restated from 0216:** the kernel holds the type, the roster,
and the ordering; the domain holds the meaning — derivations, valences,
behavior — via inherent impls on its own types, local traits, or free
functions (orphan-rule-legal; `pub use` keeps call sites source-compatible).

**What the criterion refuses:** placement by anticipation ("might be shared
someday"), closed kernel enums that enumerate per-domain content (adding a
domain must never edit the kernel — open registries carry growing
vocabularies), and any promotion that changes a committed spelling without
an epoch.

This resolves idea-registry row `DOM-kernel-owns-vocabulary` (raw → ratified,
pointing at this spec's decision) — clause (a) plus the tag discipline in §3
is that row's "more-than-one-domain test or the kernel turns junk drawer"
gate, made mechanical.

## 3. The placement ratchet: `tools/placement-audit`

A standalone tool outside the workspace, same shape and machinery as
`tools/type-audit` (syn-based source walk, no build). Three parts:

**Detector.** Walks `kernel/src` and `domains/*/src` for *shape twins*: two
`pub enum`s whose variant-name sets are identical, or two `pub struct`s
whose field-name sets are identical, in different crates where at least one
is a domain. Windows are out of scope for v1 (a window may import a domain,
so its duplicates are ordinary refactors, not forced ones — the
`GroundKind`/`Substrate` pair is fixed in §4 precisely because it needs no
tool). Name equality is neither necessary nor sufficient; the match is on
member sets.

**Tag grammar.** Every detected twin must carry, on each definition, one
verdict in its doc-comment paragraph (same parse rules as seam-guard's tag:
one paragraph, ends at the first blank `///` line, reasonless is a parse
error):

```
/// placement: promote(<decision or spec anchor>)
/// placement: deliberate(<why the duplication buys something>)
/// placement: deferred(<why not yet>)
```

Each tag embeds a shape fingerprint the tool computes and prints (a short
hash of the sorted member names): `placement: deferred(reason) shape(a1b2c3)`.

**Verdicts, three-valued on the seam-guard model:**

- **UNTAGGED** — a shape twin with no `placement:` tag → RED. This is the
  novelty ratchet: existing debt is tagged once at adoption; new forced
  duplicates cannot land silently.
- **STALE** — a tagged twin whose current shape no longer matches its
  fingerprint → RED, demanding re-adjudication. This is reconsider-on-touch:
  editing a type whose placement is in question reopens the question at the
  moment it is cheapest to answer, and only then. Untouched debt never nags.
- **TAGGED** — tag present, fingerprint matches → green, listed in the
  report.

**The tool detects and demands; it never decides.** Shape identity is not
semantic identity (`RotationRegime`/`Rotation` and `HabitatRealm`/`Realm`
are deliberate, information-losing projections a matcher cannot tell from
forced duplicates), so the verdict is always a human's, carried in the tag.

**Gate placement.** `placement-audit -- check` joins `make quick` and
`gate-commit` beside type-audit *if* its measured cost on the warm tree is
within 2x of type-audit's own check; otherwise it joins the stage gate's
set and the spec's plan records the measured number. (Decision rule, not a
prediction — the cost is measured at implementation, not asserted here.)

**Report artifact.** `placement-audit -- report >
docs/audits/placement-audit-roster.md`, regenerated by
`scripts/regenerate-artifacts.sh`, declared in `docs/generated-paths.txt`
**by file name** and `git add`-ed in the same commit that introduces it
(the vacuous-diff hazard both halves: new file, already-declared directory).

## 4. The first population: promotions this campaign ships

Ordered so each lands as its own commit passing the full gate. All spellings
that reach a committed artifact are preserved exactly — under 0216's ruling
these moves are not save-format epochs *because* the members keep their
spellings, and each promotion's task carries the branch table: committed
artifact bytes unmoved → proceed; any committed byte moved → STOP, that item
escalates to an epoch question rather than landing.

**Batch A — mechanical.**

1. `UnitError`: delete the astronomy and paleoclimate copies, `pub use` the
   kernel's. Follow-up recorded in the campaign ledger as deferred minors
   (neither is a shape twin the detector sees): `terrain/src/crust.rs`'s
   `Result<CrustKm, String>` and `windows/worldgen/src/harvest.rs`'s
   `LatError` (the latter dissolves with the queued angle family).
2. `GenesisError` + `GenesisOutcome<T>`: a kernel `genesis` module;
   `GenesisError` moves verbatim; `GenesisOutcome<T> { value, notes }`
   replaces both structural copies. The module doc states this is a shared
   *type*, not a new `Domain` trait member — `kernel/src/domain.rs`
   deliberately excludes genesis (Constitution §2.6).
3. `locale::Substrate` → deleted in favor of `climate::GroundKind` (locale
   already imports climate in the very shim being deleted; no kernel change).
4. Paleoclimate `day: f64` → `WorldTime`: the `pending(wave-2: day)` sites
   (`strata.rs`, `ice.rs`). Scope note, verified in source: `person`'s
   `PersonSeed` days are `waiver(decision-0126)` — a deliberate pre-commit
   DTO whose doc says each field becomes `WorldTime` in `fact()` — and are
   **out of scope**. The conversion uses the kernel's named ticks↔days
   hatch; the branch table above governs (this is the one Batch A item with
   any determinism exposure).

**Batch B — the criterion's first non-trivial uses.**

5. `Sentiment` (Eternal/Cyclic/Ambient) → `kernel/src/phenomena.rs`, beside
   the `Phenomenon` it is a pure function of — clause (b). Religion keeps
   `as_str`/`parse` (its ledger spellings) as free functions or a local
   trait — an inherent impl cannot live outside the defining crate; language
   keeps its render templates; `line_sentiment_of` is deleted.
6. `Horizon` and `CaveKind` → kernel — clause (a), on exactly the argument
   `kernel/src/band.rs`'s module doc already makes: climate does not
   *derive* a horizon, it only names one, so its mirror is forced, not
   deliberate. Terrain keeps `band_at_depth` and `Cave::from_reach` (the
   derivations); climate re-expresses `Stratum`'s rock half and
   `Formation`'s cave half in terms of the kernel types; `stratum_of_band`
   is deleted.

**Expected artifact drift, stated as a branch table:** every promotion
changes pub boundaries, so `docs/audits/type-audit-report.md` and
`docs/digest/` regenerate and land in the same commit — that is the normal
branch. Any drift in `book/src/laboratory/generated/`, census fixtures, or
client fixtures is the STOP branch.

## 5. Out of scope, recorded where each belongs

- **Batch C items** — the two environment-axis bases, the manikin
  psychology vectors, `ObjectProperty`'s altitude, the biome-name roster —
  each gets an idea-registry row citing this spec; none moves here.
- **The angle/latitude family** — already queued as the units doctrine's
  roadmap item 3; this campaign only adds `windows/worldgen::LatDeg` to the
  sites that family retires.
- **The EC wire-type invariant** (an architecture-test asserting every type
  in a registered component schema resolves to `hornvale_kernel`) — clause
  (c)'s enforcement, deliverable of the EC campaigns, recorded in the
  criterion decision as their hook.
- **Deliberate projections and homonyms** — `RotationRegime`, `HabitatRealm`,
  the `Stance` triple, the two `Formation`s, the composition-root input-DTO
  idiom — untouched, and the roster records the surveyed ones as
  `deliberate` so the ratchet's baseline is honest.

## 6. Success criteria

1. The criterion decision is ratified and indexed; metaplan §4.7 carries a
   pointer to it.
2. `placement-audit` is red on a synthetic new twin, red on a fingerprint
   mismatch, green on the tagged baseline — each pinned by the tool's own
   tests, with the mutation-proved assertion pattern (a test that mutates
   must first assert the target text exists).
3. Batches A and B land, each commit green through the gate; the twins named
   in §1 either no longer exist or carry `deliberate`/`deferred` tags.
4. The drift check passes with the roster declared in
   `docs/generated-paths.txt`.
5. Chronicle entry, book freshness sweep, retrospective — the standing
   Definition of Done.
