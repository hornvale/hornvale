# Decision Log

This directory is the durable, grep-able home for Hornvale's **ratified
decisions** — choices that are settled and must not be relitigated without new
information. Each file records one decision: the context that forced it, what
was decided, and what we accept as its cost. It consolidates the "do not
relitigate" convention that was previously scattered across the spec's
Constitution, `CLAUDE.md`, the plans' Self-Review Notes, and the chronicles.

## Conventions

- **One decision per file**, named `NNNN-kebab-title.md`, numbered in order of
  ratification.
- **Never edit a decision's substance once it is `Accepted`.** To change one,
  add a *new* record that supersedes it, and set the old record's status to
  `Superseded by NNNN`. Git carries the history; the log never rewrites it.
- **Statuses:** `Accepted`, `Superseded by NNNN`, or (rarely) `Proposed`.
- Where a decision is stated authoritatively elsewhere (the Constitution in
  the long-term-plan spec, or `CLAUDE.md`), the record **cross-links** the
  canonical source rather than duplicating its wording.
- Keep each record short — a Y-statement plus a few lines. If it needs a page,
  it is probably a spec, not a decision record.

## Template

```markdown
# NNNN. Title

**Status:** Accepted (YYYY-MM-DD) · **Decider:** Nathan

In the context of <situation>, facing <problem/forces>, we decided
<the choice>, accepting <the trade-off>.

**Context.** Why this came up.
**Consequence.** What follows, and what we knowingly give up.
**See also.** Links to spec sections, CLAUDE.md, chronicles.
```

## Index

| # | Decision | Status |
|---|----------|--------|
| [0001](0001-determinism-is-constitutional.md) | Determinism is constitutional | Accepted |
| [0002](0002-domains-depend-only-on-kernel.md) | Domains depend only on the kernel | Accepted |
| [0003](0003-trace-protocol-is-the-only-cross-domain-channel.md) | The trace protocol is the only cross-domain channel | Accepted |
| [0004](0004-no-new-dependencies.md) | No new dependencies beyond serde | Accepted |
| [0005](0005-deterministic-collections-and-sorts.md) | Deterministic collections and sorts | Accepted |
| [0006](0006-seed-labels-are-permanent-contracts.md) | Seed-derivation labels are permanent contracts | Accepted |
| [0007](0007-seed-is-identity.md) | The seed is a world's identity | Accepted |
| [0008](0008-typed-quantities.md) | Typed quantities at API boundaries | Accepted |
| [0009](0009-models-author-dice-roll.md) | Models author, dice roll | Accepted |
| [0010](0010-predicate-schema-value-kind-enforced.md) | Predicate schema: value-kind enforced, subject-kind refused | Accepted |
| [0011](0011-studies-are-data-metrics-are-code.md) | Studies are data, metrics are code | Accepted |
| [0012](0012-config-is-json-not-yaml.md) | Config files are JSON, not YAML | Accepted |
| [0013](0013-definition-of-done-includes-the-book.md) | Definition of Done includes the project book | Accepted |
| [0014](0014-fact-day-stays-bare-option.md) | `Fact.day` stays a bare `Option<f64>` | Accepted |
| [0015](0015-predicatedef-name-duplicates-key.md) | `PredicateDef.name` duplicates its registry key | Accepted |
| [0016](0016-studies-preregister-hypotheses.md) | Studies preregister their hypotheses | Accepted |
| [0017](0017-campaigns-drop-year-naming.md) | Campaigns drop the "Year" naming | Accepted |
| [0018](0018-gallery-images-are-hand-rolled-png.md) | Gallery images are hand-rolled PNG | Accepted |
| [0019](0019-no-procedural-macros.md) | No procedural macros | Accepted |
| [0020](0020-campaigns-write-retrospectives.md) | Campaigns write retrospectives | Accepted |
| [0021](0021-no-alignment-axis.md) | No alignment axis; species are not moral types | Accepted |
| [0022](0022-sim-emits-data-clients-render.md) | Sim emits data, clients render | Accepted |
| [0023](0023-in-repo-clients-carry-their-own-toolchains.md) | In-repo clients carry their own toolchains | Accepted |
| [0024](0024-settlement-name-uniqueness-is-reference-time.md) | Settlement-name uniqueness is a reference-time property | Accepted |
| [0025](0025-one-concept-name-one-owner.md) | One concept name, one owner | Accepted |
| [0026](0026-slugs-not-numbers.md) | Slugs, not numbers | Superseded by 0043 |
| [0027](0027-non-workspace-dev-tools-may-use-parser-libraries.md) | Non-workspace dev tools may use parser libraries | Accepted |
| [0028](0028-the-bare-ok-rubric.md) | The bare-ok rubric for primitives at API boundaries | Accepted |
| [0029](0029-ci-checks-500-seed-censuses.md) | CI checks 500-seed censuses; 10k runs are author-time | Accepted |
| [0030](0030-the-confidence-gradient-is-re-scored-not-frozen.md) | The Confidence Gradient is re-scored, not frozen | Accepted |
| [0031](0031-the-frontier-is-published-in-the-book.md) | The frontier is published in the book | Accepted |
| [0032](0032-calibration-loads-the-census-fixture.md) | Calibration loads the drift-checked census fixture, not a live recompute | Accepted |
| [0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md) | Serialized floats are quantized for cross-platform determinism | Accepted |
| [0034](0034-tonogenesis-is-a-regular-conditioned-merger-repair.md) | Tonogenesis is a regular conditioned sound change, not a homophony patch | Accepted |
| [0035](0035-the-capacity-floor-is-reached-by-tone-not-segments.md) | The capacity floor is reached by tone, not by adding segments to atonal species | Accepted |
| [0036](0036-the-proto-assignment-is-merger-aware.md) | The proto-root assignment is merger-aware (zero core homophony) | Accepted |
| [0037](0037-the-room-tier-ledger-is-chunk-partitioned.md) | The room-tier ledger is chunk-partitioned — only if it outgrows memory (near-term defers to The Walk §3.6) | Proposed |
| [0038](0038-identity-computes-on-the-canonical-grid.md) | Identity computes on the canonical grid; observation samples fields | Accepted |
| [0039](0039-epochs-replace-tiers-refine.md) | Epochs replace, tiers refine — a contradicting generator cannot coexist as a tier | Accepted |
| [0040](0040-nextest-is-the-gate-runner.md) | cargo-nextest is the gate's test runner | Accepted |
| [0041](0041-libm-for-portable-transcendentals.md) | libm for portable transcendentals (amends 0004) | Accepted |
| [0042](0042-github-ci-is-manual-only.md) | GitHub Actions CI is manual-only (local + AWS are the gates) | Accepted |
| [0043](0043-numbers-not-slugs.md) | Numbers, not slugs (supersedes 0026's decision-record naming) | Accepted |
| [0044](0044-shared-units-live-in-the-kernel.md) | Shared units live in the kernel (refines 0008) | Accepted |
| [0045](0045-one-canonical-census.md) | One canonical census; frozen studies are evidence | Accepted |
| [0046](0046-census-regen-is-remote-only.md) | Census regeneration is AWS-only | Superseded by 0063 |
| [0047](0047-demography-is-extracted-from-settlement.md) | A `demography` domain is extracted from `settlement` | Accepted |
| [0048](0048-flow-condensation-replaces-the-suitability-scatter.md) | Flow-condensation replaces the suitability scatter | Accepted |
| [0049](0049-the-founder-floor-migrates-to-demography.md) | The MAP-22 founder floor migrates to `demography` | Accepted |
| [0050](0050-entity-hood-for-collections.md) | Entity-hood for collections: collections become entities, singletons stay flat | Accepted |
| [0051](0051-name-salt-stable-identity.md) | Procedural names are salted by stable identity, never by a global mint counter | Accepted |
| [0052](0052-deploy-built-wasm.md) | The Casement's wasm is deploy-built, never committed | Accepted |
| [0053](0053-ocean-fraction-is-a-target-under-supply-limited-crust.md) | The ocean-fraction pin is a target, not a guarantee, under supply-limited crust | Accepted |
| [0054](0054-no-orbital-migration.md) | No semi-major-axis migration; brightening ships, migration is declined | Accepted |
| [0055](0055-external-clients-consume-a-versioned-wasm-catalog.md) | External clients consume Hornvale as a versioned wasm catalog | Accepted |
| [0056](0056-sculpting-is-terrain-epoch-v3.md) | Sculpting is terrain epoch v3 | Accepted |
| [0057](0057-banked-coastal-mechanisms-activate-on-measured-demand.md) | Banked coastal mechanisms activate on measured demand, not by default | Accepted |
| [0058](0058-the-book-is-a-derived-view-grammar-is-build-state.md) | The Book is a derived view; grammar is build-state, never world-state | Accepted |
| [0059](0059-the-book-is-the-primary-artifact.md) | The Book is the primary published artifact; Chronicle and Frontier become appendices | Accepted |
| [0060](0060-the-is-a-classification-predicate.md) | `is-a` is a kernel-core classification predicate | Accepted |
| [0061](0061-x86-sse41-for-hardware-exact-rounding.md) | x86 builds enable SSE4.1 so exact rounding uses hardware `roundsd` (byte-identical; ~15% compute win) | Proposed |
| [0062](0062-the-classification-split.md) | The classification split: `is-a` and `instance-of` coexist | Accepted |
| [0063](0063-census-regen-is-local-again.md) | Census regeneration is local again (supersedes 0046) | Accepted |
