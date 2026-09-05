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
| [0006](0006-seed-labels-are-permanent-contracts.md) | Seed-derivation labels are permanent contracts | Superseded by 0099 |
| [0007](0007-seed-is-identity.md) | The seed is a world's identity | Accepted |
| [0008](0008-typed-quantities.md) | Typed quantities at API boundaries | Accepted |
| [0009](0009-models-author-dice-roll.md) | Models author, dice roll | Accepted |
| [0010](0010-predicate-schema-value-kind-enforced.md) | Predicate schema: value-kind enforced, subject-kind refused | Accepted |
| [0011](0011-studies-are-data-metrics-are-code.md) | Studies are data, metrics are code | Accepted |
| [0012](0012-config-is-json-not-yaml.md) | Config files are JSON, not YAML | Accepted |
| [0013](0013-definition-of-done-includes-the-book.md) | Definition of Done includes the project book | Accepted |
| [0014](0014-fact-day-stays-bare-option.md) | `Fact.day` stays a bare `Option<f64>` | Superseded by 0126 |
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
| [0042](0042-github-ci-is-manual-only.md) | GitHub Actions CI is manual-only (local + AWS are the gates) | Superseded by 0125 |
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
| [0064](0064-potency-is-challenge-rating-over-thirty.md) | Potency is Challenge Rating over thirty, over the supernatural set | Accepted |
| [0065](0065-socialform-and-the-nested-capacity-lattice.md) | SocialForm is a universal axis; capacities are a nested lattice | Accepted |
| [0066](0066-language-drift-is-a-function-of-sociality-and-lifespan.md) | Language drift-rate is a function of sociality and lifespan | Accepted |
| [0067](0067-the-mind-society-vector-split.md) | The mind/society vector split | Accepted |
| [0068](0068-society-gates-on-sociality-not-sedentism.md) | The society vector gates on sociality, not sedentism | Accepted |
| [0069](0069-fine-position-is-never-serialized.md) | Fine position is never serialized — the two-tier position law | Accepted |
| [0070](0070-wounds-commit-health-folds.md) | Wounds commit; health folds — no HP counter anywhere | Accepted |
| [0071](0071-one-snapshot-per-commit.md) | One snapshot per commit; panes are pure projections | Accepted |
| [0072](0072-derived-geometry-is-causal.md) | Derived geometry is causal | Accepted |
| [0073](0073-epoch-granularity-is-declared.md) | Epoch granularity is declared, not discovered | Accepted |
| [0074](0074-capacities-are-a-chain-and-facts-gate-on-components.md) | Capacities are a chain, and species facts gate on components | Accepted |
| [0075](0075-the-causal-geometry-is-the-anchor-graph.md) | The causal derived geometry is the anchor graph, not the metric layout | Accepted |
| [0076](0076-the-situated-pole-is-egocentric-and-knowledge-limited.md) | The scene protocol's situated pole is egocentric and knowledge-limited | Accepted |
| [0077](0077-zoom-in-the-room-mesh-is-path-truncation.md) | Zoom in the room mesh is path truncation; the chart may show a scale the body cannot enter | Accepted |
| [0078](0078-thresholded-classification-artifacts-are-platform-local.md) | An artifact dominated by thresholded classifications is drift-checked platform-locally, not in CI | Accepted |
| [0079](0079-census-goldens-are-authored-on-one-enforced-host.md) | Census goldens are authored on one host, and it is enforced | Accepted |
| [0080](0080-chronicity-is-a-diagnostic-the-alarm-is-stuck.md) | `chronicity` is a diagnostic; the population-health alarm is `stuck` | Accepted |
| [0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md) | One heavy writer per box, claimed at the write seam | Accepted |
| [0082](0082-locale-chamber-place.md) | Locale, chamber, place — "room" unqualified is retired | Superseded by 0101 |
| [0083](0083-a-label-per-algorithm-and-never-in-advance.md) | A seed-derivation label is declared per *algorithm*, and never in advance | Accepted |
| [0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md) | An epoch is declared only when a derivation moved — `room/furnishing` stays at v1 | Accepted |
| [0085](0085-derived-geometry-reads-the-durable-signal.md) | Derived geometry reads the durable signal, not the living one | Accepted |
| [0086](0086-the-heavy-tier-runs-on-the-canonical-box.md) | The heavy tier runs on the canonical box; campaigns run on the Mac | Accepted |
| [0087](0087-a-benchmarks-timings-are-a-record-not-a-golden.md) | A benchmark's timings are a record, not a golden | Accepted |
| [0088](0088-the-suite-watches-its-own-clock.md) | The suite watches its own clock — a failing test, not a dashboard | Accepted |
| [0089](0089-an-epoch-freezes-when-it-can-be-stamped-on-a-saved-world.md) | An epoch freezes when a world saved from `main` can carry it (refines 0006) | Accepted |
| [0090](0090-the-canonical-host-is-audited-not-assumed.md) | The canonical host is audited, not assumed — and the audit came back clean | Accepted |
| [0091](0091-glibc-does-not-explain-it-and-the-machine-does-not-matter.md) | glibc does not explain 0063, and the machine does not matter | Accepted |
| [0092](0092-derivation-at-named-sites.md) | Derivation happens at named construction sites; readouts take artifacts | Accepted |
| [0093](0093-seed-hunting-is-not-a-test-mechanism.md) | Seed-hunting is not a test mechanism | Accepted |
| [0094](0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md) | A deliberate duplicate shares its *roster*, never its *derivation* | Accepted |
| [0095](0095-a-corpus-is-an-instrument-never-a-standard.md) | A trope corpus is a provenance-stamped instrument, never a standard | Accepted |
| [0096](0096-diversity-is-terminal-and-rubberbanding-is-multi-axis.md) | Peoples-diversity is a terminal value; rubberbanding is multi-axis contest, never handicap | Accepted |
| [0097](0097-assert-the-robust-half-measure-the-fragile-half.md) | Assert the robust half in the gate; measure the fragile half in the census | Accepted |
| [0098](0098-hornvale-is-single-player.md) | Hornvale is single-player, forever | Accepted |
| [0099](0099-worlds-are-version-locked.md) | Worlds are version-locked to the code that generated them | Accepted |
| [0100](0100-fact-phenomenon-myth.md) | Fact, phenomenon, myth — the three registers, and what may be written where | Accepted |
| [0101](0101-geometry-and-society-are-separate-vocabularies.md) | Geometry and society are separate vocabularies — locale/site/chamber, community/settlement | Accepted |
| [0102](0102-one-per-cell-was-an-index-artifact.md) | The one-community-per-cell rule is an index artifact, not a design position | Accepted |
| [0103](0103-suitability-and-headcount-are-distinct-types.md) | Dimensionless suitability and headcount capacity are distinct types | Accepted |
| [0104](0104-a-threshold-must-know-its-variates-distribution.md) | A threshold must know the distribution of the variate it reads — warp at the call site | Accepted |
| [0105](0105-water-keeps-bands-rock-becomes-a-graph.md) | Water keeps bands, rock becomes a graph — supersedes The Stratum's D3 for the rock realm only | Accepted |
| [0106](0106-a-constants-justification-must-match-its-kind.md) | A constant's justification must match its kind | Accepted |
| [0107](0107-habitability-is-a-relation-not-a-constant.md) | Habitability is a relation, not a constant | Accepted |
| [0108](0108-a-reachability-claim-is-a-census-coverage-table.md) | A reachability claim is a census coverage table | Accepted |
| [0109](0109-a-population-claim-is-asserted-in-the-gate-over-the-fixture.md) | A population claim is asserted in the commit gate, over the committed fixture | Accepted |
| [0110](0110-the-census-is-the-suites-shared-world-building-pass.md) | The census is the suite's shared world-building pass | Accepted |
| [0111](0111-a-census-resident-check-needs-a-live-tripwire.md) | A census-resident check requires a live tripwire in the gate | Accepted |
| [0112](0112-the-synthetic-route-requires-committed-facts.md) | The synthetic route requires that the behaviour read committed facts | Accepted |
| [0113](0113-the-dev-profile-is-optimized-workspace-wide.md) | The dev profile is optimized workspace-wide | Accepted |
| [0114](0114-a-native-client-drives-across-the-linker-and-reads-across-the-serializer.md) | A native client drives across the linker and reads across the serializer | Accepted |
| [0115](0115-a-clients-mirror-may-omit-a-channel.md) | A client's mirror may omit a channel, and the omission is the enforcement | Accepted |
| [0116](0116-possession-is-a-parameter-not-a-fixture.md) | Possession is a parameter, not a fixture | Accepted |
| [0117](0117-the-client-re-derives-nothing-the-sim-emits.md) | The client re-derives nothing the simulation already decides | Accepted |
| [0118](0118-the-board-is-an-orphan-ref-of-immutable-posts-never-rerooted.md) | The board is an orphan ref of immutable posts, never rerooted | Accepted |
| [0119](0119-an-instruments-silence-means-the-claim-held.md) | An instrument's silence means the claim held, and nothing else | Accepted |
| [0120](0120-the-affinity-ladders-level-is-derived-not-authored.md) | The affinity ladder's level is derived, not authored | Accepted |
| [0121](0121-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md) | An ordinal field may band a blend; a nominal field must take a partition | Accepted |
| [0122](0122-an-emit-gate-is-not-a-grain-gate.md) | An emit gate is not a grain gate — a field's doc names the condition that makes it absent | Accepted |
| [0123](0123-disclose-a-resolution-rather-than-refine-a-field.md) | When a view is finer than the model behind a field, the document discloses the resolution rather than the field inventing detail | Accepted |
| [0124](0124-a-refinement-preregisters-a-conservation-criterion.md) | A refinement preregisters a conservation criterion, not only variation criteria | Accepted |
| [0125](0125-github-actions-is-retired.md) | GitHub Actions is retired — every gate is local (supersedes 0042) | Accepted |
| [0126](0126-fact-day-is-a-typed-world-time.md) | `Fact.day` carries a typed `WorldTime` (supersedes 0014) | Accepted |
| [0127](0127-identity-keys-and-discrimination-keys-are-different-kinds.md) | An identity key and a discrimination key are different kinds | Accepted |
| [0128](0128-name-the-transformation-a-quantity-is-gauge-under.md) | Name the transformation a quantity is gauge under | Accepted |
| [0129](0129-the-board-gets-a-risk-scoped-lane-and-a-path-scoped-hook-rule.md) | The board gets a risk-scoped lane and a path-scoped hook rule | Accepted |
| [0130](0130-a-sub-threshold-watercourse-is-a-narrow-channel.md) | A sub-threshold watercourse is a narrow channel, not an absent one | Accepted |
| [0131](0131-refuted-is-a-seventh-registry-status.md) | `refuted` is a seventh registry status | Accepted |
| [0132](0132-three-gates-named-for-the-campaign-moment.md) | Three gates, named for the campaign moment | Accepted |
| [0133](0133-nontrivial-checks-run-in-one-serial-lane.md) | Nontrivial checks run in one serial lane | Accepted |
| [0134](0134-a-partition-statistic-refuted-by-its-own-mechanism-is-retired-not-rescued.md) | A partition statistic refuted by its own mechanism is retired, not rescued | Accepted |
| [0135](0135-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md) | A capability corpus is a sibling to a trope corpus, never a member | Accepted |
| [0136](0136-a-coverage-verdict-cites-a-checked-anchor.md) | A coverage verdict cites a machine-checked anchor | Accepted |
| [0137](0137-the-craton-clamp-is-a-budget-not-a-limit.md) | The craton clamp is a budget, not a limit | Accepted |
| [0138](0138-a-preregistered-criterion-may-be-restated-when-its-estimator-is-wrong.md) | A preregistered criterion may be restated when its estimator is wrong | Accepted |
| [0139](0139-main-advances-only-through-the-lock.md) | Main advances only through the lock | Accepted |
| [0140](0140-the-stage-gate-is-a-kind-of-queue-request.md) | The stage gate is a kind of queue request, not a dispatch path | Accepted |
| [0141](0141-compass-navigation-is-an-overlay.md) | Compass navigation is an overlay, never the graph | Superseded by 0506 |
| [0142](0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md) | A rendering channel carries one axis, and a lost axis is declared | Accepted |
| [0143](0143-the-underworld-carries-two-ladders.md) | The underworld carries two ladders, and neither derives the other | Accepted |
| [0144](0144-a-caves-depth-is-a-budget-in-metres.md) | A cave's depth is a budget in metres, not a stratigraphic band | Accepted |
| [0145](0145-one-community-per-place-not-per-cell.md) | One community per place, where a place is a cell and a rung | Accepted |
| [0147](0147-a-feature-is-individuated-by-traversal-its-name-derived.md) | A landscape feature is individuated by traversal, and its name is derived, never committed | Accepted |
| [0156](0156-a-typology-bundle-is-authored-not-derived.md) | A typology bundle is authored, not derived | Accepted |
| [0157](0157-never-ship-a-typology-bundle-no-family-uses.md) | Never ship a typology bundle no family uses | Accepted |
| [0158](0158-a-trill-is-not-an-exotic-manner.md) | A trill is not an exotic manner | Accepted |
| [0159](0159-focus-is-the-clients-one-input-mode.md) | Focus is the client's one input mode, and it is shown | Accepted |
| [0166](0166-a-generated-artifact-cannot-be-merged-by-regenerating-it.md) | A generated artifact cannot be merged by regenerating it | Accepted |
| [0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md) | A driver is interchangeable, and a possessed body is a creature | Accepted |
| [0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md) | The effect of an act belongs to the body, not the driver | Accepted |
| [0169](0169-mood-is-a-property-of-the-action-and-the-sigil-is-a-namespace.md) | In-character is a property of the action, and `!` is a namespace | Accepted |
| [0170](0170-an-out-of-character-act-bypasses-the-body-never-the-world.md) | An out-of-character act bypasses the body, never the world — and it may commit, stamped | Accepted |
| [0171](0171-a-players-acts-are-not-filtered-out-of-a-saved-world.md) | A player's acts are not filtered out of a saved played world | Accepted |
| [0172](0172-a-concept-with-no-possible-referent-is-an-extradiegetic-gap.md) | A concept with no possible referent is lexicalised as an extradiegetic gap | Accepted |
| [0176](0176-the-chamber-address-gains-a-floor-and-sunless-becomes-nadir.md) | The chamber address gains a floor, and `Sunless` becomes `Nadir`, on one `chamber/v3` epoch | Accepted |
| [0186](0186-an-instant-is-an-exact-tick-count.md) | An instant is an exact tick count — the lattice is the time domain | Accepted |
| [0187](0187-a-pre-genesis-sky-query-is-clamped-to-genesis.md) | A pre-genesis sky query is clamped to genesis, deliberately | Accepted |
| [0188](0188-quantize-still-governs-magnitude-time-leaves-it.md) | Quantize still governs magnitude; time leaves the contract entirely | Accepted |
| [0189](0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md) | A pre-flip world file does not load, and that is the point | Accepted |
| [0190](0190-a-reachability-trace-is-not-closed-by-finding-one-funnel.md) | A reachability trace is not closed by finding one funnel | Accepted |
| [0191](0191-an-exact-store-needs-no-quantized-query-bound.md) | An exact store needs no quantized query bound | Accepted |
| [0196](0196-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md) | A map's frame is a fact about the world, and a view of the world is a lens | Accepted |
| [0197](0197-co-location-is-not-discovery.md) | Co-location is not discovery | Accepted |
| [0206](0206-a-derived-values-key-is-its-validity.md) | A derived value's key is its validity | Accepted |
| [0207](0207-the-derived-store-is-generic-per-shape.md) | The derived store is generic per value shape, never heterogeneous | Accepted |
| [0208](0208-completing-the-key-retired-the-level-guard.md) | Completing `corner_weights`'s key retired its level guard | Accepted |
| [0216](0216-the-depth-band-roster-is-a-kernel-type.md) | The depth-band roster is a kernel type, and moving it is not an epoch | Accepted |
| [0226](0226-a-possessed-host-is-co-present-not-displaced.md) | A possessed host is co-present, not displaced | Accepted |
| [0227](0227-possession-selects-a-body-it-does-not-mint-one.md) | Possession selects a body; it does not mint one | Accepted |
| [0228](0228-a-controller-is-a-parameter-of-the-tick.md) | A controller is a parameter of the tick, not a property of the body | Accepted |
| [0229](0229-one-body-type.md) | One body type | Accepted |
| [0230](0230-a-query-bound-is-quantized-to-read-a-quantized-store.md) | A query bound is quantized to read a quantized store | Superseded by 0191 |
| [0246](0246-a-renamed-concept-keeps-its-serialized-spelling-forever.md) | A renamed concept keeps its serialized spelling forever, and the freeze needs a test that cannot be rebaselined | Accepted |
| [0247](0247-a-mesh-vertex-is-a-vertex-and-a-face-is-a-facet.md) | A mesh vertex is a `Vertex` and a face is a `Facet` | Accepted |
| [0256](0256-a-hosts-testimony-is-fallible-by-construction.md) | A host's testimony is fallible by construction, and the gap is the deliverable | Accepted |
| [0257](0257-felt-states-are-concepts.md) | Felt states are concepts, and they accede as an appended cohort | Accepted |
| [0258](0258-introspective-access-is-bounded.md) | Introspective access is bounded — a creature cannot perceive what its own arbitration suppressed | Accepted |
| [0259](0259-conceptual-deficiency-is-derived-not-authored.md) | Conceptual deficiency is derived from authored psychology, never authored per people | Accepted |
| [0260](0260-a-seed-invariant-derivation-is-a-window.md) | A seed-invariant derivation is a window, not a census metric | Accepted |
| [0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md) | A rule duplicated on purpose carries a pointer in both copies and a two-way agreement test | Accepted |
| [0266](0266-an-utterance-is-a-fact.md) | An utterance is a fact, an event is an entity, and roles are predicates on that entity | Accepted |
| [0286](0286-each-realizer-ignores-part-of-the-clause.md) | A language-neutral clause states more than any one realizer surfaces | Accepted |
| [0287](0287-a-zoom-rung-is-a-mesh-depth.md) | A zoom rung is a mesh depth | Accepted |
| [0288](0288-fog-of-war-is-band-as-alone.md) | Bands B through E share one epistemic model; fog of war is band A's alone | Accepted |
| [0289](0289-the-map-is-layers-with-distinct-cache-keys.md) | The map is layers with distinct cache keys, and terrain never shares an invalidation key with discovery | Accepted |
| [0290](0290-the-perception-layer-is-drawn-where-the-mesh-is-reachable.md) | The perception layer is drawn where the mesh is reachable | Accepted |
| [0291](0291-the-point-under-the-cursor-is-invariant-across-a-zoom-step.md) | The geographic point under the cursor is invariant across a zoom step | Accepted |
| [0292](0292-centre-on-arrival-anchor-on-gesture.md) | Centre on arrival, anchor on gesture | Accepted |
| [0293](0293-a-rung-is-a-property-of-consulting-the-map.md) | A rung is a property of consulting the map, not of the walker | Accepted |
| [0294](0294-a-mode-gesture-is-not-a-fetch.md) | A mode gesture is not a fetch | Accepted |
| [0295](0295-pre-formatted-sim-output-survives-the-prose-pane.md) | Pre-formatted sim output survives the prose pane | Accepted |
| [0296](0296-tense-is-stated-never-derived.md) | Tense is stated, never derived — a clause has no clock | Accepted |
| [0297](0297-a-predicates-valence-is-stated-once-and-commons-parts-are-selected-from-it.md) | A predicate's valence is stated once, and Common's parts are selected from it | Accepted |
| [0306](0306-a-hosts-cooperation-is-derived-from-doctrine-and-conduct-never-authored.md) | A host's cooperation is derived from doctrine and conduct, never authored | Accepted |
| [0307](0307-a-deliberate-falsehood-is-a-distinct-testimony-variant-from-a-lexical-substitution.md) | A deliberate falsehood is a distinct testimony variant from a lexical substitution | Accepted |
| [0308](0308-prior-and-fold-are-stored-separately-their-disagreement-is-the-output.md) | Prior and fold are stored separately; their disagreement is the output | Accepted |
| [0326](0326-a-clause-complement-rides-the-transitive-frame-no-sentential-valence.md) | A clause complement rides the transitive frame — no `Valence::Sentential` | Accepted |
| [0327](0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md) | Embedding and coordination are two operators — a slot and a list | Accepted |
| [0328](0328-embedding-nests-one-level-a-cap-on-demonstrated-depth.md) | Embedding nests one level — a cap on demonstrated depth, not a safety belt | Accepted |
| [0329](0329-subordination-strategy-and-conjunction-are-drawn-per-tongue.md) | Subordination strategy and conjunction are drawn per tongue, not assumed | Accepted |
| [0330](0330-the-corpus-score-is-demonstrated-not-declared.md) | The corpus score is demonstrated, not declared | Accepted |
| [0386](0386-a-corpus-declares-its-demands-or-derives-them-never-both.md) | A corpus declares its demands or derives them, never both | Accepted |
| [0387](0387-an-absent-direction-is-unknown-never-inferred.md) | Coverage is reported per direction, and an absent direction is unknown | Accepted |
| [0388](0388-the-ladder-is-a-production-instrument-and-parse-robustness-is-a-separate-axis.md) | The ladder is a production instrument; parse-robustness is a separate axis | Accepted |
| [0396](0396-a-passage-is-a-thing-and-openness-is-its-fold.md) | A passage is a thing, and `openness` is its fold (supersedes 0367) | Accepted |
| [0397](0397-the-knowledge-gate-denies-a-passage.md) | The knowledge gate denies a passage — re-keying it answers 0369 | Accepted |
| [0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md) | A capability nothing can reach is not a capability — the strongbox loses its population gate | Accepted |
| [0399](0399-closing-is-not-locking.md) | Closing is not locking — a lid and a lock are different states | Accepted |
| [0400](0400-custody-is-an-observable-not-a-vital.md) | Custody is an observable, not a vital — carried things ride the `self` channel | Accepted |
| [0446](0446-an-existential-fronts-a-locative-clause-a-transformation-not-a-sixth-valence.md) | An existential fronts a locative clause — a transformation, not a sixth `Valence` | Accepted |
| [0447](0447-definiteness-is-a-discourse-fact-derived-from-referent-recurrence-not-a-clause-field.md) | Definiteness is a discourse fact, derived from referent recurrence — not a clause field | Accepted |
| [0448](0448-a-produce-side-demand-instance-statistic-complements-the-composite-and-never-replaces-it.md) | A produce-side demand-instance statistic complements the composite, and never replaces it | Accepted |
| [0466](0466-authored-evidence-is-read-through-its-own-schema.md) | Authored evidence is read through its own schema, not the live registry | Accepted |
| [0467](0467-a-frozen-criterion-must-discriminate-against-rival-explanations-of-a-pass.md) | A frozen criterion must discriminate against the rival explanations of a pass | Accepted |
| [0506](0506-the-occupancy-lattice-is-a-cube-sphere.md) | The occupancy lattice is a cube-sphere; the icosphere stays the field substrate (supersedes 0141, amends 0287) | Accepted |
| [0507](0507-every-lattice-in-the-project-is-eight-connected.md) | Every lattice in the project is 8-connected | Accepted |
| [0508](0508-a-diagonal-costs-root-two.md) | A diagonal costs √2, because the movement clock is flat | Accepted |
| [0509](0509-a-diagonal-through-a-two-walled-corner-is-refused.md) | A diagonal through a two-walled corner is refused; one open flank permits it | Accepted |
| [0510](0510-compass-input-is-four-way-primary-and-eight-way-capable.md) | Compass input is 4-way primary and 8-way capable; no destination requires a diagonal | Accepted |
| [0511](0511-walk-depth-is-globe-level-plus-seven.md) | Walk depth is `globe_level + 7`, chosen to preserve step length | Accepted |
| [0512](0512-the-projection-is-tangent-warped-and-there-is-one-of-it.md) | The cube-sphere projection is tangent-warped, and there is one of it | Accepted |
| [0513](0513-a-quadrilateral-lattice-carries-no-third-axis-or-orientation-flag.md) | A quadrilateral lattice carries no third axis or orientation flag | Accepted |
| [0514](0514-a-census-refresh-needs-no-authorization.md) | A census refresh needs no per-run authorization (retires the AWS-spend carve-out outright) | Accepted |
| [0515](0515-a-diagonal-step-reaches-root-two-as-well-as-costing-it.md) | A diagonal step REACHES √2, as well as costing it (The Pavement; the index row was omitted at landing) | Accepted |
| [0516](0516-a-reachable-lock-implies-a-reachable-key-is-left-empirical.md) | "A reachable lock implies a reachable key" is left empirical, deliberately — wait for the residents | Accepted |
| [0536](0536-the-resident-fold-store-is-session-owned-and-never-serialized.md) | The resident fold store is session-owned and never serialized | Accepted |
| [0537](0537-a-reader-never-observes-a-fold-behind-its-ledger.md) | A reader never observes a fold behind its ledger — the seam is at read | Accepted |
| [0538](0538-the-trail-is-a-resident-index-not-a-cached-hub.md) | The trail is a resident index, not a cached hub | Accepted |
| [0539](0539-a-past-instant-read-resumes-from-the-reset-checkpoint.md) | A past-instant read resumes from the reset checkpoint, over a per-reset prefix accumulator | Accepted |
| [0540](0540-the-past-day-affect-path-preserves-the-unfiltered-reset.md) | The past-day affect path preserves the unfiltered reset, knowingly | Accepted |
| [0541](0541-a-campaign-time-hash-constant-witness-retires-at-close.md) | A campaign-time hash-constant witness retires at close | Accepted |
| [0546](0546-the-roll-is-a-pure-function-of-the-observers-room-and-it-is-diegetic.md) | The roll is a pure function of the observer's room, and it is diegetic | Accepted |
| [0547](0547-a-resident-is-a-living-person-derived-on-demand-and-never-generated.md) | A resident is a living person, derived on demand and never generated | Accepted |
| [0548](0548-individual-deviation-is-the-kinds-dispersion-drawn-per-resident.md) | Individual deviation is the kind's dispersion, drawn per resident | Accepted |
| [0549](0549-a-body-off-the-roll-is-frozen-at-its-committed-facts-and-caught-up-on-return.md) | A body off the roll is frozen at its committed facts and caught up on return | Accepted |
| [0556](0556-totality-by-registry-replaces-totality-by-compiler.md) | Totality by registry replaces totality by compiler, and every check names its direction | Accepted |
| [0557](0557-a-handle-is-a-convenience-where-a-variant-was-a-requirement.md) | A handle is a convenience where a variant was a requirement | Accepted |
| [0558](0558-sleep-is-never-gated-the-place-grades-it.md) | Sleep is never gated — the place grades it | Accepted |
| [0566](0566-a-place-is-a-graph-before-it-is-a-map.md) | A place is a graph before it is a map, and the grammar is series-parallel | Accepted |
| [0567](0567-stairs-pair-by-coordinate.md) | Stairs pair by coordinate: a stairway's two ends share a cell | Accepted |
| [0568](0568-cycle-density-is-derived-not-authored.md) | Cycle density is derived from rock and workmanship, never authored | Accepted |
| [0576](0576-the-capability-provision-table.md) | The capability provision table spans all three homes; ledger wired first | Accepted |
| [0577](0577-the-realization-witness.md) | The realization witness | Superseded by 0581 |
| [0578](0578-kinship-a-genesis-fact.md) | Kinship as a genesis fact | Superseded by 0584 |
| [0579](0579-affect-component-data-never-a-fact.md) | Affect — component data, never a fact | Accepted |
| [0580](0580-acts-are-addressable-without-being-stored.md) | Acts are addressable without being stored — a derived act view, session-home wired | Superseded by 0585 |
| [0581](0581-the-witness-is-bound-to-its-situation.md) | The witness is bound to its situation (supersedes 0577) | Superseded by 0582 |
| [0582](0582-the-witness-binding-is-bidirectional.md) | The witness binding is bidirectional (supersedes 0581) | Superseded by 0583 |
| [0583](0583-the-witness-limits-list-is-open-not-closed.md) | The witness limits list is open, not closed (supersedes 0582) | Accepted |
| [0584](0584-kinship-direction-and-the-parent-of-generation-cut.md) | Kinship direction and the `parent-of` generation cut (supersedes 0578) | Accepted |
| [0585](0585-act-chronology-completes-too-and-the-method-is-act-handle.md) | `bundle:act-chronology` completes too, and the method is `Act::handle` (supersedes 0580) | Accepted |
| [0596](0596-a-creature-feels-what-its-own-last-resolution-felt.md) | A creature feels what its own last resolution felt | Accepted |
| [0597](0597-the-roster-is-a-struct-of-arrays-with-one-append.md) | The roster is a struct of arrays with one append | Accepted |
| [0598](0598-per-turn-work-is-a-counted-budget-in-the-commit-gate.md) | Per-turn work is a counted budget in the commit gate | Accepted |
| [0606](0606-a-world-build-is-a-named-site.md) | A world build is a named site on a bidirectional roster | Accepted |
| [0607](0607-the-seed-42-fixture-is-an-input.md) | The seed-42 world fixture is an input as well as an assertion | Accepted |
| [0616](0616-a-gate-is-a-requirement-on-a-way.md) | A gate is a requirement on a way, realized as four parts kept apart | Accepted |
| [0617](0617-a-locks-substance-is-derived-from-rock-and-work.md) | A lock's substance is derived from rock and work; the pattern chooses where, never what | Accepted |
| [0618](0618-a-descent-key-makes-the-plan-a-save-format-contract.md) | A descent key's identity is a plan position, so the plan grammar is a save-format contract | Accepted |
| [0619](0619-the-drop-is-a-stairway-with-its-up-half-omitted.md) | The drop is a stairway with its up half omitted, and `Fly` is its key | Accepted |
| [0620](0620-the-cycle-pattern-inventory-is-a-frozen-corpus.md) | The cycle-pattern inventory is a frozen corpus of nine rows, and solvability for a body holding nothing is the invariant | Accepted |
| [0626](0626-a-terrain-verdict-is-held-for-the-session-keyed-by-room.md) | A terrain verdict is held for the session, keyed by room, with its terrain's identity supplied by ownership | Accepted |
| [0627](0627-the-emitter-scan-advances-through-a-read-side-verdict-index.md) | The emitter scan advances over new sightings through a read-side verdict index, and is still not a tenant | Accepted |
| [0628](0628-a-registry-rows-mechanism-is-a-count-not-a-reading.md) | A registry row's mechanism is a count, not a reading | Accepted |
| [0646](0646-the-inhabited-reading-is-a-function-of-the-plan.md) | The inhabited reading is a function of the plan, drawn by nothing and stamped on nothing | Accepted |
| [0647](0647-a-made-chamber-is-written-from-the-ledger-at-the-walk.md) | A made chamber is written from the ledger at the walk, never committed as a fact | Accepted |
| [0648](0648-the-hoarder-sits-at-the-sanctum-on-what-lies-there.md) | The hoarder sits at the sanctum on what lies there, and holds nothing | Accepted |
| [0649](0649-a-cut-place-outlives-its-people.md) | A cut place outlives its people: `Made` reads the occupation, and tenancy decides the tense | Accepted |
| [0656](0656-a-held-bodys-walk-commits-what-it-does.md) | A held body's walk commits what it does | Accepted |
| [0657](0657-off-the-walk-band-a-held-body-holds.md) | Off the walk band a held body holds | Accepted |
| [0658](0658-the-wait-line-minutes-the-held-bodys-acts.md) | The wait line minutes the held body's acts | Accepted |
| [0666](0666-the-enterability-gate-is-a-site-not-built.md) | The enterability gate is a `Site`, and `built` narrows to one kind of it | Accepted |
| [0667](0667-a-placed-site-is-re-sited-to-a-facet-by-a-seeded-draw.md) | A placed site is re-sited from its vertex to one facet by a seeded draw | Accepted |
| [0668](0668-a-site-carries-an-extent-and-this-campaign-emits-point.md) | A `Site` carries an `Extent`, and this campaign only ever emits `Point` | Accepted |
| [0669](0669-a-sites-tier-is-placed-or-derived.md) | A site's tier is `placed` or `derived`, and standing is a separate axis | Accepted |
| [0670](0670-a-placed-sites-glyph-is-drawn-ungated-its-name-is-not.md) | A placed site's glyph is drawn ungated; its name is still withheld | Accepted |
| [0686](0686-a-kinds-prevalence-never-normalises-against-its-siblings.md) | A kind's prevalence never normalises against its siblings; a simplex constraint is a structural cap on enterable density | Accepted |
| [0687](0687-a-derived-surface-reads-continuous-causes-never-categorical-labels.md) | A derived surface reads continuous causes, never categorical labels; never materialize a `productivity` field | Accepted |
| [0696](0696-the-sleep-path-is-denominated-in-the-local-day.md) | The sleep path is denominated in the local day, closing 0587's inversion | Accepted |
| [0697](0697-what-an-afforded-site-is-worth-is-a-property-of-the-sleeper.md) | What an afforded site is worth is a property of the sleeper's species | Accepted |
| [0698](0698-a-kind-is-committable-where-an-anchor-identity-is-not.md) | A kind is committable where an anchor identity is not | Accepted |
| [0726](0726-a-kind-to-kind-edge-is-derived-from-traits-both-kinds-carry.md) | A kind-to-kind edge is derived from traits both kinds carry | Accepted |
| [0727](0727-relation-rank-is-set-by-whether-a-reversal-is-wanted.md) | Relation rank is set by whether a reversal is wanted | Accepted |
| [0728](0728-a-scalar-lives-in-the-row-of-the-marker-it-depends-on.md) | A scalar lives in the row of the marker it depends on | Accepted |
| [0729](0729-recovery-grades-the-committed-kind-not-a-room-boolean.md) | Recovery grades the committed kind, not a room boolean | Accepted |
| [0730](0730-the-chooser-prefers-and-no-travel-preserves-the-diagnostic.md) | The chooser prefers; no-travel preserves the diagnostic | Accepted |
