# Eclipse Rhythm and View Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the shipped Eclipse Seasons physics into structured recurrence and observer-aware native/WASM scene output under `scene/eclipses/v3`.

**Architecture:** Keep all physical derivation in `domains/astronomy`, with pure recurrence and observer functions over existing `StarSystem`, `Calendar`, `EclipseEvent`, and typed instants. `windows/scene` owns the v3 wire document and quantized emission; CLI, WASM, and almanac are adapters over that producer and never re-derive simulation decisions.

**Tech Stack:** Rust workspace, `serde`/`serde_json`, kernel `math`, `StdDays`/`StdInstant`, native CLI, hand-written `world-wasm` ABI, committed Markdown reference and almanac artifacts, `cargo nextest`.

**Spec:** `docs/superpowers/specs/2026-09-10-eclipse-rhythm-and-view-design.md`

## Global Constraints

- Domains depend on `hornvale-kernel` and nothing else; cross-domain consumers use existing window boundaries and trace protocol.
- No new genesis draw, stream label, save fact, or epoch; reuse the landed `moon-nodes` input and preserve stream order.
- Closed-form syzygy enumeration remains the event source; do not add an N-body integrator or sampled time loop.
- Evaluate forcing and angular-size quantities at the event instant; quantize floating-point values only at scene/almanac emission.
- `scene/eclipses/v3` is the active pre-alpha contract; v2 compatibility is intentionally out of scope.
- Clients receive simulation decisions from the producer and may only re-derive presentation geometry.
- Do not add transits, occultations, standstills, tidal braking, variable stars, equation-of-time work, aurorae, mythology, or social response.

## File Map

- Modify `domains/astronomy/src/eclipses.rs`: recurrence record/helpers and one observer result over existing eclipse primitives.
- Modify `docs/audits/type-audit-report.md` and `docs/timings.md` when the repository gate regenerates mandatory reports for the public API and test/gate runs.
- Modify `domains/astronomy/src/lib.rs` only if the new public types/functions are not already re-exported by the crate’s existing surface.
- Modify `domains/astronomy/src/eclipses.rs` tests and `domains/astronomy/tests/suite/` tests: physics identities, edge cases, and determinism.
- Modify `windows/scene/src/lib.rs`: v3 schema types, observer input validation, event mapping, and JSON serialization.
- Modify `windows/scene` tests/fixtures: v3 shape, exact ticks, optional observer, and byte-stable output.
- Modify `cli/src/main.rs`: eclipse query flags and v3 output help/dispatch.
- Modify `clients/world-wasm/src/lib.rs` and its drive/check fixtures: observer-aware export and ABI validation.
- Modify `windows/almanac/src/lib.rs` and tests: structured rhythm vocabulary and optional observer reading.
- Modify `book/src/reference/scene-eclipses-v2.md` or replace it with `scene-eclipses-v3.md`: authoritative v3 contract.
- Modify `docs/audits/campaign-reconciliation.tsv` and campaign ledger as artifacts evolve; do not add census columns without measured need.

### Stage 1: Domain observables
**Goal:** Make recurrence and observer results explicit, pure astronomy outputs.
**Success Criteria:** Existing Eclipse Seasons tests remain green; new APIs cover Saros/exeligmos relationships and all observer boundary cases.
**Tests:** `cargo test -p hornvale-astronomy --lib eclipses::tests` plus focused integration tests.
**Status:** Complete

### Stage 2: Scene contract
**Goal:** Emit `scene/eclipses/v3` with recurrence records, geographic regions, and optional observer results.
**Success Criteria:** Native scene output has a stable v3 shape, exact ticks, explicit null/absent semantics, and no world mutation.
**Tests:** `cargo test -p hornvale-scene --lib eclipses`; scene JSON shape and determinism tests.
**Status:** Complete

### Stage 3: Surfaces
**Goal:** Wire v3 through CLI, WASM, almanac, and reference documentation.
**Success Criteria:** Native and WASM queries agree byte-for-byte; almanac and reference output describe the same fields and vocabulary.
**Tests:** CLI scene tests, `clients/world-wasm` checks, almanac tests, and native/WASM smoke.
**Status:** Complete

### Stage 4: Artifacts and verification
**Goal:** Re-pin only contract-bearing output and complete repository gates.
**Success Criteria:** Required fixtures and bundles are regenerated from the implementation, docs audits pass, and no census regeneration is performed without a measured schema change.
**Tests:** `make quick`, relevant local client checks, `make gate-commit`; queued stage/merge gates after review.
**Status:** In Progress

---

### Task 1: Structured recurrence and observer result

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Test: `domains/astronomy/src/eclipses.rs` and `domains/astronomy/tests/suite/`

**Interfaces:**
- Consumes: existing `EclipseEvent`, `GroundTrack`, `EclipseSight`, `Calendar`, `StarSystem`, `StdDays`, and `StdInstant`.
- Produces: a public recurrence summary containing the existing cycle fields plus explicit three-return exeligmos data, and one public observer result that distinguishes solar sight tier, lunar visibility, day/night side, and region.

- [x] **Step 1: Add failing behavior tests** for exeligmos = three selected-cycle periods, node-slip accumulation across three returns, per-moon recurrence ordering, solar track boundary/longitude-wrap visibility, long-duration tracks exceeding 180° and one full rotation, lunar night-side visibility, poles, retrograde rotation, and locked worlds.
- [x] **Step 2: Run the focused astronomy tests** and confirm the new assertions fail against the current API while the existing Eclipse Seasons tests remain green.
- [x] **Step 3: Implement the smallest pure API** over the existing helpers. Preserve `best_cycle`’s bounded search and its distinction between true Luna calibration inputs and generated-world approximations. Validate observer latitude in `[-90, 90]`, normalize longitude to the existing `[-180, 180)` convention, and route solar/lunar events through one result type without duplicating day-side logic.
- [x] **Step 4: Run the focused tests and the astronomy crate test suite**; inspect that no stream or generated-world code changed. Preserve the timing ledger row produced by the repository's timed commands.
- [x] **Step 5: Regenerate `docs/audits/type-audit-report.md` with the repository's prescribed report command, inspect that its changes are limited to the new public API audit entries, and include it with the domain commit.**
- [x] **Step 6: Commit** with a message explaining that the existing eclipse physics is being exposed as structured rhythm and observer data.

### Task 2: `scene/eclipses/v3` producer

**Files:**
- Modify: `windows/scene/src/lib.rs`
- Modify: `windows/scene/tests/suite/golden.rs` and relevant scene fixtures
- Create or modify: `book/src/reference/scene-eclipses-v3.md`

**Interfaces:**
- Consumes: Task 1 recurrence and observer APIs; existing `eclipses_scene` world/window conversion and `SceneError` handling.
- Produces: `ECLIPSES_SCHEMA = "scene/eclipses/v3"`, v3 serializable types, and an `eclipses_scene` producer that accepts an optional observer query and emits recurrence records plus per-event observation results.

- [x] **Step 1: Extend scene tests first** to assert the v3 schema, exact tick bounds, recurrence arrays, track/null behavior, omitted observer behavior, supplied observer behavior, invalid coordinates, and no mutation/draw consumption.
- [x] **Step 2: Run the scene tests** and capture the expected red failures from the v2-only types and signature.
- [x] **Step 3: Implement v3 types and mapping** using explicit `i64` ticks for wire times and `f64_field` quantizers for emitted floats. Represent absent observer input distinctly from a supplied observer whose event result is unseen. Keep event order day-ascending with moon index tie-break. Keep physical region fields separate from the observer result.
- [x] **Step 4: Regenerate the scene fixture from the producer and run the scene unit/integration tests**; compare JSON output twice for byte identity.
- [x] **Step 5: Commit** the scene contract and its reference page together so the wire shape and documentation cannot drift.

### Task 3: CLI and WASM query surfaces

**Files:**
- Modify: `cli/src/main.rs`
- Modify: `clients/world-wasm/src/lib.rs`, `clients/world-wasm/drive.mjs`, and existing world-client checks as required by the ABI
- Test: CLI scene tests and world-WASM smoke fixtures

**Interfaces:**
- Consumes: Task 2 `eclipses_scene`/`eclipses_json` and v3 schema.
- Produces: native eclipse query flags for optional latitude/longitude and an explicit observer-aware WASM export with finite/range validation at the ABI boundary.

- [x] **Step 1: Add CLI and WASM contract tests** for no observer, valid observer, longitude normalization, invalid latitude, invalid/non-finite values, and native/WASM JSON equivalence.
- [x] **Step 2: Run the focused surface tests** and confirm they fail before wiring the new arguments and v3 tag.
- [x] **Step 3: Wire the CLI parser and WASM entry point** to pass typed observer input into the scene producer. Keep seed and pins in their existing constructors; do not encode observer coordinates in pins or arbitrary JSON. Update help text and error envelopes to name v3.
- [x] **Step 4: Run CLI tests and the world-WASM local check**, including the byte-identity smoke against native output.
- [x] **Step 5: Commit** the query-surface changes separately from the domain and scene commits.

### Task 4: Almanac and documentation alignment

**Files:**
- Modify: `windows/almanac/src/lib.rs` and its eclipse tests
- Modify: `book/src/reference/scene-eclipses-v3.md` or remove the obsolete v2 reference after updating all links
- Modify: `book/src/frontier/idea-registry.md` only for the campaign’s shipped boundary and deferred follow-ups

**Interfaces:**
- Consumes: Task 1 recurrence/observer outputs and Task 2 v3 field vocabulary.
- Produces: almanac text that identifies moon/family recurrence, states the exeligmos relationship honestly, and renders observer sight results when context supplies coordinates.

- [x] **Step 1: Add almanac tests** for multiple moons, no-event windows, generated-world non-Saros wording, exeligmos output, and observer tiers including unseen.
- [x] **Step 2: Run the almanac tests** and confirm the new expectations fail against the current compact Eclipse Seasons prose.
- [x] **Step 3: Render the structured data through the existing almanac context** without introducing cultural interpretation or a second physical calculation. Keep no-event fallback text truthful for both solar and lunar families.
- [x] **Step 4: Update the scene reference and frontier rows** so v3, observer semantics, and deferred astronomy seams agree; run link and prose consistency checks.
- [x] **Step 5: Commit** almanac and reference changes with the vocabulary they document.

### Task 5: Artifact review and campaign verification

**Files:**
- Modify: generated scene/almanac fixtures and any committed client bundle that the existing checks actually author
- Modify: `docs/audits/campaign-reconciliation.tsv` and the campaign ledger

**Interfaces:**
- Consumes: completed Tasks 1–4 and their committed contracts.
- Produces: reviewed generated artifacts, a current campaign ledger, and evidence for local and queued gates.

- [x] **Step 1: Run the artifact-producing commands once**, inspect the complete diff, and classify every changed file as required contract output, expected almanac/reference output, or unexplained drift.
- [x] **Step 2: Re-run only the relevant focused checks** after resolving any artifact drift; do not rebaseline census goldens unless an actual census metric or world-generating behavior changed.
- [x] **Step 3: Run `make quick`, the affected CLI/client checks, and `make gate-commit`; record exit codes and material output in the campaign ledger.**
- [ ] **Step 4: Request code review and resolve findings through the subagent review loop.**
- [ ] **Step 5: Submit the completed branch to the Sluice stage/merge process only after the campaign-close review and required canonical checks.**
