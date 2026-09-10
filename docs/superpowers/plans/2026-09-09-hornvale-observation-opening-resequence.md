# Hornvale Observation Opening Resequence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the underworld-first public pilot order with an astronomical-to-surface opening that uses a scale ladder for presentation and the observation atlas/DAG for evidence and production dependencies.

**Architecture:** Preserve the existing deterministic observation manifest, frame-packet, Atlas renderer, and local assembly contracts. Add or reuse observation producers only where they provide an authoritative witness for an astronomical, system, planetary, or surface claim; record unsupported cells as capability requirements rather than making the public batch imply them. Keep the underworld records as an internal production-path pilot unless an editorial decision explicitly promotes or retires them.

**Tech Stack:** Existing Rust domain/worldgen producers and CLI observation commands; JSON manifests and frame packets; Atlas client renderers; shell-based local assembly and checks; existing project tests and audit gates.

**Spec:** `docs/superpowers/specs/2026-09-09-hornvale-observation-series-design.md`

## Global Constraints

- The public presentation follows `astronomical neighborhood → system → world → region → settlement → occupation layer → population → individual → language, belief, and practice`.
- The release spine is presentation order only; it must not imply causation, inheritance, or one continuous world unless the evidence record supports that claim.
- Every episode has one object, one explicit scale, one primary observable axis, one visual grammar, and one bounded observation sentence.
- Population, settlement, occupation, residue, and projected individual remain distinct counted or observed units.
- Public copy does not mention unshown capabilities, roadmap items, missing capabilities, future episodes, or comparison sources as its primary frame.
- Generated frames and videos remain local derived review artifacts; no command publishes to a social network.
- Capability work is identified at least fourteen days before demonstration and tested at least seven days before publication.
- Maintain at least seven fully approved video/copy packages before publication continues.

---

## Stage 1: Audit the astronomical-to-surface capability cells

**Goal:** Establish which astronomical, system, planetary, geographic, field, and habitat observations are already authoritative and renderable.

**Success Criteria:** A committed capability matrix names each candidate object, scale, axis, producer command, packet shape, renderer grammar, witness status, and precise gap. No candidate is called existing solely because a domain type or debug output exists.

**Tests:** Re-run the exact producer commands named by the matrix; compare repeated outputs byte-for-byte where determinism is claimed; run the existing observation validator and `make observation-check`; verify every claimed output path against the source tree.

**Status:** Not Started

### Task 1. Inventory current producers and renderers

**Files:**
- Create: `observations/atlas/2026-09-opening-capability-matrix.md`
- Modify: `observations/README.md` only where the authoritative producer-to-renderer boundary needs clarification
- Inspect: `domains/astronomy/`, `domains/climate/`, `domains/terrain/`, `windows/worldgen/`, `windows/scene/`, `cli/src/observations.rs`, `clients/atlas/src/observation.ts`

**Interfaces:**
- Consumes: existing producer commands and the `FramePacket` contract defined by the observation-series design
- Produces: a matrix whose rows are candidate cells and whose states are `existing`, `needs_observation_surface`, `needs_renderer`, or `needs_simulation_extension`

- [ ] List candidate cells at astronomical neighborhood, system, world, region, geography, field, and habitat scales.
- [ ] For each row, name the exact object, axis, unit, command or client entry point, and evidence boundary.
- [ ] Run each named command once and record the observed output path or refusal; do not infer support from a symbol search.
- [ ] Mark a row `existing` only when an authoritative producer and a compatible visual grammar both exist.
- [ ] Mark a row `needs_observation_surface` when the world computes a fact but no deterministic packet exposes the required observable.
- [ ] Mark a row `needs_renderer` when a packet exists but Atlas cannot present its object and axis without semantic invention.
- [ ] Mark a row `needs_simulation_extension` only when the phenomenon itself is absent from the authoritative world state.
- [ ] Run `git diff --check` and the focused observation checks.
- [ ] Commit with `docs: audit opening observation cells`.

## Stage 2: Build the first astronomical/system observation path

**Goal:** Make the earliest public-scale observations reproducible and visually legible without changing simulation semantics merely for presentation.

**Success Criteria:** At least one astronomical or system-scale episode has a validated manifest, deterministic frame packets, phone/laptop render coverage, and a local assembly package. If the audit finds no supported cell, the stage produces a precise capability plan instead of a fabricated episode.

**Tests:** Focused Rust observation tests for packet provenance and refusal cases; Atlas tests for labels, units, and viewport legibility; repeated export checksum comparison; shell assembly tests; `make observation-check`.

**Status:** Not Started

### Task 2. Add the smallest missing observation surface, if required

**Files:**
- Modify: the exact producer and observation-export files named by the Stage 1 matrix
- Create or modify: the corresponding focused test file under the owning crate’s existing test suite
- Modify: `observations/atlas/2026-09-opening-capability-matrix.md`

**Interfaces:**
- Consumes: one Stage 1 row marked `needs_observation_surface`
- Produces: a deterministic packet producer that exposes only already-authoritative values, with source revision and digest carried through the existing frame contract

- [ ] Choose the first supported astronomical/system cell from the matrix using visual legibility and dependency coverage, not novelty alone.
- [ ] Write a failing test for the exact packet fields, unit, ordering, seed identity, and refusal boundary.
- [ ] Implement the smallest producer/export change that makes the test pass.
- [ ] Verify that stream order, quantization, and existing world artifacts remain unchanged unless the task explicitly changes an authoritative world value.
- [ ] Re-run the producer twice and compare the complete packet sequence byte-for-byte.
- [ ] Update the matrix row from `needs_observation_surface` to `existing` only after the command and checksum witness exist.
- [ ] Commit with a headline naming the observation surface and its evidence boundary.

### Task 3. Add or extend the astronomical/system renderer

**Files:**
- Modify: `clients/atlas/src/observation.ts` only for the packet fields required by the selected cell
- Modify: `clients/atlas/src/observation_test.ts`
- Modify: `clients/atlas/src/main.ts` only if an exercised preview mount is required
- Create: `observations/fixtures/<episode-id>/render-input.json`

**Interfaces:**
- Consumes: the validated frame packet from Task 2
- Produces: deterministic phone and laptop compositions with authored labels, count units, and provenance visible without client-invented semantics

- [ ] Write failing tests for the selected object, scale, axis, count unit, title, source revision, and viewport dimensions.
- [ ] Implement the renderer using the existing observation boundary; do not derive causal or semantic classifications in the client.
- [ ] Exercise the browser preview at 390×844 and 1440×900.
- [ ] Confirm that labels remain readable and that reduced-motion/keyboard behavior is preserved where the existing preview exposes interaction.
- [ ] Run the focused Atlas tests and commit with `feat: render astronomical observation cell`.

## Stage 3: Establish the planetary/surface opening batch

**Goal:** Add the first surface-facing episodes in a scale-aware order, with each episode occupying an independently justified atlas cell.

**Success Criteria:** The opening batch contains only supported astronomical/system/world/region/surface observations, uses distinct objects and axes where warranted, and identifies any unsupported transition to settlement or social layers without presenting it as demonstrated.

**Tests:** Manifest validation for every record; deterministic export and repeated checksum comparison; phone/laptop renderer checks; local film assembly; `git diff --check`; focused client and Rust suites.

**Status:** Not Started

### Task 4. Replace the public opening records

**Files:**
- Modify: `observations/episodes/HV-001.json` through `observations/episodes/HV-008.json`
- Modify: `observations/captions/HV-001.md` through `observations/captions/HV-008.md`
- Modify: `observations/batches/2026-09-opening-batch.md`
- Modify: `observations/episodes/README.md`

**Interfaces:**
- Consumes: existing manifest/caption contracts and the Stage 1 capability matrix
- Produces: a draft opening batch ordered by scale cluster, with no unsupported public claims and with the former underworld-first selection retained only as an explicitly named internal pilot if useful

- [ ] Select eight supported cells beginning with astronomical/system/world/surface observations; do not force all eight into a single world unless the evidence records establish that continuity.
- [ ] Give each record one object, scale, primary axis, visual grammar, counted unit, observation sentence, source command, and capability state.
- [ ] Rewrite captions in Nathan’s casual voice while preserving the exact internal object, scale, and evidence boundary.
- [ ] Record unsupported settlement, occupation, population, temporal, relational, or close-reading candidates in the batch capability section rather than substituting an unrelated spatial episode.
- [ ] Generate each candidate’s packets at least seven days before its intended review/publication window.
- [ ] Keep every package `draft` until exact video and copy review occurs.
- [ ] Commit with `docs: reorder opening observation batch`.

## Stage 4: Review and prepare the reserve

**Goal:** Produce inspectable local packages and obtain the manual editorial approvals required for publication readiness.

**Success Criteria:** At least seven packages are individually reviewed and approved by Nathan; each approved package contains the exact manifest, authoritative frame checksum, video checksum when present, and final reviewed caption; the campaign ledger records the review boundary and any rejected or deferred cells.

**Tests:** Exact video/frame inspection at phone and laptop sizes; manifest validator; `make observation-check`; client checks; shellcheck; `git diff --check`; final `make gate-commit` before a merge submission.

**Status:** Not Started

### Task 5. Assemble the review set

**Files:**
- Create: ignored local outputs under `observations/render-output/HV-00N/`
- Modify: `observations/batches/2026-09-opening-batch.md` with checksums and review states only after inspection
- Modify: `docs/superpowers/ledgers/2026-09-09-hornvale-observation-series.md`

**Interfaces:**
- Consumes: the eight validated episode records and renderer outputs
- Produces: a manually inspectable reserve and a ledger record distinguishing automated evidence checks, editorial review, Nathan’s approval, and publication

- [ ] Render each package into a clean per-episode output directory.
- [ ] Assemble a derived film where the local ffmpeg path is available; otherwise record the no-video verification result without claiming a film exists.
- [ ] Inspect the exact frame/video and caption together for object, scale, axis, labels, observation sentence, duration, and phone/laptop legibility.
- [ ] Mark a package `reviewed` only after that inspection and `approved` only after Nathan explicitly approves the exact package.
- [ ] Do not mark more than seven packages approved merely to satisfy the reserve count; rejected or deferred packages remain explicitly recorded.
- [ ] Run all scoped checks and `make gate-commit`.
- [ ] Commit the review ledger and any approved-record state changes separately from generated local outputs.

## Handoff

After Stage 4 reaches seven approved packages and all close-out artifacts exist, use `closing-a-campaign` and submit a merge request through `make sluice`. Until then, use `make sluice-stage` at stage boundaries; a green stage gate does not waive the manual approval or campaign-close requirements.
