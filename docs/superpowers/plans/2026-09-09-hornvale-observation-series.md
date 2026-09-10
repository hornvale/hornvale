# Hornvale Observation Series Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the first reusable observation pipeline for Hornvale: precise episode manifests, deterministic frame exports, one production-quality visual path, and a reviewed pilot batch that can enter a seven-episode reserve.

**Architecture:** Keep the simulation authoritative and keep the client responsible for presentation. A manifest identifies one episode's object, scale, axis, world state, evidence, and visual grammar; a CLI/export surface produces reproducible observation data and frame inputs; a client renderer turns those inputs into phone- and laptop-legible frames. Public copy remains a manually reviewed draft outside the generation path.

**Tech Stack:** Rust CLI and existing worldgen/scene producers; serde JSON for machine-readable manifests and frame packets; existing PNG renderers and browser clients where they already provide the required visual grammar; Deno tests for client presentation; ffmpeg only for final local assembly if available, with PNG frame sequences remaining the authoritative render output.

**Spec:** `docs/superpowers/specs/2026-09-09-hornvale-observation-series-design.md`

## Campaign close — Complete at the user-approved scope boundary

**Status:** Complete. This campaign delivered the deterministic manifest and
export contract, the Atlas preview, the Firefox PNG raster path, ffmpeg local
assembly, and a technically verified HV-009 package. It did **not** deliver
the intended fullscreen animated/subtitle-style presentation or a reserve of
seven manually approved packages.

The user approved closing at that narrower boundary. HV-009 remains draft:
its evidence status and editorial status are `draft` and `approval` is `null`.
Nothing in this close marks it reviewed, approved, published, or counted in a
reserve.

| Work | Close disposition |
| --- | --- |
| Tasks 1–4 | Complete: manifest validation, deterministic packets, Atlas preview, and local assembly/verification shipped. |
| Task 5 | Superseded by the later evidence-backed opening-batch amendment; no eight-record public batch was fabricated. |
| Tasks 6–8 | Complete: capability audit, one supported stellar path, and a truthful one-candidate opening record shipped. |
| Task 9 | Technical HV-009 package assembly completed; its manual editorial review and the seven-package reserve are explicitly outside this campaign. |
| Task 10 | Complete: Firefox-backed rasterization with clear absence refusal shipped. |

The follow-up is required, not implied: a new renderer/presentation campaign
must replace the technical HTML-to-Firefox-per-frame proof with a persistent,
custom fullscreen animated presentation. It owns visual legibility and
subtitle-style treatment. After it produces presentation-ready exact packages,
Nathan's manual editorial review and the seven-approved-package reserve resume
as a separate production boundary. See
[`the-observation-series.md`](../../retrospectives/the-observation-series.md)
and the raw renderer idea in the registry.

## Global Constraints

- One episode observes one primary feature, axis, or dimension at one explicit scale.
- Every rendered claim names its object, counted unit, time interval, seed, and revision.
- The public sequence is Hornvale-native; comparison references remain internal metadata.
- Public copy is casual and advisory; no tool publishes or treats generated copy as approved.
- The authoritative output is deterministic data plus PNG frames; video assembly is a derived convenience.
- Capability work is identified at least fourteen days before demonstration and tested at least seven days before publication.
- Maintain at least seven fully approved video/copy packages before publication continues.
- Preserve the four visual grammars: spatial film, temporal film, relational film, and close reading.
- Do not add a new simulation mechanism merely to create a visually convenient claim; record the gap as an internal capability requirement.
- Use existing project gates and scoped tests during iteration; do not run retired whole-workspace local gate commands.

---

## File and boundary map

The first increment uses these boundaries:

- Create `cli/src/observations.rs` for manifest parsing, validation, episode status, and frame-export orchestration.
- Modify `cli/src/main.rs` to expose the `observations` command without moving existing render commands.
- Create `cli/tests/suite/observations.rs` for manifest and CLI behavior tests; add it to `cli/tests/suite.rs`.
- Create `observations/episodes/` for committed internal episode manifests and reviewed caption drafts. These are planning/evidence records, not public posts and not generated artifacts.
- Create `observations/frames/` only if the repository’s artifact policy accepts committed pilot frames; otherwise write pilot output to a documented ignored directory and commit only the manifest and a small fixture.
- Modify `scripts/regenerate-artifacts.sh` only if the pilot’s committed output is declared a generated artifact; do not add an undeclared generated path.
- Modify `clients/atlas/src/main.ts` only for the first spatial-film renderer if existing atlas behavior cannot express the required overlays without ambiguity.
- Create `clients/atlas/src/observation.ts` for observation-specific composition rather than adding episode logic to the general atlas event loop.
- Create `clients/atlas/src/observation_test.ts` for pure frame-state and label tests.
- Create `scripts/observation-film.sh` only after the frame contract is stable; it assembles verified PNG frames and writes a manifest/checksum sidecar but never publishes.
- Create `scripts/test-observation-film.sh` for shell-level failure and output-shape tests; run `shellcheck` on the script before committing it.

The plan intentionally does not create a new simulation crate in this increment. If an episode requires a new world mechanism rather than an observation surface, that requirement is recorded in the manifest and split into its own design campaign before implementation.

## Data contracts

The Rust-side manifest and the client-side frame packet must agree on these semantic fields:

```text
EpisodeManifest
  id: stable string
  title: string
  object: string
  scale: string
  primary_axis: string
  phenomenon: string
  visual_grammar: spatial | temporal | relational | close_reading
  observation_sentence: string
  world_revision: string
  seed: u64
  time_window: optional { start_day: f64, end_day: f64 }
  frame_count: positive integer
  frame_rate: positive finite number
  source_commands: non-empty list of reproducible commands
  evidence_status: draft | reviewed | approved | published
  approval: optional { reviewer: string, approved_at: string }
  capability_state: existing | needs_observation_surface | needs_renderer | needs_simulation_extension
  comparison_reference: optional internal-only object
  caption_draft: one to three strings
```

The exported frame packet contains only data needed by a renderer and carries:

```text
FramePacket
  episode_id
  frame_index
  world_seed
  world_revision
  time_day: optional finite number
  title
  labels
  spatial or temporal observations
  source_digest
```

Unknown enum values, missing required fields, zero frame counts, non-finite
numbers, empty source commands, and a public status without an approved
editorial record must fail loudly. The validator must not silently coerce a
wrong scale or counted unit.

### Task 1: Add the observation manifest model and validator

**Files:**
- Create: `cli/src/observations.rs`
- Modify: `cli/src/main.rs`
- Modify: `cli/tests/suite.rs`
- Create: `cli/tests/suite/observations.rs`
- Create: `observations/episodes/README.md`

**Interfaces:**
- `observations::EpisodeManifest` owns the serde representation and validation.
- `observations::ObservationStatus` owns the four editorial lifecycle values.
- `observations::CapabilityState` owns the four internal capability states.
- `observations::VisualGrammar` owns the four visual grammars.
- `observations::validate_manifest(&EpisodeManifest) -> Result<(), ObservationError>` validates semantic constraints.
- `observations::read_manifest(path: &Path) -> Result<EpisodeManifest, ObservationError>` parses and validates one manifest.
- The CLI gains `hornvale observations validate --manifest <PATH>` and prints one deterministic success line or a descriptive error.

- [ ] **Step 1: Write the failing Rust tests for valid and invalid manifests.**

  Add tests for:

  - a valid spatial manifest with a seed, revision, one source command, and one caption draft;
  - missing object or scale;
  - zero `frame_count`;
  - non-finite frame rate and time values;
  - an empty source-command list;
  - an unknown visual grammar;
  - `published` status without an approval record;
  - a population/settlement/occupation scale spelled as a different object;
  - comparison metadata present without changing the public title or claim fields.

  Use behavior assertions on returned errors, not implementation details of serde.

- [ ] **Step 2: Run the focused test to verify it fails.**

  Run:

  ```bash
  cargo nextest run -p hornvale --test suite -E 'test(observations)'
  ```

  Expected: compilation or test failure because the observation module and test cases do not yet exist.

- [ ] **Step 3: Implement the manifest types and validator.**

  Follow existing CLI data types and serde patterns. Keep validation explicit and local to the manifest boundary. Reject malformed semantic fields instead of defaulting them. Keep comparison metadata private to the serialized internal record and never use it to generate public titles.

- [ ] **Step 4: Wire the validation command.**

  Add the command to the existing usage text and dispatch table. Make success output include the manifest id, object, scale, axis, and frame count. Make failure output include the path and field-level reason.

- [ ] **Step 5: Add the manifest-directory README and one fixture.**

  Document that files under `observations/episodes/` are internal records and that a manifest is not publishable until its status is `approved`. Add one valid fixture for later tasks to consume.

- [ ] **Step 6: Run the focused tests and commit.**

  Run:

  ```bash
  cargo nextest run -p hornvale --test suite -E 'test(observations)'
  cargo fmt --check
  git diff --check
  ```

  Commit:

  ```bash
  git add cli/src/observations.rs cli/src/main.rs cli/tests/suite.rs cli/tests/suite/observations.rs observations/episodes/README.md observations/episodes/HV-001.json
  git commit -m "feat: validate Hornvale observation manifests"
  ```

### Task 2: Export deterministic frame packets from existing world surfaces

**Files:**
- Modify: `cli/src/observations.rs`
- Modify: `cli/src/main.rs`
- Modify: `cli/tests/suite/observations.rs`
- Create: `observations/episodes/HV-001.json`
- Create: `observations/fixtures/HV-001/expected-frame-000.json`

**Interfaces:**
- `observations::export_frames(manifest: &EpisodeManifest, out_dir: &Path) -> Result<ExportReport, ObservationError>` builds the requested world once and emits one JSON frame packet per frame.
- CLI command: `hornvale observations export --manifest <PATH> --out <DIR>`.
- `ExportReport` carries episode id, frame count, source digest, and output paths.

- [ ] **Step 1: Write the failing export tests.**

  Test that:

  - seed 42 emits the requested number of packets;
  - frame indices are contiguous and start at zero;
  - every packet repeats the episode id, seed, and revision;
  - repeated export to two directories produces byte-identical JSON;
  - a missing manifest path fails before creating an output directory;
  - a manifest with `needs_simulation_extension` is rejected by export with a message that names the required state rather than silently producing a weaker frame.

- [ ] **Step 2: Run the focused tests to verify failure.**

  Run:

  ```bash
  cargo nextest run -p hornvale --test suite -E 'test(observations)'
  ```

  Expected: failure because export is not implemented.

- [ ] **Step 3: Implement export through existing producers.**

  Start with a spatial episode backed by an existing deterministic surface, such as the terrain/biome/settlement map path. Do not recompute producer semantics in the CLI. Use the existing world builder and renderer inputs, and include a source digest so a frame cannot be mistaken for a hand-authored image.

- [ ] **Step 4: Implement atomic output behavior.**

  Write each packet to a temporary file in the requested output directory, rename only after serialization succeeds, and return a descriptive error with the episode id and frame index on failure. Do not delete an unrelated existing directory.

- [ ] **Step 5: Run deterministic export tests and inspect the fixture.**

  Run the focused observation test filter and compare the fixture with a second export. Confirm the fixture contains no client-generated classification that the producer did not provide.

- [ ] **Step 6: Commit the frame-packet export.**

  ```bash
  git add cli/src/observations.rs cli/src/main.rs cli/tests/suite/observations.rs observations/episodes/HV-001.json observations/fixtures/HV-001/expected-frame-000.json
  git commit -m "feat: export deterministic observation frames"
  ```

### Task 3: Build the first spatial-film renderer

**Files:**
- Create: `clients/atlas/src/observation.ts`
- Create: `clients/atlas/src/observation_test.ts`
- Modify: `clients/atlas/src/main.ts` only if the renderer must be reachable from the existing atlas entry point
- Modify: `clients/atlas/deno.json` only if a new explicit check/test task is required
- Create: `observations/fixtures/HV-001/render-input.json`

**Interfaces:**
- `observation.ts` exports pure functions that accept a validated frame packet and return render state; no function reads the world or derives an unprovided semantic field.
- `renderObservationFrame(packet, viewport) -> RenderState` composes the map, title, legend, scale label, and one observation annotation.
- `renderObservationFrame` supports the phone target and laptop target through viewport dimensions, not separate semantic logic.

- [ ] **Step 1: Write failing Deno tests for the frame composition.**

  Test that:

  - the episode title and object/scale labels appear;
  - the renderer uses the packet’s supplied labels and does not invent a biome/resource name;
  - the same packet gives the same render state at the same viewport;
  - the phone viewport retains the primary map and observation sentence;
  - an unknown packet schema or missing source digest is refused;
  - comparison metadata is not rendered into the public frame.

- [ ] **Step 2: Run Deno checks to verify failure.**

  From `clients/atlas/`, run the existing check and test tasks. Expected: the new test module fails because the observation renderer does not yet exist.

- [ ] **Step 3: Implement the renderer using the atlas’s existing projection and palette conventions.**

  Keep the renderer’s semantic input limited to the frame packet. Use a restrained title/legend treatment, preserve keyboard and reduced-motion behavior in any interactive preview, and avoid adding a second application shell.

- [ ] **Step 4: Add a deterministic browser fixture and visual inspection harness.**

  Render one frame at phone and laptop viewports. Verify the actual map, labels, and annotation rather than only the DOM structure. Store the fixture input, not an unreviewed screenshot, unless the project’s generated-artifact policy explicitly declares the image.

- [ ] **Step 5: Run client checks and commit.**

  Run the existing `clients/atlas` check and test tasks, then:

  ```bash
  git add clients/atlas/src/observation.ts clients/atlas/src/observation_test.ts clients/atlas/src/main.ts clients/atlas/deno.json observations/fixtures/HV-001/render-input.json
  git commit -m "feat: render observation frames in atlas"
  ```

### Task 4: Add local frame assembly and artifact verification

**Files:**
- Create: `scripts/observation-film.sh`
- Create: `scripts/test-observation-film.sh`
- Create: `observations/README.md`
- Modify: `.gitignore` only for the explicit ignored render-output directory
- Modify: `Makefile` only to add a non-publishing `observation-check` target

**Interfaces:**
- `scripts/observation-film.sh --manifest PATH --frames DIR --out DIR` validates the manifest, verifies contiguous frame packets, writes a video if the requested assembler is available, and always writes a checksum sidecar.
- The script never contacts BlueSky or any social network.
- `make observation-check` runs manifest validation, frame export, shellcheck, and fixture checks without changing committed artifacts.

- [ ] **Step 1: Write shell tests for failure modes.**

  Cover missing arguments, missing frame directory, non-contiguous frame indices, mismatched episode ids, mismatched seeds, an output path inside the input frame directory, and a successful no-video mode that still writes a checksum report.

- [ ] **Step 2: Run shellcheck and the shell tests to establish red.**

  Run `shellcheck scripts/observation-film.sh scripts/test-observation-film.sh` and the focused test script. Expected: failure because the scripts do not exist.

- [ ] **Step 3: Implement the bounded assembly script.**

  Quote every path, refuse empty or broad targets, use explicit temporary directories, and distinguish authoritative frame packets from derived video output. If ffmpeg is unavailable, report that clearly and still verify the frame sequence and write the sidecar.

- [ ] **Step 4: Add the local Make target and workflow documentation.**

  Document that frame packets are the reproducible source, video files are derived, publication is manual, and an approved package must contain the exact manifest, frame checksum, video checksum when present, and final caption text.

- [ ] **Step 5: Run shellcheck, the shell tests, and the scoped Rust/client checks.**

  Commit:

  ```bash
  git add scripts/observation-film.sh scripts/test-observation-film.sh observations/README.md .gitignore Makefile
  git commit -m "build: verify observation film assembly"
  ```

### Task 5: Create and review the first pilot batch

**Files:**
- Create: `observations/episodes/HV-001.json` through `observations/episodes/HV-008.json`
- Create: `observations/captions/HV-001.md` through `observations/captions/HV-008.md`
- Create: `observations/batches/2026-09-opening-batch.md`
- Modify: `observations/episodes/README.md`

**Interfaces:**
- Each pilot manifest validates through the Task 1 command.
- Each pilot episode names one object, scale, axis, visual grammar, and observation sentence.
- Each caption file contains a casual primary draft and up to two optional replies, clearly marked as drafts.

- [ ] **Step 1: Select eight realized observations from distinct atlas cells.**

  Use a varied opening batch rather than eight near-identical maps. The batch should include spatial, temporal, relational, and close-reading candidates where existing producers support them. Keep each candidate grounded in current output; do not create a manifest for an unimplemented capability.

- [ ] **Step 2: Record capability requirements before rendering.**

  For each candidate, mark `existing`, `needs_observation_surface`, or `needs_renderer`. If a candidate needs a simulation extension, record it in the batch file with the exact object, scale, and observable required, then replace that candidate with an existing observation for the pilot.

- [ ] **Step 3: Generate all eight frame sequences at least seven days before the intended publication window.**

  Store the revision, seed, source digest, frame checksums, and render dimensions in the batch record. Confirm that the batch can be reproduced from a clean output directory.

- [ ] **Step 4: Draft the public posts in Nathan’s casual voice.**

  Drafts may be loose, curious, and wordy. They must not mention unshown capabilities, future episodes, missing functionality, or an external comparison as the primary frame. Keep exact technical details in the internal manifest unless they are useful in the post.

- [ ] **Step 5: Perform editorial and visual review.**

  For every episode, verify the object, scale, axis, title, labels, and observation sentence. Inspect phone and laptop renders. Mark each package `reviewed`; do not mark it `approved` until Nathan has reviewed the exact video and copy.

- [ ] **Step 6: Establish the seven-package reserve.**

  Mark at least seven packages `approved` only after Nathan’s review. If fewer than seven are approved, keep the pilot in production and do not treat the batch as publication-ready.

- [ ] **Step 7: Commit the pilot records and close the first implementation increment.**

  Run the manifest validator, `make observation-check`, client checks, shellcheck, and `git diff --check`. Commit with:

  ```bash
  git add observations/episodes observations/captions observations/batches/2026-09-opening-batch.md
  git commit -m "docs: define the opening observation batch"
  ```

## Verification matrix

| Requirement | Verification |
|---|---|
| One feature/axis per episode | Manifest validator and review checklist |
| Explicit object and scale | Manifest validator; rendered labels |
| Deterministic world state | Repeat export byte comparison |
| No client-invented semantics | Frame-packet source digest and renderer tests |
| Phone/laptop legibility | Browser inspection at both viewport classes |
| Four visual grammars remain available | Pilot batch manifest review |
| No automatic publication | Shell test and code review of assembly script |
| Seven approved packages | Batch record and manual approval state |
| Capability work scheduled early | Batch records every non-existing requirement with lead time |
| Existing project conventions | Scoped tests, shellcheck, fmt, diff check, prose-subject hooks |

## Scope boundary for the next plan

After this plan is complete, a separate plan may implement the first genuinely
new observation surface or simulation extension selected from the atlas. That
plan must begin with the exact phenomenon, object, scale, axis, falsifying case,
and visual grammar. It must not be inferred from a caption request alone.

---

## Amendment: astronomical-to-surface opening

The public pilot order is amended by the approved design's scale ladder. The
existing underworld records remain useful as an internal production-path
pilot, but they are not the public opening unless a later review explicitly
promotes them.

### Task 6: Audit the opening capability cells

**Goal:** Establish which astronomical, system, planetary, geographic, field,
and habitat observations are authoritative and renderable.

**Success Criteria:** A committed matrix names each candidate object, scale,
axis, producer command, packet shape, renderer grammar, witness status, and
precise gap. A domain type or debug print alone never qualifies as existing.

**Tests:** Run every named producer command; compare repeated outputs when
determinism is claimed; run the observation validator and `make
observation-check`; verify every output path against the source tree.

**Status:** Complete

**Files:** Create `observations/atlas/2026-09-opening-capability-matrix.md`;
inspect `domains/astronomy/`, `domains/climate/`, `domains/terrain/`,
`windows/worldgen/`, `windows/scene/`, `cli/src/observations.rs`, and
`clients/atlas/src/observation.ts`.

- [x] Inventory candidate cells from astronomical neighborhood through habitat scale.
- [x] Record the exact object, axis, unit, command, client path, and evidence boundary for every row.
- [x] Run each command once and record observed output or refusal; do not infer support from symbol searches.
- [x] Mark `existing` only when an authoritative producer and compatible visual grammar both exist.
- [x] Mark `needs_observation_surface`, `needs_renderer`, or `needs_simulation_extension` at the precise failing boundary.
- [x] Run `git diff --check` and focused observation checks; commit `docs: audit opening observation cells`.

Task 6 completion: the capability matrix is committed at
`observations/atlas/2026-09-opening-capability-matrix.md`; the producer and
renderer boundaries were reviewed and approved.

### Task 7: Build the first astronomical/system path

**Goal:** Make the earliest public-scale observations reproducible and legible
without changing simulation semantics merely for presentation.

**Success Criteria:** At least one astronomical or system cell has a validated
manifest, deterministic packets, phone/laptop render coverage, and a local
assembly package. If no supported cell exists, produce the precise capability
plan instead of a fabricated episode.

**Tests:** Focused Rust packet tests, Atlas viewport tests, repeated checksum
comparison, shell assembly tests, and `make observation-check`.

**Status:** Complete

- [x] Select the first supported cell using visual legibility and dependency coverage.
- [x] Write a failing test for its packet fields, unit, ordering, seed identity, and refusal boundary.
- [x] Implement the smallest observation-surface change, if the matrix requires one; preserve world artifacts and stream contracts.
- [x] Re-export twice and compare the complete packet sequence byte-for-byte.
- [x] Extend Atlas only for supplied packet fields; exercise 390×844 and 1440×900 previews.
- [x] Update the capability matrix only after command and checksum witnesses exist; commit producer and renderer changes separately.

Task 7 completion: HV-009 now exports and renders the stellar-neighborhood
cell from the declared `scene neighbors` world artifact. The provenance fix
also validates repository-root path resolution, missing artifacts, and seed
identity.

### Task 8: Resequence the surface-facing batch

**Goal:** Replace the public opening records with supported astronomical,
system, world, region, and surface observations while retaining distinct
settlement, occupation, population, and individual layers for later cells.

**Success Criteria:** Eight draft records occupy independently justified atlas
cells, begin at astronomical/system/world/surface scales, and do not imply
unsupported temporal, relational, social, or close-reading behavior.

**Tests:** Manifest validation, deterministic export, repeated checksums,
phone/laptop renderer checks, local assembly, `git diff --check`, and focused
Rust/client suites.

**Status:** Complete

- [x] Apply the smallest truthful amendment: retain `HV-001` through `HV-008` as the internal underworld pilot and make witnessed `HV-009` the sole current opening candidate rather than fabricating eight supported cells.
- [x] Add an advisory `HV-009` caption draft in Nathan's casual voice without unshown capabilities or roadmap language.
- [x] Record omitted settlement, occupation, population, temporal, relational, and close-reading cells as internal capability requirements.
- [x] Record the 2026-09-09 generation date and 2026-09-16 earliest review/publication date for the active candidate.
- [x] Keep all package statuses `draft` until exact video and copy review.
- [x] Commit `docs: reorder opening observation batch`.

### Task 9: Assemble and review the reserve

**Goal:** Produce inspectable local packages and obtain the manual approvals
required for publication readiness.

**Success Criteria:** At least seven packages are individually reviewed and
approved by Nathan; each contains its exact manifest, frame checksum, video
checksum when present, and final reviewed caption.

**Tests:** Exact frame/video inspection at both viewport classes, manifest
validation, `make observation-check`, client checks, shellcheck,
`git diff --check`, and a final `make gate-commit`.

**Status:** Closed at the campaign's narrowed scope boundary — HV-009's
technical package is assembled; manual visual/editorial review and the
seven-package reserve are deferred outside this campaign.

- [ ] Render each package into a clean ignored directory under `observations/render-output/HV-00N/`.
- [ ] Assemble a derived film when ffmpeg is available; otherwise record no-video verification without claiming a film exists.
- [ ] Inspect video and caption together for object, scale, axis, labels, sentence, duration, and legibility.
- [ ] Mark `reviewed` only after inspection and `approved` only after Nathan explicitly approves the exact package.
- [ ] Record rejected or deferred packages without inflating the seven-package reserve.
- [ ] Record the review boundary in the campaign ledger and keep generated local outputs out of the commit.

Task 9 progress (2026-09-10): Firefox rasterized HV-009's 900 validated
packets into a `1440×900` local PNG sequence, and ffmpeg assembled the derived
30-second MP4 with an `HV-009.sha256` sidecar. The manifest and caption remain
draft with `approval: null`; this technical assembly is not Nathan's manual
visual/editorial review and does not count toward the seven-package reserve.
The user-directed close records that the HTML-to-Firefox-per-frame assembly is
a technical pipeline proof, not the intended fullscreen animated/subtitle-style
presentation. The manual review, approval, and reserve checklist above remain
unchecked and are deliberately carried to follow-up work.

### Task 10: Rasterize observation preview frames

**Goal:** Connect the validated observation packets and Atlas HTML preview to
the PNG frame sequence required by local film assembly.

**Success Criteria:** A bounded local command renders one PNG for every
validated packet at a declared review viewport, refuses missing or malformed
packets, preserves packet ordering and episode identity, and writes no video
or publication side effect. If the available environment has no sanctioned
headless raster path, the command must refuse clearly and record that
environmental boundary rather than emitting placeholder images.

**Tests:** Focused raster-command tests for packet count, contiguous frame
names, malformed input, deterministic repeated output, viewport dimensions,
and refusal when the raster backend is unavailable; `make observation-check`,
Atlas checks, shellcheck, and `git diff --check`.

**Status:** Complete

**Files:** Create `scripts/observation-render.sh` and
`scripts/test-observation-render.sh`; modify `Makefile` to exercise the
renderer without publishing; modify `observations/README.md` to distinguish
HTML inspection from authoritative PNG frame output.

- [x] Identify the existing supported browser/raster backend and record its exact invocation; do not add a new dependency without repository precedent.
- [x] Write failing tests for one PNG per packet, contiguous names, viewport dimensions, malformed packet refusal, and deterministic reruns.
- [x] Implement the smallest local renderer that consumes the existing Atlas preview HTML and writes only declared frame outputs.
- [x] Run the renderer twice on HV-009 and compare the complete PNG sequence; distinguish byte identity from visual identity if the backend is platform-local.
- [x] Run shellcheck, focused Atlas tests, `make observation-check`, and `git diff --check`; commit `build: render observation frames locally`.

Task 10 completion: the local renderer is implemented and reviewed. It uses
Firefox headless when available and refuses clearly when that backend is
absent; no placeholder raster or video is accepted.
