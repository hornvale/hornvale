# Hornvale observation series campaign ledger

## #1 [G2] — approved design and implementation plan

- **Question:** What is the smallest coherent implementation increment for the approved Hornvale observation-series design?
- **Decision:** Begin with the observation manifest, deterministic frame-packet export, one spatial-film renderer, local frame assembly verification, and an eight-episode pilot batch. Keep new simulation mechanisms out of this increment unless a separately designed episode requires one.
- **Why:** The approved design separates the evidence, production, and release graphs. A bounded capture substrate is the smallest increment that can prove the public episode type and establish the seven-package reserve without prematurely choosing every future atlas cell.
- **Alternatives discarded:** Implementing the entire open-ended episode population in one plan would mix independent simulation extensions, renderers, media records, and editorial workflow. Treating captions as publishable output would violate the manual approval boundary.
- **Ideonomy passes / overturns:** One ideonomy pass plus three independent reviews. The pass enriched the design with scale substitution, axis substitution, and combination. The editorial review overturned a strictly prerequisite-first public order; the final design separates evidence dependencies from release order. No unresolved design reversal remains.
- **Capture actions:** The approved design is `docs/superpowers/specs/2026-09-09-hornvale-observation-series-design.md`. The implementation plan is `docs/superpowers/plans/2026-09-09-hornvale-observation-series.md`. The reconciliation table records both artifacts. No code or public media has been published.

## Execution scan

| Task | Shared file/interface | Producer → consumer | Finding and ruling |
|---|---|---|---|
| 1 → 2 | `cli/src/observations.rs`, manifest and validator | Task 1 produces validated manifests; Task 2 extends the same module with frame export | Compatible. Task 1 keeps parsing and validation reusable and does not bake in frame-only fields. |
| 1 → 5 | `observations/episodes/`, manifest status and capability state | Task 1 defines the record contract; Task 5 supplies the pilot records | Compatible. `approved` remains a human review state; Task 5 may not infer approval from generation. |
| 2 → 3 | frame-packet JSON contract | Task 2 produces deterministic packets; Task 3 consumes only supplied fields | Compatible. The client must not derive semantic classifications absent from packets. |
| 2 → 4 | frame directory and export report | Task 2 emits contiguous packets; Task 4 verifies and assembles them | Compatible. Task 4 treats checksum sidecars and videos as local artifacts, not published output. |
| 3 → 4 | rendered frame inputs and viewport behavior | Task 3 defines renderable state; Task 4 assembles rendered frames | Compatible. The shell layer remains presentation plumbing and does not become a second renderer. |
| 4 → 5 | verification commands and artifact policy | Task 4 establishes local checks; Task 5 uses them for the pilot | Compatible. Committed manifests/captions remain internal records; generated frames stay ignored unless policy changes explicitly. |
| 1 | `cli/src/observations.rs`, CLI command, tests, fixture | Tests exercise the parser/validator and command specified by the task | Internally consistent. The valid fixture is intentionally introduced in Task 1 for later tasks. |
| 2 | export implementation, fixture, CLI command | Tests compare repeated exports and inspect packet provenance | Internally consistent. Existing world surfaces are the only simulation dependency in this increment. |
| 3 | atlas renderer, tests, render fixture | Pure render tests exercise phone/laptop composition and labels | Internally consistent. No browser event-loop integration is required by this task. |
| 4 | two shell scripts, README, Make target | Shell tests invoke the assembler and its failure cases | Internally consistent. `shellcheck` is an explicit gate before commit. |
| 5 | eight manifests, captions, batch record | Review checklist exercises every manifest and caption package | Internally consistent. The task records unsupported candidates rather than inventing simulation mechanisms. |

Ruling: execute the plan in task order. The shared interfaces are deliberately additive; no task conflict requires a plan change before Task 1.

## Release-order ruling — scale ladder with atlas branches

- **Question:** How should the public observation series orient its first
  episodes without collapsing the atlas into a rigid staircase?
- **Decision:** Use a hybrid release model: the public presentation follows a
  scale ladder from astronomical neighborhood through system, world, region,
  settlement, occupation layer, population, individual, and finally language,
  belief, and practice; the production and evidence plans remain an atlas/DAG
  that can branch, interleave, and revisit a scale.
- **Why:** A literal descent gives viewers an intuitive visual spine, while
  the atlas preserves independent distinctions and prevents a presentation
  order from implying unsupported causation. This also moves the opening away
  from the underworld pilot's accidental dependence on the first available
  spatial renderer.
- **Alternatives discarded:** A strict funnel would force every later episode
  to follow one selected world and could overstate causal continuity. A flat
  atlas would preserve precision but provide too little audience orientation.
- **Ideonomy passes / overturns:** One substitution/dimension/combination pass.
  It surfaced presentation spine versus evidence dependency as separate
  dimensions; no overturn, but it added the requirement to distinguish
  causation from nested description and shared provenance.
- **Capture actions:** The public-release-spine section was added to the
  approved design. No pilot package statuses or implementation files were
  changed by this ruling.

## Review ruling — Task 1, round 1

Ruling: the reviewer’s strict-record findings are load-bearing. Amend Task 1 before proceeding: reject duplicate and unknown JSON fields, make approval state explicit and internally consistent, require a reproducible repository-relative fixture command, and align the manifest with the approved spec’s separate evidence and editorial fields. The cost is a slightly larger record contract now; the benefit is that later frame export cannot preserve ambiguity or claim provenance it cannot replay.

## Task 1: complete

- **Commits:** `9d525615d` and `8a26f4bd2`
- **Result:** Manifest parsing, strict validation, approval semantics, CLI validation, and the first internal fixture are implemented and reviewed.
- **Evidence:** 24 focused tests passed; the reviewer also recorded 404 tests passed, formatting, clippy, type-audit, placement-audit, plumb, and quick-gate success.
- **Review:** Task review approved with no remaining findings.

## Review ruling — Task 2, round 1

Ruling: reject export for every capability state other than `existing`, and make reruns deterministic in an existing output directory by refusing or isolating stale frame files. The reviewer demonstrated that the current implementation can claim an authoritative packet where no observation surface exists and can report fewer packets than the directory contains. Add regression tests before proceeding.

## Task 2: complete

- **Commits:** `f2836f823` and `80b850f50`
- **Result:** Deterministic frame packets, source digests, atomic writes, CLI export, unsupported-state refusal, and stale-owned-frame cleanup are implemented.
- **Evidence:** 30 focused tests passed; independent exports produced identical frame-000 SHA-256 values; formatting and diff checks passed.
- **Review:** Re-review approved with no remaining findings.

## Review ruling — Task 5, round 1

Ruling: set pilot durations to the governing 30-second minimum (`900` frames at `30` fps), but leave every package in draft until exact video and copy review has occurred. The first batch cannot claim completion from deterministic packet tests alone; the current renderer/assembly path must produce inspectable package artifacts before a package can become `reviewed`, and Nathan alone can make it `approved`.

## Review ruling — Task 3, round 1

Ruling: strengthen the client boundary before accepting the renderer. Add runtime packet validation, preserve `u64` seed identity as a string, use locale-independent ordering, and provide an actual browser/visual inspection harness for phone and laptop targets. The cost is a stricter client input contract and a small harness; the benefit is that a “rendered” observation is tested as an actual presentation rather than only as an abstract object.

## Review ruling — Task 3, round 2

Ruling: finish the client contract at the numeric boundary and make the preview genuinely inspectable. Enforce canonical decimal `u64` range and finite time values, expose a browser-consumable preview path, and render the spatial/provenance fields required by the episode contract. This prevents a visually attractive but semantically incomplete frame from entering the production pipeline.

## Review ruling — Task 3, round 3

Ruling: carry `count_unit` through the Rust manifest, exported frame, fixture, and client parser, and wire the preview mount into an exercised browser entry path. A client-supplied count unit would violate the producer/client boundary; an uncalled mount would be a visual claim without a witness.

## Review ruling — Task 4, round 1

Ruling: constrain manifest IDs before using them in filesystem paths, and make reruns reconcile derived video state rather than leaving an unchecked stale file. The assembly tool is a local artifact boundary; path traversal and stale checksums would undermine that boundary even though fresh-output tests pass.

## Task 4: complete

- **Commits:** `9645e7d2e`, `792ec1716`
- **Result:** Local frame/video assembly verification, checksum sidecars, safe target handling, shell tests, and the non-publishing `observation-check` target are implemented.
- **Evidence:** 20 shell tests, 31 Rust observation tests, and 39 Atlas tests passed; shellcheck and `make observation-check` passed.
- **Review:** Re-review approved with no remaining findings.

## Task 3: complete

- **Commits:** `d4cab8582`, `ca990b240`, `a950f5929`, `ec6b748ea`, `5480c0ecf`
- **Result:** Pure spatial render state, strict packet parsing, exact numeric handling, producer-owned count units, deterministic ordering, and LinkeDOM-inspected phone/laptop previews are implemented.
- **Evidence:** 39 Deno tests and 31 Rust observation tests passed; checks, formatting, lint, and diff checks passed.
- **Review:** Final re-review approved with no remaining findings.

## Reincorporation ruling — The Staple D4

Ruling: absorb `origin/main` before the next stage request so the observation
campaign is tested against the current canonical line, including The Staple
D4. The sanctioned absorb resolved the generated type-audit report through
regeneration; the resulting merge is clean with no source conflict. The
rebaseline regenerated the current local artifact measurements. A fresh
commit gate is required before resubmission.

## Execution scan — astronomical-to-surface amendment

| Task | Shared file/interface | Producer → consumer | Finding and ruling |
|---|---|---|---|
| 6 → 7 | capability matrix and observation packet contract | Task 6 identifies a supported cell or an exact missing boundary; Task 7 implements only that selected boundary | Compatible. Task 7 must not claim a cell `existing` until Task 6 records a command and witness. |
| 6 → 8 | capability matrix and opening batch | Task 6 supplies the admissible cells; Task 8 selects only rows with an authoritative witness | Compatible. Unsupported social, temporal, relational, and close-reading cells remain internal requirements. |
| 7 → 8 | producer packets and Atlas renderer | Task 7 supplies the first public-scale packet and renderer; Task 8 consumes the same contract | Compatible. The batch may not invent a client-side semantic field absent from the packet. |
| 8 → 9 | episode manifests, captions, and batch record | Task 8 supplies draft records; Task 9 adds checksums and manual review state | Compatible. Task 9 cannot mark `approved` without Nathan's explicit review of the exact package. |
| 6 | matrix, commands, determinism checks | The matrix tests its own support classifications against observed commands | Internally consistent; a refusal or missing witness is a capability result, not a failed public episode. |
| 7 | producer, packet, renderer, local assembly | The selected cell is exercised end-to-end | Internally consistent; simulation changes are out of scope unless the matrix proves the phenomenon absent. |
| 8 | eight manifests, captions, batch | Each record names one object, scale, axis, unit, and sentence | Internally consistent; the underworld pilot is not silently relabeled as the public opening. |
| 9 | local outputs, review ledger, gate | Exact media/copy review precedes status changes | Internally consistent; generated outputs remain ignored and publication remains manual. |

Ruling: proceed with Task 6. The amendment adds a release-order and capability
audit slice without changing the established packet, renderer, or approval
contracts. Its only planned cross-task handoff is the capability matrix, whose
rows are evidence-backed before later tasks consume them.

## Task 6: complete

- **Commit:** `e42db32ee`
- **Result:** Added the evidence-backed astronomical-to-habitat capability
  matrix, including repeated producer witnesses, Atlas parser checks, and the
  underworld-only observation-export refusal boundary.
- **Evidence:** `make observation-check` passed; the task reviewer approved
  spec compliance and quality with no findings.
- **Review:** Approved; no fix round required.

## Task 7: complete

- **Commits:** `31a493625`, `f4e1530a0`, `0f706fc73`, `0db9c9f53`,
  `b81cb4835`, `6f7bbd536`, `3e6c2b48c`, `8b657bdb8`
- **Result:** Added the HV-009 stellar-neighborhood observation path, including
  declared-world loading, seed validation, deterministic arbitrary-world
  export, packet-driven Atlas rendering, and phone/laptop preview coverage.
- **Evidence:** 33 focused observation tests, 40 Atlas tests, 20 assembly
  assertions, and `make gate-commit` passed. Two full 900-frame exports were
  byte-identical.
- **Review:** Initial review found false arbitrary-seed provenance. Fix round
  1 added artifact and seed validation; re-review found repository-root path
  resolution incomplete. Fix round 2 corrected that boundary and added the
  non-root-CWD regression. Final scoped re-review approved with no new
  Critical or Important findings.

## #2 [G5] — Task 8 opening-batch amendment

- **Question:** How can Task 8 truthfully resequence eight public opening
  records when the capability matrix and Task 7 witness support only the
  stellar-neighborhood exporter path?
- **Decision:** Retain `HV-001` through `HV-008` as the draft internal
  underworld production-path pilot. Place the existing `HV-009`
  stellar-neighborhood record first in release order and amend the public
  opening batch to one active candidate; leave later cells unassigned while
  recording their exact implementation boundaries.
- **Why:** The matrix marks world, elevation, and habitat cells as needing an
  observation adapter; the system and regional cells need a renderer; and
  the social cells have no qualifying matrix witness. Eight captions or a
  relabelled underworld readout would be an invented public claim, not eight
  independent records.
- **Alternatives discarded:** Renumbering or copying `HV-009` eight times
  would duplicate one cell. Promoting the underworld pilot would violate the
  approved release spine. Calling unaudited social cells simulation gaps
  would pretend to know the first failing boundary.
- **Ideonomy passes / overturns:** One dimension-identification/map pass,
  using discovery-versus-invention and predictability. It separated
  discovered, witnessed support from authored record ordering and confirmed
  that only deterministic, present packet evidence—not expected future
  adapter work—can populate this batch. No overturn.
- **Capture actions:** The amended batch records `needs_observation_surface`,
  `needs_renderer`, and unassessed capability-audit boundaries separately.
  The next public record requires a follow-up implementation task; no public
  status or approval changed.

## Task 8: complete

- **Commit:** `94e3927ce`
- **Result:** Resequenced the opening truthfully: HV-009 is the sole current
  public candidate; HV-001–HV-008 remain the draft internal underworld pilot.
  Later cells and their exact adapter/renderer boundaries are recorded rather
  than fabricated.
- **Evidence:** 35 focused observation tests, 40 Atlas tests, deterministic
  HV-009 export checks, and `make gate-commit` passed.
- **Review:** Approved with no findings. The reviewer confirmed that all
  statuses remain draft and that the batch does not claim unsupported coverage.

## Task 9 ruling — rasterization prerequisite

Ruling: pause Task 9 at local assembly because the supported HV-009 export has
900 validated JSON packets and Atlas HTML previews but no PNG sequence. The
film assembler correctly refuses this state while ffmpeg is available. Add a
bounded rasterization task before attempting video assembly; do not create
placeholder images, claim a no-ffmpeg result, or change editorial statuses.

## Task 10: complete

- **Commit:** `172f1f78e`
- **Result:** Added a bounded Firefox-backed HTML-to-PNG renderer with packet
  identity checks, contiguous output validation, dimension checks, atomic
  replacement, and clear backend refusal.
- **Evidence:** Six raster tests, 40 Atlas tests, 20 film-assembly tests, and
  the prose commit suite passed. The actual environment lacks Firefox, so the
  renderer correctly refused and emitted no placeholder PNGs.
- **Review:** Approved with no findings.

Ruling: leave Task 9 blocked until a supported Firefox executable is installed
or supplied through `HV_OBSERVATION_FIREFOX`. That is an environment
prerequisite, not an editorial approval or a reason to change the package
status.

## #3 [G5] — Task 10 headless-raster boundary

- **Question:** Which existing local backend, if any, can turn the exact
  self-contained Atlas laptop HTML into the declared `1440×900` PNG sequence?
- **Decision:** Use Firefox headless screenshotting when its executable is
  available; otherwise the renderer must refuse. Do not use macOS Quick Look
  as a substitute.
- **Why:** Quick Look rendered the exact HTML deterministically but only as a
  `1440×1440` thumbnail. Firefox 137.0.2 rendered the exact `1440×900` HTML
  twice with matching local SHA-256 bytes, but its application bundle then
  disappeared from the environment before a full HV-009 run completed. The
  command now sees no Firefox and exits with the explicit unavailable-backend
  error rather than inventing pixels.
- **Alternatives discarded:** Resizing or cropping the Quick Look thumbnail
  would misrepresent the Atlas viewport. A new browser dependency would exceed
  the task's no-new-dependency boundary. Placeholder PNGs would falsely
  unblock film assembly.
- **Ideonomy passes / overturns:** One combination/graph pass over backend
  autonomy and packet cardinality. It exposed backend persistence—not one
  successful screenshot—as the decisive hub for a 900-frame sequence. No
  overturn: Firefox remains the only exact candidate, with refusal required
  when it is absent.
- **Capture actions:** The ignored task report records commands, hashes, and
  the current refusal. No editorial record changed; Task 9 remains blocked on
  a stable local raster backend.

## Task 10: complete

- **Result:** Added the bounded renderer/refusal command, its six focused
  checks, `observation-check` wiring, and the HTML-versus-PNG documentation.
- **Evidence:** Shellcheck, 40 Atlas tests, 20 film checks, six renderer
  checks, and `git diff --check` passed. The full HV-009 output is deliberately
  absent because the only exact backend did not remain installed.

## Task 9: HV-009 package assembled — manual review pending

- **Result:** With Firefox 155.0.1 available at the sanctioned macOS path,
  the foreground renderer wrote 900 `1440×900` HV-009 PNG frames. The
  foreground film assembler then wrote the derived 30-second `HV-009.mp4` and
  its 1,802-line `HV-009.sha256` sidecar under the ignored
  `observations/render-output/HV-009/package/` directory.
- **Evidence:** The exact render and film commands exited 0. Three sampled
  frames (000, 450, and 899) report `1440×900`; `HV-009.mp4` is nonempty at
  159,026 bytes and its SHA-256 is
  `b3858d66bc33eee237e4a4c0f73333d2bf79d4cde5434ea26682bdc21cff438e`.
  `ffprobe` reports its video stream as `1440×900`, `30/1` fps, 900 decoded
  frames, and 30.000000 seconds. After a rerun, `shasum -a 256 -c
  HV-009.sha256` from the ignored package directory passed all 1,802 entries;
  the sidecar covers the manifest, all 900 packets, all 900 PNGs, and the
  video digest with paths relative to that directory. Isolated Firefox
  captures were technically inspected at the phone `390×844` and laptop
  `1440×900` viewports: both retain the supplied title, stellar map, unit,
  object/scale/axis legend, observation sentence, and provenance.
- **Review boundary:** This is technical package assembly only. HV-009's
  manifest, caption, evidence status, and editorial status remain draft,
  `approval` remains `null`, and nothing is reviewed, approved, published, or
  counted toward the seven-package reserve. Its earliest review or publication
  date remains 2026-09-16. Nathan must manually inspect the exact film and
  caption for object, scale, axis, labels, sentence, duration, and legibility
  before any editorial state changes.

## #4 [G6] — user-approved close at the technical scope boundary

- **Question:** Should this campaign remain open for the intended presentation
  and seven-package reserve after it technically verified HV-009?
- **Decision:** Close the campaign. The user explicitly approved the narrower
  completed scope: deterministic manifest/export, Atlas preview,
  Firefox-per-frame rasterization, ffmpeg assembly, and a technically verified
  HV-009 local package. Do not submit this branch to the sluice from this
  close; the controller will review, gate, push, and enqueue the closure
  commit.
- **Why:** The exact raster loop launches Firefox once per frame, and the
  output is a static text layout rather than the intended fullscreen animated,
  subtitle-style presentation. The package establishes pipeline viability but
  does not establish presentation readiness. HV-009 remains draft with
  `approval: null`; manual review and a seven-approved-package reserve are
  different work.
- **Alternatives discarded:** Keeping the campaign open would mix a finished
  deterministic transport/proof path with an unbounded presentation redesign
  and editorial production queue. Marking HV-009 reviewed or approved, or
  counting it toward a reserve, would contradict its unchanged manifest.
- **Ideonomy passes / overturns:** N/A — explicit user G6 decision; no new
  design choice was auto-resolved.
- **Capture actions:** `docs/retrospectives/the-observation-series.md` routes
  the custom renderer/presentation campaign, HV-009's later manual review,
  and the reserve. `RENDER-observation-presentation` records the measured
  renderer follow-up. The plan now records campaign status Complete at this
  deliberate boundary.
