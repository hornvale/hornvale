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
