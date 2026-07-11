# Workflow / tooling improvements (ideonomy session, 2026-07-10)

Twenty workflow/testing/tooling/process improvements, captured in the idea
registry as **TOOL-14…23** and **PROC-6…10** (20 originals consolidated into
15 rows by merging genuine same-idea pairs). This plan tracks execution.

> Note: this is a **separate workstream** from the active Lexicon-homophony
> campaign (which owns `IMPLEMENTATION_PLAN.md` and the uncommitted
> `branches-family` / `metrics.rs` changes). Keep the two threads' commits
> apart.

## The backlog (registry IDs)

- **TOOL-14** — cost-ordered gate as named targets (+ pre-commit cheap half)
- **TOOL-15** — single artifact-regeneration manifest (CI + local share it)
- **TOOL-16** — fixture-staleness detector with an actionable message
- **TOOL-17** — per-artifact CI jobs + path filter
- **TOOL-18** — distributional-fairness property tier (no-strict-dominator)
- **TOOL-19** — human-readable lab regression view
- **TOOL-20** — golden-master accept harness (`REBASELINE=1`)
- **TOOL-21** — mutation-test the determinism guards
- **TOOL-22** — scheduled full 10k census
- **TOOL-23** — cross-profile determinism sweep
- **PROC-6** — result-quieting guard (mechanize ADR 0016)
- **PROC-7** — mechanical Definition-of-Done gate
- **PROC-8** — knowledge-base drift linters (book staleness + decision-log)
- **PROC-9** — layering diagram emitted from its enforcer
- **PROC-10** — `just doctor` / repo self-map

## Stage 1: The three keystones (this pass)

**Goal**: land the three lowest-risk, highest-leverage items that unblock the
rest. Each is small, removes a recurring hazard, and is a dependency of later
rows.

### 1a. TOOL-14 — the gate as named targets
**Deliverable**: a `Makefile` (zero-install; `just` is not present) with
`quick` (fmt+clippy), `gate` (full three-check gate), `fmt`, `clippy`,
`test`, `rebaseline`/`artifacts`, plus a `scripts/hooks/pre-commit` running
only the cheap half and a `make install-hooks` target (not auto-run — it
mutates local git config).
**Success**: `make quick` and `make gate` reproduce the CLAUDE.md gate;
shellcheck-clean hook.
**Status**: Complete

### 1b. TOOL-15 — the single artifact manifest
**Deliverable**: `scripts/regenerate-artifacts.sh` holding the regeneration
commands currently inlined in ci.yml's "Artifacts are current" step; ci.yml
calls the script then keeps its verification tail (`release_determinism`,
`type-audit check`, `git diff --exit-code`). `make rebaseline` calls the same
script. Exclude-glob relocation deferred (CI-only; noted as follow-up).
**Success**: CI regeneration behaviour byte-identical; script is
shellcheck-clean; local `make rebaseline` regenerates the same set.
**Status**: Complete

### 1c. PROC-6 — the result-quieting guard
**Deliverable**: `windows/lab/tests/preregistration_guard.rs` — default-deny
scan of the lab's `*calibration*.rs` files: every `#[ignore]` must carry a
reason string sanctioned as either cost (census/expensive/slow/~Ns) or a
decision cite (ADR/decision/4-digit). Bare or result-quieting ignores fail.
Self-tested with inline good/bad samples.
**Success**: green on the current tree (the one census cost-ignore is
sanctioned); the bad-sample unit tests prove rejection.
**Status**: Complete

## Stage 2: The fixture spine + review surface

**Goal**: the two rows that build on TOOL-15's regeneration spine (TOOL-16,
TOOL-20) plus the review-surface companion (TOOL-19). Plan:
`docs/superpowers/plans/2026-07-10-workflow-improvements-stage-2.md`. All three landed; see the plan file for details.

### 2a. TOOL-16 — the fixture-staleness probe
**Deliverable**: `windows/lab/tests/fixture_staleness.rs` — regenerate the
first 3 seeds of each committed census live and compare (quantize-
canonicalized) against the committed `rows.csv`; fail with the `make
rebaseline` instruction. Also repairs `census_fixture_matches_live_run`,
which compared full-precision live rows to quantized fixture rows (broken
silently by the quantization epoch; CI never runs it).
**Status**: Complete

### 2b. TOOL-20 — the golden-master accept harness
**Deliverable**: `hornvale_kernel::golden` (`assert_golden`/`check_golden`,
`REBASELINE=1` to accept) replacing the ad-hoc comparisons in
`cli/tests/lens_purity.rs`, `windows/scene/tests/golden.rs`, and
`windows/worldgen/tests/proto_goblinoid_golden.rs`; `make
rebaseline-goldens`. `scripts/freeze-fixture.sh` narrows to historical pins
(the `pre-*` fixtures are frozen history, never rebaselined) — a scoped
deviation from the registry row's "replace freeze-fixture.sh".
**Status**: Complete

### 2c. TOOL-19 — the human-readable lab regression view
**Deliverable**: `hornvale lab diff <STUDY> <OLD_CSV> <NEW_CSV>` rendering
which metric moved and by how much (distribution deltas + numeric mean
shift); `make lab-diff STUDY=<name>` diffs the working tree against HEAD.
**Status**: Complete

### Stage 2 deferred follow-ups (from the final whole-branch review)

All five resolved in Stage 3a:
- Timing-accuracy sweep for the "~145s" census lore: a debug-profile run of
  the ignored guard measured ~450s. Touches the `#[ignore]` reason string
  in `windows/lab/tests/calibration.rs`, the sanctioned sample in
  `preregistration_guard.rs`, and CLAUDE.md; do as one coordinated edit
  with a release-vs-debug note.
- Add the `calibration-loads-the-census-fixture` decision cite to the
  probe's missing-row panic (it carries only the `make rebaseline` fix).
- Extract the duplicated quantize-canonicalize helper (in
  `fixture_staleness.rs` and `calibration.rs`) into `hornvale_lab`.
- Add a diff.rs test isolating mean-only movement (values shift within one
  histogram bucket: distribution unchanged, mean line still rendered).
- `make lab-diff`: fail fast with the real cause when
  `git show HEAD:...rows.csv` fails (typo'd STUDY / brand-new study).

## Stage 3: Drift-checks and orientation (this pass)

**Goal**: the Stage-2 deferred follow-ups plus the three locally-verifiable
process rows. Plan:
`docs/superpowers/plans/2026-07-10-workflow-improvements-stage-3.md`.

### 3a. Stage-2 deferred follow-ups
**Deliverable**: shared `canonical_row` in `hornvale_lab`; timing-accuracy
sweep of the "~145s" claims (measured ~450s debug); decision cite in the
probe's missing-row panic; a mean-only-movement diff test; fail-fast
`make lab-diff`.
**Status**: Complete

### 3b. PROC-8 — the decision-log consistency linter
**Deliverable**: `docs_consistency` resolves every `decision NNNN` /
`ADR NNNN` / hyphenated `decision <slug>` cite in `.rs`/`.sh` sources
against `docs/decisions/`. The book-staleness half is deferred: mtimes do
not survive git checkout; it needs a git-log-based freshness design.
**Status**: Complete

### 3c. PROC-9 — the layering page emitted from its enforcer
**Deliverable**: `cli/tests/architecture.rs` renders the enforced graph to
`book/src/reference/layering-generated.md`, golden-checked (REBASELINE=1
accepts); wrapper chapter + SUMMARY entry.
**Status**: Complete

### 3d. PROC-10 — `make doctor`, the repo self-map
**Deliverable**: `scripts/doctor.sh` printing pointers to the authoritative
orientation sources (layering, gate, determinism contracts, artifact
provenance, doc map, live git state) plus an orphaned-decision scan.
**Status**: Complete

## Stage 4+ (later passes)

TOOL-17/21/22/23 are CI-topology changes needing CI iteration. PROC-7 (the
mechanical DoD gate) is CI-shaped too. TOOL-18 is sim-facing test policy
(fairness batteries) — spec it deliberately. PROC-8's book-staleness scan
needs the git-log design. TOOL-15's exclude-glob relocation remains.
**Status**: Not Started
