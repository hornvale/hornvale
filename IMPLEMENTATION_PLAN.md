# The Actants — implementation plan

Spec: `docs/superpowers/specs/2026-07-27-the-actants-design.md`
G4 self-reviewed against the approved spec, 2026-07-27.
The Accession is merged and absorbed (`e6a56fc9`), so Stage B's precondition
holds.

## Stage 1: Orphan species (A1)
**Goal**: `hornvale concepts --manifest` reports kinds in
`biosphere_registry()` with no `{kind}-kind` concept.
**Where**: `cli/src/concepts.rs`, beside `orphan_phenomena`.
**Success**: the line lists the 12 unnamed fauna, derived not literal.
**Tests**: line contains a known fauna kind, excludes a peopled kind, and is
computed from the registry (asserted by comparing against a set built from
`biosphere_registry()` in the test).
**Status**: Not Started

## Stage 2: Orphan actions (A2)
**Goal**: same, over the GOAP action roster.
**Where**: `Action::all()` in `windows/vessel/src/liveness.rs` + the house
destructure tripwire, so a fifth variant cannot compile unlisted; the audit
itself in `cli/src/concepts.rs`.
**Success**: all four actions report as orphans today.
**Tests**: `all()` covers every variant (tripwire); the line is derived from
`Action::all()`.
**Status**: Not Started

## Stage 3: The prose line (A3)
**Goal**: name the third reverse direction as *unaudited* with its count; do
not audit it (needs a design line first — followup).
**Status**: Not Started

## Stage 4: Stage-A drift check
**Goal**: prove Stage A moves no world.
**Success**: seed 42 byte-identical to the merge base;
`regenerate-artifacts.sh` touches exactly `concept-manifest-generated.md`.
**Status**: Not Started

## Stage 5: The naming (Stage B)
**Goal**: register the 12 species concepts and the 4 act concepts, at
accession epoch 1.
**Steps**:
1. `ConceptKind::Act`; re-kind `eat`/`sleep`/`die` from `Quality`.
   **Verify separately** that `ConceptKind` feeds nothing generative — if it
   does, the re-kind is not free and gets its own decision.
2. Species: derive the roster from `biosphere_registry()`, authored glosses
   ("a giant elk", not "a giant-elk").
3. Acts: `move`/`drink`/`rest`/`eat` owned by `language` (0025 — `eat` is
   already language's).
4. Append every new concept to `EPOCH_COHORTS` as cohort 1.
**Success**: seed 42 ledger byte-identical; artifact diff is added rows only;
`cli/tests/accession.rs` green.
**Status**: Not Started

## Stage 6: Close
**Goal**: orphan lines read `none`; A4's tests pass **unchanged** (the proof
they asserted derivation, not population). Full gate; DoD walk.
**Status**: Not Started
