# The Accession — implementation plan

Spec: `docs/superpowers/specs/2026-07-27-the-accession-design.md`
G4 self-reviewed against the spec, 2026-07-27. Four stages.

## Stage 1: The accession register
**Goal**: `hornvale_language::concept_epoch(name) -> u32`, backed by an
authored cohort table whose epoch-0 cohort is exactly the 76 concepts
registered today.
**Design**: cohorts, not a name→epoch map — `EPOCH_COHORTS: &[&[&str]]`, where
a concept's epoch is the index of the cohort containing it. A later campaign
appends a cohort; it never edits an existing one (a concept that moved cohorts
would reshuffle, which is the whole thing being prevented).
**Unknown names return 0** — fail-safe, preserving today's behaviour for the
synthetic ids the language unit tests use. Loudness comes from Stage 3, not
from a panic in the draw path.
**Success**: `concept_epoch` returns 0 for all 76, 0 for an unknown name.
**Tests**: epoch-0 roster is exactly 76; no concept appears in two cohorts;
unknown → 0.
**Status**: Complete

## Stage 2: Order by (epoch, core_rank, id)
**Goal**: `assign_proto_roots` sorts epoch-first, so a later-epoch concept
always lands strictly last and cannot displace a prior assignment.
**Success**: seed 42 byte-identical to the merge base (every concept is
epoch 0, so the order is unchanged by construction — but asserted, not
assumed).
**Tests**: strengthen `assign_proto_roots_is_insertion_stable_for_earlier_sorting_concepts`
— keep the existing last-position case (it proves the property the fix rests
on) and add a **mid-sorting epoch-1** case, which fails on `main`.
**Status**: Complete

## Stage 3: The parity check
**Goal**: a concept registered without a cohort entry fails loudly.
**Why**: the authored table's one failure mode is a forgotten row, which
defaults to 0, sorts mid-alphabet, and silently restores the churn — the same
disease as the drift this campaign fixes (spec §3.2).
**Where**: `cli/tests/` — needs `register_all`, like `correspondence.rs`.
**Success**: every registered concept is in exactly one cohort; the test fails
if a concept is added without one.
**Tests**: parity both ways (no registered concept missing from the table; no
table entry naming an unregistered concept — a typo'd row is equally a defect).
**Status**: Complete

## Stage 4: Byte-identity and the gate
**Goal**: prove criteria 1 and 2 empirically.
**Success**:
1. Seed 42 ledger byte-identical to `origin/main`; `regenerate-artifacts.sh`
   produces zero diff.
2. Registering `otyugh-kind` (the 65-fact offender) at epoch 1 leaves the
   ledger byte-identical — spiked and reverted, evidence recorded here.
3. `make gate` green; type-audit clean.
**Status**: Complete
