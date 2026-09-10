# The Wanderers Test Hardening — Design

## Purpose

The Wanderers campaign changes stellar topology and anchor admission. That is
allowed to change world-derived population, naming, and walk witnesses, but it
must not force unrelated tests to re-pin incidental outputs. This design
separates durable behavioral claims from generated-world sample values.

## Boundaries

### Genesis sky observation

The worldgen sky regression will test the genesis observation path directly:
genesis uses the unoccluded observation primitive, while presentation reads
may apply weather and visibility. The test will assert the structural
relationship among the emitted belief fact families and the selected
observation path, not an exact seed-42 pantheon count.

### Detent emitter witness

The emitter-timeline witness will use a deterministic fixture or constructed
shape whose setup guarantees at least one emitter scan. The test will keep its
reachability floor and timeline-copy observation. It will not depend on a
lucky full-world seed whose population can change when astronomy changes.

### Warrant errand witnesses

The warrant tests will assert registered errand keys, producer provenance,
step coverage, and rendering behavior over the residents the current walk
actually reaches. Exact before-image population belongs to a separate fixture
contract and will not be used as a proxy for errand registration or rendering
correctness.

## Non-goals

- Do not weaken the actual production invariants.
- Do not re-pin the four failing values.
- Do not change the Wanderers world-generation behavior to preserve old test
  populations.
- Do not broaden the test suite with unrelated seed sweeps.

## Verification

Each boundary gets a focused red/green test cycle. The relevant crate suites
then run together, followed by the local commit gate. The held Sluice request
is superseded only after the branch is green and clean.
