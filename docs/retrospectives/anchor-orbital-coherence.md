# Anchor Orbital Coherence — retrospective

Process, not product. The product is in [the chronicle](../../book/src/chronicle/anchor-orbital-coherence.md).

## What worked

- **The bounded vertical slice was the right abstraction level.** Starting
  with the anchor gave the state contract four real consumers without
  inventing a generalized multi-body hierarchy. The typed state stayed small,
  while each consumer retained its semantic projection.
- **The explicit-time rule made hidden state testable.** Negative-time,
  reordered-query, and scene-byte repeatability tests exposed ambient-time or
  cached-state regressions that ordinary same-time golden tests would not.
- **The mutation tripwire improved confidence.** The cross-consumer battery
  does not merely check that every consumer calls one helper; it changes a
  projected eclipse value and asserts that the coherence check names the
  broken projection.
- **The gate found a determinism violation before commit.** The first final
  gate caught `f64::atan2` in the new test, and the fix moved it to the kernel's
  deterministic math wrapper. The repository gate then passed all 1,424
  sub-floor tests.

## What to carry forward

- **Generated-artifact commands need a narrow-output contract.** The first
  Task 4 regeneration touched unrelated gallery, world, and client outputs.
  Inspecting the diff and reverting those paths kept the campaign scoped, but
  future artifact work should name the exact expected output set before the
  command runs and verify it immediately afterward.
- **Companion tests must be named when a battery is composed.** The new
  cross-consumer battery owns coherence, ordering, and mutation detection;
  existing contract and eclipse tests own unsupported-time, frame, wrap, and
  valid-empty-event cases. The close record should describe that composition,
  not imply every assertion lives in one file.

## Deferred at close

- The shared state is anchor-only. Generalizing it to moons, wanderers,
  comets, or binary companions needs its own state-family design.
- Deep-time forcing remains a separate model and was deliberately not equated
  with instantaneous orbital radius.
- The campaign did not run a census. The timing ledger retains prior local
  gate/rebaseline measurements, including census-tail-labelled probes, but no
  canonical census was executed; any population-level measurement belongs to
  the canonical census cadence.
