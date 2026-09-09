# Skyworld Seams campaign ledger

**Campaign:** Skyworld Seams · **Branch:** `campaign/skyworld`
**Parent campaign:** `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`
**Status:** G4 plan prepared; implementation not started.

This ledger records the follow-up seam-audit campaign separately from the
parent Skyworld implementation ledger. The parent ledger retains the broader
Skyworld design, species, lifecycle, tethering, Waterworld, and cultural
follow-ups; this file records only the boundary and cost experiment.

## #1 [G4] — How should the Skyworld seam audit be executed?

**Recommendation:** use four bounded tasks: map the existing seam with
non-vacuous environment probes; verify independent environmental propagation;
measure substrate reuse and requested-work scaling; then record the reusable
cross-realm contract and close the slice.

**Why:** the approved seam design identifies uncertainty about ownership and
cost, not a missing realm feature. The plan keeps the existing
`skyworld_from` boundary and only permits a production wrapper or measurement
hook when a failing probe demonstrates that it earns its complexity. It also
puts the expensive or potentially misleading questions behind explicit tests:
source perturbations must change, reconstruction counters must stay at zero,
and detail/query paths must remain pure.

**Alternatives discarded:** adding a `BuildDepth` rung or save fact would make
the overlay durable before its cost is known; a universal realm or Sugar
abstraction would erase world-specific axes; adding organisms, species, or
lifecycle would multiply consumers before the boundary is measured; a dense
planet-by-time atmospheric simulation would violate the bounded-work goal.

**Plan:** `docs/superpowers/plans/2026-09-09-skyworld-seams.md`.

## #2 [G5] — Does the environmental propagation matrix prove its dependencies?

**Decision:** accept the propagation matrix after two review-driven fix
rounds. Surface/climate perturbations now prove source inequality before
checking dependent overlay outputs; the preservation probe regenerates both
fixtures and verifies terrain, climate, and `BiomeExpr` remain intact.

The tectonic probe initially remained correlational because plate-count
fixtures changed elevation, coast, climate, and edifices together. The final
fix extracts the pure score contribution, passes the real per-vertex
`has_edifice` flag from production, and unit-tests the documented `0.25`
increase directly. This makes the test fail if the edifice bias is removed.

**Evidence:** all 34 Skyworld integration tests and the edifice unit test pass
after The Staple D4 was incorporated from `origin/main`; the merged-tree
commit gate also passed. No Skyworld production API, BuildDepth rung, save
fact, stream roster, or cross-realm implementation was added.

**Capture:** Task 2 commits `b5c9b39d3`, `a410a2bfb`, and `4d9f3005b`.

## Deferred

- No organisms, species, lifecycle mutation, tethering, mutable atmosphere,
  Waterworld implementation, or census refresh belongs to this slice.
- The parent Skyworld ledger remains the source of truth for broader design
  follow-ups and previously recorded ideonomy results.
