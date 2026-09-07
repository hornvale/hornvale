# Route-cache follow-up

## Stage 1: Measure current-relative route reuse
**Goal**: Establish the real query population and reuse shape of the current-relative water fold.
**Success Criteria**: A deterministic probe reports total searches, unique route keys, repeat opportunities, peak live key population, and growth across the existing seed-42 and seed-17 workloads.
**Tests**: Focused vessel probe; existing current-relative fold tests; deterministic repeat run.
**Status**: Complete

## Stage 2: Select the smallest justified mechanism
**Goal**: Compare fresh searches, a per-decision one-to-many distance field, and a bounded position-keyed cache using Stage 1 evidence.
**Success Criteria**: A written decision identifies the key, owner, capacity or lifetime, invalidation rule, and the evidence threshold for adoption; explicitly reject alternatives that lack demonstrated benefit.
**Tests**: Review the measurement output and verify no public or serialized surface changes are assumed without evidence.
**Status**: Complete — no persistent cache justified; per-decision distance field deferred.

## Stage 3: Implement only if measurement warrants it
**Goal**: Add the selected mechanism with red-first behavioral and determinism tests.
**Success Criteria**: Current-relative behavior is unchanged, cache state is bounded and deterministic, and all affected observers remain covered.
**Tests**: Focused vessel tests, gate-commit checks, and the appropriate stage-gate queue run.
**Status**: Complete — no production implementation required.

## Stage 4: Close the campaign
**Goal**: Review, verify, and submit the exact tested branch through the Sluice if code or committed measurement artifacts changed.
**Success Criteria**: Required review and verification evidence is recorded; no stale plan remains after completion.
**Tests**: `make gate-commit`; queued stage/merge checks as required by the final diff.
**Status**: In Progress
