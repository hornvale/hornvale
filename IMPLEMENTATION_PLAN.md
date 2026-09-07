## Stage 1: Population substrate and pure epidemiology
**Goal**: Establish the authoritative population view, pathogen catalogue, and kernel-only rules.
**Success Criteria**: Five kinds registered; H-M1, H-M2, and H-M6 pass; epidemiology depends only on kernel.
**Tests**: CCS, persistence, wave, outbreak, and catalogue-count tests.
**Status**: Complete — `bdff16910`; review approved after fix round 1

## Stage 2: Baked epidemic and history facts
**Goal**: Add deterministic epidemic phases, paired facts, Plague endings, and bake v4.
**Success Criteria**: H-M3/H-M5 pass; paired facts round-trip; expected genesis artifacts regenerate.
**Tests**: History phase, stream, graph boundary, emission, and byte-identity tests.
**Status**: Complete — `39087f079`, `0d37f1ef0`, `7619761e3`; final review approved

## Stage 3: Endemic read and Lot projection
**Goal**: Add substrate-based endemic burden, causes, composite cases, and materialized outbreak outcomes.
**Success Criteria**: H-M4 passes; Lot payload is additive; composites remain non-causal.
**Tests**: Lot attribution, endings, slots, payload, and byte-identity tests.
**Status**: Complete — focused Lot and exhibit gates green

## Stage 4: Laboratory instrumentation
**Goal**: Add six metrics, preregistered prediction readout, and cost measurement.
**Success Criteria**: H-P1–H-P6 recorded; census columns and provenance are current.
**Tests**: Metric registry, readout, schema, and timing checks.
**Status**: Not Started

## Stage 5: Genesis closure
**Goal**: Refresh artifacts, census, anomaly/Gnomon witnesses, book, registry, and campaign record.
**Success Criteria**: Canonical stage/merge gates pass and all documentation is fresh.
**Tests**: Local commit gate plus canonical sluice verification.
**Status**: Not Started
