# 0607. The seed-42 world fixture is an input as well as an assertion

**Status:** Proposed (2026-09-02) · **Decider:** Nathan · **Relates to:**
[0032](0032-calibration-loads-the-census-fixture.md),
[0125](0125-github-actions-is-retired.md),
[0606](0606-a-world-build-is-a-named-site.md)

In the context of the workspace's test suite rebuilding the same seed-42
world hundreds of times — a full build costs ~3.0s in a debug build against a
~15ms read of the already-committed `cli/tests/fixtures/world-seed-42.json`
(5.5 MB, 21,635 facts), and nextest's process-per-test model means an
in-process memo recovers nothing (93 of 100 world-building processes in one
observed run built exactly one world) — we decided that
**`hornvale_worldgen::seed_42_world()` reads the committed fixture instead of
rebuilding it, and the fixture is treated as an input a test may consume, not
only as an assertion target.**

**Context.** This extends decision 0032's pattern (the census's `rows.csv`
loaded instead of recomputed) from a study's summary output to a world
itself, and records an improvement on it rather than a repetition: 0032 split
its freshness guarantee across two halves, a suite test proving
`load_rows(fixture) == run(&study)` and a CI step regenerating the fixture and
diffing it. Decision 0125 deleted CI outright, which is where 0032's own
guarantee-split used to live for every other committed artifact. Here the
freshness guarantee lives entirely inside the suite: `seed_42_world()`'s own
byte-identity test
(`windows/worldgen/tests/suite/fixture.rs::the_fixture_equals_a_live_build`)
pins load equals build directly, and `cli/tests/suite/lens_purity.rs` and
`cli/tests/suite/repose_byte_identity.rs` independently build seed 42 and
assert the committed file's bytes — so the fixture's freshness is watched by
tests already in the gate rather than by a step nothing runs anymore.

**What the fixture cannot carry.** `GeneratedTerrain` and `GeneratedClimate`
are `Clone` but deliberately not `Serialize` ("recomputed on demand, never
serialized"), so a disk fixture structurally cannot supply them — a caller
that needs those objects still calls a build entry point and carries an
`artifacts` reason on the `world-build-sites.tsv` roster (decision 0606).
This is why the loader's saving is not uniform: a caller that only reads
committed facts sees the full ~200x (~3.0s to ~15ms), while a caller that
also needs the derived terrain/climate objects pays the sculpt and the fit on
top of the read, recovering only ~2.7x.

**Consequence.** `seed_42_world()` is read at runtime
(`std::fs::read_to_string` against a path built from
`env!("CARGO_MANIFEST_DIR")`), not baked in with `include_str!` — the file is
5.5 MB, and every test binary that wants a world would otherwise pay that
compilation-unit cost, which is what dominates this project's gate. It lives
in `windows/worldgen` (the composition root) rather than in `cli`, so
`env!("CARGO_MANIFEST_DIR")`'s one fixed `../../` prefix resolves correctly
from any crate that calls it, verified by a cross-crate probe from
`windows/vessel`. The world-seed-42.json fixture itself does not move: this
decision changes who reads it, never what it contains.

**See also.** Decision 0032 (the pattern this extends); decision 0125 (why
the freshness split moved entirely into the suite); decision 0606 (the
build-site roster this loader lets shrink). Slug filename per decision 0026.
