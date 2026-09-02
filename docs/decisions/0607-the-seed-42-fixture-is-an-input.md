# 0607. The seed-42 world fixture is an input as well as an assertion

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates to:**
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
serialized"), so a disk fixture structurally cannot supply them. Decision
0606's `artifacts` reason is **reserved for a caller that needs those objects
and cannot obtain them from a loaded world; no such caller exists today, and
no roster row carries that reason.** Both artifact-needing modules this
campaign migrated (`windows/scene`'s surrounds, `windows/worldgen`'s exposure
suite) re-derive terrain and climate from the *loaded* world — `terrain_of(&w)`
then `climate_from(&w, &terrain)`, the same derivation they ran before — so
the read serves them and both carry `identity`, not `artifacts`. This
paragraph asserted an `artifacts` row in the present tense as ratified, which
was checkable against `world-build-sites.tsv` and false; the reason code is
kept, unused and declared, because the taxonomy should name the case that a
future non-`Serialize` consumer would fall into. What is true either way is
that a caller needing those objects pays the sculpt and the fit on top of the
read, which is why the loader's saving is not uniform, and the design spec's own
first estimate of the spread (`~3,000 ms -> ~1,110 ms`, "~2.7x") was invalidly
derived — it divided a quiet-box numerator by a contended-box denominator from
two different measurement runs — and is superseded by measurement, not
repeated here. The measured spread runs from **~200x** for a bare fixture read
against a bare build (~15 ms vs ~3.0 s) at one end, through **~4.0x-4.2x**
for the two modules measured that also need the derived terrain/climate
objects (`windows/scene` surrounds, `windows/worldgen` exposure), down to
**25%-48%** (roughly 1.3x-1.9x) for larger modules where the fixture read sits
beside a fixed cost of its own (`windows/vessel` `session::tests` 41%,
`session_snapshot` 48%, `the_blocking` 33%, `windows/worldgen --lib` 25% —
`docs/superpowers/ledgers/2026-09-02-the-reservoir.md` entry #10 has the full
derivation and names the arithmetic fault the first estimate made).

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

**Consequence for decision 0090's cross-host binary-identity oracle.** Every
production `env!("CARGO_MANIFEST_DIR")` expansion bakes the absolute build
directory into the shipped binary, which is why 0090 amendment 2 freezes the
set of such sites in `cli/tests/fixtures/manifest-dir-uses.txt` and requires a
grower to say what it does to the oracle — the list "may shrink freely;
growing it is a deliberate act that should say what it does to the oracle."
`windows/worldgen/src/fixture.rs`'s loader adds one, taking that list from two
entries to three (`cli/src/main.rs`, `windows/lab/src/blackbox.rs`, now
`windows/worldgen/src/fixture.rs`). This is **accepted**, recorded here rather
than only in the landing commit message (`b44db18ee`), because a commit
message is not where a durable consequence belongs: it does not change what
the oracle *requires* — both hosts still qualify by building at the same
absolute path, a condition two sites already made necessary for anyone
building outside a fixed-path image — only how many sites there are to keep
that condition true at. The alternative (resolving the workspace root at
runtime by walking up from `current_dir()`) trades a documented,
compile-time-visible fact for an undocumented runtime dependency on the
working directory, which is the wrong direction to route around a guard whose
purpose is exactly this visibility.

**See also.** Decision 0032 (the pattern this extends); decision 0125 (why
the freshness split moved entirely into the suite); decision 0606 (the
build-site roster this loader lets shrink). Slug filename per decision 0026.
