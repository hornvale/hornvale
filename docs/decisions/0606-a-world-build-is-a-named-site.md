# 0606. A world build is a named site on a bidirectional roster

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates to:**
[0092](0092-derivation-at-named-sites.md),
[0041](0041-libm-for-portable-transcendentals.md),
`cli/tests/suite/test_binary_ratchet.rs`

In the context of the workspace's six world-build entry points
(`build_world`, `build_world_from_components`, `build_world_to`,
`build_world_to_with_artifacts`, `build_world_observed`, `history_for`) being
called from hundreds of sites
across the test suite — a full seed-42 build costs ~3.0s in a debug build, and
nextest's process-per-test model means a per-process memo recovers nothing
(decision 0032 recorded the identical conclusion for the census) — we decided
that **every call site to one of these entry points is a named row on a
roster, checked in both directions**, rather than left to accrete silently or
banned outright by a lint.

**Context.** The mechanism ruling: a source-scan enforcement test
(`cli/tests/suite/world_build_sites.rs`, roster in
`cli/tests/fixtures/world-build-sites.tsv`) rather than a `clippy.toml`
`disallowed-methods` entry. Decision 0092 already recorded the trap in that
lint: it is one on/off switch per scope, and a single crate-level `#[allow]`
there silenced all 24 platform-libm bans (decision 0041) across worldgen as a
side effect. Grandfathering hundreds of build sites into the same mechanism
would repeat that collateral at a far larger scale. `test_binary_ratchet.rs`
is the shipped idiom for this shape — freeze a roster, check both directions —
and CLAUDE.md names `cli/` as the home of the workspace-wide enforcement
tests.

**The mechanism.** Each roster row is `<repo-relative path>\t<count>\t<reason:N
...>`, keyed by an occurrence count per file rather than by line number, so an
unrelated edit above a site cannot redden the guard. Five reason codes:
`build-path` (asserts on the build itself — byte-identity, stream consumption
order, pin isolation), `artifacts` (needs `GeneratedTerrain`/
`GeneratedClimate`, which are `Clone` but not `Serialize`), `identity` (a
world identity with no committed fixture: a seed, a pin set, a build depth or
a component set), `production` (a real build on a real code path), and
`unmigrated` — grandfathered, the only reason that may not grow, held to
`UNMIGRATED_CEILING` in `world_build_sites.rs` **by equality**, so a
reclassification that lowers the tally must lower the constant in the same
commit rather than bank the difference as headroom for undeclared debt. The guard
asserts *live ⊆ roster* and *roster ⊆ live*; it does not assert that a row's
reason is the correct one, only that every site is accounted for. It is a
textual scan, so an entry-point name inside a comment or string literal counts
as a site — the same accepted imprecision `build_path_embedding.rs` states of
itself.

**Consequence.** At ratification the roster was generated in full, every row
`unmigrated` by construction: 350 sites across 200 files, `UNMIGRATED_CEILING`
set to 350. A generated file that size invites rubber-stamping, so no row may
carry a permanent reason code except by a human reclassifying it — the ratchet
starts at its maximum and only ever shrinks. This campaign's three migration
tasks moved it to **334** (16 points), and the roster stands at **356 sites
across 201 files**.

**`build_world_from_components` was the sixth entry point, and it was missing
from the list this decision ratified.** The paragraph above named five; the
composition root exports six, and `build_world` is a thin wrapper over
`build_world_from_components`. A needle is a name plus an open paren, so
`build_world`'s needle could never match a call to the longer name, and 9 live
sites across 5 files were invisible — including all of
`windows/worldgen/tests/suite/repose_exposure.rs`, which held a full-depth
build helper and no roster row at all. The final whole-branch review found it
by reading the composition root's `pub fn`s; the list had *looked* audited
because it adjudicated `simulate_world` explicitly (see below), which is a
different act from enumerating. **All 9 were classified on their merits and
`unmigrated` did not move**, so `UNMIGRATED_CEILING` stays 334: two are
production delegations in `windows/lab/src/metrics.rs` and
`windows/worldgen/src/lib.rs`, two assert byte-identity between this entry
point and `build_world` (`build-path`), and five build an identity no fixture
carries — a component set (`goblin_solo`, `warren_readout`'s emptied realm
registry) or a seed sweep (`repose_exposure`). This is a **scan-coverage
correction, not new debt**: the sites were always there and always ran; only
the roster's knowledge of them changed. The roster's *stated scope* was
corrected in the same commit, in the TSV header and the guard's module doc,
and now also states what it still does not scan (`examples/`).

**`UNMIGRATED_CEILING` is a debt counter, not a performance metric, and reading
it as the latter is this campaign's most misreadable number.** The roster
counts *sites textually present in a file*, not *call sites reached at
runtime* — a single row can gate dozens of callers through a shared helper.
This campaign's own migrations moved 239 call sites (measured at `c3f35ef9e`
with a word-boundary regex: 110 in `windows/vessel`, 54 in `windows/scene` and
`windows/worldgen`'s exposure suite, 75 across Task 5's remaining seed-42
helpers — 32 in `windows/vessel` (`session_snapshot.rs` and `the_blocking.rs`)
and 43 in `windows/worldgen`) while the ceiling moved only 16 points. Task 5
is the sharpest case: of `windows/worldgen/src/lib.rs::generated`'s 53 call
sites, the **43 that pass seed 42 stopped building** — they now read the
fixture — while the ~10 that pass another seed still build, because no fixture
exists for them, which is what the helper's own doc comment says. (This
paragraph had that backwards as ratified, reading "stopped building for every
seed **other** than 42"; the guard is `if seed == 42`, so seed 42 is precisely
the arm that stopped.) The file's own row did not move at all, because
`generated`'s single `build_world` call is still textually present in the
source, now behind that guard. Two of the 43 have since been taken back off
the helper deliberately — `generated_worlds_are_deterministic` and
`glossed_names_are_stable_across_two_builds` compare two independent builds,
so reading one file twice made them vacuous, and each now keeps a local
builder carrying a `build-path` reason. A reader who takes "350 → 334" as
"the campaign closed 4.6% of the redundancy" has mistaken the debt counter for
the payoff. The payoff is measured separately, in CPU-seconds
(`docs/timings.md`), and stood at **567.0 CPU-seconds** across six modules at
campaign close — see the chronicle and `docs/superpowers/ledgers/2026-09-02-the-reservoir.md`
entry #11 for the full derivation and a caution about how that call-site count
was itself twice miscounted before landing.

**See also.** Spec `docs/superpowers/specs/2026-09-02-the-reservoir-design.md`
§3.1-§3.2 and §8; decision 0032 (the census's identical nextest-process-model
conclusion); decisions 0092 and 0041 (the `disallowed-methods` collateral this
avoids repeating). Slug filename per decision 0026.
