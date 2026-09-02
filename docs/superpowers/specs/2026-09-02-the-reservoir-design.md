# The Reservoir — one built world, drawn many times

**Date:** 2026-09-02 · **Branch:** `campaign/the-reservoir` (based on `25ee1d830`)
· **Decision block:** 0606-0615 · **Ledger:**
`docs/superpowers/ledgers/2026-09-02-the-reservoir.md`

A reservoir is filled once and drawn from many times. The test suite fills its
own, repeatedly, from the same spring — and nothing in the repository notices.
This campaign gives the suite one stored world to draw on, and a ratchet so the
refilling cannot quietly come back.

---

## 1. The problem, measured

A full world build costs about three seconds, and four fifths of that is one
stage.

```
  build profile, seed 0-2, `--example profile_build -- 3`   total 8.989 s / 3 seeds
  ------------------------------------------------------------------------------
  astronomy                  0.001 s    0.0%
  terrain                    1.010 s   11.2%
  climate+settlements        7.260 s   80.8%   <-- four fifths of every build
  alignments                 0.002 s    0.0%
  culture+religion+species   0.045 s    0.5%
  deep-time                  0.494 s    5.5%
  planet                     0.008 s    0.1%
  peoples                    0.137 s    1.5%
  person                     0.032 s    0.4%
```

**And the suite pays it over and over for the same world.** `build_to` was
instrumented with an RAII recorder (pid, seed, depth, pin-hash, elapsed) and a
full-workspace run started. The default profile is fail-fast, and a pre-existing
flake (§9, P1) stopped it at test 274 of 4869 — so this is a **sample of the
first 285 tests, not the whole suite**, and no figure below is extrapolated from
it.

```
  world builds observed                                    107
  distinct (seed, depth, pin-hash) identities               14
  builds of the single dominant identity                    71   <-- seed 42, Full, default pins
  distinct processes that built at least one world         100
    of those, processes that built exactly one              93
    processes that built two                                 7
```

Seventy-one builds of one identical world, in 285 tests. The suite as a whole is
4,869 tests across 64 binaries.

### 1.1 The suite's shape, from the committed baseline

`docs/timings/test-baseline-lefford.tsv` (recorded at `be7fe3ac5`) gives per-test
durations, so the cost surface needs no new measurement:

```
  1,085 named tests            16,061.5 CPU-s      mean 14.8 s
  3,789 below-floor tests         118.7 CPU-s
  tests needed to reach 50% of the total:  228     <-- no pole; a flat surface
```

A flat distribution with no pole is the signature of *many tests each paying a
fixed cost*, which is what a redundant world build is. By crate:

```
  windows/worldgen   5,262.2 s   32.8%
  windows/vessel     5,001.0 s   31.1%
  windows/lab        2,035.0 s   12.7%
  cli                1,392.5 s    8.7%
  windows/book       1,079.8 s    6.7%
  windows/scene        982.7 s    6.1%
  ----------------------------------------
  six crates                     98.1% of suite CPU time
```

### 1.2 The reservoir already exists, and nothing drinks from it

`cli/tests/fixtures/world-seed-42.json` is **5,563,005 bytes** holding **21,635
facts**, 130 predicates, 13 phenomenon kinds and 252 concepts. It is the
byte-golden of exactly `build_world(Seed(42), SkyPins::default(),
SkyChoice::Generated, TerrainPins::default(), SettlementPins::default())`, and
two tests already build that world and assert byte-equality against it
(`cli/tests/suite/lens_purity.rs`, `cli/tests/suite/repose_byte_identity.rs`).

**No test reads it as an input.** It is used only as an assertion target.

Reading it is roughly two hundred times cheaper than rebuilding it. Measured
back to back in one process (a scratch example, since deleted; box at load
24-32, so absolutes are inflated ~3x against §1's quieter run — the **ratios**
are what carry):

```
  World::from_json (5.5 MB)          14.2 / 16.9 / 21.0 ms      21,635 facts
  World::clone                        1.4 / 1.4 ms              21,635 facts
  World::to_json                     17.9 ms
  ---------------------------------------------------------------------------
  build_world (Full)              9,363.2 ms                    21,635 facts
  build_world_to(Settlements)    10,322.4 ms                    18,386 facts
  build_world_to(Terrain)         1,093.7 ms                       110 facts
  build_world_to(Astronomy)           0.5 ms                        83 facts
  WorldComponents::assemble           0.1 ms
```

Against §1's quieter ~3,000 ms Full build: **load is ~200x cheaper, clone
~2,000x.** (`Settlements` reading above `Full` in that column is contention
noise and is exactly why the absolutes are not quoted as costs; the ordering of
magnitudes is unaffected.)

---

## 2. What the measurement rules out

Two attractive answers are dead, and the repository already knew about one.

**2.1 In-process memoisation buys almost nothing.** nextest is process-per-test.
The probe measured 100 distinct processes building worlds, of which **93 built
exactly one** and 7 built two — so a `OnceLock`/`LazyLock` memo keyed on world
identity would recover on the order of 7%, not 90%. This is not a new finding:
decision **0032** weighed and rejected it in writing — "cargo-nextest (its
process-per-test model re-initialises the `LazyLock` census once *per test* —
strictly worse for this suite, not better)". The orphaned processes left behind
when the probe run was killed confirmed the model directly: each was
`hornvale_vessel-<hash> --exact session::tests::<name> --nocapture`, one process
per test.

Consequence: **the reuse carrier must be cross-process**, which means on disk.

**2.2 A disk fixture cannot carry the derived artifacts.** `GeneratedTerrain`
and `GeneratedClimate` are `Clone` but deliberately **not** `Serialize` —
`domains/climate/src/provider.rs:71` says so outright: "Recomputed on demand,
never serialized." So a `World` fixture carries the ledger (all 21,635 facts,
including everything the 80.8% climate+settlements stage commits) but **not** the
terrain sculpt or the climate fit.

Consequence: the payoff is a spread, not a single number, and this spec will not
pretend otherwise:

```
  a test that reads only facts          3,000 ms -> ~15 ms          ~200x
  a test that then needs terrain        3,000 ms -> ~1,110 ms        ~2.7x
```

Both are worth having. Neither is 200x across the board.

---

## 3. The design

### 3.1 The ratchet: a source-scan enforcement test, not a clippy ban

Nathan's proposal was to mark the build pathway deprecated and allowlist
sanctioned patterns. That is decision **0092** — "the weir" — already ratified
and in force for `terrain_of` / `climate_from` / `demography_report_from` via
`clippy.toml`'s `disallowed-methods`, with a scoped `#[allow]` citing 0092 as the
greppable allowlist. The instinct is the project's own doctrine, not desperation.

**The clippy mechanism is nevertheless wrong at this scale, on 0092's own
evidence.** `disallowed-methods` is one lint with one on/off switch per scope.
0092 records that a single crate-level allow silenced all 24 platform-libm bans
(decision 0041) across worldgen — a constitutional determinism guard, disabled
as a side effect of bucketing a *different* lint entry, caught only in review.
0092 carried ~31 production sites. This campaign would carry hundreds of
grandfathered sites, and therefore hundreds of scopes in which the libm ban and
the weir both go dark. Trading a cost problem for a determinism-guard hole is
the wrong trade in this repository above all others.

So: **a workspace enforcement test in `cli/tests/suite/`**, modelled on
`cli/tests/suite/test_binary_ratchet.rs` — which exists for a structurally
identical problem (a new `tests/*.rs` silently adds a compilation unit; 13 had
crept back before the guard was written) and is the shipped idiom. CLAUDE.md
names `cli/` as the home of the workspace-wide enforcement tests.

The guard scans every workspace crate's `src/**` and `tests/**` for calls to the
build entry points and requires each site to appear in a roster. **Scanning
`src/` as well as `tests/` is deliberate and load-bearing**: the single largest
target in the repository, `seam_world` with 84 callers, lives in a `#[cfg(test)]
mod tests` block *inside* `windows/vessel/src/session.rs`. A tests-only scan
would miss the flagship. Scanning all of `src/` also avoids having to locate a
test module's textual span, which would be fragile.

Entry points scanned: `build_world`, `build_world_to`,
`build_world_to_with_artifacts`, `build_world_observed`, `history_for`,
`simulate_world`.

**Three-valued, checked in both directions** — the shape `tropes check`, the
timings baseline, type-audit's `waiver(...)` and seam-guard all use:

```
  site present, not rostered        -> RED   ("a new world build needs a reason")
  site present, rostered            -> green, counted and printed
  rostered, site gone               -> RED   ("delete the row")
```

The third arm is what keeps the roster honest. A one-directional list can only
ever be satisfied, so it rots; this one fails the moment a migration lands
without its row being removed. It is also the answer to 0092's stated objection
to a roster file ("never a second document to keep in sync"): that objection is
sound against a unidirectional list and dissolves under bidirectional checking.

### 3.2 The roster and its reason codes

`cli/tests/fixtures/world-build-sites.tsv`, one row per site:

```
  <repo-relative path>  <site key>  <entry point>  <reason>
```

**The site key is deliberately not a line number** — see §9. A line-keyed row
invalidates on any edit above it, so the bidirectional check would redden on
unrelated commits and be trained away exactly as `test_binary_ratchet.rs` warns.
The plan chooses the key against the real scan (enclosing function name is the
obvious candidate, since §3.4 shows the sites are already funnelled through named
helpers); this spec fixes the requirement — **stable under edits elsewhere in the
file** — and not the mechanism.

Reasons — this is where the *why does this build* taxonomy lives, which is the
right key for an allowlist rather than "does this call site memoise":

```
  build-path   asserts on the build ITSELF: byte-identity, stream consumption
               order, pin isolation. Must build. PERMANENT.
  artifacts    needs GeneratedTerrain / GeneratedClimate, which are Clone but
               not Serialize (§2.2), so no fixture can supply them. PERMANENT.
  identity     needs a seed or pin set with no committed fixture. PERMANENT.
  production   a real build on a real code path (the CLI, a window's entry
               point, worldgen's own pipeline). PERMANENT.
  unmigrated(<why>)  grandfathered. THE ONLY REASON THAT MAY NOT GROW.
```

**The ratchet is then one assertion: the number of `unmigrated(...)` rows never
increases.** A guard that failed on the mere existence of the grandfathered set
would be red on day one and trained away by day two — `test_binary_ratchet.rs`
says exactly this in its own module doc. Splitting permanent reasons from
`unmigrated(...)` makes the ratchet measurable without making it a wall, and gives the
campaign a single number to drive down.

Roster size, measured across the workspace at `25ee1d830`:

```
  crate                 src   tests   total
  windows/worldgen       51     147     198
  windows/vessel         15      36      51
  cli                    20      26      46
  windows/hearsay         0      21      21
  windows/lab             2      18      20
  windows/scene          11       3      14
  windows/book            3       0       3
  windows/locale          0       1       1
  windows/almanac         0       1       1
  ------------------------------------------
  TOTAL                 102     253     355
```

355 rows, generated once and then frozen. Bounded and reviewable.

### 3.3 The fixture as a declared input

A loader in `hornvale-worldgen` — the composition root, which every crate that
builds a world already depends on, so no new layering edge appears:

```rust
/// The committed seed-42 world, read from disk rather than rebuilt.
pub fn seed_42_world() -> World
```

Two mechanics matter and both are chosen against measured cost:

- **Runtime read, never `include_str!`.** 5.5 MB baked into N test binaries hits
  the compilation-unit cost that *is* this project's gate cost — CLAUDE.md
  records the 260 test targets alone costing +157.5 s of kernel time merely to
  exist. A literal that large across dozens of units would regress the axis this
  campaign exists to improve.
- **One fixed relative path, verified rather than assumed.**
  `env!("CARGO_MANIFEST_DIR")` expands against the crate containing the macro —
  worldgen — so a single `../../cli/tests/fixtures/` prefix resolves correctly no
  matter which crate calls the function. The tree's existing idiom at
  `windows/worldgen/tests/suite/repose_exposure.rs:1924` proves only a
  *neighbouring* claim (worldgen's own test reading worldgen's dir), so the real
  one was measured with a scratch probe — a `pub fn` in worldgen returning
  `env!("CARGO_MANIFEST_DIR")`, called from an example in `cli`:

  ```
    worldgen fn reports:               .../the-reservoir/windows/worldgen   <-- called FROM cli
    cli's own env! is:                 .../the-reservoir/cli
    cli-relative ../../ resolves?      false        <-- negative control
    worldgen-relative ../../ resolves? true
  ```

  The negative control is the load-bearing half: the prefix is **crate-specific**,
  so the loader must live in exactly one crate with one prefix. A per-crate
  helper duplicating the `../../` would resolve outside the repository from `cli`
  and `kernel` (one level deep) while working from every `windows/*` and
  `domains/*` crate (two levels deep) — a bug that would pass in most crates and
  fail in two. Scratch reverted; `git status` clean.

Test support in production source follows `kernel/src/golden.rs`, which is
already exactly that. The fixture itself does **not** move (ledger #5): the
prose in `docs/generated-paths.txt` about `cli/tests/fixtures/` being byte-golden
and frozen-historical-pin territory would have to be rewritten, and that is a
category-sensitive edit that does not belong in this campaign.

**Freshness is guarded by a test, not by CI.** This is where the design improves
on decision 0032. 0032 split its guarantee across CI — "the artifact step proves
the committed fixture is *fresh* (regenerate + `git diff`), and the calibration
tests prove the fresh fixture reproduces *ground truth*" — and decision **0125**
subsequently deleted CI entirely. Here the freshness guard is
`lens_purity.rs` / `repose_byte_identity.rs`: they build seed 42 and assert
byte-equality against the fixture, inside the suite, on every run. Nothing
external is relied upon.

### 3.4 The migration surface: seven helpers, 240 reachable sites

The builds are already funnelled through per-module helpers, which is what makes
this tractable. **694 of 942 test-side world acquisitions (74%) route through
just 70 helper functions**, and the concentration at the top is extreme:

```
  callers  helper                     file                                  cum
      84   seam_world                 windows/vessel/src/session.rs          84
      53   generated                  windows/worldgen/src/lib.rs           137
      47   generated                  windows/book/src/lib.rs               184
      36   constant                   windows/worldgen/src/lib.rs           220
      34   build_world (test wrapper)  windows/worldgen/src/lib.rs           254
      31   world                      windows/scene/src/surrounds.rs        285
      27   world                      windows/worldgen/tests/suite/exposure.rs  312
      24   seam_world                 windows/vessel/tests/suite/session.rs 336
```

**But concentration is not reachability, and an earlier draft of this section
conflated them.** The table above ranks helpers by caller count and says "six
bodies reach 285 sites", which is true and was the wrong number to plan against:
two of those six do not build the identity the fixture holds. Checked against
each helper's actual body and each caller's actual argument:

```
  callers  helper                                    identity          reachable?
      84   vessel/src/session.rs::seam_world         42 / Generated    YES
   43/53   worldgen/src/lib.rs::generated(seed)      42 / Generated    YES (seed-42 arm)
      31   scene/src/surrounds.rs::world             42 / Generated    YES
      27   worldgen/tests/suite/exposure.rs::world   42 / Generated    YES
      24   vessel/tests/suite/session.rs::seam_world 42 / Generated    YES
      16   vessel/…/session_snapshot.rs::world       42 / Generated    YES
      15   vessel/…/the_blocking.rs::world           42 / Generated    YES
  --------------------------------------------------------------------------
     240   reachable with the fixture that already exists
  --------------------------------------------------------------------------
      47   book/src/lib.rs::generated(seed)          *seed 1* / Gen.   NO
      36   worldgen/src/lib.rs::constant(seed)       42 / *Constant*   NO
```

`book/src/lib.rs::generated` is called with seed 1 twenty-eight times, seed 2
four times, seed 3 twice — and **seed 42 not once**. `worldgen::constant` builds
under `SkyChoice::Constant`, a different pin signature (almost certainly the
second-commonest identity in §1's sample, at 21 builds). Neither can read a
seed-42-Generated fixture at all.

**So: 240 call sites are reachable with the fixture that already exists, and 83
more are blocked behind a second (seed 1, Generated) and third (seed 42,
Constant) fixture.** Authoring those is out of scope per §7 — a committed World
fixture becomes a determinism reference, which is a deliberate act — and they are
recorded as the campaign's sized follow-on rather than smuggled in.

The call sites themselves do not change in either case; only helper bodies do.

The flagship is exact. `windows/vessel/src/session.rs:7938`:

```rust
fn seam_world() -> World {
    build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated,
                &TerrainPins::default(), &SettlementPins::default())
        .expect("seed 42 builds")
}
```

That is byte-for-byte the world the fixture holds. Its module has 117 `#[test]`
functions, 85 of which call `seam_world()`, and its 103 named tests carry
**1,554.1 CPU-s** on lefford. Under nextest each is its own process paying its
own build.

Staging follows the concentration, one helper per stage, each independently
verifiable and independently revertable.

---

## 4. What must keep building

The permanent exempt set, by reason code:

- **`build-path`** — the determinism guards. `lens_purity.rs` and
  `repose_byte_identity.rs` (the fixture's own freshness guards, which must build
  or the whole scheme is circular); the pin-isolation tests in
  `domains/astronomy/tests/` and `domains/terrain/tests/` that assert a pinned
  path consumes the same draws as the unpinned one; anything asserting on stream
  consumption order. These are the tests whose subject *is* the build.
- **`artifacts`** — anything needing `GeneratedTerrain` or `GeneratedClimate`
  objects (§2.2). A fixture structurally cannot supply them.
- **`identity`** — non-default seeds and pin sets. Fourteen identities appeared
  in the sample; only the dominant one gets a fixture in this campaign.
- **`production`** — every real build path. Untouched.

---

## 5. Coverage: the honest accounting

If N tests stop building and start loading, they stop exercising the build path.
That is a real change and it deserves to be stated rather than waved past.

**The argument that the loss is small:** the golden test compares all 21,635
facts byte-for-byte. A worldgen regression that moves any fact reddens it,
comprehensively. Two hundred tests each building the same world and each
asserting on one slice of it detect strictly *less* about whether the world
moved than one test comparing the whole thing. On the "did the world change"
axis, the golden dominates the redundancy it replaces.

**The residue, named:** a test that loads cannot catch

1. a build-time **panic** or a `GenesisError` regression on the default path
   (though `build-path` and `production` sites still exercise it);
2. drift in the **derived artifacts**, which the fixture does not carry at all
   and which no committed artifact currently pins byte-for-byte;
3. a non-determinism that manifests only under some process/thread interleaving
   — though builds are deterministic and single-threaded per test, so this is
   the weakest of the three.

Item (2) is the one worth watching, and it is pre-existing rather than created
here: nothing today pins the terrain sculpt's bytes. This spec does not fix it
and does not make it worse; §9 records it as an open question.

---

## 6. Success criteria

Testable, in the order a plan would verify them:

1. **The guard refuses a new site.** A scratch build site added to a test file
   with no roster row fails the guard, citing the file, the line and the reason
   codes available. Deleted immediately after, with `git status` confirming no
   trace — the rejection demonstration 0092 §4.1 performed.
2. **The guard refuses a stale row.** A roster row whose site no longer exists
   fails, naming the row to delete.
3. **`unmigrated(...)` cannot grow.** Adding a row beyond the frozen count
   fails. This is the ratchet, and it is the criterion the campaign exists for.
4. **The fixture equals a live build.** `lens_purity.rs` and
   `repose_byte_identity.rs` stay green, unmodified. If they need modifying, the
   scheme is wrong.
5. **`seed_42_world()` is byte-identical to `build_world(Seed(42), …)`** —
   asserted directly, both directions serialized and compared, in worldgen's own
   suite.
6. **The migrated helpers change no test outcome.** Each staged helper migration
   leaves its crate's suite green with no test body edited. A test that fails
   after its helper switches to the fixture has found a real dependency on the
   build and gets a roster row with a permanent reason instead.
7. **Measured, not assumed.** Each stage records a before/after row in
   `docs/timings.md` for its crate's scoped suite, on one box, with load average
   noted. The campaign's headline number is the sum of those, never an
   extrapolation.

---

## 7. Non-goals

- **Depth-scoping.** `build_world_to` already exists and `BuildDepth::Astronomy`
  costs 0.5 ms against Full's 3,000 ms, so pushing tests down the ladder is a
  real and large lever — 247 `build_world(` call sites against 55
  `build_world_to(` workspace-wide. It is **MAP-25, already shipped**, and
  reopening it here would blur two efforts. A `unmigrated(...)` row whose right fix is
  a shallower depth rather than a fixture is still a legitimate migration; the
  ratchet does not care which lever closes a row.
- **Making the build faster.** 80.8% of a build is `climate+settlements`. That is
  a genuine optimisation target and it is not this campaign.
- **Serializing the derived artifacts.** Would widen the payoff from ~2.7x to
  ~200x for artifact-needing tests, and is a save-format-adjacent change
  deserving its own campaign and its own decision.
- **The remaining identities.** Only the dominant seed-42 Generated identity gets
  a fixture here. The two that would unblock the most work are named and sized in
  §3.4 — a seed-1 Generated fixture (47 sites, `book/src/lib.rs::generated`) and a
  seed-42 Constant one (36 sites, `worldgen/src/lib.rs::constant`) — because a
  committed World fixture becomes a determinism reference, and authoring one is a
  deliberate act rather than a convenience.
- **Fixing P1** (§9). Recorded, not fixed.

---

## 8. Decisions to ratify

From block 0606-0615:

- **0606 — A world build is a named site on a bidirectional roster.** The
  mechanism ruling: a source-scan ratchet rather than a `clippy.toml` entry,
  because `disallowed-methods` is one switch per scope and grandfathering
  hundreds of sites would silence decision 0041's libm bans and 0092's weir in
  every one of them. Records the reason-code taxonomy and the
  `unmigrated(...)`-never-grows ratchet. Relates to 0092, 0041, and
  `test_binary_ratchet.rs`.
- **0607 — The seed-42 world fixture is an input as well as an assertion.**
  Extends decision 0032's pattern from the census to the world itself, and
  records the improvement on it: the freshness guarantee lives in a suite test,
  not in the CI step decision 0125 deleted.

---

## 9. Risks and open questions

- **P1, a pre-existing flake, found during measurement.**
  `cli/tests/suite/scene_surrounds_colour_cli.rs:42` writes
  `std::env::temp_dir().join("hv-scene-surrounds-colour-test.json")` — a fixed,
  unsuffixed name. It failed with `World::load: NotFound` *after* the CLI had
  written the file successfully, taking a fail-fast run down at test 274 of 4869.
  Not caused by the instrumentation (the probe wrote only to an absolute
  scratchpad path). Fix shape: a per-process suffix, or `CARGO_TARGET_TMPDIR`.
  Recorded as P1 in the campaign ledger; **not** fixed here.
- **The suite-wide prize is not measured.** The 107-builds figure is a sample of
  the first 285 tests. A full instrumented run was attempted and abandoned: at
  observed throughput it projected to roughly three hours on a box already at
  load average 42, and running it would have hammered a machine in active use.
  Deliberately **not** extrapolated. If the exact share of 16,061 CPU-s is wanted
  before committing, the honest way to get it is an instrumented run on lefford.
  The design does not depend on it — a ~200x ratio justifies the work at any
  plausible count, and the ratchet's value is independent of the count entirely.
- **P2 — nothing pins the derived artifacts' bytes** (§5, residue 2).
  Pre-existing, neither created nor fixed here. Worth a `TOOL-*` row if the
  registry does not already carry one. Parked in the campaign ledger.
- **The roster is 355 rows and will be generated, then reviewed.** A
  generated-then-frozen file of that size invites rubber-stamping. Mitigation:
  the initial generation assigns **every** row `unmigrated(...)`, and a permanent
  reason code is only ever assigned by a human looking at the site. That way the
  ratchet starts at its maximum and every reclassification is a deliberate,
  reviewable act rather than a bulk assertion made by a script.
- **The roster's line numbers will churn.** A row keyed on `<path> <line>`
  invalidates on any edit above it, which would make the bidirectional check
  scream on unrelated commits. The plan must resolve this — key on
  `<path> <enclosing fn>` instead, or on path plus a count of sites in the file.
  Flagged rather than decided: it is an implementation choice the plan should
  make against the real scan, not a design commitment made from outside the code.

---

## 10. Measurement appendix

Every number above, and the command that produced it. Run on this Mac
(10 cores) at `25ee1d830` unless noted; load average stated where it matters.

```
  §1 build profile
    cargo run -q -p hornvale-worldgen --example profile_build -- 3

  §1 redundancy sample (instrumentation since reverted; `git status` clean)
    RAII recorder added to `build_to` in windows/worldgen/src/lib.rs,
    appending (pid, seed, depth, pin-hash, argv test name, elapsed);
    cargo nextest run --workspace          [stopped by P1 at 274/4869]

  §1.1 suite shape
    awk over docs/timings/test-baseline-lefford.tsv   (recorded at be7fe3ac5)
    cargo nextest run --workspace          [reported "4869 tests across 64 binaries"]

  §1.2 fixture contents
    python3 json.loads over cli/tests/fixtures/world-seed-42.json
      -> 5,563,005 bytes; ledger.facts 21,635; registry.predicates 130;
         registry.phenomenon_kinds 13; registry.concepts 252
    NOTE: python len() on a decoded str returns 5,562,098 CHARACTERS;
          the byte count is 5,563,005 (907 multi-byte chars). The Rust
          measurement, which reads bytes, agrees at 5,563,005.

  §1.2 load-vs-build (scratch example, since deleted; load avg 24-32)
    windows/worldgen/examples/hv_scratch_loadbench.rs

  §2.1 process-per-test
    93 of 100 world-building pids built exactly one world (probe data);
    orphaned processes after the kill read
      hornvale_vessel-<hash> --exact session::tests::<name> --nocapture
    Corroborated by decision 0032's own rejection of the LazyLock approach.

  §2.2 artifacts not serializable
    grep -B4 "pub struct GeneratedTerrain" domains/terrain/src/provider.rs
    grep -B4 "pub struct GeneratedClimate" domains/climate/src/provider.rs
      -> both `#[derive(Debug, Clone)]`; climate's doc: "Recomputed on
         demand, never serialized."

  §3.3 cross-crate env! expansion (scratch since reverted; git status clean)
    pub fn in windows/worldgen/src/lib.rs returning env!("CARGO_MANIFEST_DIR"),
    called from cli/examples/hv_scratch_manifest.rs:
      worldgen fn reports  .../windows/worldgen   (called from cli)
      cli's own env!       .../cli
      cli-relative ../../ resolves?      false    <-- negative control
      worldgen-relative ../../ resolves? true

  §3.2 roster size / §3.4 helper concentration
    python3 scan over each crate's src/ and tests/ for the six entry points
      -> 355 sites (102 src, 253 tests)
      -> 694 of 942 test-side acquisitions through 70 helpers (>=3 callers)
    NOTE: the first run of this scan included .claude/worktrees/, which
          duplicated every crate. Figures above are main-checkout only.

  §3.4 vessel flagship
    117 `#[test]` fns in windows/vessel/src/session.rs's tests module;
    85 seam_world() calls, 17 world_at() calls;
    103 named session::tests = 1,554.1 CPU-s (lefford baseline)
```

**Two cautions this appendix exists to carry.** The absolutes in §1.2 were taken
at load average 24-32 on a box in active use and are inflated roughly 3x against
§1's quieter run — one column even reads `Settlements` above `Full`, which is
impossible and is the contention showing. Every claim in this spec rests on the
**ratios**, which are stable across both runs. And the 107-build figure is a
sample of 285 tests, never scaled to 4,869.
