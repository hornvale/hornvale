# Fast Gate Tiers — Design

**Status:** approved (design), pending spec review
**Date:** 2026-07-13
**Campaign:** local-dev acceleration (Tier 1 + Tier 2 of the three-tier ask;
Tier 3 "census-as-data" is a separate spec)

## Problem

The default commit gate — `cargo test --workspace` (`make gate`) — takes
**~43.5 min** on an M1 Max (measured 2026-07-13, under parallel-session
contention; the structural floor on a quiet machine is ~35 min because
`cargo test` runs test *binaries* sequentially and the heavy tests live in
different binaries, so they cannot overlap).

The cost is concentrated in **five binaries running live-worldgen
correctness batteries** — the "run the whole pipeline as evidence" class of
test:

| Binary | Duration | Heavy test(s) |
|---|---|---|
| `fixture_staleness` | 875s | `census_fixtures_match_a_probe_of_live_seeds` (1 of 10) |
| `hornvale_lab` (lib) | 767s | 5 `runner::` + 3 `metrics::` tests (of 99) |
| `pin_enumeration` | 494s | `full_pin_product_is_enumerated` (whole binary) |
| `sky_exit_criterion` | 175s | `graded_pins_never_fail_above_min` (1 of 6) + peers |
| `hornvale_worldgen` (lib) | 87s | `build_world_..._byte_for_byte` (1 of 61) |

The other 39 test binaries total ~3.5 min combined.

These batteries are the project's correctness *evidence*, not per-save
tripwires. Per the owner's stated priority — *fast local iteration; a
corner case that breaks worldgen 1% of the time is acceptable so long as it
is caught eventually; never grind development or risk a large cloud bill* —
this evidence belongs in an **on-demand / nightly** tier, not in the loop a
developer runs on every commit.

## Goal

`cargo test --workspace` (the default `make gate`) completes in **≤ 5 min**
on a quiet M1 Max, while the full evidence remains runnable on demand
(`make gate-full`) and on the cloud, with **zero loss of coverage** — the
heavy tests are *deferred*, not deleted, and stay visible as an "N ignored"
count on every default run.

## Approach

Gate each heavy live-worldgen test with the standard-library mechanism:

```rust
#[test]
#[ignore = "heavy: live-worldgen battery; runs in `make gate-full` / cloud nightly"]
fn census_fixtures_match_a_probe_of_live_seeds() { ... }
```

- **`#[ignore]` only** — std-only, no cargo-feature plumbing across 15
  crates, no new dependencies. Chosen over a `heavy` cargo feature
  (workspace feature-wiring across every crate) and over a runtime env-var
  gate (an early-return test reports green while verifying nothing — bad
  signal hygiene). `#[ignore]` keeps the deferred tests **counted and named**
  in every default run.
- **Consistent reason string** — every gated test carries the literal token
  `heavy:` in its ignore reason, so the heavy tier is enumerable by grep and
  the roster cannot silently drift.
- Default `cargo test` skips `#[ignore]` tests automatically; the heavy tier
  runs with `-- --include-ignored` (runs both normal *and* ignored — not
  `--ignored`, which runs *only* ignored).

### Initial gate roster (first pass — the tests exceeding 60s)

1. `windows/lab/tests/fixture_staleness.rs::census_fixtures_match_a_probe_of_live_seeds`
2. `windows/worldgen/tests/pin_enumeration.rs::full_pin_product_is_enumerated`
3. `cli/tests/sky_exit_criterion.rs::graded_pins_never_fail_above_min`
4. `windows/worldgen/src/lib.rs::build_world_with_default_roster_matches_build_world_byte_for_byte`
5. `windows/lab/src/runner.rs`: `parallel_run_matches_sequential`,
   `five_seed_study_runs_and_is_deterministic`, `refusals_are_rows_not_errors`,
   `row_count_is_seeds_times_pin_sets`, `csv_round_trips_comma_containing_text_fields`
6. `windows/lab/src/metrics.rs`: `shape_metrics_are_present_deterministic_and_sane`,
   `core_homophony_is_zero_for_every_daughter_under_the_merger_aware_assignment`,
   `family_battery_metrics_are_deterministic_across_two_builds`

### Measure → gate → re-measure loop

The initial roster targets the >60s tests. Gating it should land the default
suite near ~5–7 min. **The binding success criterion is the ≤5-min wall
time, not the exact roster.** If the re-measure is still over 5 min, gate the
next-heaviest *live-worldgen or subprocess* tests until under — candidates in
likely order: the remaining `sky_exit_criterion` CLI world-build tests
(`rotation_flip_flips_the_religion`, `moons_flip_...`, `worlds_survive_reload_byte_identically`,
`scout_is_deterministic_and_finds_three_moon_worlds`, `refusals_are_recorded_in_the_world`),
then the astronomy/terrain property batteries (`genesis_properties`,
`tectonic_properties`), then `species_worlds`. Prefer gating live-worldgen /
subprocess tests over pure-logic ones — a fast unit test earns its place in
the commit gate.

## The tier ladder (Tier 2 — mostly naming what exists)

| Command | Scope | Time | Role |
|---|---|---|---|
| `make quick` | fmt + clippy | seconds | pre-commit |
| `make gate-fast` | changed crates only | <1 min | inner loop |
| `make gate` | full suite, heavy skipped | **≤5 min** | commit gate |
| `make gate-full` | `+ -- --include-ignored` | ~40 min | local full evidence |
| cloud (`gate-remote.sh`) | `make gate-full` + regen | ~39 min | on-demand / nightly |

Only `gate-full` is new; the rest exist. `gate-remote.sh` currently runs
`make gate` + `regenerate-artifacts.sh`; it changes to `make gate-full` so
the cloud path runs the full evidence (the whole reason to reach for the
cloud). A scheduled cron nightly is a separate, optional follow-up — the
owner has not committed to a fixed schedule, and on-demand is sufficient
for now.

## Out of scope

- **CI (`.github/workflows/ci.yml`)** — currently frozen (cross-platform
  divergence, main unpushed). When un-frozen, whether CI runs `make gate`
  (fast) or `make gate-full` (full) is the CI owner's call; this spec does
  not change ci.yml. The stated goal is *local* dev speed.
- **Census-as-data (Tier 3)** — its own spec.
- **The libm merge** — a separate in-flight thread; this branch builds on
  local `main` as it stands.

## Success criteria

1. `time cargo test --workspace` ≤ 300s wall on a quiet M1 Max.
2. `cargo test --workspace -- --include-ignored` runs the full set; the
   ignored count on a default run equals the gated roster size.
3. `grep -rn 'heavy:' --include=*.rs` enumerates exactly the gated roster
   (the tier is greppable and cannot silently drift).
4. `make gate-full` exists and runs `--include-ignored`.
5. `scripts/gate-remote.sh` runs the full tier.
6. CLAUDE.md's gate description and the Makefile help reflect the ladder.
7. No test is deleted or weakened; deferral only.

## Testing

The change is test-infrastructure, so "tests" here means measurements:

- Baseline captured: 2610s wall, per-binary breakdown above.
- After gating: `time cargo test --workspace` (assert ≤300s) and
  `cargo test --workspace -- --include-ignored 2>&1 | grep 'ignored'`
  (assert the ignored count matches the roster and the full run is green).
- A tiny guard test (or a line in the existing architecture/doc-consistency
  test) asserting the `heavy:` grep roster is non-empty and every match
  carries the full reason string — so the convention is enforced, not
  aspirational.
