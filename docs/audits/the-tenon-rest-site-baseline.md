# The rest-site baseline, before any surface is added

**Taken:** 2026-09-04, on the Mac, at `campaign/the-tenon` before any of The
Tenon's mechanism tasks. **Instrument:**
`windows/lab/examples/rest_site_census.rs`
(`cargo run --release -p hornvale-lab --example rest_site_census`).

**One-time measurement, not a regenerated artifact.** It is deliberately NOT
listed in `docs/generated-paths.txt`: nothing regenerates it, so a drift check
over it would compare a dated measurement against a world that has moved, and
`git diff --exit-code` would then redden for the very change this document
exists to be the *before* half of.

## The headline

**Afforded rest bouts are real and reachable today — the afforded path is not
dead.** Over 24 seeds, 40 ticks, 10 bodies each: **601 of 5,096 bouts (11.79%)
were graded `Afforded`**, spread across **11 of the 24 seeds**. The remaining
13 seeds produced **zero** afforded bouts, seed 42 among them.

That last fact is the instrument's own positive control, and it is worth more
than the rate. Decision 0697 recorded — independently, from the affect-trace
byte-golden — that seed 42's traced window is entirely open ground. This probe,
built from a different direction and reading committed facts rather than a
fold, reports seed 42 at 0/259 afforded. A probe that agreed with a known zero
*and* found nonzero elsewhere is a probe that is measuring something.

## The sweep

| | |
|---|---|
| seeds | 24: `42, 7, 1234, 13, 1, 2, 3, 5, 8, 11, 17, 23, 29, 31, 37, 41, 53, 59, 61, 67, 71, 73, 79, 83` |
| ticks per seed | 40 (`HEALTH_TICKS`' value) |
| bodies per seed | 10 = 6 settlement-derived (`HEALTH_NPCS`) + 4 wild (`HEALTH_WILD`) |
| worlds that built | 24 of 24 |
| **runs that truncated** | **0 of 24** — every run completed all 40 ticks |
| cost | ~11 s/seed, ~4.5 min for the sweep, `--release`, on the Mac |

**The truncation check is not decoration.** `windows/lab/src/health.rs` has
`Err(_) => break` at lines 164 and 289: either arm silently stops a simulation
on any commit error, and two calibration tests once passed on runs stopped at
day 1 (`TOOL-lab-run-simulation-swallows-commit-errors`). The probe reports the
tick count each run actually completed and prints the list of truncated seeds;
that list is empty, so every number below is a measurement rather than a
broken instrument's zero.

## The five counts

### 1–2. Bouts, and their grade

| | count | share of bouts |
|---|---:|---:|
| `RESTED` bouts | 1,407 | 0.2761 |
| `SLEPT` bouts | 3,689 | 0.7239 |
| **total bouts** | **5,096** | 1.0000 |
| graded `Afforded` | **601** | **0.1179** |
| graded `Bare` | 4,495 | 0.8821 |

Per seed (`ticks` is the completed count; all 40):

| seed | ticks | rested | slept | afforded | bare | slept-on |
|---:|---:|---:|---:|---:|---:|---:|
| 42 | 40 | 64 | 195 | 0 | 259 | 0 |
| 7 | 40 | 64 | 168 | 0 | 232 | 0 |
| 1234 | 40 | 3 | 283 | 7 | 279 | 6 |
| 13 | 40 | 301 | 0 | 201 | 100 | 0 |
| 1 | 40 | 4 | 106 | 0 | 110 | 0 |
| 2 | 40 | 5 | 107 | 0 | 112 | 0 |
| 3 | 40 | 69 | 247 | 0 | 316 | 0 |
| 5 | 40 | 4 | 152 | 0 | 156 | 0 |
| 8 | 40 | 225 | 0 | 0 | 225 | 0 |
| 11 | 40 | 37 | 99 | 0 | 136 | 0 |
| 17 | 40 | 58 | 269 | 35 | 292 | 27 |
| 23 | 40 | 22 | 86 | 53 | 55 | 33 |
| 29 | 40 | 153 | 289 | 125 | 317 | 88 |
| 31 | 40 | 5 | 143 | 0 | 148 | 0 |
| 37 | 40 | 28 | 113 | 0 | 141 | 0 |
| 41 | 40 | 34 | 190 | 0 | 224 | 0 |
| 53 | 40 | 15 | 104 | 76 | 43 | 61 |
| 59 | 40 | 40 | 238 | 3 | 275 | 2 |
| 61 | 40 | 119 | 101 | 4 | 216 | 1 |
| 67 | 40 | 36 | 158 | 35 | 159 | 31 |
| 71 | 40 | 109 | 215 | 0 | 324 | 0 |
| 73 | 40 | 3 | 118 | 57 | 64 | 57 |
| 79 | 40 | 2 | 168 | 0 | 170 | 0 |
| 83 | 40 | 7 | 140 | 5 | 142 | 5 |

**Seeds with at least one afforded bout: 11 of 24 (0.4583).** Seeds with none:
`42, 7, 1, 2, 3, 5, 8, 11, 31, 37, 41, 71, 79`.

### 3. `SLEPT_ON` facts, by the `KindId` in `Value::Text`

| kind | count |
|---|---:|
| `bed` | **311** |

**One kind, and that is the campaign's premise stated as a measurement.**
`bed` is today's only carrier of `ObjectProperty::SupportsRest`, so it is the
only kind any body has ever slept on, in any of these 24 worlds.

**311 `SLEPT_ON` facts against 601 afforded bouts is not a discrepancy.**
`SLEPT_ON` is `Action::Sleep`'s site: it is committed for a `SLEPT` bout only,
never for a `RESTED` one. Seed 13 is the clean witness — 201 afforded bouts,
0 `SLEPT` bouts, 0 `SLEPT_ON` facts. The two numbers count different things and
agree wherever they can be compared.

### 3b. The cross-check that makes the grade believable

The `Afforded` grade above is a reconstruction (see the last section); the
`SLEPT_ON` count is written by production code. They are countable against
each other, and the arithmetic holds:

`afforded - slept_on` should be the number of afforded **`RESTED`** bouts,
because `SLEPT_ON` is committed for a `SLEPT` bout only. So it must be
non-negative and never exceed that seed's `RESTED` count. It never is and
never does, on any of the 24 seeds:

| seed | afforded − slept-on | rested |
|---:|---:|---:|
| 1234 | 1 | 3 |
| 13 | 201 | 301 |
| 17 | 8 | 58 |
| 23 | 20 | 22 |
| 29 | 37 | 153 |
| 53 | **15** | **15** |
| 59 | 1 | 40 |
| 61 | 3 | 119 |
| 67 | 4 | 36 |
| 73 | 0 | 3 |
| 83 | 0 | 7 |

Seed 53 is the tight one: every one of its 15 `RESTED` bouts was afforded, and
the two numbers meet exactly. Sweep-wide, 601 − 311 = 290 afforded `RESTED`
bouts against 1,407 `RESTED` bouts total. A reconstruction that over-graded
would break the upper bound; one that under-graded would go negative. Neither
happens.

### 4. Walked rooms by `(is_built, is_cold)` quadrant

Distinct `FacetId`s per seed, summed across the sweep, from each body's
committed `agent-at` trail sampled at every simulated day.

| quadrant | rooms | share | affords rest to some body |
|---|---:|---:|---|
| `built=false cold=false` | 1,627 | 0.7406 | **no** |
| `built=false cold=true` | 407 | 0.1852 | **no** |
| `built=true cold=false` | 129 | 0.0587 | **no** |
| `built=true cold=true` | **34** | **0.0155** | **yes** |
| total | 2,197 | | |

**The whole of today's afforded path is 1.55% of walked rooms.** Exactly one of
the four quadrants `interior_of`'s entire input set can produce ever offers a
body anywhere to lie down; the other three — 98.45% of where bodies actually walk —
are bare ground, always, in every world.

The probe prints these as QUADRANTS rather than the spec's and plan's own
word for them, which `cli/tests/suite/lexicon_guard.rs` reserves for a mesh
vertex; they are the same four.

This is the measurement the spec's §7 table is a design against, and it is
consistent with the standing census column `cold-built-room-share` (median
0.183 of BUILT rooms; 129 + 34 = 163 built rooms here, of which 34 are cold =
0.209) without being the same number — that column is a share of built rooms
over a settled world, this is a share of *walked* rooms over a walking
population, and the two denominators are different on purpose.

### 5. Truncation

None. 24 of 24 runs completed all 40 ticks. See the sweep table above.

## What this baseline does and does not license

- It **licenses** a before/after claim on the afforded share (0.1179) and on
  the reachable-quadrant count (1 of 4). Both are counts over a named sweep, with
  the denominator reported, per decision 0097.
- It **does not** license a claim about a single world: the per-seed spread is
  enormous (0 to 201 afforded bouts), so a one-seed re-measurement after Task 7
  would be an anecdote. Re-run the same 24 seeds.
- It **does not** measure P2 (a reversal) or P4 (a consequential choice) at
  all. Both are undefined today: with one sleepable kind there is no pair to
  order and no choice to make. They become measurable only after Task 7.

## Two reconstructions, and how they can rot

The probe cannot call the functions it measures, because both are private and
widening them for a measurement's benefit would change the surface being
measured. It reconstructs each, and both are exact **today**:

1. **`liveness::room_affords_rest`** (`fn`, `windows/vessel/src/liveness.rs:3724`).
   That function is
   `sleep_site::select_sleep_site(&interior_of(room, terrain), body).is_some()`;
   `select_sleep_site` (`pub(crate)`, `sleep_site.rs:63`) is
   `interior.ids().into_iter().find(|&a| offered_to(interior.anchor(a).kind, body).contains(&OfferedVerb::Sleep))`,
   and `.find(..).is_some()` is `.any(..)`. The probe inlines that `.any(..)`
   over the two genuinely `pub` functions, `interior::interior_of` and
   `affordance::offered_to`.
2. **The `SiteGrade` resolution inside `liveness::rest_timeline`**
   (`fn`, `liveness.rs:3802`): a bout's room is the last committed `agent-at`
   at or before its day, defaulting to `body.home`. The probe resolves it with
   `liveness::agent_position`, which is `latest_committed_position` (`day <= t`,
   last matching fact) with exactly that home default — the same trail, not a
   second one.

**If either private function changes, these drift silently and nothing will
say so.** That is the price of not widening them, and it is recorded here
rather than left for a later reader to discover.

The probe also rebuilds `LocaleTerrain` once after the walk rather than
capturing each tick's. That is exact rather than approximate: `interior_of`
reads `terrain.is_built` and `terrain.is_cold` and nothing else
(`interior/derive.rs:20`); `is_built` tests membership of the `built_rooms` set
computed once before the walk, and `is_cold` is a temperature read at
`FURNISHING_REFERENCE_DAY`. Neither is a function of what the walk committed.
