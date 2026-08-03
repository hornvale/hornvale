# The Contour — baseline (Task 5)

**This file is a frozen measurement. Do not edit it after this commit.**
Task 7's readout compares its own numbers against exactly what is recorded
here; if this file changes after the fact, the comparison is worthless. If a
mistake is found in this baseline, it must be corrected by a *new*, clearly
labelled record (or a superseding note), never by editing these numbers in
place — the same discipline `docs/decisions/` uses for ratified decisions.

## Commit SHA

```
c405a5e216776c9beaae35e4e82a1b07b0258abe
```

(`git rev-parse HEAD` at measurement time; `2026-07-30 17:51:43 -0400`,
subject `feat(the-contour): register M4 on present-day terrain, labelled
(spec 2.4 amendment 4)`.)

## The bake is unmodified at this SHA

`defensibility` (and its per-cell view, `weakest_point_defensibility`) exist
in `windows/worldgen/src/history_bake.rs`, but neither is called from the
two functions the plan's Task 6 will wire them into: `best_home` (the
settle-site chooser) or `maybe_raid` (the raid/dominance test). Proof —
grepping each function's body for `defensib` finds nothing:

```
$ grep -n "^    fn best_home\|^    fn maybe_raid" windows/worldgen/src/history_bake.rs
1243:    fn best_home(
2644:    fn maybe_raid(&mut self, raider: usize, era: &EraClimate, year: f64) {

$ sed -n '1243,1335p' windows/worldgen/src/history_bake.rs | grep -in "defensib"
(no output)

$ sed -n '2644,2915p' windows/worldgen/src/history_bake.rs | grep -in "defensib"
(no output)
```

(`best_home` runs 1243–1335; `maybe_raid` runs 2644–2915, up to the next
`fn` at 2916.) The only caller of `weakest_point_defensibility` in the whole
tree is the measurement path itself — the `defensibility-capacity-rank-corr`
metric (`windows/lab/src/metrics.rs:3625`) — plus its own property tests in
`windows/worldgen/tests/defensibility_field.rs`:

```
$ grep -rn "weakest_point_defensibility(" --include=*.rs . | grep -v /target/
windows/worldgen/tests/defensibility_field.rs:134:    let weakest = weakest_point_defensibility(&g, CellId(2));
windows/worldgen/tests/defensibility_field.rs:178:    assert_eq!(weakest_point_defensibility(&g, CellId(2)), via_0.min(via_1));
windows/worldgen/tests/defensibility_field.rs:189:        weakest_point_defensibility(&isolated, CellId(0)),
windows/worldgen/src/history_bake.rs:188:pub fn weakest_point_defensibility(graph: &ConnectionGraph, cell: CellId) -> f64 {
windows/lab/src/metrics.rs:3625:        defs.push(hornvale_worldgen::weakest_point_defensibility(&graph, cell));
```

So the field is built and readable, but has no path into the bake's raid or
settlement-siting decisions. The bake's behaviour at this SHA is byte-for-
byte what it was before this campaign started.

## M1 — the inherited cascade-size metric: NOT REGISTERED

The Tumult and The Tithe both reported a cascade-size distribution
(`BakeCensus.cascade_hist` / `hornvale_worldgen::cascade_sizes`,
`windows/worldgen/src/history_bake.rs`). Per task instructions, I confirmed
its exact registered lab-metric name before writing the study:

```
$ cargo run -p hornvale -- lab list-metrics | grep -i cascade
(no output — only a false-positive substring hit inside
 homophony-merger-share-goblin's doc text, which is unrelated)
```

**No such metric is registered in the lab's `registry()`
(`windows/lab/src/metrics.rs`).** `cascade_sizes()` / `BakeCensus.cascade_hist`
exist only as an internal `windows/worldgen` type, read directly by that
crate's own tests (e.g. the heavy `cascade_sizes_are_measured_and_the_shape_
adjudicated` battery referenced in the Tumult plan) — never exposed through
`Metric` / `lab list-metrics` / a study JSON. It is therefore **not included**
in `studies/the-contour.study.json`'s `metrics` array; a study cannot name an
unregistered metric (`Study::validate` rejects unknown names). This is
reported explicitly per the task brief rather than silently omitted. If M1
comparability against The Tumult/The Tithe is required, it will need either
a new registered lab metric wrapping `cascade_sizes()`, or a direct
`windows/worldgen` test reading `BakeCensus.cascade_hist` the same way those
two campaigns did — a decision for whoever executes Task 7.

## The exact command

```
cargo run -p hornvale -- lab run studies/the-contour.study.json
```

(Note: the study lives at `studies/the-contour.study.json`, not
`windows/lab/studies/...` as the task brief's Step 2 command says — the
`studies/` directory is a workspace-root directory, not nested under
`windows/lab/`. Verified against every existing shipped study, e.g.
`studies/the-census.study.json`.)

Output:

```
study the-contour: 30 rows, 0 refusals; summary + 5 charts published.
```

30 rows, 0 refusals, as expected. Raw CSV:
`book/src/laboratory/generated/the-contour/rows.csv`.

## Study definition as run

`studies/the-contour.study.json`:

```json
{ "name": "the-contour",
  "description": "Preregistered hypothesis (decision 0016): a second contest axis (cell defensibility), uncorrelated with strength and entering at the dominance test, raises the number of peoples surviving to bake end. The entity-size distribution is the open question and both branches are informative: a heavier tail supports the sigmoid wager; a geometric tail alongside a diversity rise localises the missing term in per-community deviation rather than leaving it unlocated. This is the BASELINE run — measured on the unmodified bake, before the defensibility mechanism is wired into maybe_raid/best_home (see docs/superpowers/plans/the-contour-baseline.md). NOTE: the inherited cascade-size metric reported by The Tumult and The Tithe (BakeCensus.cascade_hist / hornvale_worldgen::cascade_sizes) is NOT a registered lab metric — confirmed absent from `lab list-metrics` — so it cannot appear in this study's metrics list; it stays comparable only via the raw histogram those two campaigns' own retrospectives recorded.",
  "seeds": { "from": 1, "count": 30 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": [ "peoples-alive-at-bake-end", "largest-holding-share", "defensibility-capacity-rank-corr" ] }
```

Two schema corrections versus the task brief's Step 1 draft, made after
reading a shipped study (`studies/the-census.study.json`) as instructed:

- The `Study` struct (`windows/lab/src/study.rs`) has no `hypothesis` field.
  The hypothesis prose is carried in the required `description` field
  instead (every shipped study uses `description`, not `hypothesis`).
- `Seeds` is `{ from: u64, count: u64 }`, not `{ from, to }`. Seeds
  `1..=30` (matching The Tumult and The Tithe exactly) is therefore
  `{ "from": 1, "count": 30 }`.
- `pin_sets` is required (`Study::validate` rejects an empty list); added
  the shipped-study convention `[{ "label": "default", "pins": [] }]`.

`cargo test -p hornvale-lab --test preregistration_guard` passes
unaffected (the guard scans `windows/lab/tests/*calibration*.rs`; this
study adds no such file).

## Baseline distributions (seeds 1..=30, pin set `default`)

All 30 seeds produced a row; 0 refusals reported by the runner.

### `peoples-alive-at-bake-end`

Present for all 30 seeds (never Absent — it is a plain count, always
defined). Distribution is bimodal: the mode is 5 (full survival of the
shipped roster), with a distinct low mode at 0 (total extinction).

| stat | value |
|---|---|
| n | 30 |
| min | 0 |
| q10 | 0 |
| q25 | 5 |
| median (q50) | 5 |
| q75 | 5 |
| q90 | 5 |
| max | 5 |
| mean | 3.9667 |
| stdev | 2.0254 |

Value counts: `5` → 23 seeds, `0` → 6 seeds, `4` → 1 seed.

Per-seed (seed: value): 1:5, 2:5, 3:5, 4:5, 5:5, 6:0, 7:5, 8:5, 9:0, 10:5,
11:5, 12:5, 13:5, 14:5, 15:5, 16:5, 17:5, 18:0, 19:4, 20:0, 21:5, 22:0, 23:5,
24:5, 25:5, 26:5, 27:5, 28:5, 29:0, 30:5.

### `largest-holding-share`

Present for 24/30 seeds; **Absent for exactly the 6 seeds where
`peoples-alive-at-bake-end` is 0** (seeds 6, 9, 18, 20, 22, 29) — consistent
with the metric's own documented Absent condition (no live community exists
to compute a share of when nobody survives). This is the expected,
non-degenerate shape, not an error.

| stat | value (over the 24 present rows) |
|---|---|
| n present | 24 |
| n absent | 6 |
| min | 0.0076184379 |
| q10 | 0.0089692 |
| q25 | 0.0105980 |
| median (q50) | 0.0137384 |
| q75 | 0.0205305 |
| q90 | 0.0525822 |
| max | 0.082405345 |
| mean | 0.0222406 |
| stdev | 0.0204147 |

Right-skewed: median (~0.0137) well below mean (~0.0222), with a handful of
high outliers (seeds 11, 12, 19, 27 all >0.045) pulling the tail. No value
repeats — all 24 present values are distinct.

Per-seed (seed: value or `Absent`): 1:0.0105759, 2:0.0195982, 3:0.0127325,
4:0.0158942, 5:0.0076184, 6:Absent, 7:0.0083241, 8:0.0140427, 9:Absent,
10:0.0105010, 11:0.0466082, 12:0.0551425, 13:0.0148121, 14:0.0109549,
15:0.0160654, 16:0.0106054, 17:0.0197253, 18:Absent, 19:0.0824053,
20:Absent, 21:0.0271289, 22:Absent, 23:0.0104744, 24:0.0080020, 25:0.0116749,
26:0.0134342, 27:0.0712788, 28:0.0229464, 29:Absent, 30:0.0132281.

### `defensibility-capacity-rank-corr`

Present for all 30 seeds (never Absent — every world has ≥2 habitable cells
with non-constant defensibility/capacity series). Centered near zero, with
substantial spread in both directions — consistent with the study's
preregistered framing that this axis is designed to be uncorrelated with
strength/capacity.

| stat | value |
|---|---|
| n | 30 |
| min | -0.36532184 |
| q10 | -0.19328309 |
| q25 | -0.09271928 |
| median (q50) | -0.00385596 |
| q75 | 0.06178445 |
| q90 | 0.15760587 |
| max | 0.18387989 |
| mean | -0.02000 |
| stdev | 0.13775 |

Per-seed (seed: value): 1:0.0094703, 2:0.0296283, 3:-0.0368686,
4:-0.0955412, 5:0.1004277, 6:-0.2443338, 7:0.0188972, 8:0.1790302,
9:-0.1625870, 10:-0.0381479, 11:0.0596242, 12:0.0379648, 13:0.1838799,
14:-0.0171822, 15:0.1650533, 16:-0.0635181, 17:0.0766758, 18:0.0518533,
19:-0.1097010, 20:-0.1208479, 21:-0.3653218, 22:0.0625045, 23:0.0254216,
24:0.1449884, 25:0.1567784, 26:-0.1876108, 27:-0.2904279, 28:-0.0842535,
29:-0.0677766, 30:-0.0180011.

## What this baseline does NOT include

- **M1 (cascade-size)**: not registered as a lab metric; see above. Not
  present in this baseline's numbers.
- The generated book artifacts under
  `book/src/laboratory/generated/the-contour/` (rows.csv, schema.json,
  summary, charts) produced by the run above are left untracked by this
  task's commit — the raw per-seed table in this file is the frozen record
  Task 7 compares against; whether/when those generated artifacts get
  committed is left to a later task's book-freshness sweep.
