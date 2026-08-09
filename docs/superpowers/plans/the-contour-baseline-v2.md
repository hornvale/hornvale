# The Contour — baseline v2 (epoch re-measurement)

**This file is a frozen matched-pair measurement, taken on the post-epoch
tree.** It supersedes `the-contour-baseline.md` as the comparator for the
readout, but that file is **not edited or deleted** — it is the record of
what was measured pre-epoch and it stays, per the same discipline
`docs/decisions/` uses for ratified decisions.

## Why this file exists

`the-contour-baseline.md` was measured at `c405a5e2`, before `BAKE` was
bumped from `history/bake` to `history/bake/v2`
(`domains/history/src/streams.rs`). `Seed::derive` is FNV-1a over the
label's bytes (`kernel/src/seed.rs:70`), so the label bump re-mints **every**
draw the bake takes, on top of whatever the position-aware conflict
mechanism itself changed. The old baseline is therefore void as a
comparator for the shipped tree: it differs from the shipped world in two
ways at once (mechanism absent vs present, AND pre-epoch vs post-epoch
derivation), and a difference against it cannot be attributed to either
cause alone.

This file is a **fresh matched pair on the post-epoch derivation only**: both
arms share the bumped label, so a difference between them is attributable to
the mechanism alone, cleanly.

## Commit / arm setup

```
Epoch-bump commit (both arms' common ancestor): 7070d187af28a4b6684cceb29fdd52e76c404f6
  feat(the-contour): declare the epoch — bump history/bake to /v2
```

- **Arm B (live)** — this commit, unmodified. `defensibility`
  (`windows/worldgen/src/history_bake.rs:134`) is the real function, wired
  into `maybe_raid` and `best_home` exactly as shipped.
- **Arm A (baseline, neutralised)** — the same commit with `defensibility`
  stubbed to unconditionally `return 1.0;` (the multiplier identity), added
  as a working-tree-only edit:

  ```rust
  fn defensibility(graph: &ConnectionGraph, from: CellId, to: CellId) -> f64 {
      // THE-CONTOUR EPOCH REMEASURE — ARM A NEUTRALIZATION, WORKING TREE ONLY.
      if true {
          return 1.0;
      }
      // ... original body, unreached, NOT deleted ...
  }
  ```

  This keeps the label bump, keeps the function's call sites wired exactly
  as shipped (so the *shape* of the raid/settle decision is identical — the
  term is present and multiplies, it just always multiplies by 1.0), and
  never touches `DEF_MIN`/`DEF_MAX`/`DEF_CENTER`/`DEF_SCALE`, which stay
  frozen per spec §4.4. This is a closer neutralisation than "delete the
  call site" would be: it isolates exactly the one thing the campaign
  changed (whether the multiplier carries information) rather than also
  changing control flow.

  The edit was never committed. It was captured with
  `git stash push -u -m "the-contour-arm-a-neutralized-and-m1-scratch"`
  (stash SHA `251f432ac3ddf69c4d2793064ad0ae2e21b48047`, captured via
  `git stash list --format='%H %gs'`) and then dropped by that same SHA once
  its measurements were recorded (`git stash drop stash@{0}`, which echoed
  back `Dropped stash@{0} (251f432ac3ddf69c4d2793064ad0ae2e21b48047)` —
  confirming no other session's stash entry was touched on the shared
  stack). `git status --porcelain` and `git diff --stat` were both empty
  immediately before the drop, i.e. the tree was verified byte-identical to
  the committed epoch-bump commit before the stash was discarded.

## The exact commands

```
cargo run -p hornvale -- lab run studies/the-contour.study.json          # 30 seeds, per arm
cargo run -p hornvale -- lab run <scratch copy, seeds 1..=100>           # 100 seeds, per arm
```

Run order: Arm B 30 → Arm B 100 → (stub applied) → Arm A 30 → Arm A 100 →
(stash + drop). The committed study `studies/the-contour.study.json` was
never edited; the 100-seed run used a scratch copy with
`"seeds": {"from": 1, "count": 100}`, same correction the original baseline
and readout both recorded (schema is `{from, count}`, not `{from, to}`).

M1 (`cascade_sizes` / `BakeCensus.cascade_hist`) is still not a registered
lab metric (confirmed again at this commit: `lab list-metrics | grep -i
cascade` returns only the same unrelated substring hit inside
`homophony-merger-share-goblin`'s doc text). A scratch test
(`windows/worldgen/tests/zzz_contour_m1_scratch.rs`, mirroring
`history_tithe.rs`'s own `history_for` + `cascade_sizes` instrument exactly)
was written, run against both arms, then deleted along with the stub via the
same stash-and-drop described above — never committed.

## Results — `peoples-alive-at-bake-end` (M3's registered metric)

30-seed matched pair, per seed:

| seed | A (neutral) | B (live) | seed | A (neutral) | B (live) |
|---|---|---|---|---|---|
| 1 | 5 | 5 | 16 | 5 | 5 |
| 2 | 5 | 5 | 17 | 5 | 5 |
| 3 | 5 | 5 | 18 | 0 | 0 |
| 4 | 5 | 5 | 19 | 4 | 4 |
| 5 | 5 | 5 | 20 | 0 | 0 |
| 6 | 0 | 0 | 21 | 5 | 5 |
| 7 | 5 | 5 | 22 | 0 | 0 |
| 8 | 5 | 5 | 23 | 5 | 5 |
| 9 | 0 | 0 | 24 | 5 | 5 |
| 10 | 5 | 5 | 25 | 5 | 5 |
| 11 | **5** | **4** | 26 | 5 | 5 |
| 12 | **4** | **5** | 27 | 5 | 5 |
| 13 | 5 | 5 | 28 | 5 | 5 |
| 14 | 5 | 5 | 29 | 0 | 0 |
| 15 | 5 | 5 | 30 | 5 | 5 |

Exactly two seeds differ (11 and 12), in opposite directions, and this
pattern is identical (same two seeds, same directions) at 100 seeds. Every
other seed — including the full extinction set — is byte-identical between
arms at both sample sizes.

| | n | mean | distribution | extinction seeds |
|---|---|---|---|---|
| A, 30 | 30 | 3.9333 | `5`×22, `4`×2, `0`×6 | 6, 9, 18, 20, 22, 29 |
| B, 30 | 30 | 3.9333 | `5`×22, `4`×2, `0`×6 | 6, 9, 18, 20, 22, 29 |
| A, 100 | 100 | 4.0600 | `5`×78, `4`×4, `0`×18 | +30 more, identical set to B |
| B, 100 | 100 | 4.0600 | `5`×78, `4`×4, `0`×18 | identical set to A |

The pooled mean is **exactly** unchanged, at both sample sizes, because
seeds 11 and 12 swap in opposite directions (11: 5→4, 12: 4→5) — the two
moves cancel exactly, not approximately. The extinction set (6/30 at 30
seeds, 18/100 at 100 seeds) is unchanged in both identity and size.

## Results — `largest-holding-share` (M2's registered metric)

| stat | A, 30 (n=24) | B, 30 (n=24) | A, 100 (n=82) | B, 100 (n=82) |
|---|---|---|---|---|
| min | 0.007206 | 0.007237 | 0.007206 | 0.007237 |
| q25 | 0.010663 | 0.010850 | 0.011730 | 0.012309 |
| median | 0.015160 | 0.015278 | 0.015875 | 0.016091 |
| q75 | 0.017344 | 0.019119 | 0.018580 | 0.021425 |
| q90 | 0.045141 | 0.042029 | 0.052726 | 0.049401 |
| max | 0.067538 | 0.067538 | 0.085185 | 0.106154 |
| mean | 0.019499 | 0.019824 | 0.020939 | 0.022035 |
| stdev | 0.015849 | 0.015267 | 0.017137 | 0.017773 |
| CV (stdev/mean) | 0.8128 | 0.7701 | 0.8184 | 0.8066 |
| max/median | 4.454 | 4.421 | 5.367 | 6.598 |
| q90/median | 2.978 | 2.751 | 3.322 | 3.070 |

`Absent` is exactly the 6 (30-seed) / 18 (100-seed) extinction seeds in both
arms, as expected (no live community ⇒ no share to compute).

## Results — `defensibility-capacity-rank-corr` (M4, supplementary, not a frozen prediction)

| stat | A, 30 | B, 30 | A, 100 | B, 100 |
|---|---|---|---|---|
| n | 30 | 30 | 100 | 100 |
| mean | 0.014141 | −0.020287 | 0.021085 | −0.030202 |
| stdev | 0.019950 | 0.137276 | 0.024555 | 0.143921 |
| SE | 0.003642 | 0.025063 | 0.002456 | 0.014392 |
| 95% CI | [0.0070, 0.0213] | [−0.0694, 0.0288] | [0.0163, 0.0259] | [−0.0584, −0.0020] |

**Arm A's M4 reading is not a usable "pre-mechanism" comparator and is
reported only for completeness.** `weakest_point_defensibility` calls the
stubbed `defensibility` internally, so under the Arm A stub it collapses to
a near-constant field (1.0 everywhere there is a traversable approach,
`DEF_MAX` = 1.25 only for the rare wholly-unreachable cell) — Arm A's tiny
stdev (0.02–0.025, vs. Arm B's 0.14) is exactly that collapse, not a
genuine narrow distribution of real terrain. This is expected: the stub
neutralises the *raid/settle decision*, which is what M3/M2 need, but M4
directly measures the *shape of defensibility itself*, which the stub
flattens by construction. **Arm B (live, post-epoch)** is the meaningful
reading here, and it lands at mean −0.0203 (30 seeds) / −0.0302 (100 seeds),
both statistically indistinguishable from zero at 30 seeds and both close to
the pre-epoch, pre-mechanism baseline (`the-contour-baseline.md`: mean
−0.0200, 95% CI [−0.069, +0.029]) and the pre-epoch live re-measurement in
the current readout (mean −0.0200, CI [−0.0692, +0.0293]). M4's null is
robust across the epoch bump.

## Results — M1, hand-obtained (`cascade_sizes`, not a registered lab metric)

| sample | hist (bin0, bin1, 0×10) | cascades | raided | S (secondaries) | P (conquests) | **σ** |
|---|---|---|---|---|---|---|
| A (neutral), 1..=30 | `[663, 33]` | 696 | 7338 | 729–762 | 6576–6609 | **0.1103–0.1159** |
| B (live), 1..=30 | `[808, 60]` | 868 | 7808 | 928–988 | 6820–6880 | **0.1349–0.1449** |
| A (neutral), 1..=100 | `[1854, 102]` | 1956 | 21758 | 2058–2160 | 19598–19700 | **0.1045–0.1102** |
| B (live), 1..=100 | `[2531, 188]` | 2719 | 24551 | 2907–3095 | 21456–21644 | **0.1343–0.1443** |

σ computed as [`S_lo/P_hi`, `S_hi/P_lo`], the same interval form
`history_tithe.rs`'s own instrument uses. The mechanism raises σ by roughly
**24–27%** at 30 seeds and **28–31%** at 100 seeds — directionally
consistent with, and somewhat larger than, the ~15–27% rise the pre-epoch
readout reported (that comparison used a different, pre-epoch derivation and
is not a like-for-like number, only a directional cross-check). `raided`
rises 6.4% (30 seeds) and 12.8% (100 seeds). Shape is unchanged: both arms,
both sample sizes occupy exactly two bins (size 1, size 2–3), zero cascades
of size ≥4, out of totals from 696 to 2719 pooled cascades — the hard cutoff
persists.

## Verification: tree state after the arm swap

```
$ git status --porcelain
(empty)
$ git diff --stat
(empty)
```

Confirmed byte-identical to commit `7070d187` before the stash was dropped.
`defensibility` in the working tree is the real, shipped function — no `if
true { return 1.0; }` remains anywhere in the tree.
