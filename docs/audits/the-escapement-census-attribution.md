# The Escapement — census attribution, the three moved cells

**Status:** committed finding. **Campaign:** The Escapement. This is the
forensic investigation decision
[0190](../decisions/0190-a-reachability-trace-is-not-closed-by-finding-one-funnel.md)
summarizes, and the evidence behind the spec's §1 correction and both
calibration re-pins. It was written to the campaign's git-ignored scratch and
promoted here unchanged (bar this header, and the appendix in §9) before the
worktree was recycled: a durable record may not cite a path that dies with a
worktree. §9 is a **second, separate investigation** of the same flip, rescued
from the same scratch on the campaign's last commit.

**Investigated**: 2026-08-24, worktree `.claude/worktrees/the-escapement`, branch
`campaign/the-escapement` (HEAD `6da5de512`).
**Method**: build worlds and run the two metrics on this Mac at a ladder of
commits; ablate the suspect hunks; reconstruct the census hash arithmetically.

---

## Verdict

**The representation flip (`9ad5911a3`) did not move a single census cell.**

All three moved cells are caused by **`3bc4fd871`** —
*"feat(astronomy)!: port the tick->StdDays funnel and fix local_day for negative
time"*, Phase A Task 5. That commit is an **ancestor of `5cba6dada`**: the
premise that `5cba6dada` is "the last commit before the representation flip" is
wrong. Phase A (`fa5885bfd`, `3bc4fd871`, `5b6b5f5d3`, `26e2af764`) landed
*before* `5cba6dada`; `9ad5911a3` is Phase B.

**No seed-derivation label and no stream consumption order is involved.** This
is a **derived value** landing differently — spec §2.1 clause 2. §4's
load-bearing claim survives, but its *scope* needs restating: the campaign
changed generation at `3bc4fd871`, not at the flip, and it changed it by fixing
a bug rather than by re-encoding time.

---

## 1. A settlement name did change — ground truth

`hornvale new --seed <s>` at `545b53f46` (parent of the culprit) vs `3bc4fd871`,
diffing `is-settlement` subjects' `name` / `name-gloss`:

```
seed 267 (354 settlements, same subject set)
  10760661430244475913  'Zaṅo'            gloss 'sun'                       ->  'Nraṅra'           gloss 'ford'
  10760661430244475980  'Bozozogbzo'      gloss 'coast'                     ->  'Zagzo'            gloss 'river'
  10760661430244476266  'Bozozogbzozozo'  gloss 'coast-temperate-forest'    ->  'Bozozokbzozozo'   (gloss unchanged)
  10760661430244476583  'Zagbzabozozogbzo' gloss 'day-coast'                ->  'Zagbzabozozokbzo' (gloss unchanged)
seed 831 (274 settlements, same subject set)
  10760661430244474881  'Zonggong'        gloss 'tropical-seasonal-forest'  ->  'Zazszang'         gloss 'closed-canopy'
```

`5cba6dada` (pre-"flip") vs `HEAD`: **27,166 / 13,232 facts, zero non-`day`
differences.** Every difference is the `day` field's re-encoding (f64 days ->
i64 ticks). The flip is inert for these worlds.

## 2. Reproduction ladder (this Mac is a faithful oracle)

The two metrics, seeds 267/831, run as a two-metric study:

| commit | 267 name-transparency | 267 name-collision-rate | 831 name-collision-rate |
|---|---|---|---|
| `635d116d7` (last census refresh, The Burr) | 0.89830508 | 0.49742268 | 0.43521595 |
| `545b53f46` (culprit's parent) | 0.89830508 | 0.49742268 | 0.43521595 |
| `3bc4fd871` (culprit) | **0.90677966** | **0.49226804** | **0.42857143** |
| `5cba6dada` (pre-Phase-B) | 0.90677966 | 0.49226804 | 0.42857143 |
| `HEAD` (`6da5de512`) | 0.90677966 | 0.49226804 | 0.42857143 |

Row 1 reproduces the **committed** golden exactly and row 5 reproduces the
**refresh** exactly, on aarch64/Darwin against lefford-authored values — so the
cross-host confounder is excluded for these three cells.

`git bisect` over the 701 commits `635d116d7..5cba6dada` (10 steps, each a build
+ study run) returned `3bc4fd871` as the first bad commit.

## 3. Mechanism — established, not hypothesised

`3bc4fd871` has exactly two production hunks. One is inert, one is the cause.

- `provider.rs`: `StdDays(time.day().max(0.0))` -> `StdDays(time.as_std_days().max(0.0))`.
  **A no-op under Phase A**: `WorldTime::day()` is literally `self.as_std_days()`,
  and `as_std_days()` is `self.day`.
- `kernel/src/field.rs`: **test-only** in this commit.
- `calendar.rs` `Calendar::local_day`, the day-**fraction** formula:

```rust
// before
let fraction = (local.fract() + self.forcing.day_phase_offset).fract();
Some((local as u64, fraction))
// after
let index = local.floor();
let fraction = (local - index + self.forcing.day_phase_offset).rem_euclid(1.0);
Some((index as i64, fraction))
```

**Two ablations at `3bc4fd871`, each rebuilt and re-diffed:**

| variant | seed 267 name/gloss diffs vs `545b53f46` | seed 831 |
|---|---|---|
| as committed | 4 | 1 |
| old fraction + old `as u64` index | **0** | **0** |
| **new floor index + old fraction** | **0** | **0** |

So the **index** change is inert (every production caller discards it, as the
commit message claimed) and the **fraction** change is the entire cause.

### The negative path is LIVE, contrary to the spec's trace

Instrumenting `local_day` to compare both formulas during one seed-267 build:

```
2,776,344 probe lines; 1,293,003 fraction divergences; 1,483,341 index divergences
every single fraction divergence has local < 0; ZERO at local >= 0
example: t=-236.69856716699405  local=-262.29  old=-0.1594399910535491  new=0.8405600089464509
```

Backtrace of the first divergence:

```
local_day (calendar.rs:625)
  <- at_local_fraction (heliacal.rs:81)
  <- heliacal_events
  <- GeneratedSky::phenomena (provider.rs:1938)
  <- kernel::phenomena::observe (phenomena.rs:261)
  <- worldgen::observe_with_sources (lib.rs:4871) <- lib.rs:7831
  <- build_to (lib.rs:7513) <- build_world <- cmd_new
```

Spec §1 and decision 0187 traced the negative path as unreachable because
`GeneratedSky::t` clamps with `max(0.0)`. **`heliacal.rs` and `night_sky.rs`
call `Calendar::local_day` directly, below that funnel.** The spec names those
two files as the funnel-bypassing sites but records them as passing a hardcoded
`StdDays::new(0.0)` — that is true of worldgen's `night_sky_lines`, and false of
the `phenomena()` path, which passes real times.

The negative time is not exotic. `heliacal_events` computes

```rust
let year_start = t.0 - calendar.year_phase(t) * year;
```

and scans SAMPLES points from there. At genesis (`t = 0`), `year_phase(0)` is the
drawn `year_phase_offset` in `[0,1)`, so `year_start` is negative for essentially
every world, and the early samples are pre-genesis. The old code returned a
**negative** day fraction there, which `at_local_fraction` then used as
`day_start = t_sample - fraction * day_length` — placing the local day's start
*after* `t_sample`. That is exactly the class of defect the campaign set out to
fix, so **the new values are the correct ones**; the census move is the fix's
fingerprint, not a regression.

### How a day fraction reaches a settlement's name

Probing `presiding` per settlement at `3bc4fd871`, new fraction vs old, seed 267
— **2 of 354 settlements differ**:

```
cell 15861  old: kind="celestial-body"   presiding=Some("sun")   n_seen=9
            new: kind="heliacal-setting" presiding=None          n_seen=10
cell  1045  old: kind="heliacal-setting" presiding=None          n_seen=11
            new: kind="seasonal-cycle"   presiding=Some("day")   n_seen=10
```

Chain: fraction -> `at_local_fraction`'s recovered local-day start -> which
heliacal risings/settings the scan finds -> the observed phenomena list and its
length -> `seen.first()` -> `presiding` -> `settlement_site_concepts` -> the
drawn gloss and name (`windows/worldgen/src/lib.rs:7831`). The two settlements
whose *gloss* is unchanged but whose surface moved by one voicing (`g`/`k`) are
downstream of the same phenomena set through the per-species lexicon
(`exposure_from_in`), not through the concept list.

## 4. Boundary effect or systematic? — both, at different layers

- **Input side: systematic and large.** 1.29 M divergent `local_day` calls in a
  single seed-267 build; the divergence is up to a full 1.0 in fraction, not a
  ULP. Every world with a non-zero `year_phase_offset` takes the path.
- **Output side: a discrete rank flip, hence rare.** The value only becomes
  observable when it changes `seen.first()` — the top-ranked phenomenon. That
  happened for 2 of 354 settlements in seed 267 and 1 of 274 in seed 831.
- **Across seeds: rare.** Seeds 0–19 built under both binaries: **0 name/gloss
  differences in all 20.** Consistent with 3 moved cells in 227,000.

So "2 seeds in 1000" is not a rounding boundary in the arithmetic — it is a
salience-ranking tie being broken differently by a systematically corrected
input.

## 5. Label / stream-order: NOT involved

| check | result |
|---|---|
| files touched by `3bc4fd871` | `calendar.rs`, `provider.rs`, `field.rs` (tests), `docs/` only |
| any `stream`/label constant added or changed | none |
| `World.derived_under` (the stream-label stamp) | **identical**, both seeds |
| `World.registry` | **identical**, both seeds |
| fact count | **identical** (27,166 / 13,232) |
| `(subject, predicate, provenance)` sequence, index by index | **identical** |

Only four `object.Text` values differ, at the same indices. Nothing was inserted,
dropped or reordered. Naming draws are salted per settlement
(`salt = u64::from(s.cell.0)`), so a changed concept list in one settlement
cannot shift another's draws — which is why the ledger stays index-aligned.

**Conclusion: a derived VALUE, not a seed label and not a stream order.**

## 6. The `fnv1a64` field — arithmetically closed

`windows/lab/src/schema.rs:95` computes `fnv1a64` over **`csv.as_bytes()` — the
entire `rows.csv` body** (it sits under the manifest's `"rows"` key, beside
`"count"`). It is not a hash of the column list, which is why a byte-identical
227-column header does not pin it.

Verified rather than argued:

```
committed rows.csv                        -> 0x9d61df647f25151f   (== schema.json's old value)
same file, exactly the 3 cells rewritten  -> 0x666c13b545cae7e3   (== the refresh's new value)
header byte-identical: True
```

The reconstruction reproduces the refresh's hash **exactly**, which independently
proves the refresh moved **exactly those three cells and nothing else** in the
whole 227 x 1000 census.

## 7. What this means for the campaign

- §4's claim "no seed label, no draw, no stream consumption order moved" is
  **true**, and now has direct evidence on the naming path, not just the
  astronomy/terrain pin-isolation tests.
- §4's framing "representation changed, generation did not" is **too strong as
  written**. Generation did change — at `3bc4fd871`, by fixing a live
  negative-time defect, not by re-encoding an instant. The flip itself
  (`9ad5911a3`) moved zero census cells and zero non-`day` ledger bytes.
- **Decision 0187 and spec §1 contain a factual error worth correcting before
  close**: both state that no caller can reach `local_day` with a negative
  value. `heliacal::at_local_fraction` does, on essentially every world, at
  genesis. `3bc4fd871`'s commit message ("Latent, not live", "All six production
  callers ... are unaffected", "no golden moved") is wrong on all three counts —
  the goldens it did not move were the seed-42 ones; the census was not run.

## 8. Reproduction notes

- Probe study JSONs: `{"seeds":{"from":267,"count":1}}`, metrics
  `["name-transparency","name-collision-rate"]`. Run from a scratch cwd so
  `lab-out/` and `book/src/laboratory/generated/` land outside the repo.
- All builds `cargo build --release -p hornvale` in a throwaway detached
  worktree; removed afterwards. The campaign worktree was never modified.
- No census was run and `HV_CENSUS` was never set.

---

# Appendix — §9. The walk-band `189 → 190` A/B (a separate investigation)

**Scope note.** Sections 1–8 attribute three moved *census* cells and conclude
the representation flip (`9ad5911a3`) moved none of them. This appendix is a
different question about the same flip: why the seed-42 possession walk's
`wait 90` reported **190** committed facts where it had reported **189**. It is
recorded here because it is the campaign's other piece of forensic evidence and
the chronicle keeps only its story. The two investigations share a subject and
nothing else.

**Why it is worth committing.** Rediscovery is expensive and getting more so:
both sides of a landed flip must be built (the pre-flip tree is now reachable
only by checkout of `5cba6dada`), the drive tick re-instrumented, and the
gallery walk re-run. It is also the campaign's cleanest instance of its own
through-line — **two confident causal guesses and one measurement, and the
measurement disagreed with both.**

## 9.1 Method

`Session::wait` was instrumented on both sides of the flip to dump every fact
the homeostatic drive tick commits. The pre-flip tree was exported with
`git archive 5cba6dada`, built, and the *same* transcript run on each — each
against a world built by its own binary, so nothing is cross-format:

```
scripts/possession-walk.txt, seed 42, `possess --world <wsky>`
  pre-flip  (5cba6dada) : 189 facts
  post-flip (9ad5911a3) : 190 facts
```

## 9.2 The per-predicate table, which killed both guesses at once

```
per-predicate     old   new
  agent-at         10    10
  drank            77    77
  eaten            21    21
  rested           81    82   <- the only one that moved
per-entity: 5 of 6 identical; 9630022852472602624 (a wild rust-monster) 7 -> 8
```

**`agent-at` being UNCHANGED is the single load-bearing datum.** Both standing
explanations were position-path mechanisms, and a position-path mechanism has
to show up here:

- *the window-boundary explanation* (the implementer's original claim) — a
  window-end shift admits or drops an action at the boundary; it did not;
- *the `d <= t` / `npc.home` fallback* (the reviewer's leading candidate, and
  the same class of bug The Hand had found independently) — `Ledger::commit`'s
  upward day-rounding making a fact invisible to its own filter, so the
  creature falls back to `npc.home`.

A probe that restored the deleted quantize block verbatim also returned 190.
**That probe proves nothing and the reason is recorded deliberately**: post-flip
a tick's `as_std_days()` is already an 8-significant-digit-clean decimal at
these magnitudes, so the restored block is a no-op — verified directly,
`quantize(0.01172) == 0.01172` and `quantize(90.39858) == 90.39858`. A
confirming result known to be vacuous was refused rather than counted.

## 9.3 The real cause: a fact-day feedback loop that compounds

A drive fact's day is now rounded onto the tick lattice **at emission** instead
of to 8 significant digits **at commit**. The homeostatic loop reads its own
committed fact days back (`last_drank` / `last_rested` / `last_ate`) to decide
when the next threshold is crossed, so that perturbation feeds itself:

```
  #  action   old day       new day       divergence
  1  rested   7.813555      7.81356              0.5 ticks
  2  rested   10.92545      10.92546             0.6
  3  drank    15.47436      15.47411            25.0
  4  rested   15.47596      15.47571            25.0
  5  drank    30.81292      30.78            3,292.4
  6  rested   30.81452      30.7816          3,292.4
  7  rested   38.82369      38.41732        40,636.6
  8  rested   —             45.95916        (NEW: fits inside the 90-day window)
```

Monotone, ~50–130× per action. By day 38 the accumulated divergence is
**0.41 days**, and a sixth `rested` fits where five did before.

## 9.4 The magnitude, and the two independent derivations

**The first divergence is half a tick, and that is 562× the mechanism
originally named** — matching the reviewer's independently-derived 560×. Half a
tick is 5.0e-6 days; the session clock's own f64→lattice shift is 8.9e-9 days,
i.e. 0.00089 ticks. Two people reasoning from different starting points reached
the same order for a divergence neither had correctly attributed.

## 9.5 What is and is not established

**Established**: the extra fact is one `rested` by one entity, produced by
compounding of the emission-lattice change through the drive loop, with the
divergence measured at every step.

**Not established**: a closed-form reason why *that* creature and not another.
It is the one whose fatigue threshold sat nearest a crossing, which is a
property of the seed rather than of the change.

**Open, and filed nowhere else** (final-review Minor 8): whether 0.41 days of
accumulated drift over 38 simulated days, near genesis, is acceptable. The
chronicle explains the mechanism and never asks. Spec §1 concedes this is the
region where the removed encoding was the finer of the two, which is precisely
where the question bites.
