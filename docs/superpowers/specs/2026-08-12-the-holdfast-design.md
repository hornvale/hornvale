# The Holdfast — design

**Status**: at G3, awaiting spec review.
**Branch**: `campaign/the-holdfast`, from `main` at `ab26668f`.

A holdfast clamps work to the bench so it stops moving. This campaign
stops the world-generation hot path from recomputing values that do not
change.

## 1. The finding

Whole-suite profile, lefford (40 cores, load 0.59, uncontended), `ab26668f`,
`perf record -F 199` over `cargo nextest run --workspace`, 2,040,781 samples:

| share | symbol |
|---|---|
| 14.46% | `libm::math::exp::exp` |
| 9.55% | `core::…FnMut::call_mut` |
| 9.31% | `hornvale_kernel::noise::value_noise_2d` |
| 6.00% | `hornvale_kernel::ecology::ConditionResponse::eval` |

By crate: `libm` 25.77%, `hornvale_kernel` 25.04%, `core` 15.66%.

**`libm` is not the defect and is not the target.** Decision 0041 routed
every transcendental through pure-Rust `libm` for cross-platform
bit-identity, and The Pyx (0090) verified the result. The cost is the
*call count*, which is ours.

Call-graph profile of one regressed test
(`hornvale-worldgen::history_gates$migration_fires_at_volume`, 5.5x
against its Jul-31 baseline), inclusive shares:

```
stage                                    70.40%
└─ build_to::{{closure}}                 62.56%
   └─ bake_history_from                  49.02%
      └─ bake_history_from::{{closure}}  49.02%   per-era loop
         └─ per_species_capacity_at      46.91%
            └─ CellMap::from_fn          45.84%   one globe pass
               └─ …::{{closure}}         45.84%
                  └─ tolerance_liebig    11.78%
                     └─ eval → exp       20.64%
```

`hornvale_kernel::math::exp` and `ConditionResponse::eval` are both
exactly 20.64% — every `exp` on this path comes from `eval`. 70.85% of
all stacks pass through `CellMap::from_fn`; allocation is only 3.14%, so
this is compute and dispatch, not allocator pressure.

`tolerance_liebig` (`windows/worldgen/src/lib.rs:1150`):

```rust
cn.temperature.eval(s.temperature_c, floor_buf)
    .min(cn.moisture.eval(s.moisture, floor_buf))
    .min(cn.insolation.eval(s.insolation, floor_buf))
    .min(cn.elevation.eval(s.height_asl_m.get(), 0.0))
```

`eval` is a Gaussian — `exp(-0.5 * z * z)` — one `exp` per call. Four
calls per (cell x species x era), all evaluated eagerly to feed a `min`.
`CLIMATE_ERAS = 25`; a level-6 globe is 40,962 cells.

## 2. Which axes are era-invariant (verified, not inferred)

`EraAdjust` (`:2284`) carries exactly two fields. `substrate_field_at`
(`:2311`) applies them to exactly two `Substrate` members:

| axis | expression | era? |
|---|---|---|
| temperature | `mean_temperature_at(cell) + adjust.temp_offset` | **VARYING** |
| moisture | `climate.moisture_at(cell)` | **INVARIANT** |
| insolation | `*insolation.get(cell)` (already hoisted) | **INVARIANT** |
| elevation | `elevation_at(cell).above(adjust.sea_level)` | **VARYING** |

`floor_buf = sovereignty_floor(bio.mass, bio.potency)` (`:1326`, `:1612`)
is per-species and computed outside the cell loop — invariant.

**So the moisture and insolation responses depend only on
`(species, cell)` and are bit-identical across all 25 eras.**

> **Correction of record.** An earlier framing of this campaign claimed
> insolation + *elevation*. That was read off a doc comment describing
> `EraContext.elevation` — a *relief* field — and wrongly transferred to
> `s.height_asl_m`, height above *sea level*, reached through a per-era
> `Substrate`. Hoisting elevation would have frozen sea level across eras
> and silently changed world generation; `:2311`'s own comment names the
> consequence: "a glacial low-stand exposes shelf as land with no mask."
> The count (2 of 4) survived the correction; the attribution did not.

## 3. Changes

### B — short-circuit the Liebig minimum (first; premise-independent)

Three axes are floored at `floor_buf` and cannot return below it;
elevation is floored at `0.0` and can. Evaluate elevation first: if it is
`<= floor_buf` it is already the minimum and the other three `exp`s are
provably dead.

Independent of §2 entirely — it holds whatever the invariance analysis
concluded, which is why it goes first.

**Identity risk**: `f64::min` returns the non-NaN operand when one side
is NaN, so a short-circuit must not change NaN propagation. The
implementer establishes whether any axis can produce NaN before relying
on the ordering, and preserves `min`'s exact semantics.

### A — hoist the era-invariant response curves

Precompute the moisture and insolation responses per `(species, cell)`
once per world; index them inside the era loop.

Byte-identical by construction: identical inputs to an identical `libm`
call yield identical bits. No arithmetic is reassociated and no operation
order changes — the call simply happens earlier and once.

Memory: `2 x cells x species x 8` bytes; at level 6 that is ~655 KB per
species. Bounded and acceptable.

Applies to **both** `tolerance_liebig` and its shadow-mode two-tier
successor (`:1189`-`:1199`), which carries the same four axes.

### D — split `per_species_capacity_at`'s field construction

46.9% inclusive, and 45.84% of that is a single `CellMap::from_fn`. It
builds six globe-wide fields per call. Separate era-varying from
era-invariant construction, the move `EraInvariantSupply` already made
one level up.

Largest prize, broadest blast radius, and the change most likely to
collide with The Glasshouse. **Scoped in this spec but gated: if B and A
land clean and the measured delta meets §4, D may be deferred to its own
campaign rather than widened here.**

## 4. Preregistered hypothesis and success criteria

Frozen before any implementation, per decision 0016. Nothing mechanical
compares a result to this; it is honoured by reading it.

**H1 (call count).** B and A together eliminate >= 40% of `exp` calls on
the `tolerance_liebig` path. Arithmetic: A removes 24 of every 25 calls
on 2 of 4 axes = 48% of that path's `exp`, before B contributes.

**H2 (whole-suite).** Whole-suite `exp` share falls from its measured
14.46% to <= 10.0%, measured the same way on the same host.

**H3 (wall clock).** `hornvale-worldgen` summed exec time on lefford
falls by >= 15% from its measured 3203.1 s.

**H4 (identity, binding).** Byte-identity is preserved exactly. This is
not a target but a gate: any drift fails the campaign regardless of H1-H3.

**Falsification is a result.** If A's measured saving is materially below
48% of the path, the likely cause is that `bake_history_from` calls
`per_species_capacity_at` fewer times than 25 per world, and the era loop
is not the multiplier assumed here. That finding is reportable as the
headline; it must not be rescued by retuning the criteria after
unblinding.

**Baselines** (lefford, `ab26668f`, uncontended, load 0.59):

- whole suite summed exec 9964.5 s; `hornvale-worldgen` 3203.1 s
- `make ci` wall 349.985 s, user 11260.504 s, cpu_ratio 33.67
- `migration_fires_at_volume` 32.435 s
- whole-suite `exp` 14.46%; `eval` 6.00%

## 5. Determinism protocol

Byte-identity **by construction, not by test** — the standing rule for
perf work in this repo. Verification, all required:

1. seed-42 world hash unchanged
2. >= 40-seed world-hash sweep, baseline vs changed, shas diffed
3. `lens_purity` and `graph_byte_identity` green
4. `make rebaseline`, then `git diff --exit-code` over every generated
   directory — including `docs/audits/` (the type-audit report drifts on
   any pub-boundary change) and `docs/digest/`
5. `make gate` green on the Mac
6. the 1000-seed census is the real gate — **census regen is an autopilot
   carve-out and is NOT authorised by this spec.** It is requested
   separately at G6 if the change reaches merge.

## 6. Interactions and risks

**The Glasshouse holds a `hold-off`** on `domains/astronomy/`,
`domains/climate/`, `domains/terrain/` — an epoch re-centring the
temperature baseline (new astronomy seed label, stream-order slot,
`temperature.rs` latitude profile, terrain hypsometry). The Holdfast
touches `windows/worldgen/` and `kernel/src/ecology.rs`, so no path
collision. But The Glasshouse deliberately *moves the values* flowing
into `tolerance_liebig`, while The Holdfast must *preserve* them. Both
can hold — they are orthogonal — but whichever lands second re-baselines
the other's identity evidence. **Merge order matters and is a G3
question for Nathan.**

**Shadow-mode successor.** The Tense's two-tier `gate x modifier`
tolerance sits directly below `tolerance_liebig`, "not yet binding". The
hoist is orthogonal to the combining rule, and applying it to both keeps
that successor cheap to land.

**Selection effect, argued against this campaign.** `EraInvariantSupply`
and `EraContext` exist because a prior campaign already ran this play.
The easy hoists are done, so what remains un-hoisted may remain so
because it *could not* be — and §2 found exactly that for elevation. The
two axes claimed here were checked individually rather than assumed.

## 7. Out of scope

- Any change to `libm` or to how transcendentals are computed (0041).
- Reducing `CLIMATE_ERAS` — a fidelity cut, an autopilot carve-out.
- Fusing the ~370 `CellMap::from_fn` sites (followup F2; float multiply
  is not associative, so fusion is subtler than it looks).
- Fixturing the suite's genesis-paying tests (followup F1) — attacks
  suite cost, not simulation cost.
- Census regeneration.

## 8. Readout — supersedes §3's plan half

**Shipped: B only.** A and D are folded into one future campaign
(followup F5). §3 stands as written; this section records what happened
against it.

### Measured (lefford, interleaved A/B/A/B/A/B, both arms same contention)

| measure | `ab26668f` | `46b18c8e` | delta |
|---|---|---|---|
| `migration_fires_at_volume` wall | 39.36 s | 31.58 s | **−19.8%** |
| `math::exp` inclusive | 22.66% | 12.86% | — |
| `ConditionResponse::eval` inclusive | 30.54% | 15.11% | — |
| `tolerance_liebig` inclusive | 31.20% | 18.34% | — |
| `per_species_capacity_at` inclusive | 50.52% | 41.38% | — |

In absolute time (share × own-run wall): `exp` 8.92 s → 4.06 s
(**−54.5%**), `eval` 12.02 s → 4.77 s (−60.3%).

### Hypotheses

- **H1 (≥40% of path `exp`): PASSED**, at ~54.5%. Two caveats stated
  rather than buried: H1 was preregistered in *calls* and was measured in
  *time* — a fair proxy for a fixed-cost function, not the same quantity;
  and the two perf profiles were recorded under different load (6817 vs
  9665 stacks), so each percentage is internally valid but comparing them
  assumes non-`exp` work scaled evenly. The interleaved wall-clock figure
  is the independent measurement and agrees.
- **H2 (whole-suite `exp` ≤ 10.0%): NOT MEASURED.** Requires a
  whole-suite profile; lefford was carrying another session's job at
  3925% CPU and it was not worth displacing for this.
- **H3 (worldgen −15%): NOT MEASURED at crate scale.** The one test
  measured moved −19.8%, which is *not* the crate-level claim H3 made.
  Recorded as unmeasured rather than inferred from one test.
- **H4 (identity, binding): PASSED.** seed-42 sha unchanged
  (`ab2fec35…`, 13533 facts, village Googo); 40/40-seed sweep
  byte-identical; `make rebaseline` drift-clean across every generated
  directory; 856/856 scoped worldgen+kernel tests.

### The falsification arrived, in a shape §4 did not predict

§4 preregistered that a low A saving would mean the era loop was not the
multiplier assumed. A's saving *is* low — ~3% against the ~48% forecast —
but the stated cause is wrong. The multiplier was right; **B removed most
of the multiplicand before A could claim it.** B's shortcut fires ~73% of
invocations, solved from the measured `exp` shift via
`(4 − 3p)/4 = 0.455`, and on those it already skips the two axes A would
hoist. A reaches only the remaining 27%.

Recorded, not retuned. The lesson worth carrying: **two optimisations on
the same path do not compose additively, and A's value had to be
recomputed as a marginal figure after B rather than reused as the
standalone estimate.** The ordering decision in ledger #3 — B first
because it was premise-independent — turned out to also be the ordering
that revealed this, which was luck rather than foresight.
