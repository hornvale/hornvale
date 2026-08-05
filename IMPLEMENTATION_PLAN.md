# HANDOFF — The Tilth / The Fallow, next session

Paste-able context for a fresh session. Written 2026-08-05 at the end of a long
session; everything below is verified against the repo, not remembered.

## Situation in one paragraph

Two campaigns are in flight on **one branch**, `campaign/the-tilth`, in worktree
`.claude/worktrees/the-tilth`. It is **9 commits ahead of main** (main is at
`148a77a4`). **The Keeping already merged and pushed.** The gate on this branch is
**RED on exactly one test, deliberately** — read the next section before touching
it. The immediate next task is the **bake rewire**, which both campaigns now need
and which was deliberately deferred because it is determinism-sensitive.

## ⚠ The deliberate red — do NOT "fix" this

```
docs_consistency::the_history_page_prose_names_the_cell_it_renders   FAILS
```

Its message: *"the history showcase page renders an EMPTY column — it is the
showcase for stratigraphy and is telling readers the feature did nothing. Repoint
`history_site` … and rewrite the framing paragraph."*

**The test is right and the feature did do nothing.** Adopting Lieth removed the
hard zero below 2 °C, which is what had been evicting communities from marginal
ground — so deep history flattened from **16 stacked steadings to 1**. Verified with
`hornvale history --site 1400` on before/after worlds.

**Repointing `history_site` to make this green would be the prose form of retuning a
constant to rescue a prediction.** It stays red until The Fallow restores the churn
from causes the world can explain. If you need the gate green for an unrelated
reason, say so explicitly rather than quietly rewording the page.

## What is actually in the code

**Merged to main (The Keeping):** decisions 0098–0103. `CarryingInput.habitable` →
`is_land` (the flag conflated land AND temperate AND moist; only land admits no
gradient). `SuitabilityMap`/`CapacityMap` newtypes in `kernel/src/ecology.rs` with
`modulated_by` as the only legal product.

**On this branch (The Tilth), all committed:**
- `carrying_capacity` now uses **Lieth's Miami model** — `npp_temperature` (monotone,
  saturating, never zero) and `npp_precipitation` on real mm/yr from
  `climate.precip_at`. The old symmetric tent is gone.
- The **aridity double-count** is gone from `hostility` (moisture was counted twice —
  ~20× penalty on semi-arid ground).
- `per_species_suitability` combines tolerances by **Liebig minimum**, not product,
  via the shared `tolerance_liebig` helper.
- **`per_species_capacity` exists and is UNUSED.** It returns `Vec<(u32, CapacityMap)>`
  with a dimensional Michaelis–Menten ceiling. **Wiring it into the bake is the job.**

**The Fallow: spec + feasibility only. No production code.**

## THE TASK: the bake rewire

Two things must be threaded through `windows/worldgen/src/history_bake.rs`:

1. **Per-species capacity** — replace `Bake.capacity: &CellMap<f64>` + binary
   `factor()` with `per_species_capacity`'s output, so `eff_capacity` is per people.
   `factor()` becomes **ice-only** (a glacier is not a niche disagreement).
2. **The Fallow's `tilth` stock** — see its spec §3.1. It must be **re-derivable from
   seed + era series**, computed inside the bake's existing epoch loop. Committing it
   would be a save-format epoch and is the failure mode, not the design.

Call sites to expect: `factor`, `eff_capacity`, `vacant_habitable` (becomes
`vacant_for(era, cell, people)`), `pressure_of`, `best_home`, `nearest_dest`.

**Follow the existing idiom:** `takes_the_initiative`, `concealment_of` and
`horizon_of` already take a `KindId` and look something up. Do that, but resolve to
a dense index once — a `BTreeMap` lookup per cell per species in `best_home`'s ring
scan would be a hot-path regression (`kernel/CLAUDE.md`: dense index → `Vec`).

**Index alignment matters:** the bake holds `peoples: Vec<KindId>`, and
`per_species_capacity` tags by position in the `species_biosphere` slice it was
given. Those must be built from the same ordering, in the same order. `HistoryPlacement.tag`
already documents this "build-local dense index, never identity" convention.

### ⚠ Determinism hazards, specifically

`CLAUDE.md` is blunt that kernel/bake bugs are *catastrophic and silent*. In this
file the exposure is concentrated in:

- **`best_home`'s ring scan** — `nearest_ring` consumes a full ring before expanding,
  and ties break on `score.total_cmp` → weakest defender → **lowest `CellId`**.
  Changing evaluation order changes worlds.
- **`nearest_dest`** — refugial first, then river proximity, then lowest `CellId`.
- **Stream consumption order** — the gate's pin-isolation tests catch violations, but
  only if you run them. Adding a draw is epoch-triggering; prefer deriving.

Verify with byte-identity, not by reading: `cargo run --release -p hornvale -- new
--seed 42 --out /tmp/a.json`, change nothing observable, regenerate, `cmp`.

## Derived constants — already measured, do not casually re-pick

```rust
CAPACITY_V_MAX = 140.2    // = 68.87 / (0.8138 * 0.6035); target is the PRE-campaign
                          // good-ground level (a gauge choice, decision 0104)
CAPACITY_K_M   = 0.03004  // median axis_supply over land, n = 401,148
```

Both are in `windows/worldgen/src/lib.rs` and were re-derived **after** stage 1+4
moved the physics. **If physics moves again they are stale** — re-run
`tilth_probe.rs`, which prints the derivation longhand. Decision 0104 binds this:
Earth-contingent values may not be calibrated against Hornvale's own census.

## Preregistered hypotheses — thresholds are frozen (decision 0016)

**The Tilth H1** — all six settling species win non-zero best-fit territory on ≥3/5
seeds. **MEASURED: 5/6.** Liebig moved `goblin` 0→458 and `bugbear` 0→927; `gnoll`
stays at **0** for a *trophic* reason (its arid optimum has no photosynthate-derived
food, and it eats `ANIMAL_PREY 0.65 + PLANT_FORAGE 0.35`, both functions of
`base_carrying`). **Threshold not met, and not moved.**

**The Fallow H1** ≥4 layers · **H2** ≥20% of ruins anthropogenic · **H3** a cell
sustainable under herding and ruinous under farming. **H4's first null is
UNANSWERED** — my variance probe was under-powered (climate multiplier only reached
0.50 at amplitude 0.8, not the 0.2 implied). Drive variance from the real
paleoclimate era series before claiming anything about it.

## Instruments (all `#[ignore]`d; run with `-- --ignored --nocapture`)

| test | what it does | needs a world? |
|---|---|---|
| `keeping_probe.rs` | 5 real seeds: clusters, headroom, per-species best-fit | yes (~8 s) |
| `tilth_probe.rs` | derives `V_max`, `K_m`; prints the arithmetic | yes |
| `tilth_phase_diagram.rs` | **168 synthetic situations**, differential diagnosis | **no** (ms) |
| `fallow_feasibility.rs` | one-cell 80-epoch time series, column depth | **no** (ms) |

The synthetic two are the better instruments and cost nothing: a generated world
only samples the narrow joint distribution terrain and climate produce *together*.

## Findings that are settled — do not re-derive

- **The species-blind capacity CANCELS from `argmax`**, so the base field cannot
  influence *who* wins a cell. Only the per-species term can.
- **Buffered axes cannot discriminate.** `sovereignty_floor` keeps temperature,
  moisture and insolation at 0.31–0.50 however hostile the ground; **only elevation
  (floor `0.0`) can decide a cell.** Hence hobgoblin 112/168 and kobold 56/168 (all
  the 3500 m band). *This is a separate campaign — the floor, not the roster.*
- **The trophic chain is one-way**: `forage = base × 0.5`, `prey = forage × 0.1`,
  `DETRITUS_AMBIENT = 0.2` flat on land, nothing returns.
- **24 of 30 species have no placement at all** — only kind-declaration facts
  (`species-*`). No cell, no population. A dragon lives nowhere.
- **`subsistence` and `occ-tech` are committed and consulted nowhere** in demography
  or the bake.
- **The Fallow's bifurcation**: `tilth_eq = regen/(regen+extraction)`; columns need
  degradation to *outrun* population adjustment (`GROWTH_RATE` 0.2/epoch). Cyclic
  side gives 14–17 layers against the historical 16.

## Process hazards hit this session (seven of the first kind)

1. **Reasoning from a parameter without checking its frame.** Seven instances. Cures:
   *evaluate the function and print the table* — it took under a minute every time
   and overturned something every time. See memory
   `evaluate-the-curve-not-the-constant`.
2. **`git diff` cannot detect a stale generated artifact.** It only goes stale
   relative to a *regeneration*. Use `make rebaseline` **then** diff. Caught twice by
   the pre-commit hook, never by me.
3. **A `cd` outside the worktree resets cwd to the main checkout** — one stray `sed`
   modified main, and a byte-identity check compared main against itself. Prefix
   commands with an explicit `cd` to the worktree.
4. **The pre-commit hook only runs `make quick`** (fmt/clippy/type-audit). It cannot
   see a stale golden, so `git add -A` swept stage 1+4's code into an unrelated commit
   and left the branch red.
5. **Never put `cargo test` in a compound shell command** — a guard hook blocks the
   *entire* invocation, so earlier `python3`/`sed` steps silently never run.

## Open decisions for Nathan

- **The census** — owed since The Keeping's step B, deliberately deferred, belongs on
  **lefford**, and is an autopilot **carve-out** needing explicit authorization.
- **The bifurcation** — widen the boundary, or derive the constants from it? Shipping
  `extraction` picked just past the cliff recreates the fragility this arc exists to fix.
- **The sovereignty-floor campaign** — its own spec; it flattens *who wins* where The
  Fallow flattens *whether anyone stays*.
- **`Biome` as a derived query** (BIO-6) — a cross-repo `scene/tiles/v1` contract, so
  decide before speccing the biosphere arc.
- **Gnoll: omnivore → scavenger.** Authored as `ANIMAL_PREY 0.65 + PLANT_FORAGE 0.35`
  ("a pack hunter that also forages"), but gnolls are hyena-folk and hyenas scavenge.
  `DETRITUS` is documented "available to decomposers and scavengers" and is flat on
  land. Measured: arid capacity **0.0 → 98.1**. Lore fix and mechanical fix coincide.
- **The biosphere arc** — `BIO-6`/`BIO-7`/`BIO-24`/`BIO-detritus-derived`: thousands of
  derived flora/fauna, detritus as their byproduct, closing the loop.
