# The Keeping — Step A (naming + types), per decision 0103

## Scoping correction, stated up front

0103 says step A "costs no census rebaseline and no world-identity change." That
is true of the **typing** work and **false** of the metric rename: measured,
`habitable-fraction` appears 33 times across `.csv` / `.sql` / `.md`, so it is a
census column and renaming it changes committed goldens. A therefore splits:

| step | what | rebaseline? |
|---|---|---|
| **A1** | the newtypes + the transposed names + the saturation comment | **no** |
| A2 | F9: `is_habitable` → `is_vale_like`, metric included | **yes** (columns) |
| A3 | type-audit reaches container payloads (0103 §2) | tool change, own campaign |

**This plan is A1 only.** A2 and A3 are recorded as followups, correctly costed.

## Design choice: field newtypes, not element newtypes

0103 requires that `capacity := suitability` not compile. Two ways:

- **element**: `CellMap<Suitability>` / `CellMap<Headcount>` — maximum safety,
  but every arithmetic site in ~20 files changes.
- **field** (chosen): `SuitabilityField(CellMap<f64>)` /
  `CapacityField(CellMap<f64>)` — the *container* is typed; elements stay `f64`,
  so arithmetic inside a field is untouched and only construction and read sites
  change.

The field variant is chosen because **the bug was a field-level substitution**,
not a float-level one: §3.2 proposed handing one whole field where the other was
expected. Typing the container catches exactly that, at a fraction of the churn.
Elements can be tightened later if a float-level confusion ever appears.

## Stages

### Stage 1 — the newtypes in the kernel
**Goal**: `SuitabilityField` and `CapacityField` in `kernel/`, since demography
and worldgen both need them and a domain may not import a window (decision 0044:
shared code goes *down* into the kernel).
**Success**: validating constructors (`SuitabilityField::new` rejects any element
outside `[0,1]`; `CapacityField::new` rejects negatives/non-finite); `at(cell)`
readers; `#![warn(missing_docs)]` satisfied; type-audit tags present.
**Tests**: construction accepts a valid field; rejects out-of-range; `at` reads
back; the two types do not interconvert.
**Status**: Complete

### Stage 2 — `carrying_capacity` returns `CapacityField`
**Goal**: demography's field carries its units in its type.
**Success**: `hornvale_demography::carrying_capacity -> CapacityField`; all 15
call sites compile; no numeric change.
**Tests**: existing demography tests pass unchanged.
**Status**: Complete

### Stage 3 — `niche_per_species_k` is renamed and returns `SuitabilityField`
**Goal**: the function named for carrying capacity stops returning a suitability.
Rename to `per_species_suitability`; return `Vec<(u32, SuitabilityField)>`.
**Success**: all 14 referencing files compile; no numeric change.
**Tests**: `generalist_baseline`, `generalist_distinctness`, `confluence`,
`demesne`, `non_void_roster`, `occupancy_readout`, `branches_identity` green.
**Status**: Complete

### Stage 4 — the transposed name and the saturation comment
**Goal**: `bake_history_from`'s `let suitability = carrying_capacity(..)` becomes
`let productivity`, and `supply / (1.0 + supply)` carries a comment naming it as
the line that converts a magnitude into a ratio (0103 §4).
**Success**: no variable named `suitability` holds a capacity anywhere.
**Status**: Complete

### Stage 5 — verify no behavioural change
**Goal**: prove A1 is inert, rather than asserting it.
**Success**: seed 42's `world.json` **byte-identical** before/after (`cmp`);
`make gate` green; `git diff --exit-code` clean over
`book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`.
**Status**: Complete EXCEPT `make gate` — deferred on box contention (see
outcome below). Byte-identity and the artifact diff both verified.

---

## A1 outcome (2026-08-04)

All five stages complete. Verified rather than asserted:

```
seed 42 world.json          BYTE-IDENTICAL to the pre-change baseline (cmp)
cargo fmt --all             clean
cargo clippy --all-targets  0 findings
type-audit check            exit 0
kernel + demography tests   307 passed, 0 failed
```

**Design deltas from the plan, both recorded rather than silently taken:**

1. Named `SuitabilityMap` / `CapacityMap`, **not** `…Field`. `kernel/src/field.rs`
   already defines `Field<T>` as a function over (space × time); reusing the word
   would have produced exactly the blur 0103 exists to prevent. `…Map` echoes the
   `CellMap` they wrap.
2. Added `CellMap::map_indexed` rather than a raw `from_values(Vec<T>)`
   constructor — length-preserving by construction, so a typed map can combine
   with another without an unwrap that could desynchronise lengths.

**Correction to 0103 itself**, made the same day and before it reached main: its
§2 claimed the type-audit "never reaches a container's payload." Implementing §1
falsified that within minutes — the audit flagged four untagged `CellMap<f64>`
positions immediately. The real blind spot is *granularity*: one verdict per
named position, so `Vec<(u32, CellMap<f64>)>` gets one tag and
`bare-ok(index: return)` satisfied it while the `f64` went undescribed.

**Not done, and why** — `make gate` was not run: loadavg was 23.8 on ten cores,
and CLAUDE.md is explicit that a concurrent gate costs ~30 min and "both look
hung." `make quick`'s three checks all pass, the changed library crates are
green, and byte-identity holds. **The full gate must run on a quiet box before
merge.**

## Followups

- **A2 (F9)** — `is_habitable` → `is_vale_like`, including the
  `habitable-fraction` metric. Measured: 578 occurrences of "habitable" and 33 in
  `.csv`/`.sql`/`.md`, so it **is** a census column and **does** need a
  rebaseline. Not bundled here for that reason.
- **A3** — teach the type-audit a verdict-per-primitive for compound positions
  (0103 §2, as corrected).
- **Downstream typing** — `condense_tagged` / `coexist::pack` / `byproducts` and
  the bake still take bare `CellMap<f64>`; each unwrap is explicit and commented.
  Typing them through is mechanical and independent.
- **Historical documents left alone** (0082's precedent): the old
  `niche_per_species_k` name survives in past specs, plans, and
  `book/src/laboratory/the-terminator-probe.md`, which said what they said.

---

# Step B — decompose `CarryingInput.habitable` (NOT delete it)

## The trap this avoids

§8 said "delete `carrying_capacity`'s `if !i.habitable { return 0.0 }`". Checked
before implementing: **that would open the ocean.** `is_habitable` is three
conjuncts —

```rust
elevation_m >= sea_level_m && (-5.0..=35.0).contains(&temp_c) && moisture >= 0.2
```

— and the `habitable` bool is the *only* land test `carrying_capacity` performs.
Deleting it makes ocean cells (high moisture, moderate temperature) the most
productive ground on the planet.

The other two conjuncts, though, are **already smooth downstream** and the hard
cut is redundant with them:

```
npp       = min(temp_response(t), moisture)      -> 0 at moisture 0, 0 outside [2,42]C
aridity   = ((0.2 - moisture).max(0) * 5).clamp(0,1)   -> folded into hostility
capacity  = BASE * npp * bonus * (1 - hostility)
```

So temperature and moisture already grade to zero on their own. Only *land* needs
to stay a hard gate.

## The change

`CarryingInput.habitable: bool` becomes `is_land: bool`, and the composition root
passes `!terrain.is_ocean(cell)` instead of `climate.habitability()`. One flag
that conflated three predicates becomes one flag that means one thing — the same
decomposition 0103 performed on suitability-vs-capacity.

**What this opens** (measured in Task 0): the arid band (moisture `(0, 0.2)`,
18–32% of land) and the very-hot band (temp `(35, 42]`, 0–18%). **What it does
NOT open**: the cold, because `temp_response` is zero below 2 °C — the third gate
§8 already flagged. Cold remains a roster problem (step D), exactly as predicted.

## Stages

### B1 — the decomposition
**Success**: `is_land` replaces `habitable`; root passes `!is_ocean`; workspace
builds; demography's own tests re-expressed against the new meaning.
**Status**: Complete

### B2 — measure against the frozen preregistration
**Goal**: §4's H1 (scatter), H2 (reach), H3 (count) are now testable; the probe
is the instrument and Task 0's numbers are the baseline.
**Success**: probe re-run on all five seeds; deltas recorded here.
**Status**: Complete — see the B outcome below

### B3 — artifacts and gate
**Success**: `make rebaseline` (NOT diff-alone — A1's lesson), `make gate` green,
timings ledgered. Census regen is a CARVE-OUT and needs Nathan's explicit
authorization; it is NOT part of B3.
**Status**: Complete except the census carve-out

## B outcome (2026-08-04) — necessary, and measurably insufficient

Probe re-run on all five seeds, against Task 0's frozen baseline:

```
seed        NEW ground opened      survivable   expandable   expansion ratio
             (K>0, gate-excluded)   (eff>1.0)   (eff>11.43)   before -> after
42               70  ( 0.63%)            0           0        0.72x -> 0.74x
7              3126  (16.41%)           78           0        0.91x -> 1.27x
999999         2040  (12.72%)           20           0        0.95x -> 1.44x
16244...       2697  (22.77%)           61           0        0.94x -> 1.43x
1234             97  ( 0.84%)            1           0        0.47x -> 0.49x
```

**B works**: the gate no longer forbids the arid and very-hot bands, and the
expansion ratio crosses 1 on three of five seeds. **B is also insufficient, for
exactly the reason Task 0 finding 2 predicted**: of all that newly-reachable
ground, at most 78 cells on any seed clear the viability floor and **zero clear
the daughter-founding bar, on every seed**. `eff_capacity = capacity × K` and K is
the dimensionless saturation (~0.01–0.05), so marginal ground collapses below
viability before anyone can live on it. **C (desaturate) is required for B to
mean anything** — which is what §8 meant by "B + C as one measurement", now
measured rather than argued.

Direction checks, which is how we know these are the intended mechanism and not
contamination:
- Seed 7 gained the most ground (16.4%) and is also the seed whose stratigraphy
  pin moved most (1 → 6 reordered cells). Consistent.
- The tropical/polar K ratio ROSE (31.0099 → 31.0649). Hot-arid ground is
  tropical, never polar (the poles stay closed by `temp_response` below 2 °C), so
  a rise is the only admissible direction.
- Seed 42 gained least (0.63%, none survivable) and its kobold settlement count
  held at 97 — only the naming draw shifted. A change that opens marginal ground
  should move names and not counts.

**Four pins re-pinned**, each with its cause and a direction argument recorded in
place: `lens_purity` seed-42 fixture (via `rebaseline-goldens`), lab
`seed_42_name_syllables` (254/97 → 252/97), demesne `k_biomass_gradient`
(31.0099 → 31.0649), `history_emit` stratigraphy (seed 7: 1 → 6).

**Gate green: 2936/2936**, wall 707.087 s.

**The censuses are now STALE** — `make rebaseline` skips them by design, so
`book/src/laboratory/generated/the-census/` no longer matches the code. That is
the carve-out and needs explicit authorization; it also belongs on lefford.

## Open question for Nathan — a fidelity call (carve-out)

**Moisture is counted twice in `carrying_capacity`, and it is why the newly-opened
arid ground is unusable.**

```
npp      = min(temp_response(t), moisture)                    <- moisture as a Liebig limiter
aridity  = ((0.2 - moisture).max(0) * 5).clamp(0, 1)
hostility= max(unrest, aridity)
capacity = BASE * npp * bonus * (1 - hostility)               <- moisture AGAIN, as hostility
```

At moisture 0.1 that is a ~10× penalty through `npp` **and** a further 2× through
`hostility` — ~20× total on ground that is merely semi-arid. Whether that
double-count is intended is a fidelity question, not a tuning one, so it is
brought rather than decided (autopilot carve-out: accuracy tradeoffs always come
to Nathan). If unintended, removing the aridity term from `hostility` is the
single highest-leverage change available to make arid ground actually livable —
and it would land naturally with C.
