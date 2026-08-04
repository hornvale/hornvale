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
