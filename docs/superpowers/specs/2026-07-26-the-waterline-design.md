# The Waterline — a medium for the habitat model

**Campaign**: The Waterline (prerequisite to BIO-35 Stage 2 / The Chase)
**Date**: 2026-07-26
**Status**: spec, awaiting G3 review

## 1. Summary

The habitat model has no concept of **medium**. Nothing anywhere asks whether a
cell is land or water, or which of those a kind lives in. The consequence,
measured at seed 42 on current main:

```
kind                 land    ocean    total   ocean %
xorn                    7    25982    25989    100.0%
rust-monster         2832     9801    12633     77.6%
twig-blight            65     1345     1410     95.4%
goblin                  0      930      930    100.0%

world: 2,904 land cells / 38,058 ocean cells
```

The goblin dominates 930 cells, every one below sea level and none on land.

This campaign adds the missing concept: a per-kind `HabitatDomain` on
`BiosphereTraits`, evaluated against terrain's existing `is_ocean` predicate,
applied as **support restriction** — a hard zero on carrying capacity for a
kind outside its medium.

Every shipped kind is `Terrestrial` in v1, so the content is degenerate and the
outcome is a land mask. The *mechanism* is general, so adding a marine kind
later is authoring, not a code change.

## 2. The measured diagnosis

Two independent defects, both verified by
`windows/worldgen/tests/waterline_probe.rs` (committed at `0041069f`):

**2.1 `habitability` is not a land test.** Climate's `CellMap<bool>` marks
**3,817 ocean cells habitable against 392 land cells**. It answers whether the
*climate* is livable, and open ocean is thermally stable and wet, so it passes
easily. It is correct at its own question and was never asked this one.

**2.2 `MINERAL` and `DETRITUS` bypass it entirely.** `PHOTOSYNTHATE` and
`PLANT_FORAGE` ride `base_carrying`, which consumes habitability — which is why
goblin and twig-blight read exactly `0.000000` in rejected cells. Mineral comes
from lithology and detritus is an ambient constant; neither is gated. In a
sampled cell at −4,120 m:

```
goblin 0.000000   xorn 0.000485   rust-monster 0.000313   twig-blight 0.000000
```

That fully explains the table in §1: goblin's ocean cells are *habitable* ocean
where forage flows; xorn's are *non*-habitable deep ocean where only the two
ungated axes do.

**Why it went unnoticed:** settlement placement filters to land downstream, so
every human-visible output stayed sane while the density field did not.

## 3. Why support restriction, and not a smaller number

The defect lifts to a shape with one well-known answer: *a scoring function
with unbounded support, plus an unconditional lower bound, applied over a
domain where the question is undefined.* Machine learning masks logits to −∞
before the softmax; statistics declares the support; databases distinguish
`NULL` from `0`. None of them fixes it by shrinking values.

That matters here because **dominance is `argmax`, a comparative operation**.
The goblin holds its 930 ocean cells not because its density is large but
because nothing else is larger there. At a floor of `1e-9` it still holds them.
Only removing a cell from contention removes it from contention.

This falsifies the most attractive alternative framing — *"the real bug is that
`sovereignty_floor` grants 0.33–0.76 of full habitat response unconditionally,
so scope the floor."* That may well be a real defect, and it is captured as its
own registry row, but it cannot fix an argmax and so cannot be this campaign's
answer.

`ConditionResponse::eval` is `floor + (1 − floor)·devotion·exp(−½z²)` — a
Gaussian with unbounded support over a floor. Elevation's floor is already hard
(`0.0`), and it still excludes nothing, because a Gaussian never reaches zero.
Softness is not the problem; undeclared support is.

## 4. The design

### 4.1 `HabitatDomain` on `BiosphereTraits`

```rust
/// The medium a kind's body lives in — a universal biosphere axis, like
/// `social_form` (decision 0065). Every kind with a body is in some medium;
/// this is not a capacity only some kinds carry, so it is a field rather than
/// a component store.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HabitatDomain {
    /// Lives above the waterline. Every shipped kind, in v1.
    Terrestrial,
    /// Lives below it. No shipped kind yet — the aquatic roster is this
    /// campaign's sequel.
    Aquatic,
    /// At home in both (a shore-dweller, an otter, a crocodilian).
    Amphibious,
}
```

Placed on `BiosphereTraits` beside `social_form`, following The Eremite's
precedent for a universal axis. A component store is the idiom for an optional
*capacity*; a universal axis is a field, and gets totality for free rather than
needing an integrity rule to enforce it.

### 4.2 The gate, at K

In `niche_per_species_k`, a kind outside its medium reads **zero carrying
capacity**, before supply and condition terms are combined:

```rust
let permitted = match bio.habitat_domain {
    HabitatDomain::Terrestrial => !terrain.is_ocean(cell),
    HabitatDomain::Aquatic => terrain.is_ocean(cell),
    HabitatDomain::Amphibious => true,
};
if !permitted {
    return 0.0;
}
```

Gating **K**, not each supply field, is deliberate. The measured defect has two
independent sources (§2), and per-axis gating would have to fix both paths
while leaving the next axis added ungated by default. Gating K covers every
axis at once, present and future, and states the actual claim: *this kind
cannot be here* — not *this kind finds no food here*.

`terrain.is_ocean(cell)` already exists (`elevation_at(id) <
globe.sea_level`) and is already used by `confluence.rs`, `demesne.rs`, and
`traversal.rs`. No new medium field is derived; the campaign reads the one
terrain already publishes, which keeps this correct under The Sundering's
time-varying sea level for free.

### 4.3 `habitability` is untouched

It answers a real and different question, and answers it correctly. The medium
gate is orthogonal and additional. A kind must clear both: the right medium
*and* a livable climate.

## 5. Preregistered predictions

**P1.** After the gate, no kind dominates any ocean cell. The land/ocean table
in §1 becomes land-only for every kind.

**P2.** Xorn's dominated-cell count collapses from 25,989 to at most the 2,904
land cells, and the "largest domain on the world" title changes hands or
changes magnitude by an order of magnitude.

**P3.** Goblin dominates zero cells before the change and a non-zero number of
*land* cells after — or, if it still dominates nothing, that is a real finding
about the peoples' competitive position and is reported, not tuned away.

**P4 (the one I am least sure of).** **Settlement placement does not move.**
Land cells' competitive shares are computed per-cell, so removing ocean from
contention should leave them identical — but `emigration_pressure` and any
global normalization in the coexistence stack could couple them. If seed 42's
flagship settlement name or position changes, world identity has drifted and
the campaign's cost is much larger than its diff. **This is measured before the
fix is built** (§7), not discovered after.

**P5.** The four peoples' *relative* K ordering on land is unchanged — this
campaign removes cells from contention, it does not re-weight the survivors.

## 6. Blast radius, by consumer class and by committed artifact

Both partitions, per The Vigil's retrospective lesson.

### Consumers

| class | sites | effect |
|---|---|---|
| K computation | `niche_per_species_k` | the gate |
| Registry literals | every `BiosphereTraits { .. }` in `domains/species` (16 kinds) plus Lab's synthetic rosters in `windows/lab/src/roster.rs` | one new field each |
| Dominance metrics | `menagerie_full_roster_dominant_breakdown` and the `#[ignore]`d `≥6`-distinct-dominants target | counts move; the ignored target may move toward or away from its threshold — reported either way |
| Coexistence stack | `demography_report`, `couple_trophic`, `cell_share` | fewer cells with non-zero density; no formula change |
| Settlement placement | genesis attractors | P4 — expected unchanged, measured |
| Lab metrics | anything reading density or dominance | census-regenerating (§8) |

### Committed artifacts

| artifact | expected |
|---|---|
| census goldens (`book/src/laboratory/generated/**/rows.csv`) | **change** — the carve-out |
| `cli/tests/fixtures/world-seed-42.json` | unchanged iff P4 holds; the tripwire decides |
| `book/src/gallery/*` almanacs, dictionary, phonology | unchanged (no sky, language, or perception input) |
| `docs/audits/type-audit-report.md` | one new tagged item |
| The Demesne's chronicle prose | see §9 |

## 7. Task ordering — P4 first

The plan's first task measures P4 against a throwaway gate, before any authored
field exists. If settlement placement moves, the campaign's shape changes
(world identity drift, a re-baselined fixture, possibly an epoch conversation)
and that must be known at task 1, not task 5.

## 8. Determinism and the census carve-out

- **No epoch**: no seed-derivation label, stream consumption order, or noise
  constant changes.
- **No new predicate**, no serialized state. `BiosphereTraits` is authored
  build state, not save format.
- **No draws move** — *if* P4 holds. If it does not, placement changes and
  drawn names follow it.
- **Census regeneration is required** and is a carve-out: explicit owner
  authorization before the regen runs.

## 9. The Demesne's claim

The Demesne's chronicle reports the xorn winning "the largest domain any kind
holds — a mineral-eater owning its mountains as the place-identity model
intends." Measured on current main, that domain is **7 land cells and 25,982
ocean cells**.

The rank-restoration that campaign measured is real and this campaign does not
dispute it: the xorn genuinely went from noise to the largest domain. What the
prose reads as mountains is seafloor. After The Waterline the claim becomes
true or becomes false, and either way the chronicle needs re-reading against
the new numbers. Correcting another campaign's shipped chronicle is the owner's
call, flagged at G3 rather than done silently.

## 10. Non-goals

- **The aquatic roster.** v1 authors zero `Aquatic` kinds; 93% of the world
  becomes honestly empty. That is the point — an honest empty ocean creates the
  demand a dishonest full one hides. Sequel.
- **Scoping `sovereignty_floor`.** Falsified as this campaign's fix (§3);
  registry row.
- **The prey field** (The Chase) — downstream of this, unblocked by it.
- **Freshwater.** The Freshet's `WaterKind` salt/fresh distinction is not read
  here; `is_ocean` is the only medium test in v1. A lake is not yet a medium.
- **Re-tuning β or any competition constant** to compensate for the removed
  cells.
