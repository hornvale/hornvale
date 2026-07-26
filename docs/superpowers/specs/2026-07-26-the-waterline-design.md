# The Waterline — a medium for the habitat model

**Campaign**: The Waterline (prerequisite work surfaced by BIO-35 Stage 2 / The Chase)
**Date**: 2026-07-26
**Status**: spec, **substantially corrected 2026-07-26 — see §11**

## 1. Summary

Two resource-supply axes — `MINERAL` and `DETRITUS` — are not gated by the
habitability mask that gates the other two, so the kinds that eat them hold
carrying capacity below sea level. Measured at seed 42:

```
                 BEFORE                          AFTER (this campaign)
kind          land    ocean   total          land    ocean   total
xorn             7   25,982  25,989             7   25,982  25,989   (Lithic — kept)
rust-monster 8,719    3,914  12,633         8,719        0    8,719   (Terrestrial)
twig-blight  1,410        0   1,410         1,410        0    1,410   (unaffected)
goblin         930        0     930           930        0      930   (unaffected)

world: 11,066 land cells / 29,896 ocean cells (sea level = −2,936.17 m)
```

The fix introduces the concept the model lacks — **medium** — as a per-kind
`HabitatDomain` on `BiosphereTraits`, applied as support restriction at the
carrying-capacity layer.

## 2. The measured diagnosis

**One defect, precisely located.** `PHOTOSYNTHATE` and `PLANT_FORAGE` ride
`base_carrying`, which consumes climate's habitability mask. `MINERAL` comes
from lithology and `DETRITUS` is an ambient constant; **neither is gated at
all**. In a sampled cell at −4,120 m:

```
goblin 0.000000   twig-blight 0.000000   xorn 0.000485   rust-monster 0.000313
```

Only the two mineral feeders leak. Everything else is already excluded.

**Habitability is already a land test**, contrary to this spec's first draft.
`climate::is_habitable` is:

```rust
elevation_m >= sea_level_m
    && (HABITABLE_MIN_C..=HABITABLE_MAX_C).contains(&temp_c)
    && moisture >= HABITABLE_MIN_MOISTURE
```

Measured: **4,209 habitable cells, every one on land, zero at sea.** It works.

**Why it still needs replacing for this purpose.** Its doc says what it is —
*"whether a cell could host a vale-like settlement"* — a **settlement-suitability**
test. Reusing it as the medium gate would import "moisture ≥ 0.2" and
"−25 °C ≤ T ≤ 35 °C" into the requirements of a rock-eating construct, which is
wrong for the same reason the current bug is wrong: it answers a different
question and happens to correlate. The medium axis says what it means.

## 3. Why support restriction, and not a smaller number

The defect lifts to: *a scoring function with unbounded support, plus an
unconditional lower bound, applied over a domain where the question is
undefined.* ML masks logits to −∞ before the softmax; statistics declares the
support; databases distinguish `NULL` from `0`.

It matters because **dominance is `argmax`**. A cell containing only tiny
values still has a largest one, so shrinking values cannot remove a kind from
contention — only restricting the support can. This is why "scope
`sovereignty_floor`" (which grants 0.33–0.76 of full habitat response
unconditionally on three of four axes) is *not* this campaign's fix, however
much it looks like the root cause. It is captured as its own registry row.

## 4. The design

### 4.1 `HabitatDomain` on `BiosphereTraits`

```rust
pub enum HabitatDomain {
    /// Lives above the waterline.
    Terrestrial,
    /// Lives below it. No shipped kind yet — the aquatic roster is a sequel.
    Aquatic,
    /// At home in both, moving between them — an otter, a crocodilian.
    Amphibious,
    /// Lives *in the substrate*, which underlies both land and sea floor, and
    /// is therefore indifferent to the waterline above it. A xorn swims
    /// through stone; the ocean over its head is not its medium.
    Lithic,
}
```

Placed beside `social_form`, following The Eremite's precedent for a universal
biosphere axis (0065).

`Lithic` and `Amphibious` share a permit-everywhere gate in v1 but make
different claims, and the distinction is worth carrying: `Amphibious` means *at
home in both media and moving between them*; `Lithic` means *in neither — in a
third medium that underlies both*. When a future campaign gives the substrate
its own extent (ice sheets, deep sediment, exposed bedrock), `Lithic` gains a
real gate and `Amphibious` does not.

### 4.2 Authoring

| kind | domain | why |
|---|---|---|
| xorn | `Lithic` | `Ametabolic`, pure-`MINERAL`, burrows through stone. Its domain is the world's rock, most of which happens to lie under water. |
| rust-monster | `Terrestrial` | `Ectotherm`, pure-`MINERAL`, but a walking beast that eats metal objects — it lives on the surface, not in it. |
| the other fourteen | `Terrestrial` | surface dwellers, all. |

### 4.3 The gate, at K

First statement inside `niche_per_species_k`'s per-cell closure:

```rust
let wet = terrain.is_ocean(cell);
let permitted = match bio.habitat_domain {
    HabitatDomain::Terrestrial => !wet,
    HabitatDomain::Aquatic => wet,
    HabitatDomain::Amphibious | HabitatDomain::Lithic => true,
};
if !permitted {
    return 0.0;
}
```

Gating **K**, not each supply field, states the actual claim — *this kind
cannot be here* — and covers every axis at once, including any added later.
`terrain.is_ocean(cell)` (`elevation_at(id) < globe.sea_level`) already exists;
no new field is derived, and it tracks The Sundering's time-varying sea level
for free.

## 5. Preregistered predictions

**P1.** After the gate, `rust-monster` holds zero ocean cells (was 3,914) and
its land count is unchanged at 8,719.

**P2.** `xorn` is **unchanged** at 7 land / 25,982 ocean — it is `Lithic`, and
rock is rock. Consequently `demesne.rs`'s xorn assertion, which fails under a
Terrestrial-xorn reading, **passes**.

**P3.** `twig-blight` and `goblin` are unchanged (they already held zero ocean
cells).

**P4 — already measured, Task 1, and it held.** Zeroing *all* ocean K left seed
42 byte-identical (3,553 facts, village `Qvooshtvoagootao`, `lens_purity`
passing). Settlement placement does not move, so world identity does not drift.

**P5.** No test in the workspace reddens except those pinning `rust-monster`'s
or the aggregate dominance counts.

## 6. Blast radius

### Consumers

| class | sites | effect |
|---|---|---|
| K computation | `niche_per_species_k` | the gate |
| Registry literals | 16 in `domains/species`; two `..goblin_bio` spreads in `windows/worldgen` and Lab's clones inherit | one field |
| Dominance metrics | `menagerie_full_roster_dominant_breakdown`, `demesne.rs` | rust-monster's counts move; xorn's do not |
| Coexistence stack | `pack` filters `present` to `k > 0.0`, so a zeroed kind is simply absent | no formula change |
| Settlement placement | genesis attractors | unchanged (P4, measured) |

### Committed artifacts

| artifact | expected |
|---|---|
| `cli/tests/fixtures/world-seed-42.json` | **unchanged** (P4) |
| census goldens | changed only where a metric reads density/dominance — **much narrower than first thought**, since only rust-monster moves. Verify before requesting the carve-out. |
| gallery, dictionary, phonology, almanacs | unchanged |
| `docs/audits/type-audit-report.md` | unchanged (an enum, no primitive tag) |

## 7. Determinism

No epoch, no new predicate, no serialized state, no stream draws. `P4` is
measured, not assumed.

## 8. What this campaign no longer claims

The first draft justified The Waterline as a **prerequisite** for the prey
field on the grounds that prey production was 77.9% oceanic. **That figure was
wrong** (§11). Corrected: prey production is **91.8% land, 8.2% ocean**, and
the richest prey cell in the world is on land.

So The Chase would not have put dragons to sea in any dominant way, and this
campaign is not load-bearing for it. It remains worth doing — 3,914 wrongly
held ocean cells is a real defect, and the medium concept is a real gap — but
it is a **correctness campaign in its own right**, not a blocker. The Chase can
proceed before or after it.

## 9. The Demesne

The Demesne's chronicle reports the xorn winning "the largest domain any kind
holds — a mineral-eater owning its mountains." Measured: 7 land cells and
25,982 ocean cells.

Under §4.2's authoring the domain is **kept** and its acceptance test passes,
so nothing is falsified — but the prose is still inaccurate, and the accurate
version is better: the xorn owns the world's *rock*, and most of a world's rock
lies beneath its ocean. A one-sentence chronicle correction at close, owner's
call.

## 10. Non-goals

The aquatic roster; scoping `sovereignty_floor`; the prey field; freshwater as
a medium; re-tuning β.

## 11. Correction notice

This spec's first draft was built on a probe that classified ocean as
`elevation < 0.0`. **Sea level on seed 42 is −2,936.17 m**, and terrain already
publishes `is_ocean` (`elevation < sea_level`). The two disagree on 8,162
cells, and every land/ocean figure in the first draft was wrong. Specifically:

| first draft | corrected |
|---|---|
| 2,904 land / 38,058 ocean | **11,066 land / 29,896 ocean** |
| "goblin dominates 930 cells, all below sea level, none on land" | goblin's 930 cells are **all land**; unaffected by this campaign |
| "habitability marks 3,817 ocean cells habitable vs 392 land" | **4,209 habitable cells, all land, zero ocean** — habitability is already a land test |
| "prey production is 77.9% ocean" | **8.2% ocean** |
| two independent defects | **one**: `MINERAL`/`DETRITUS` bypass habitability |

The design survived the correction — support restriction and a medium axis are
still right, and Task 1's byte-identity result never depended on the
classification. The *motivation* did not: this is a smaller campaign than it
was sold as. The measurement used an assumed constant where the codebase
published the real predicate, and four rounds of conclusions were drawn before
anyone checked. Retrospective headline.
