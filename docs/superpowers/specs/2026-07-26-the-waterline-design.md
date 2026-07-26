# The Waterline — a medium for the habitat model

**Campaign**: The Waterline (prerequisite work surfaced by BIO-35 Stage 2 / The Chase)
**Date**: 2026-07-26
**Status**: spec, **corrected and extended 2026-07-26 — see §11 (correction) and §4.4 (sovereignty)**

## 1. Summary

Two resource-supply axes — `MINERAL` and `DETRITUS` — are not gated by the
habitability mask that gates the other two, so the kinds that eat them hold
carrying capacity below sea level. Measured at seed 42:

```
                 BEFORE                          AFTER (this campaign)
kind          land    ocean   total          land    ocean   total
xorn             7   25,982  25,989             7   29,896  29,903   (Lithic — GROWS, see P6)
rust-monster 8,719    3,914  12,633         8,719        0    8,719   (Terrestrial)
twig-blight  1,410        0   1,410         1,410        0    1,410   (unaffected)
goblin         930        0     930           930        0      930   (unaffected)

world: 11,066 land cells / 29,896 ocean cells (sea level = −2,936.17 m)
```

Xorn's ocean holding *grows* under the gate rather than staying put: it goes
from 87% of ocean cells to 100%, because the Terrestrial kinds that used to
leak onto the seafloor (rust-monster) vacate it, and `argmax` hands their
former cells to the one kind still contesting them (see P6).

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

At least three kinds leak through this one defect, of which only two are
visible in the before/after dominance tables above. `xorn` and `rust-monster`
win an argmax on ocean cells and so show up as measured ocean counts; `otyugh`
(a `DETRITUS` feeder) held `K > 0` on **all 29,896 ocean cells** pre-gate too,
but never won an argmax there, so it never appears in a dominance table even
though it was leaking exactly the same way and the gate fixes it too.
Dominance tables undercount the defect's true reach — they show who won, not
who was present.

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
contention — only restricting the support can.

This is why §4.4's sovereignty revision, though folded into the same campaign,
**cannot substitute for the medium gate**. Gap-closing shrinks a misplaced
creature's response; it does not remove it from contention, and an argmax over
a cell where every value is tiny still returns one of them. The two changes are
complementary and neither implies the other: the gate decides *where a kind can
be at all*, the sovereignty form decides *how sharply it falls off within
that domain*.

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

### 4.4 Sovereignty — attempted, measured, DEFERRED to its own campaign

**Not shipped in this campaign.** Folded in at the owner's direction, built,
measured, and then removed when the measurement showed it needs a calibration
study rather than a code change. The implementation is preserved at commit
`4f852fd2` (recoverable by SHA) for the sequel to cherry-pick.

**The defect it addresses is real.** `sovereignty_floor(mass, potency)` ∈
[0, 0.95] is added as an unconditional lower bound to every buffer-able
condition response:

```rust
value = floor + (1 - floor) * devotion * exp(-z^2 / 2)
```

The floor never decays with distance from the optimum, so a goblin retains
**33.5%** of full habitat response at any temperature, moisture or insolation
whatsoever, and a red dragon **78.5%**. The name says *sovereignty* — imposing
your conditions on a place — while the mechanism implements **ubiquity**:
presence everywhere at reduced strength. Its own doc calls that a virtue: *"a
soft preference that never excludes."*

**The replacement that was built and measured.** Sovereignty as the fraction of
the environmental gap a creature closes for itself:

```rust
value = devotion * exp(-z^2 / 2)     where z = (field - optimum) * (1 - s) / width
```

equivalently `effective_width = width / (1 - s)`. Accommodation (a dragon heats
its lair, shifting its effective optimum) and endurance (a wider tolerance
band) are the same algebra. It needs no new tuning constant, drops `floor` from
`eval`, keeps the buffered-vs-hard axis split for free (elevation passes
`s = 0`), reproduces today's formula exactly at `s = 0`, and — unlike a floor —
decays.

**Why it was deferred — the campaign's most valuable finding.** With the
revision applied, mean per-claimed-cell coexistence diversity falls to **1.333**
across five seeds (1.257–1.438) against the preregistered band **[1.5, 3.0]**,
where monoculture is ~1 and undifferentiated sharing ~4. Every seed fails, and
not marginally.

The mechanism is now clear: **the coexistence diversity this project calibrated
β to produce was substantially an artifact of the sovereignty floor.** With the
floor, a marginal species held K ≈ `floor × conditions` in every cell —
comparable in magnitude to the well-adapted one — so it took a real share and
the cell read as diverse. Without it, the marginal species sits at K ≈ 1e-8
there, its share collapses, and the cell goes to a single winner.

β = 2.0 was frozen by a 13-seed × 10-β sweep conducted **under the old response
shape**. Changing the shape invalidates that calibration, so the frozen
constant no longer means what it was chosen to mean. Fixing this properly
therefore requires re-sweeping and re-freezing β, which is a calibration study
and its own campaign — not a line of arithmetic bolted onto a medium gate.

Four tests moved under the revision, two of them un-re-pinnable by discipline:
`beta_calibration_freeze` (the band above), `demesne`'s material-dominant floor
(fell to exactly 2, failing `> 2`), `gathering_calibration`'s population
conservation ceiling, and `session_snapshot`'s golden bytes — the last worth
noting on its own, since world JSON is byte-identical yet *gameplay* state
depends on K.

## 5. Preregistered predictions

**P1.** After the gate, `rust-monster` holds zero ocean cells (was 3,914) and
its land count is unchanged at 8,719.

**P2.** `xorn`'s own K field is **unchanged** — it is `Lithic`, permit-
everywhere, and its raw response to a cell never differs before/after the
gate. Its *dominance* count still moves, because dominance is `argmax` over
every kind and rust-monster vacates the ocean cells it used to contest (see
P6: measured 7 land / 29,896 ocean under the gate, up from 25,982). Either
way, `demesne.rs`'s xorn assertion, which fails under a Terrestrial-xorn
reading, **passes**.

**P3.** `twig-blight` and `goblin` are unchanged (they already held zero ocean
cells).

**P4 — MEASURED, and it held for the gate (and, separately, for the deferred
sovereignty revision too).** With the medium gate applied, seed 42 is
**byte-identical to the committed fixture** (`cmp` clean, `lens_purity` passing, 3,553 facts, village
`Qvooshtvoagootao`). Neither change touches a committed fact: settlement
placement reads land-filtered attractors, and the habitat model lives
downstream of the ledger in derived-view territory. **The campaign has no
save-format exposure.**

**P5.** Land stays fully occupied — every one of the 11,066 land cells has a
dominant before and after.

**P6 — HELD.** Under the intended authoring (xorn `Lithic`), xorn retains its
seafloor domain and `demesne.rs` passes. Measured: xorn 29,896 ocean / 7 land
under the gate alone. Note it now holds **100% of ocean cells**, up from 87%
before, because it is the only non-`Terrestrial` kind — see §9.

**P7.** Census goldens move only where a Lab metric reads density or dominance.
Anything reading committed facts is unchanged (P4). With the sovereignty
revision deferred, the shipped change moves only `rust-monster`'s ocean cells
and xorn's ocean share, so the expected census movement is small or nil —
verified before any regeneration is requested.

**P7 — SCOPE HELD, MAGNITUDE FALSIFIED.** The *scope* half was right: only
metrics reading density/dominance moved, and every changed column traces back
to `rust-monster`'s ocean cells and xorn's ocean share, exactly as predicted —
nothing that reads a committed fact moved (P4 holds). The *"small or nil"*
half did not: `the-census`'s `rows.csv` changed on **1000 of 1000 seeds**, and
`composition-variance` moved on every one of them (the summary's `>= 0.1`
movement bucket went from 979 to all 1000). A dominance metric turned out far
more sensitive to which kind wins an argmax than the prediction assumed —
a narrow mechanical cause (one column of counts moving cell-by-cell) produces
a population-wide statistical effect once it is aggregated per seed.

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

The aquatic roster; the prey field; freshwater as a medium; re-tuning β; **the sovereignty revision** (§4.4, deferred with its
measurement to its own campaign); and **per-axis sovereignty** — the observation that temperature and moisture are
axes a creature can *accommodate* (build a lair, store water) while insolation
can only be *endured*, so sovereignty arguably belongs only on the
accommodation axes. That is a third semantic decision and folding it in would
mean none of the three gets measured cleanly. Registry row.

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

---

## 12. Outcome (2026-07-26)

**The medium half of this spec did not ship.** While it was in flight, The Tumult (`56364acf`, `341eb9f9`) fixed the same defect by masking the five resource-supply fields to land rather than gating assembled K on a per-kind medium, and its reasoning is better: `ResourceVector` is already a Hutchinsonian resource-hypervolume niche, so habitat is that niche's shadow rather than a second axis, and two mechanisms can contradict each other (an aquatic diet with a terrestrial medium is expressible and incoherent). `HabitatDomain`, the K gate, and the `Lithic` authoring were removed at `559a1510`; the removal changed no world, because every shipped kind was either `Terrestrial` (redundant with the supply mask) or the xorn (permitted everywhere).

The Tumult also found the deeper cause this spec missed: the leak was masked by an accident until The Tumult's own elevation re-datum removed it. Before it, ocean cells sat ~4 km from every authored elevation optimum, so the Gaussian condition term zeroed the seabed for everyone.

**What shipped instead:** the census canonical-host guard ([decision 0079](../../decisions/0079-census-goldens-are-authored-on-one-enforced-host.md)), the probe's sea-level correction, and two findings now carried as registry rows — BIO-40 (sovereignty should decay; the coexistence diversity β was calibrated against is an artifact of the floor) and UNI-39 (world-identity neutrality is not census neutrality). §8's retraction stands and is the better-documented half of this spec: The Waterline was never a prerequisite for the prey field.

Chronicle: [The Waterline](../../../book/src/chronicle/the-waterline.md). Retrospective: [the-waterline](../../retrospectives/the-waterline.md).
