# The Lantern — design

**Status:** spec, awaiting G3 review. **Rewritten** after a structural finding
re-cut the campaign (§9).
**Date:** 2026-08-07
**Campaign:** The Lantern — what a built place is made of, and the light that
falls on it.
**Measured on:** `64c0daac` (main, The Beholding's close).

## 1. The question

Nathan's ask: *"having color in the world… multiple materials for the walls
and floors that, if only subtly, add visual variety and richness"* and
*"underworlds that reflect the color of a torch, of luminescent fungi, of
lava."* Aesthetics first.

**Materials and light are one campaign, not two, and the structure of the
codebase forces it.** Light needs something to fall on; materials need
something to reveal them. Specifically:

- The chamber band has a pane and a `SessionPlan`, but its
  `PaletteEntry.color` slot is **deliberately empty** — The Beholding shipped
  it unpopulated because `CellKind::Wall` carries no material.
- Indoors there is no illuminant at all, and The Beholding refused to colour a
  chamber with the noon sun because that is a falsehood.

So neither half renders alone. Together they do, and the result is what the
ask describes: a stone-walled room and a timber one, both warm under a
hearth, visibly different from each other.

## 2. What exists, and what is missing

**Ships, unused:**

- `AnchorKind::Hearth` — doc: *"A fire: emits warmth and light; the canonical
  gathering place."* Placed in built interiors. Never read as a light.
- `illuminant::planck_relative(nm, kelvin)` — a general blackbody, **private**,
  used only by `daylight`. A torch is the same function at 1900 K.
- `shadowcast(lattice, from, radius) -> BTreeSet<Cell>` — The Sighting's
  symmetric integer FOV. Its doc already says *"A radius below 1 **lights**
  the origin alone."*
- `lithology::reflectance(buf, rock)` — the local stone, already a `Mixture`.
- The achromatic channel and the lexicon's `gloom`/`shadow`/`starlit` ladder.

**Missing:**

- **`CellKind::Wall` has no material.** No fabric, no source. This is
  `MAP-building-fabric`.
- **No interior illuminant.** This is `MAP-interior-light`.

**Measured before this spec was written** (`lantern_probe.rs`, no world
build) — one limestone wall, five lights, human eye:

| light | | sRGB |
|---|---|---|
| daylight 5800 K | near-white | `[228, 230, 223]` |
| torch 1900 K | warm amber | `[136, 111, 39]` |
| hearth 1200 K | deep ember | `[92, 60, 3]` |
| lava 1100 K | darker red | `[85, 53, 2]` |
| fungi ~490 nm | cool blue-green | `[111, 142, 179]` |

The same stone. Nothing about the wall changed.

## 3. Materials

A cell type's **fabric** is derived, never drawn — from world-state that
already exists:

```
fabric(cell_kind, settlement) ->
    Stone      <- local lithology is competent and near      reflectance DERIVED from bedrock
    Timber     <- forested biome, temperate                  authored
    Cob/Brick  <- deep soil, dry climate                     authored, tinted by the soil's iron
    Thatch     <- roof/floor only; grassland, wet            authored
```

Stone's reflectance is **derived** from `lithology::reflectance` at the
containing cell, so a village on granite and one on basalt are visibly
different buildings. That is the same "derive, don't author" move The
Overburden reached for soil, and it costs no new authored data.

Floors take a parallel, shorter list (packed earth, timber, flagstone) and
`Threshold` stays `None` — an opening is not a fabric, which The Beholding
already established.

**`PaletteEntry.color` finally fills**, which is the slot The Beholding
shipped empty and named this campaign as the filler for.

## 4. Light

### 4.1 Propagation is the shipped shadowcaster

`shadowcast` is *symmetric*, so "what can see this cell" and "what light
reaches this cell" are the same set. **Light needs no new geometry.** A
source's reach is its FOV at its radius; a cell's incident light is the sum
over reaching sources.

That sum is the **additive law**, which the kernel declared and deferred to
this campaign by name (`color.rs:135`): *"Additive (not implemented; arrives
with multi-light): two torches on one wall. **Sum the illuminants**, not the
reflectances."* Precedent, not invention.

### 4.2 Sources

| source | placed by | spectrum |
|---|---|---|
| hearth | `AnchorKind::Hearth`, already in built interiors | blackbody 1200 K |
| doorway | a `Threshold` to outdoors, at the sun's real altitude | The Beholding's daylight, attenuated |
| **implicit torch** | the possessed agent itself | blackbody 1900 K |

**The implicit torch is Nathan's call at G3 and it is a stated assumption,
not invented physics:** a possession is assumed to be carrying a light. It
makes the explicit carried torch a refinement rather than a new mechanism,
and it means a possession is never stranded in the dark with no inventory to
fix it. It also gives the roguelike look directly — 1900 K renders limestone
`[136,111,39]`, warm amber.

Attenuation with distance is `1/(1+d²)`-shaped and authored; it is a free
parameter and the spec says so.

### 4.3 Emitters are illuminants at their own cell

A glowing thing lights its own cell and its neighbours through the same
shadowcast. This conflates emission with self-illumination — stated plainly —
but needs **no new term in `sense()`** and gets the visible result right.
Fungi and lava arrive with the cave campaign (§8); the mechanism lands here.

### 4.4 The scotopic term

**Measured:** in near-darkness a human and a kobold go black *together* while
their rod signals genuinely differ (0.0003 vs 0.0004). The rod is
`ChannelRole::Achromatic`, so **no projection reads it** — correctly, since a
rod carries no hue — which means night vision cannot reach the screen at all.

So `to_srgb` gains a scotopic term: below an authored photopic threshold, the
achromatic channel contributes **equally to all three output slots**, blended
in as the photopic signal fades. Grey sight, which is what night vision is.

The rod still carries no hue — contributing equally to R, G and B, it can
never shift one. **In daylight the term is exactly zero**, so every colour The
Beholding emits is unchanged. That is a byte-identity requirement with a test,
not an aspiration. It also answers the Pigment's open risk 5.

## 5. Determinism and epoch analysis

**No epoch.** No seeded draw, no `streams.rs` constant. Fabric is derived from
lithology, biome and climate; sources are read off placed anchors; light is a
derived view computed per turn and never stored.

| change | verdict |
|---|---|
| `planck_relative` made `pub` + emitter constructors | additive |
| `PaletteEntry.color` populated | **the chamber fixtures move** |
| light field from `shadowcast` | new derived view, nothing stored |
| the scotopic term | **zero in daylight**, pinned by byte-identity |
| the walk band | untouched |

`planck_relative`'s doc defends its midpoint sampling *"at main-sequence
temperatures"*. A torch at 1900 K is far outside that range; the
justification must be **re-stated for the new range**, not silently
inherited. This is the same shape as the three framing errors in §9.

## 6. Preregistered claims

**H1 — fabrics are distinguishable.** On **real terrain**, swept across
seeds, two settlements whose local lithology differs produce stone walls
differing by more than a stated threshold in at least one channel. *Falsified
if bedrock variation is too small to survive the fabric transform* — which
The Beholding's 28-vs-2 result proves is not automatic.

**H2 — the lights are distinguishable on real fabric.** A hearth-lit cell and
a doorway-lit cell in the same room differ. Strongly indicated by the probe
on authored limestone; the claim is that it survives derived fabric.

**H3 — daylight is byte-identical.** Every colour The Beholding emits at the
surface is unchanged after the scotopic term. A requirement, with a test.

**H4 — night vision reaches the screen.** At an illuminance where a human's
emitted colour is `[0,0,0]`, a kobold's is not. *Falsified if the threshold
that achieves this perturbs daylight* — in which case the term is wrong.

## 7. Testing

- Every guard states what would make it fire and is mutation-proven. The last
  campaign shipped seven green-and-vacuous guards, **all from plan text**.
- **Drive the whole seam** in one test — fabric → palette → light field →
  `sense` → emitted colour. Unit tests at each node with none on the path is
  the shape that hid The Beholding's `sightOf` defect.
- The additive law gets a **positive control**: two sources give a strictly
  brighter cell than either alone.
- Sweep seeds; never pin one.

## 8. Out of scope

- **The underworld.** It has no chart of its own (§9); fungi and lava wait for
  one. The emitter mechanism lands here so that campaign is a data change.
- **Dirt vs paved roads.** Nathan asked for these; they are *outdoor*
  surfaces on the walk band, which belongs to the surface/cover campaign
  parked on `the-overburden`. **Named here so it is not silently dropped.**
- **A carried light made explicit**, temporal decay (a hearth burning down),
  and a true emission term in `sense()`. Registry rows.

## 9. The finding that re-cut this campaign

The first draft of this spec scoped The Lantern to *"interiors and the
underworld"*. **The underworld has no view to light.**
`the_underground_band_folds_into_walk_as_map_does` asserts a fold, and its own
doc says: *"Standing in a cave chamber, the pane shows a chart of the country
overhead — **which is odd**… the invariant worth pinning is not 'the pane is
right here' but 'the pane and the verb cannot drift apart here'."* The repo
knows, and has deferred deciding.

That is the **third framing error of this session, all one shape**: reaching
for a shipped, plausible-sounding thing and asserting it answers the question
asked.

| what I asserted | what it actually is |
|---|---|
| `snow_fraction` is ground snow cover | the fraction of *precipitation* falling as snow |
| the chlorophyll red edge widens the observer gap | real, but at 720 nm where human cones are blind |
| the underground band is a view to light | a fold into the *surface* chart |

Every *measurement* was right; every *attribution* was wrong. The countermeasure
that worked all three times was the same: run the probe before writing the
claim. §6's claims all name their substrate for this reason.

## 10. Risks

1. **Fabric derivation may be too coarse to vary.** If most settlements sit on
   similar rock, H1 fails and the campaign ships stone walls that all look
   alike. Measure before claiming.
2. The attenuation constant and the scotopic threshold are the two free
   parameters. Neither may be tuned after unblinding.
3. **The chamber fixtures move** when the palette fills. Re-pin in the
   drifting commit, never at the close.
4. `the-delvers` is active and touches the underworld. Read its chronicle.
