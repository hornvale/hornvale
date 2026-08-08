# The Lantern — design

**Status:** **G3 approved** (owner, 2026-08-07) — amended at approval; see
§5.1, §5.2, §6 and §7. **Rewritten** before that after a structural finding
re-cut the campaign (§10).
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

**Measured** (`lantern_probe.rs`, no world build) — one limestone wall, five
lights, human eye. **Re-run under §5.2's band integral**, which the probe now
calls directly rather than reimplementing:

| light | | sRGB |
|---|---|---|
| daylight 5800 K | near-white | `[228, 230, 223]` |
| torch 1900 K | warm amber | `[137, 111, 40]` |
| hearth 1200 K | deep ember | `[92, 61, 4]` |
| lava 1100 K | darker red | `[85, 53, 2]` |
| fungi ~490 nm | cool blue-green | `[111, 142, 179]` |

The same stone. Nothing about the wall changed.

**The move to the band integral cost these rows at most one `u8` step**, and
three of the five did not move at all: torch `[136, 111, 39] → [137, 111, 40]`
and hearth `[92, 60, 3] → [92, 61, 4]`; daylight, lava and fungi are
unchanged (fungi is not a blackbody and could not move). This is much smaller
than the ~34 % worst-band error the midpoint rule carries at 1100 K.

**Why — and it is not peak normalization.** These figures are already
post-normalization, so dividing by the peak band cannot be the cancelling
step. The real reason is that **the large relative errors live entirely in the
dimmest bands**, where they have almost nothing to be a fraction of:

| T | worst relative error | at band | that band's normalized value | worst **absolute** change |
|---|---|---|---|---|
| 5800 K | 0.16 % | 360 nm | 0.75 | 0.0012 |
| 1900 K | 10.1 % | 360 nm | 9.6e-04 | 0.0016 |
| 1200 K | 28.2 % | 360 nm | 2.6e-06 | 0.0026 |
| 1100 K | 33.0 % | 360 nm | 6.2e-07 | **0.0028** |

A `u8` step is 0.0039 of full scale. The absolute change never reaches one,
so the rendered triple can move at most a single step — which is what the
table above shows.

**But read the last column downward.** The absolute change *grows* as the
source cools — 0.0012, 0.0016, 0.0026, 0.0028 — and at 1100 K it is already
**71 % of a `u8` step**. Flames are not comfortably below the quantization
floor; they are approaching it, and a colder emitter would cross it. That is
an independent argument for §5.2's node count, and a reason the cave campaign
should not assume its emitters inherit this result.

*The prediction that the flame rows would "shift by up to ~34 %" was wrong
about the rendered surface. The per-band number was right; the inference to
what a screen shows was not — the §10 shape once more, this time in this
spec's own risk register. Recorded rather than quietly dropped.*

## 3. Materials

A cell type's **fabric** is derived, never drawn — from world-state that
already exists:

```
fabric(cell_kind, settlement) ->
    Stone      <- local lithology is competent and near      reflectance DERIVED from bedrock
    Timber     <- forested biome, temperate                  authored
    Cob/Brick  <- deep soil, dry climate                     authored (see below)
    Thatch     <- roof/floor only; grassland, wet            authored
```

Stone's reflectance is **derived** from `lithology::reflectance` at the
containing cell, so a village on granite and one on basalt are visibly
different buildings. That is the same "derive, don't author" move The
Overburden reached for soil, and it costs no new authored data.

Floors take a parallel, shorter list (packed earth, timber, flagstone) and
`Threshold` stays `None` — an opening is not a fabric, which The Beholding
already established.

**Cob is NOT "tinted by the soil's iron", and this spec said so in error.**
Task 3 went looking for the axis and it does not exist: the material buffer
carries iron only *categorically*, as `is_iron_rich(rock)` — a boolean over
rock class — with no continuous iron content to tint anything by. Minting one
would have been an unrequested second fabric axis, so cob ships authored and
flat. Corrected here rather than quietly implemented as something else.

**MEASURED — H1 held, decisively.** Across 8 seeds and **1505 generated
settlements** on real terrain, derived stone spans **102 `u8` steps** in the
widest channel, and the *median* pair of settlements differs by **41**. Risk 1
— stone walls that all look alike — did not occur.

One number is worth carrying forward: **p10 = 1**. A tenth of settlement pairs
differ by a single `u8` step, because settlements cluster on shared rock
classes. The median carries H1; the tail is a constraint on §7's lens, which
must not compress dynamic range.

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

Attenuation with distance is `1/(1+d²)`-shaped and authored. **It is not a
free parameter, and an earlier draft of this spec was wrong to call it one.**
`shadowcast` is symmetric and the implicit torch rides on the observer, so
every cell you can see is lit by your own torch *by construction*: attenuation
is the only thing producing a light gradient in a possessed chamber, and
chambers are a few cells across. Two consequences follow, and §6 depends on
both:

- **A placed light is what makes darkness legible**, not the carried one. The
  hearth earns its place on this argument rather than on flavour.
- The value may not be tuned to rescue a claim (§11, risk 2), which matters
  more now that it is load-bearing rather than cosmetic.

### 4.3 Emitters are illuminants at their own cell

A glowing thing lights its own cell and its neighbours through the same
shadowcast. This conflates emission with self-illumination — stated plainly —
but needs **no new term in `sense()`** and gets the visible result right.
Fungi and lava arrive with the cave campaign (§9); the mechanism lands here.

**The error is not uniform, and that changes why lava is deferred.** Because
the emitting cell's own reflectance still filters light the cell is supposedly
*producing*, the error scales with how dark the emitter is:

| emitter | own reflectance | renders as |
|---|---|---|
| fungus | pale, greenish | green — benign |
| **lava** | basalt/obsidian, near zero in every band | **near-black, while its neighbours render red** |

So the model is right for fungi and visibly wrong for lava — the one emitter
whose own reflectance is near zero. §9 defers lava for want of *data*; it now
also has a **correctness** reason, and the cave campaign must not pull lava
back in without `RENDER-emission-term`.

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
| `planck_relative` **moves down into the kernel** | see below |
| the blackbody becomes a **band integral** | **measured: no committed byte moves** |
| `PaletteEntry.color` populated | **the chamber fixtures move** |
| light field from `shadowcast` | new derived view, nothing stored |
| the scotopic term | **zero in daylight**, pinned by byte-identity |
| the lens (§7) | screen only; never reaches an artifact |
| the walk band | untouched |

### 5.1 The blackbody moves down into the kernel

A hearth is not astronomy, and making `planck_relative` `pub` where it sits
would have `windows/vessel` import `hornvale_astronomy` to light a fire. The
rule, which classifies the three existing functions with no residue:

> **A spectral law that takes no world-state belongs to the kernel; a law
> parameterized by domain state stays in its domain.**

| function | world-state argument | home |
|---|---|---|
| `planck_relative(nm, kelvin)` | none | **`kernel::color`** |
| `at_elevation(base, elev_deg)` | a sun's elevation | astronomy (stays) |
| `daylight(&Star)` | a `Star` | astronomy (stays) |

Precedent is decision 0044 and `domains/CLAUDE.md`: *"If two domains need to
share code, it goes down into the kernel, not sideways."* `Illuminant`,
`BANDS`, `BAND_CENTERS_NM` and `math::exp` are already there, so the function
moves with no new dependency. The body is unchanged and pure, so `daylight`
is bit-identical across the move — **which the plan must check rather than
assert**.

### 5.2 Sampling: the band integral, and its node count

`daylight`'s doc defends a **midpoint sample** *"at main-sequence
temperatures"* — a justification that lives on the *consumer*, so a new
consumer inherits nothing by default. Re-stating it for 1100–1900 K was
required, and the re-statement failed: measured against a converged
reference, the worst-band error is **0.26 % at 5800 K but 34 % at 1100 K**,
because below the grid the visible range is the steep, strongly convex Wien
tail and a midpoint sample underestimates a convex mean.

**So the blackbody becomes a band integral** (Simpson's rule over each band's
40 nm span). The kernel's own doc already pointed here — `BAND_CENTERS_NM`:
*"Anything integrating over a band (**Planck sampling**, a sensitivity curve)
wants the **edges**."* This is the accuracy choice, taken deliberately
(owner's call, 2026-08-07), with the aesthetic half moved to the lens in §7.

**Measured cost: none.** A spike (13 nodes, reverted) left the full suite at
3135/3135 and moved no committed colour byte, because at 5800 K the midpoint
error is `1.56e-3` relative and a `u8` step is `3.9e-3` — the change lands
below quantization. That result is **mutation-proven in both directions**: a
gross perturbation (band 9 halved) reddens
`hornvale-vessel::session_snapshot::the_client_fixtures_are_current`, and the
band integral does not. **`make rebaseline` + `git diff` is NOT the guard
here** — it regenerates nothing carrying a daylight-derived colour and
returned an empty diff under the gross mutation too. The live guard is the
fixture test above.

**The node count is a permanent contract** — change it later and every colour
moves — so it was chosen by measurement. Worst-band error against a
4097-node reference, relative to the `3.9e-3` `u8` step:

| nodes | 1900 K | 1100 K | 900 K | 800 K | 700 K |
|---|---|---|---|---|---|
| 5 | 5.9e-05 | 1.6e-03 | **4.1e-03** | **6.9e-03** | **1.2e-02** |
| 9 | 3.7e-06 | 1.0e-04 | 2.7e-04 | 4.6e-04 | 8.5e-04 |
| **13** | 7.3e-07 | 2.0e-05 | 5.3e-05 | 9.3e-05 | **1.7e-04** |

**13 nodes**, which stays ≥20× below quantization down to 700 K — a dull red
glow, colder than anything this spec names — so a later ember or forge cannot
force the constant to change. Five nodes already fails by 900 K. Cost is 130
`exp` calls per illuminant, derive-once.

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
surface is unchanged **after the scotopic term and after the §5.2 sampling
change**. A requirement, with a test — and the test is
`the_client_fixtures_are_current`, which §5.2 mutation-proved is the live
guard. Note the direction: H3 constrains what the *new terms* may do to the
old colours. It is not a claim that no colour may ever change.

**H4 — a cell's light field can reach zero, and the rod still carries a
signal there.** Stated at the **model** level, deliberately: below an
authored photopic threshold a human's emitted colour is `[0,0,0]` where a
kobold's is not, probed on `to_srgb` at a stated illuminance. *Falsified if
the threshold that achieves this perturbs daylight* — in which case the term
is wrong.

**H4a — reachability, reported not predicted.** How dark does a chamber cell
actually get under the implicit torch? §4.2 makes this genuinely uncertain:
the lit set equals the FOV set, so only attenuation darkens anything, and
chambers are small. **This is a reading, not a claim** — it may report that
H4's regime is unreachable on the chamber band, which is a finding about
where the campaign's drama lives, not a failure. **The attenuation constant
may not be tuned to make it come out otherwise** (§11, risk 2).

**MEASURED — H4 held, H4a read negative, and both deserve their qualifiers.**

*H4 held, by exactly one byte.* At an illuminance of `1.6e-6` a human emits
`[0,0,0]` and a kobold does not, and the kobold's three slots are **equal** —
which is the assertion that matters, because it proves the pixel came from the
rod path rather than from a cone channel that happened to survive. But the two
eyes share one rod curve scaled by 1.5, so **there is no illuminance at which
the human is black and the kobold is bright**. H4 establishes that the cliff
*exists*; its *height* is carried by the hue test, where the rod puts 29 (human)
and 38 (kobold) on the screen against a required floor of 20.

*Two constants turned out to be load-bearing rather than decorative*, and both
are the wrong-attribution shape:

- `SCOTOPIC_NORM` is the **standard** rod's, shared across observers. Deriving
  it per-observer would divide a species' `scotopic_gain` straight back out, and
  a kobold would render pixel-for-pixel identical to a human — night vision
  measured correctly and attributed to nothing.
- `SCOTOPIC_GAIN = 1000` is required. Measured: at unit gain the rod's image
  falls below one screen count *everywhere in its own regime*, so the term would
  have shipped green and done nothing — precisely the defect it exists to
  remove.

**H4a: `[2, 2, 0]` is the dimmest visible chamber cell across the sweep**, four
times the photopic threshold and two bytes clear of the floor. So **H4's regime
is unreachable on the chamber band**, as §4.2's geometry predicts. Pinned as an
inverted tripwire — the test asserts *zero* achromatic cells, since three equal
slots are the signature of the scotopic path having run — so a future red there
is a finding to read, never a constant to move.

*Why H4 is split.* An earlier draft asserted H4 at the emitted-colour level
on the chamber band, where it may be unreachable in practice — a true
measurement attached to the wrong subject, which is the §10 shape exactly.
Splitting it keeps the falsifiable half falsifiable and reports the rest
honestly.

**Every claim above is measured on UNLENSED colour** (§7).

## 7. The lens

§5.2 chose accuracy in the model. The look is then recovered where it belongs:
a **lens** — a presentation-layer filter over the emitted colour that
optimizes for legibility and mood (owner's call, 2026-08-07).

This is the project's own spine rather than an invention: decision 0022 (the
sim emits data, clients render), The Beholding's shipped CLI colour lens, and
The Idioms' Orrery render-style layer. Four constraints:

1. **One-way and downstream of `sense()`.** The lens never feeds back into the
   model, the ledger, or a fact. Aesthetics must not contaminate physics —
   that separation is the whole reason the split is worth having.
2. **It transforms the emitted triple**, never the illuminant or the
   reflectance. Brightening an illuminant changes the world; brightening an
   output changes the picture.
3. **Disclosable and defeatable**, per `RENDER-9`. An unlensed mode is what
   makes this a lens rather than a lie.
4. **Built LAST, and never on during measurement.** §6's claims read unlensed
   colour. H1 is the claim that can genuinely fail — if bedrock varies too
   little, stone walls all look alike — and a saturation-boosting lens would
   hide exactly that. Honest path first, measured, then made pretty. It is
   also the only way to tell whether the room looks right because the model
   works or because the filter is doing the work.

**Lensed colour must never land in a committed artifact.** If it did, the lens
parameters would become a save-format-class contract. Screen only.

## 8. Testing

- Every guard states what would make it fire and is mutation-proven. The last
  campaign shipped seven green-and-vacuous guards, **all from plan text**.
- **Drive the whole seam** in one test — fabric → palette → light field →
  `sense` → emitted colour. Unit tests at each node with none on the path is
  the shape that hid The Beholding's `sightOf` defect.
- The additive law gets a **positive control**: two sources give a strictly
  brighter cell than either alone.
- Sweep seeds; never pin one.

## 9. Out of scope

- **The underworld.** It has no chart of its own (§10); fungi and lava wait for
  one. The emitter mechanism lands here so that campaign is a data change.
- **Dirt vs paved roads.** Nathan asked for these; they are *outdoor*
  surfaces on the walk band, which belongs to the surface/cover campaign
  parked on `the-overburden`. **Named here so it is not silently dropped.**
- **A carried light made explicit**, temporal decay (a hearth burning down),
  and a true emission term in `sense()`. Registry rows.

## 10. The finding that re-cut this campaign

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

## 11. Risks

1. **Fabric derivation may be too coarse to vary.** If most settlements sit on
   similar rock, H1 fails and the campaign ships stone walls that all look
   alike. Measure before claiming — **in the first task, not at the readout**,
   since H1 needs only lithology and no light at all. §7's lens would hide
   this failure, which is why the lens is built last.
2. The attenuation constant and the scotopic threshold are the two authored
   parameters, and **neither may be tuned after unblinding**. Attenuation is
   no longer merely cosmetic: per §4.2 it is the sole source of light gradient
   under the implicit torch, so H4a rides on it directly.
3. **The chamber fixtures move** when the palette fills. Re-pin in the
   drifting commit, never at the close. The guard is
   `hornvale-vessel::session_snapshot::the_client_fixtures_are_current`;
   **`make rebaseline` + `git diff` does not see colour** (§5.2 proved this
   with a gross mutation that left the artifact diff empty).
4. `the-delvers` is active and touches the underworld. Read its chronicle.
   Checked at G3: **no file overlap** with vessel, colour, illuminant,
   lithology or interior.
