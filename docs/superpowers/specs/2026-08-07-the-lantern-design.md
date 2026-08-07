# The Lantern — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-08-07
**Campaign:** The Lantern — coloured light underground, and the dark between.
**Measured on:** `64c0daac` (main, The Beholding's close).

## 1. The question

Nathan's ask, verbatim: *"very richly colored underworlds that 'reflect' the
color of a torch, of luminescent fungi, of lava."* Aesthetics first.

The Pigment built colour as `illuminant × reflectance × observer`. The
Beholding wired the *observer*. Nobody has ever varied the **illuminant**
except by moving the sun. Underground there is no sun, and so — today —
there is no light at all: a cave's cells are coloured by the same daylight
that falls on the surface, which is simply false.

**This campaign is cheap and dramatic in the same measure**, because the
machinery is already multiplicative. Measured, before this spec was written
(`windows/worldgen/tests/lantern_probe.rs`, no world build) — one limestone
wall, five lights, human eye:

| light | | sRGB |
|---|---|---|
| daylight 5800 K | near-white | `[228, 230, 223]` |
| torch 1900 K | warm amber | `[136, 111, 39]` |
| hearth 1200 K | deep ember | `[92, 60, 3]` |
| lava 1100 K | darker red | `[85, 53, 2]` |
| fungi ~490 nm | cool blue-green | `[111, 142, 179]` |

The same stone. Nothing about the wall changed.

## 2. What ships, and has never been used

- **`AnchorKind::Hearth`** — doc: *"A fire: emits warmth and light; the
  canonical gathering place."* Placed world-state since The Hearth. Nothing
  has ever read it as a light.
- **`illuminant::planck_relative(wavelength_nm, t_kelvin)`** — a general
  blackbody, **private**, used only by `daylight`. A torch is the same
  function at 1900 K. Making it public is this campaign's cheapest step.
- **`shadowcast(lattice, from, radius) -> BTreeSet<Cell>`** — The Sighting's
  symmetric integer shadowcasting. Its own doc already says *"A radius below
  1 **lights** the origin alone."*
- **`terrain::cave_at(cell) -> Option<Cave>`** with `CaveKind` and
  `deepest_band` — the underworld this campaign lights.
- **`pack_depths(p).luminance`** and the lexicon's luminance ladder
  (`gloom`, `shadow`, `starlit`). A kobold has words for darkness a human
  lacks, and nothing has ever had occasion to use one.
- **The achromatic channel** (The Beholding), whose scotopic gain comment
  reads: *"it exists so a later naming campaign has the axis."*

## 3. The model

### 3.1 Light propagation is the shipped shadowcaster

Because `shadowcast` is *symmetric*, "what can see this cell" and "what light
reaches this cell" are the same set. **Light needs no new geometry.** A
source's reach is the FOV run from its cell at its radius; a cell's incident
light is the sum over the sources that reach it.

That sum is the **additive law**, which the kernel already declared and
deferred to exactly this campaign (`color.rs:135`): *"Additive (not
implemented; arrives with multi-light): two torches on one wall. **Sum the
illuminants**, not the reflectances."* So the composition rule is precedent,
not invention.

Attenuation with distance is `1/(1+d²)`-shaped and authored; it is the one
free parameter here and the spec says so.

### 3.2 Two families of emitter

```
blackbody(t_kelvin, peak)   torch 1900K, hearth 1200K, lava 1100K
narrow(shape, peak)         fungi ~490nm — NOT a blackbody
```

Bioluminescence is a narrow emission line, not a hot body, and this is
precisely why `Illuminant` being a full spectrum rather than a colour
temperature earns its keep. Both families are `Illuminant`; nothing
downstream distinguishes them.

### 3.3 Where sources come from — derived, never drawn

**No new seeded draw, and therefore no epoch.** Each source is read off
world-state that already exists:

| source | placed by | spectrum |
|---|---|---|
| hearth | `AnchorKind::Hearth`, already in every built interior | blackbody 1200 K |
| lava | `CaveKind` lava-tube caves in mafic rock, already classified | blackbody 1100 K |
| fungi | **derived** from cave conditions — wet, deep, organic | narrow ~490 nm |
| daylight leak | a cave entrance's distance from the surface | the sun, attenuated |

Fungi are the only genuinely new placement, and they are *derived* from
`cave_at` plus moisture and depth rather than drawn, which is what keeps this
campaign epoch-free. Light itself is a **derived view over cells**, computed
per turn and never stored — the same posture the NPC layer already takes.

### 3.4 The keystone: darkness, and the channel that can't be seen

The interesting half of a lighting campaign is the dark, and this is where
the campaign has real design content rather than plumbing.

**Measured:** in near-darkness a human and a kobold go black *together*,
while their rod signals genuinely differ:

```
light   human sRGB   kobold sRGB   human rod   kobold rod
1.000   [136,111,39] [128,128,39]     0.2919      0.4379
0.010   [  8,  5, 1] [  7,  7, 1]     0.0029      0.0044
0.001   [  1,  1, 0] [  1,  1, 0]     0.0003      0.0004
```

The rod is `ChannelRole::Achromatic`, so **no projection reads it** — by
construction, correctly, because a rod carries no hue. The consequence is
that night vision cannot currently reach the screen at all, and a kobold in a
dark cave sees exactly what a human sees: nothing.

**The addition this campaign owes.** Real vision is *mesopic* in the
transition: as the photopic channels fall below usefulness, rod response
takes over and delivers **luminance without hue** — which is why night vision
is grey. So `to_srgb` gains a scotopic term: below an authored photopic
threshold, the achromatic channel contributes to all three output slots
equally, blended in as the photopic signal fades.

This keeps every existing guarantee. The rod still carries no hue — it
contributes *equally* to R, G and B, so it can never shift one. In daylight
the term is zero and **every colour The Beholding emits is unchanged**, which
is a byte-identity requirement, not an aspiration.

It also answers the Pigment's open **risk 5** — *"whether the luminance
ladder (gloom/shadow/starlit) should preempt the hue ladder below some
threshold is unresolved"* — with a threshold that is now a real, measured
quantity rather than a guess.

## 4. Determinism and epoch analysis

**No epoch.** No seeded draw, no `streams.rs` constant. Sources are read off
placed anchors and classified caves; fungi are derived; light is a view.

| change | verdict |
|---|---|
| `planck_relative` made `pub` | additive; no output moves |
| `blackbody` / narrow-emitter constructors | additive |
| light field from `shadowcast` | new derived view, nothing stored |
| the scotopic term in `to_srgb` | **must be zero in daylight** — pinned by a byte-identity test against The Beholding's emitted colours |
| interiors and caves gain an illuminant | their cells' colours change; today they are lit by a sun that is not there |

`planck_relative`'s doc justifies its midpoint sampling *"at main-sequence
temperatures"*. A torch at 1900 K is far below that range, so the
justification must be re-stated for the new range rather than silently
inherited — this is the same shape as the `snow_fraction` misreading that
cost The Overburden its framing.

## 5. Preregistered claims

Each names its substrate.

**H1 — the lights are distinguishable on real cave rock.** On **real
terrain**, for cave cells swept across seeds, the emitted colour under a
torch differs from under fungal light by more than a stated threshold in at
least one channel. *Already strongly indicated* by the probe on authored
limestone (`[136,111,39]` vs `[111,142,179]`); the claim is that it survives
real lithology, which The Beholding's 28-vs-2 result proves is not automatic.

**H2 — night vision reaches the screen.** With the scotopic term, at an
illuminance where a human's emitted colour is `[0,0,0]`, a kobold's is not.
*Falsified if the threshold that achieves this also perturbs daylight
colours* — in which case the term is wrong, not the threshold.

**H3 — daylight is untouched.** Every colour The Beholding emits at the
surface is **byte-identical** after the scotopic term lands. Not a
prediction; a requirement with a test.

**H4 — the dark is reachable.** Some cave cells in some worlds have **no
light source at all** and emit no colour. *Falsified if fungi or lava are
derived so liberally that true darkness never occurs*, which would mean the
derivation is decorative rather than physical.

## 6. Testing

- Every guard states what would make it fire and is mutation-proven. This
  project shipped seven green-and-vacuous guards in the last campaign, all
  from plan text.
- **Drive the whole seam** — source placement → shadowcast → summed
  illuminant → `sense` → emitted colour — in one test, not four unit tests
  with an untested path between them.
- Sweep seeds; never pin one.
- The additive law gets a **positive control**: two sources must give a
  strictly brighter cell than either alone.

## 7. Out of scope

- **A carried light.** No inventory exists; a torch you hold is a different
  campaign. Registry row.
- **Temporal decay** — a hearth burning down, a torch guttering. `WorldTime`
  shaped, deferred. Registry row.
- **A true emission term** in `sense()`. This campaign treats an emitter as
  an illuminant at its own cell, which conflates emission with
  self-illumination and gets the visible result right for no API change.
  Alchemy and magic will want the real thing. Registry row.
- **The walk band**, already lit at its real sun altitude by The Beholding.
- **Materials for walls and floors** (C3) and **the surface cover model**
  (C2, parked on `the-overburden`) — the rest of the programme.

## 8. Risks

1. **A possession that `delve`s into an unlit cave is in absolute darkness
   and the pane goes black.** Correct, dramatic, and possibly unwelcome.
   **This is a gameplay consequence and goes to Nathan at G3.**
2. The attenuation constant and the scotopic threshold are the two free
   parameters. Both authored, both stated; neither may be tuned after
   unblinding to rescue a claim.
3. Lava as a light source implies lava is *present*, not merely that the rock
   is basalt. `CaveKind`'s lava tubes are ancient, not molten. **The spec
   must not light a cold tube.** This is the campaign's most likely
   correctness error.
4. `the-delvers` is active and touches the underworld. Read its chronicle
   before absorbing.
