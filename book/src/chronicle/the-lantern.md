# The Lantern

[The Beholding](./the-beholding.md) coloured the world through the possessed
species' eyes, and left one slot in its own output deliberately empty:

```rust
pub struct PaletteEntry {
    pub color: Option<[u8; 3]>,   // always None
}
```

The chamber band — the floor plan you see when you `enter` a building — had a
pane, a plan, and a colour field that nothing ever filled. The reason was
honest: `CellKind::Wall` carried no material, so there was no reflectance to
read; and there was no interior illuminant, so lighting a room with the noon
sun would have been the invented effect the project forbids itself.

This campaign fills the slot. Doing so required both halves at once, and the
recognition that it required both is the first thing worth recording.

## Materials and light are one campaign, because the graph does not split

The design opened as two campaigns — a *building fabric* campaign and an
*interior light* campaign — and the first draft of the spec split them. The
split does not survive its own structure:

```
  fabric  ──derives──▶  reflectance ──┐
                                      ├──▶ sense() ──▶ the pixel
  sources ──derives──▶  illuminant ───┘
```

`sense()` is a cut vertex. Remove either input and the output is undefined: a
material campaign has nothing indoors to be lit by, and a light campaign has
nothing for the light to fall on. Neither ships a coloured room alone, and a
coloured room was the whole ask. The two were re-cut into one before any code
was written.

## The dark needs no new geometry

The keystone came from negating each definitional property of "light source" in
turn, and the second negation is the one that mattered.

*Negate emitting* and you get an absorber — a shadow — and the shadow already
shipped. [The Sighting](./the-sighting.md) built `shadowcast(lattice, from,
radius)`, symmetric integer field-of-view. Because it is **symmetric**, "what
can see this cell" and "what light reaches this cell" are the same set. Light
propagation therefore needed no new algorithm at all: a light field is the
shipped FOV, run once from each source and summed band-wise.

```rust
pub fn light_field(lattice: &Lattice, sources: &[Source]) -> BTreeMap<Cell, Illuminant>
```

One detail in that signature is a modelling claim rather than an
implementation choice. **An unreached cell is absent from the map, not present
with a zero illuminant.** The two render identically and they are different
models: `illuminant × reflectance × observer` over an absent cell correctly
yields nothing at all. A `BTreeMap` entry holding `[0.0; BANDS]` would pass a
screenshot and fail the model.

Light is a *derived view*, never stored: no seed label, no epoch, no new draw.
Sources are things (a hearth, a doorway, a torch); light is a field over cells,
recomputed from them.

## What a wall is made of

`Fabric` derives from ground that already ships — lithology, biome, and the
soil buffer — through categorical rules rather than thresholds:

| fabric | what admits it |
|---|---|
| stone | competent bedrock (the complement of the too-weak rock classes) |
| timber | a forested biome |
| cob | more than a metre of regolith to win the earth from |
| thatch | the fallback for a floor |

The rock class is read categorically for the same reason
`LocaleContext::reflectance_at` reads it categorically: averaging granite with
basalt names a rock that is not there. And a threshold — a doorway — gets no
fabric at all, because an opening is not a material. That `None` travels all
the way to the browser, where the client draws it uncoloured.

## H1: the stone varies, and the flagships do not

**H1 — two settlements on different bedrock produce visibly different stone
walls** — was preregistered as the claim that could genuinely fail. The
Beholding had already shown that bedrock variation is not automatically
visible: its own surface measurement came back 2 `u8` steps out of 255 on real
terrain, against 28 on authored fixtures.

Measured across 1505 settlements over eight seeds, under a 5800 K reference
light:

```
  max channel spread    102 u8 steps
  distribution          p10 1, median 41, max 102   (1,131,760 pairs)
```

H1 held comfortably. But **the tenth percentile is 1**, and that number turned
out to be the more interesting one. A separate sweep of four seeds found that
*every* flagship settlement — the one a possession starts in — stands on
**alluvium**. Both readings are true and they are not in tension: fabric varies
richly across the settlement *population*, and it is the flagships that
cluster, plausibly for a real reason, since alluvium is river valley and
[The Confluence](./the-confluence.md) measured settlements condensing near
fresh water.

The consequence is about players rather than metrics. H1 measured the
population; a possession samples the head of it. The *typical played
experience* of this campaign is alluvium walls, even though the world's stone
genuinely varies. That is a true fact about where people build, and the lever
that would change it is where a possession starts — not what fabric derives
from.

## The blackbody moves down, and becomes an integral

A torch is the same function as a star at a different temperature, so
`planck_relative` — private in `domains/astronomy`, used only by `daylight` —
had to become reachable from a window. Making it `pub` where it stood would
have made a hearth import the astronomy crate. The rule that settled it
generalizes:

> **A spectral law that takes no world-state belongs to the kernel; a law
> parameterized by domain state stays in its domain.**

It classifies the three existing functions with no residue: `planck_relative(nm,
kelvin)` takes no world-state and moves to `kernel::color`; `at_elevation` takes
a sun's elevation and `daylight` takes a `Star`, and both stay in astronomy.

Then the sampling rule was re-derived rather than inherited, and the
measurement overturned the expectation. `daylight` samples each 40 nm band at
its midpoint, justified in its own doc "at main-sequence temperatures". At
flame temperatures the visible range is not near the Wien peak — it is the
steep, strongly convex Wien *tail*, and a midpoint sample underestimates a
convex mean badly:

| T | Wien peak | worst per-band error |
|---|---|---|
| 5800 K | 500 nm | 0.26 % |
| 1900 K | 1525 nm | 10.3 % |
| 1100 K | 2634 nm | 34.2 % |

Band ordering survives at every temperature, so the ordering claims were never
at risk. The rendered triples were not. A midpoint rule 34 % wrong at 1100 K
cannot be the basis for a campaign whose subject is what a hearth looks like,
so the sampler became a 13-node Simpson integral over each band.

**The node count is a permanent contract**, because changing it later moves
every colour, so it was chosen by measurement rather than taste. Against a
4097-node reference, relative to the `3.9e-3` size of one `u8` step:

| nodes | 1900 K | 1100 K | 900 K | 800 K | 700 K |
|---|---|---|---|---|---|
| 5 | 5.9e-05 | 1.6e-03 | **4.1e-03** | **6.9e-03** | **1.2e-02** |
| 13 | 7.3e-07 | 2.0e-05 | 5.3e-05 | 9.3e-05 | 1.7e-04 |

Thirteen stays at least twenty times below quantization down to 700 K — a dull
red glow, colder than anything the campaign names — so a later ember or forge
cannot force the constant to change. Five already fails by 900 K.

The change cost nothing. **H3 required that every colour The Beholding emits
be byte-identical after the new terms**, and it was: at 5800 K the midpoint
error is `1.6e-3` relative, below a `u8` step, and no committed colour sat on a
rounding boundary. The flame-lit colours are new, so there was nothing there to
preserve.

## The darkness that is not reachable

**H4 — a cell's light field can reach zero, and the rod still carries a signal
there** — was stated at the *model* level, and held by exactly one byte. At an
illuminance of `1.6e-6` a human emits `[0, 0, 0]` and a kobold does not, and
the kobold's three slots are **equal**, which is the assertion that matters: it
proves the pixel came from the achromatic path rather than from a cone channel
that happened to survive. This finally cashes the rod channel The Beholding
built and never reached — its own comment said the scotopic gain "exists so a
later naming campaign has the axis."

Two constants in that term turned out to be load-bearing rather than
decorative, and both are the same shape of near-miss:

- `SCOTOPIC_NORM` is the **standard** rod's, shared across observers. Deriving
  it per-observer would divide a species' `scotopic_gain` straight back out,
  and a kobold would render pixel-for-pixel identical to a human — night vision
  computed correctly and attributed to nothing.
- `SCOTOPIC_GAIN = 1000` is required. At unit gain the rod's image falls below
  one screen count *everywhere in its own regime*, so the term would have
  shipped green and done nothing — precisely the defect it exists to remove.

**H4a asked the separate, genuinely uncertain question: how dark does a chamber
actually get?** It was preregistered as a *reading*, not a claim, and it read
negative — which is the campaign's most useful structural finding.

Because `shadowcast` is symmetric and the possession carries an implicit torch,
**every visible cell is lit by construction**. The torch's radius and the sight
radius are necessarily one quantity: if the torch reached less far than sight,
there would be cells you can see that nothing illuminates, which is not dim but
incoherent. (`SIGHT_RADIUS`'s own doc, written before this campaign existed,
had already left the seam: "the day a light model arrives there is exactly one
place to replace.") So the only thing darkening anything is distance falloff,
and the darkest visible cell is the one at the sight radius, at `1/(1 + 4²)` of
full.

Measured across four seeds, the dimmest visible chamber cell renders
`[2, 2, 0]` — four times the photopic threshold, two bytes clear of black — and
**zero** cells in the sweep are achromatic. H4's regime is unreachable on the
chamber band.

That is a finding about where the drama lives, not a failure. It is what makes
the **hearth** matter: a second source at a different colour temperature is the
only thing in the band that can break a purely radial gradient. And it fixes
the attenuation constant in place — a later campaign that moves it to
manufacture a dark cell would be tuning the instrument to the answer. The
reading is pinned as an *inverted tripwire*: the test asserts *zero* achromatic
cells, so a future red there is a finding to read rather than a constant to
relax.

## The hearth had no cell

`AnchorKind::Hearth` has shipped since The Hearth, documented as "a fire: emits
warmth and light." Placing a light at it turned out to be a task of its own,
because **`Cell` does not appear anywhere in the interior model**. The interior
is *topological* — anchors and the relations between them; the lattice is
*spatial* — cells. Nothing joined them. "The hearth is already placed" was true
about the anchor graph and false about the lattice, which is the model a light
must be positioned in.

`CellKind::Wall`'s own doc had stated the intent all along — "a place in its own
right — an alcove, a screen or **a fireplace** is an anchor at one of these" —
so the model always meant a hearth to sit at a wall cell; only the join was
missing. It is derived deterministically from the chamber's wall ring. A
derivation is legal for a window; a seeded draw would not be, so it consumes no
stream.

**H2 — a hearth-lit cell and a doorway-lit cell in the same room differ** — was
then measurable on derived fabric rather than on an authored swatch. On all four
seeds, at cells equidistant from both sources:

```
  seed    1  on Alluvium:  hearth [6, 2, 0]   doorway [25, 24, 19]
  seed    7  on Alluvium:  hearth [6, 2, 0]   doorway [26, 25, 17]
  seed   42  on Alluvium:  hearth [3, 1, 0]   doorway [15, 15, 10]
  seed 1024  on Alluvium:  hearth [6, 3, 0]   doorway [25, 24, 20]
```

A 1200 K ember bed against daylight through an opening: the hearth-lit cell is
deep red and nearly out of blue, the doorway-lit cell is near-neutral. The two
lights in one room are what a `u8` triple can distinguish.

## Accuracy in the model, the look in a lens

Choosing the band integral was choosing accuracy over prettiness, and the
answer to prettiness was to put it somewhere it cannot contaminate anything: a
**lens**, a filter over the emitted triple, downstream of everything.

This is the project's own spine rather than an invention — decision 0022 (the
sim emits data, clients render), The Beholding's CLI colour lens, and
[The Idioms](./the-idioms.md)' Orrery render-style layer. Four constraints, and
the fourth is the one that protects the campaign:

1. **One-way and downstream of `sense()`.** `apply` takes three bytes and
   returns three bytes; there is no other entry point, so there is nothing for a
   feedback path to be written through.
2. **It transforms the emitted triple**, never the illuminant or the
   reflectance. Brightening an illuminant changes the world; brightening an
   output changes the picture.
3. **Disclosable and defeatable.** `--lens off` is the exact identity, and the
   drawn plan names the lens it drew through.
4. **Built last, and never on during measurement.** Every claim above reads
   *unlensed* colour. A saturation boost applied earlier could have rescued H1 —
   precisely the failure the campaign exists to be able to detect — and no one
   could then tell whether the room looks right because the model works or
   because the filter is doing the work.

The obvious lens is a contrast or saturation boost. Both were refused by
measurements the campaign already held. H1's `p10 = 1` means any transform whose
slope drops below 1 erases the tenth percentile outright while the median goes
on looking fine. And H4a's finding is that the chamber never reaches black, so
the room's legibility problem is not contrast at the top but **crushed detail at
the bottom**. The curve is therefore a shadow expansion with **slope ≥ 1 across
the entire range the model produces**, giving up separation only above a ceiling
nothing has ever been seen to reach. Its two segments meet at slope 1 by
derivation rather than by tuning — the shadow gamma is pinned to
`KNEE / (KNEE + LIFT)` — and a guard asserts the relation, so retuning one
constant cannot silently break the join.

It carries **no hue term**, deliberately. Seed 42's possessed bugbear is a
dichromat: its projection writes the same value into the red and green slots of
every triple. A warm tint — the obvious way to add mood — would hand that eye a
red/green distinction its own physiology never produced, which is a lie about
the observer rather than a filter over the picture. So it is one scalar curve
applied identically to all three slots, and the warmth stays where it is already
physical: in the 1900 K torch and the 1200 K hearth the model actually lights
the room with.

The defaults then point opposite ways, which is the whole point.
`PossessOpts::default()` is **unlensed**, because a possession's output is
routinely captured — the book's gallery transcripts are `possess --script`
output, and the client fixtures are snapshots of a default session — and lensed
colour must never land in a committed artifact. The interactive CLI path opts
*in*. The wasm ABI is unlensed too, and that is the boundary where it matters:
decision 0055 runs the determinism guarantee up to and including the ABI, so the
sim hands the client the model's own bytes and what the page does with them is
the client's business.

## What the client did not have to change

The Beholding's plumbing prediction was that the campaign filling the colour
slot would need "no client change, only a colour on the wire." That held
exactly. The browser client's diff for this campaign is a comment.

## What is deferred, and why the deferral is now load-bearing

An emitter — a luminescent fungus, a lava flow — is modelled here as an
illuminant *at its own cell*. It needs no new term in `sense()`, no change to
`Observer`, and it gets the visible result right: green light pools around the
fungus and the black rock beside it reads green.

It is also, precisely, wrong about autonomy. An illuminant is externally
driven; an emitter is self-driven. Modelling the second as the first means
**the cell's own reflectance filters light the cell is supposedly producing**,
and the error scales with how dark the emitter is:

| emitter | own reflectance | renders |
|---|---|---|
| fungus | pale, greenish | green — benign |
| lava | basalt, near zero in every band | **nearly black, while its neighbours glow red** |

So the model is right for fungi and visibly wrong for lava. Lava and fungi were
already out of scope, since the underworld has no chart of its own to appear
on — but that deferral now carries a *correctness* reason rather than only a
scope one, and the campaign that lights a cave must not pull lava in without a
real emission term first. It is registered, cross-linked to the row that already
named `EMIT` as a verb a `[0, 1]` scalar cannot express.

The sibling gap is the same shape from the other side. The four spectral roles
are reflector (shipped), absorber (`shadowcast`, shipped), scatterer
(`at_elevation`, shipped) and **transmitter** — and the transmitter is the one
still modelled as a bare number: this campaign's doorway is daylight attenuated
by a scalar. A canopy passes green, ice passes blue, a stained window passes
whatever it was made to.
