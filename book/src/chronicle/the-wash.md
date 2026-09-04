# The Wash

A wash is a thin layer of dilute colour laid over a drawing. A watercolour is
built from several of them, each transparent, each doing one job. It is the
oldest answer to a problem this campaign had to solve twice: how to add
information to a picture without destroying what is already there.

[The Hachure](the-hachure.md) fixed the world map's geometry — it reads
elevation between the terrain mesh's samples now, and draws rivers as lines
rather than painting them as slabs. This campaign fixed its **ink**. And the
first thing it had to fix was a diagnosis.

## The map was starved, not coarse

The plate's colour came from six constants indexed by elevation band:

```rust
const RELIEF_COLORS: [[u8; 3]; 6] = [ /* blue, teal, green, olive, brown, white */ ];
```

The glyph ladder was indexed the same way. So both channels carried the same
quantity — height — and biome carried nothing. A tropical rainforest and a
high desert at 1200 m rendered identically.

This is a **hypsometric tint**, the school-atlas convention where green means
*low*, and it carries that convention's famous defect: readers see green and
infer vegetation. A hypsometric map paints the Sahara green because the
Sahara is low. Hornvale was doing the same thing for the same reason.

The obvious repair is to key colour on biome instead. That was the campaign's
original design, and it was wrong — not incorrect, but redundant. Hornvale
already had the answer.

## What was already there

`kernel/src/color.rs` — shipped by an earlier campaign called The Pigment —
models colour properly, and states its own architecture better than a summary
could:

> Colour is not a property of an object. A material has a *reflectance* — the
> fraction of light it returns per wavelength, identical in a cave and at
> noon. Light has a spectrum. An eye has sensitivity curves and collapses the
> arriving mixture to one number per channel. Colour exists only where all
> three meet, which is why every observer variation (species vision, colour
> blindness, a screen reader taking none of it) is the same operation with a
> different observer.

Ten spectral bands. `Reflectance`, `Illuminant`, `Observer`, `Signal`. And it
was **wired**, across five crates: ground reflectance per place from
`windows/locale`, a spectral *mixture* of cover endmembers, rock reflectance
from lithology, built-material reflectance for masonry, daylight and
golden-hour attenuation from astronomy, a light-field with falloff and
occlusion in `windows/vessel`, and a per-species observer resolved from a
creature's eyes.

The room-scale view ran on all of it. The world map ran on none of it. `git
grep` over the game client for any spectral type returned **zero**.

So the defect was never missing data. It was a **missing consumer** — and the
campaign that set out to build an appearance system found one already built,
one directory over, in use by a different view of the same world.

## Reflectance, illuminant, observer

What the map does now, per tile:

```
reflectance   the ground's own spectral curve, from the locale
illuminant    daylight attenuated by the sun's altitude — ONE per draw
observer      the terminal's own, which owns the collapse
──────────────────────────────────────────────────────────────
channel       observer.collapse(reflectance × illuminant)
```

The sim supplies **spectra**; the client supplies the **observer** and owns
the quantization. No sim code emits an RGB triple. This is the kernel's
existing quantize-at-emit-only discipline applied to ink: the world says what
the ground returns and what light falls on it, and the display says what a
screen can show.

The glyph is untouched. It still carries elevation order, which was correct
all along — a glyph carries *order*, colour carries *category and substance*,
and the two had simply been carrying the same thing.

Colour degradation stopped being a special case. `NO_COLOR`, a 16-colour
terminal, and a colour-vision variant are three **observers** over one
pipeline, exactly as a creature's eyes are. The terminal is an observer.

## One illuminant, and where it stands

The design said the illuminant was "diurnal and uniform across the plate", so
computing it once per draw was a free optimisation. That was false, and the
campaign nearly shipped it.

Solar altitude depends on latitude **and** hour angle. A plate at globe rung
spans the entire planet — every latitude, every longitude — so a single
illuminant lights the night side as if it were noon. There is no
rung-independent sense in which one illuminant is correct for a whole map.

The resolution is not an approximation dressed as a fact. It is a stated
cartographic convention: **the map is lit as it is where the reader stands.**
One illuminant per draw at the observer's own latitude and hour, applied
uniformly — which makes one computation correct *by construction* rather than
by luck. The room-scale view had been doing exactly this for one observer all
along; the map does the same thing at a scale where the approximation is
weaker, and says so.

What that costs is written into the spec rather than left to be discovered:
**no terminator sweeps the map.** At a coarse rung the far side of the world
carries the reader's sunlight. A map is a document you consult, not a
satellite photograph. A terminator would mean a per-tile illuminant and a
different campaign.

## Rate is the cache key

Colour turned out not to be one thing. It is a stack of layers at different
rates: cover is geological, snow and chlorophyll are seasonal, sunlight is
diurnal, weather would be per-turn, ornament ticks at 300 ms, the cursor is
instantaneous.

And that ladder was **already the cache ladder**. Decision 0289 makes the map
layers with distinct cache keys, and those keys already sat at exactly these
rates — the terrain layer never invalidated, the perception layer per turn,
the marquee on a poll timeout. The rate spine only named what was there and
added one invariant:

> A layer may never read data that changes faster than its own rate.

That invariant is not decorative and it did not wait for a future campaign to
matter. It was load-bearing on the first day, in two directions at once.

**It caught a wrong declaration immediately.** The plan declared the terrain
layer *seasonal*, because reflectance is seasonal. But the drawn grid holds
an **ink**, not a reflectance — so the layer's rate is the fastest thing it
reads, which is the diurnal illuminant. The declaration was corrected by a
red before it was ever believed.

**And it named the shape of the campaign's one Critical defect.** Adding
season and illuminant to the tile cache's key made lookups correct — and left
eviction blind. Nothing dropped a tile of a superseded *light*. Measured: 1600
tiles resident against a bound of 320, growing eight per light, never
recovering, while the cache's own capacity test passed because it drove
everything under a single fixed light. An existing invariant, violated on the
shipped path, by a test that could not see it.

The lesson the implementer wrote into the fix is the one worth keeping: **a
new key column is a change to eviction, not only to lookup.** And its own
earlier report had named that area and filed it as missing *coverage* — when
it was a missing *fix*. "Untested" is the comfortable misfiling of "broken".

## What was measured

Both preregistered predictions held, which is worth stating precisely because
this project ships nulls as headlines and did not have to here.

**The diurnal arm.** A low sun is measurably warmer than a high one — not by
the test's arithmetic but by the physics underneath it: atmospheric
attenuation goes as `exp(-K·airmass·(λ_ref/λ)⁴)`, so a low sun's longer path
suppresses short wavelengths far more than long ones, and the long/short band
ratio must rise as the sun drops.

**The seasonal arm**, measured over a population rather than an anecdote: of
40,962 grid facets on seed 42, **77** are snow-covered at midwinter and not
at midsummer. Winter albedo exceeds summer at **77 of 77**, ratio median
2.28. Two red controls, run against production code and restored, established
that the sample-validity clause and the directional clause fail independently
— and the reviewer showed that independence is *structural*: the dominance
computation reads a cover's weight, while albedo reads its reflectance, so
darkening snow cannot move dominance at all.

The measured quantity was **substituted** during the campaign, and the spec
says so. The frozen preregistration named a snow-endmember weight; that
proved unreachable from the client, and composed albedo — what actually
reaches the map — was measured instead. The substitution was forced by
reachability and settled before any result was seen. But an unrecorded swap
is indistinguishable from metric-chasing, so it is recorded.

## What this campaign did not do

It invented no detail. [The Hachure](the-hachure.md) deferred a coherent
noise field below the terrain mesh's floor, and it stays deferred, because
inventing information for a channel that is discarding the information it
already has is the wrong order. The map now consumes what the world knows;
what it should *imagine* is a later question.

It ships no ornamental layer. The rung exists, the mechanism exists — the
client already animates its marquee on a poll timeout, so the clock was never
missing — and nothing rides it yet. Ocean surface motion is the obvious
first, and it is a separate campaign.

And the overture's atlas view lost its land colour. It has no locale context
to ask, and building one would cost roughly 200 ms on the startup path, so it
passes nothing and its land renders uncoloured — measured at 10.6% of that
panel, the ocean being unaffected. That is a real regression, contained and
reversible, and it is written down rather than discovered later.
