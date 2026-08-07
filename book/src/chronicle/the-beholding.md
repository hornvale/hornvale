# The Beholding

[The Pigment](./the-pigment.md) built a real spectral colour model —
reflectance times illuminant times observer, ten bands, an eye whose channel
count is deliberately a list rather than a fixed four — and then stopped one
line short of using it:

```rust
if !self.srgb_native || signal.get().len() != 4 { return None; }
```

**A non-human observer has no truthful sRGB image, and the kernel refused to
invent one.** That refusal was correct and it survives. But it meant the
observer slot the Pigment left open could not actually be filled: wire a
goblin's eye into a chart and the chart comes back grey, indistinguishable
from a build with no colour layer at all.

This campaign fills the slot. Possessing a bugbear means seeing the world in
bugbear colours — which required not making the projection *true*, because it
cannot be, but making it **sayable**.

## Three measurements, taken before anything was written

The design began with a probe that needs no world build, only the seven
authored hue exemplars. All three of its findings would otherwise have
entered the spec as confident assertions.

**A tiered eye derived from a tiered gate cannot tell its own species
apart.** The obvious derivation keys the eye to `pack_depths`' hue tier — the
same ladder that already decides how many colour *words* a species owns.
But hobgoblin (night vision 0.60), bugbear (0.70) and kobold (0.90) all land
on hue depth 3 or 2, and the first candidate gave all three the
byte-identical swatch set. A model with fewer species than the roster is not
a model of species. The fix was to keep the channel *count* on the tier and
make the *degree* of merging continuous in night vision, which separates
them.

**Signal distance is the wrong discriminability metric.** For a full
trichromat it put red and green at 0.025 relative separation — nearly the
closest pair in the whole exemplar set. Signal magnitude carries luminance
and luminance swamps hue. The metric that measures what an observer swap is
claimed to move is chromaticity: each channel's share of the total.

**And then the falsification.** The candidate dichromat did not confuse red
and green. Measured on chromaticity, human and bugbear separation were
indistinguishable — about 0.025 against 0.026 — and red/green did not even
appear among the eight pairs the bugbear lost most.

## Why a dichromat with a rod is a trichromat

The cause was specific and it is the most interesting thing the campaign
found. Green's exemplar peaks at 520 nm, which is exactly the scotopic
peak — so the retained rod channel carries the red–green distinction all by
itself. And `sense` treats every channel alike, so a "dichromat" that still
has a rod is a trichromat to any metric that counts all its channels.

Real dichromats have rods. What makes them dichromats is that a single
achromatic channel's signal cannot be told apart from intensity, so it
contributes no hue at all. **The model had no way to say that**, because an
observer had no notion of what a channel is *for*. The shipped `to_srgb`
already assumed one — its own comment read *"the scotopic channel carries no
hue and is not projected"* — but the assumption was hardcoded into one
observer instead of being something any observer could declare.

So channels gained roles. A channel is `Chromatic` or `Achromatic`; an
achromatic channel is read by no projection and counted by no chromaticity
metric. With the rod excluded, the same measurement on the same exemplars
under the same light reads **human 0.0680, bugbear 0.0541**. A dichromat
separates red from green less than a trichromat does.

It is worth being exact about the shape of that, because the tidy version is
a lie. The claim was frozen in the approved spec as **false**, with an
instruction to ship the null as the headline and explicitly not to retune the
merge to rescue it. It then came true — not by tuning, but because the
enabling change the falsification itself pointed at was built. No constant
moved. The honest summary is that the *first* measurement was a measurement
of the instrument rather than of the eye: the model was not failing to
produce dichromats, the metric was counting a channel that carries no hue.

## Every projection is a lie; cartography's answer is to name it

Putting a two-chromatic-channel signal on a three-channel screen loses
something, necessarily. The discipline borrowed here is the one map
projections have used for four centuries: do not search for a true
projection, **name the projection and state which invariant it preserves.**

A projection therefore carries a registered name, a sentence saying what
survives it, the three channel indices that drive red, green and blue, and a
per-channel normalizer. The standard observer's projection is called
`native`; a merged eye's is `yellow-blue`, and it preserves *"the
short-to-long opposition; the red–green axis is not carried."*

`yellow-blue` drives both red and green from the same merged channel, so
every triple it emits has **R equal to G exactly**. That is not an artifact
to be smoothed away. It is what a colour space with no red–green axis
honestly looks like on a screen that has one, and it is precisely what the
caption declares.

Two details are load-bearing rather than decorative. The normalizers are
**carried, not derived**, for the standard observer: its shipped constants
are rounded channel sums, and recomputing them live would move the last bits
of every colour that observer has ever emitted. And a projection cannot be
inferred from peak wavelength, which was the tempting shortcut — ranking the
standard observer's channels by peak gives long 600, medium 560, scotopic
520, short 440, so a "three longest peaks" rule would feed the **rod** into
blue and contradict the mapping the kernel already ships. With roles
declared, the question never arises.

## The eye a species implies

Channel *count* is read off the same hue ladder that decides a culture's
colour vocabulary, so the eye and the lexicon cannot disagree by
construction: a species that lacks a word for green lacks the channel that
would distinguish it. A full trichromat keeps four channels; an anomalous
trichromat has its medium and long curves each pulled halfway toward their
mean; a dichromat merges them into one.

The *degree* of merging runs continuously with night vision — 0.2 of the way
for a hobgoblin, 0.4 for a bugbear, 0.5 for a gnoll, 0.8 for a kobold — which
is what separates species that share a tier. The authored reasoning is the
one the hue ladder already states, with one addition: a species that sees
well in the dark spent less of its history straining at daylight hue
distinctions, and the trade is *physical*. Rod-dominant sight and a
compressed long–medium separation are the same adaptation seen from two
sides.

Two claims about this were frozen in advance and both hold.

**The human row is not privileged.** The derivation applied to human's
perception vector reproduces the standard observer exactly — same curves,
same roles, same projection, same emitted bytes. The standard observer stops
being a base case that everything else deviates from and becomes one row of
the roster that happens to be ours.

**Species sharing a night vision derive the identical eye.** Kobold and all
three chromatic dragons sit at 0.90, so a black dragon and a kobold see the
same colours. This is asserted rather than left implicit, because it is a
*stated consequence* of a model that reads exactly one axis, not an accident
— and it is the honest place for a later campaign to hang a clade its own
eye.

## The document says whose eyes

A client cannot caption what it cannot see, so the scene document carries the
declaration: the observer's name, its channel count, how many of those
channels are chromatic, the projection's name, what that projection
preserves, and the sun's altitude that lit the colours.

**Four of those six are overwritten by the builder** from the observer
actually used, discarding whatever the caller claimed. That overwrite is the
whole reason the block can be trusted. A caller can name an eye and state a
sun angle — the two things a set of sensitivity curves genuinely cannot
supply, since curves know neither their own species nor the time of day — but
a caller cannot make a document assert an arity or a projection its colours
did not come from.

Nothing about this is committed. Colour is derived at the emit boundary from
mineralogy and stellar mass, which the ledger already holds, so the campaign
owes no epoch, no stream label and no seed draw. Both new keys are omitted
entirely when absent, so a document built without colour is byte-for-byte
what it was before the layer existed.

## The light, measured

Out of doors the chart is lit by the real sun: daylight from the star's own
derived temperature, attenuated for the sun's altitude at the observer's hour
and latitude. All three pieces already shipped; the campaign only had to move
the "which light?" question to the point where the answer is known.

The prediction was that a low sun reddens the chart, with an explicit risk
that quantization to bytes would eat the effect. It does not. At seed 42's
flagship latitude, roughly 5.7° south:

| sun altitude | R:B ratio |
|---|---|
| 78.567° (noon) | 1.206 |
| 39.409° | 1.237 |
| 15.048° | 1.416 |
| 10.980° | 1.576 |
| **6.911°** | **1.917** |
| 2.841° | 5.455 |

The effect is real and strongly non-linear near the horizon, and that
non-linearity is itself a lesson. The campaign's first probe sat at
**−13.442°** — *below* the horizon, where the attenuation model clamps to its
maximum airmass — and reported a ratio of 31.0. That number is not wrong, but
it measures the clamp rather than Rayleigh attenuation, which is a weaker
claim than the hypothesis makes. The published pair is 6.911° against noon,
and the shipped test now asserts its low probe is above the horizon before it
compares anything.

At noon, a bugbear standing in the flagship's neighbourhood sees all
thirty-one cells as `[123, 123, 102]` — red and green equal, exactly as the
projection promised, the blue channel a fifth lower. Under a night sky the
same cells read `[31, 31, 1]`, and through a kobold's eyes `[30, 30, 1]`. One
byte apart, from a merge fraction of 0.4 against 0.8. This model produces
species that see *differently*; it does not produce species that see
*dramatically* differently, and the difference is a hue shift plus the loss
of an axis rather than a different world.

## What a possession sees now

The observer is chosen by name rather than by a toggle. `eyes kobold`,
`eyes human`, `eyes own`, `eyes off` — an unknown name fails loudly and lists
the roster rather than guessing. Naming matters for a reason beyond taste:
which species you possess depends on the seed's flagship, so a three-value
toggle would mean demonstrating that only the observer varies by *varying the
world*. With names, one world, one room, one hour, and the eye is the only
thing that moves.

`eyes off` declines the observer step entirely and restores byte-identical
output — the same posture a screen reader takes toward an image, since
withholding a channel is not the same as rendering it grey.

Seed 42's flagship agent is a bugbear, so the default possession already
looks through a dichromat's eyes. That is a happy accident: the path most
likely to be exercised is the one that would fail most visibly.

In the browser the panes now return grids of cells rather than lines of text,
one span per run of like-coloured cells, so node count tracks colour runs
rather than multiplying by the cell count every turn. Every glyph is set as
text and never as markup — the floor plan draws a character taken from a
sim-authored noun, and a settlement named with an angle bracket must never
reach a parser.

The tint is **bedrock**, and it is withheld wherever the glyph is drawing
something other than that ground — water, a mark, the observer's own cell.
The Pigment established the rule; this campaign carries it into two more
renderers and the caption reports counts that partition the chart, so a
reader can check the sentence against the picture. At this world's flagship
room every cell is river, so the terminal chart tints nothing at all and says
so: *0 tinted, 31 withheld*. The document still carries all thirty-one
colours. It is the *drawing* that declines to assert them, which is the whole
of the discipline in one line.

## What the chamber band does not do

Indoors, nothing is coloured, and the reason is that two models do not exist
yet rather than that two functions were not called.

A wall carries no material. `CellKind::Wall` is documented as "the building's
fabric" and holds no lithology, no soil, no source — so there is no
reflectance to read. Where a wall's colour comes from (local bedrock, the
soil buffer, climate, culture) is a modelling question with real content, not
a lookup. And indoors the light is not the noon sun: the doorway is an
aperture and the hearth is a Planck emitter at flame temperature, which would
be the first illuminant in this project that is not a star.

So the floor plan's palette ships with a colour slot that is empty for every
entry. Filling it with the outdoor sun and the bedrock beneath the floor
would be exactly the invented effect this project forbids — a picture
asserting something no model supports. The slot is the honest half of the
work; the building's fabric and the interior illuminant are both carried
forward in [the idea registry](../frontier/idea-registry.md) as the two
models that would unlock colour indoors.

---

Colour was never a property of a thing. It is what happens where a material,
a light and an eye meet. The Pigment made that computable; this campaign made
the *third* term vary, and made the document say which one it was.
