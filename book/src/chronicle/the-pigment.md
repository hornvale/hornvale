# The Pigment

Hornvale had four colours and no colour model.

A biome carried twenty-two hand-picked RGB triples, tagged `bare-ok(artifact)`
and honest about it — they existed for the PNG and nothing else. Lithology
projected an `Appearance` vector whose `hue` was one scalar running "grey to
ochre". Astronomy could tell you a star was amber, as a *word*, from a switch
over a class enum. And the language domain held Berlin & Kay's colour-term
acquisition ladder, fully implemented, gating which terms a lexicon owns
against a species' night vision — a machine for deciding what a people can say
about colour, wired to nothing that produced any.

None of the four could talk to another, and none could answer the question a
walked world actually asks: *what colour is this, to whoever is looking, in
this light?*

## Colour is not a property of an object

The sentence that organizes everything else is that colour is not in the
thing. It is a three-way product.

A material has a **reflectance** — the fraction of light it returns at each
wavelength — and that is a property of the stuff, identical in a cave and at
noon. Light has a **spectrum**. An eye has a handful of sensitivity curves and
collapses the arriving mixture down to that many numbers. Colour exists only
where all three meet.

Every hard thing this campaign was asked for follows from that. A goblin
seeing differently is a substituted observer. Colour blindness is a
substituted observer. A screen reader is the observer step declined. An amber
sun reddening a green cloak differently than it reddens a red one is the
per-wavelength product doing its work. Store a finished colour and all four
become guesswork, because the observer step has already happened and been
thrown away.

So the substrate is ten uniform 40 nm bands whose edges span 340–740 nm, and
the whole computation is

```
signal[c] = Σ over bands of  reflectance[b] × illuminant[b] × sensitivity[c][b]
```

Multiplication and addition, over fixed-size arrays, in a fixed order. IEEE
754 requires both to be exact, so this is bit-identical across platforms
without touching `libm` at all — which makes the spectral model *cheaper* for
determinism than the perceptual alternative it was chosen over, since hue
angles need `atan2` and a perceptual space needs a `cbrt` the kernel does not
carry. The grid reaches into the near-ultraviolet not because anything sees
there yet, but because the grid is a contract: widening it later would rewrite
every authored reflectance in the repository.

Nothing is committed. The ledger holds causes — mineralogy, stellar mass,
pigment identity — and colour is derived at the emit boundary, so the campaign
owes no epoch, no stream label, and no seed draw.

## What the world turned out to look like

Terrain's reflectance is a second projection of axes the material buffer
already stored: silica splits felsic from mafic, carbonate brightens
everything, and the rock class says which stones are iron-dominated. No new
data, no new draw. Under this world's own star the rocks come out pale grey
granite, dark grey basalt, and a tan ironstone.

Tan, not red. The reason is worth stating because the temptation is to fix
it: iron oxide's reflectance peaks in the three longest bands, exactly where
the standard observer's long channel has fallen to a third of its maximum and
its medium channel has gone dark. Most of the light ironstone returns arrives
where the eye barely responds, so its warmth reads as blue-*deficiency* rather
than red-dominance. That is what ochre is. Steepening the curve until rocks
looked red would be authoring toward a desired picture instead of from the
mineralogy — and it would have broken the naming result below, which depends
on ironstone landing in brown's basin.

The star's light comes from Planck's law at an effective temperature derived
from the mass the seed already fixed — no draw — and carries the same
containment rule stellar age carries: it feeds colour and nothing else, never
insolation, never climate. Across the whole mass draw that spans 4528 K to
6772 K, and the derived K/G and G/F boundaries land at 5191 K and 5907 K,
inside the published bands, so the temperature and the spectral class the star
already prints do not contradict each other.

## The naming, and the correction

Naming compares a sample against remembered examples under the light both
share. So an exemplar is stored as a *reflectance*, never a finished colour,
and goes through the same illuminant and the same eye as the sample before
anything is compared. That is what lets an observer with four channels, or
two, or five, name colours with no re-authoring at all.

The first design said: sense the sample, sense each exemplar the speaker's
lexicon holds, take the nearest. Measuring the seven exemplars killed it.
Distance in signal space is dominated by brightness, so `brown` came out
nearest neighbour to four of the seven terms, the whole dim corner collapsed
inside a radius smaller than the gap to `light`, and the namer would have
essentially never said "light" or "yellow". Normalizing brightness away fails
in the opposite direction: `dark` and `light` have the *same* neutral
chromaticity by construction, and nothing chromatic can separate them.

The fix was already in the data. Berlin & Kay's stage I is achromatic —
macro-black against macro-white — which is why the pack's hue ladder puts
`dark` and `light` at rank 1, and why a culture's depths are tracked as *two*
numbers, hue and luminance. The ladder had always been two axes. So naming
decides on the axis each term lives on: luminance for the achromatic pair,
chromaticity for the five hues. Collapsing them into one metric was the
mistake, and the repository had been documenting the correct answer since
before the campaign started.

## What the claims measured

Two predictions were frozen before the code that could move them, and a third
was frozen mid-campaign when a measurement suggested the first two were
answerable for a trivial reason.

**A stone is named differently by two peoples standing in the same light.**
This holds. The outcrop's nearest term is `brown`, but neither people has that
word: the goblins, who see moderately well in the dark, reach `blue` on the
ladder and stop short of `brown`, and say **yellow**. The kobolds, whose night
vision is keen enough that daylight hue distinctions never repaid the
evolutionary cost, hold only `dark`, `light`, and `red` — and say **red**. Each
falls back to the nearest word its language actually owns. Nothing in the code
branches on species; the difference is entirely the ladder.

**The same stone changes name between noon and dusk.** This is false, and it
is the campaign's headline. The illuminant moves enormously — the peak band
migrates from green to deep red, per-band survival spans six orders of
magnitude, and the ranking of `red` against `yellow` inverts — but the ochre
outcrop sits so deep inside brown's basin that dusk pushes it *further in*
rather than out. It is brown at both ends of the day.

The prediction was wrong about this stone, not about the world. A sweep of 273
surfaces afterwards found 125 of them — forty-six percent — that do change
name between those elevations, across sixteen distinct transitions: blue to
green, brown to red, yellow to dark. That sweep was run after unblinding and
is recorded as exploratory rather than as a claim. What the campaign
*predicted* and what the campaign *confirmed* are kept apart on purpose.

**The third claim was not answerable at all**, and finding out why was worth
more than either answer. It had been added to guard against a confound: the
light does not only redden as the sun drops, it dims about eightfold, and a
dimmed sample drifts toward the dark exemplar for reasons that have nothing to
do with hue. So the third claim renormalized both illuminants to equal peak
radiance to isolate the hue half.

It could never have discriminated anything. Naming compares sample against
exemplars sensed under the *same* light, and both of its axes self-calibrate —
chromaticity normalizes to unit sum, and the dark/light split is the midpoint
between those two exemplars under whatever light is current. Scale an
illuminant by any positive factor and every signal scales with it: the
chromaticities are untouched and the luminance comparison becomes the same
inequality with a common factor on both sides. Peak-normalization is exactly
such a scaling. The third claim and the second were always the same
experiment, which the sweep confirmed independently by moving the same 125
surfaces under both.

The guard was unnecessary. It was written between the measurement that
motivated it and the correction that made it moot, and it survives as the
thing it accidentally proved: this namer has **colour constancy**. That is a
real guarantee and it constrains what comes next — a nocturnal eye cannot be
modelled as the same eye under dimmer light, because that yields byte-identical
names. Night vision has to change what the eye is sensitive *to*, not how much
of it arrives.

## Drawing it

The terminal chart gained a second registered lens rather than a tint on the
existing one, so the charts published in this book render through the
untouched original and are byte-identical.

The lens colours bedrock, which raised a question the design had not
anticipated: what should it do to a river? Tinting the water with the colour
of the granite beneath it would make the picture assert something the caption
would then have to retract. The rule shipped instead is that the tint is
applied only where the glyph is drawing the ground — withheld from water, from
the marks that name settlements, and from the observer's own position, which
names you. One predicate rather than three exceptions. The caption then
reports three counts that partition the chart — so many tinted, so many
withheld, so many carrying no colour at all — and a reader can count glyphs and
check the arithmetic.

At this world's flagship room the lens tints nothing at all: every cell in the
neighbourhood is river, and the caption says so. That is more informative than
a grey-tinted river would have been. It says you are standing in water, and
this way of looking has nothing to tell you about rock you cannot see.

Where the coast comes in, twenty-one land cells tint and ten are withheld —
and all twenty-one share a single colour, because at the depth a walker
occupies the whole neighbourhood sits inside one cell of the canonical grid.
Biome, water and relief are each equally uniform there. The colour is exactly
as finely resolved as everything the chart already carried, which is neither a
defect in colour nor an accident: it is the grain of the world at that scale,
and now there is a test that says so.
