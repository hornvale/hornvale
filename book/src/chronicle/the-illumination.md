# The Illumination

*Illumination* is two things at once, and the word has carried both since the
Middle Ages: the light that falls on a thing, and the coloured decoration a
scribe adds to a page. The first is physics. The second is a claim about what
matters, made in pigment. A chart is always both — a rendering of what light
did, and an argument about what is worth seeing — and this campaign is about
keeping them from being confused with each other.

Its predecessor, [The Rhumb](./the-rhumb.md), settled what a chart's
*orientation* means. This one rewrites how the chart is *drawn*.

## Three questions and two channels

Look at any cell of the walk band and you are asking three different kinds of
question about it.

*What is this?* — a **nominal** question. Meadow or scree or snowfield; the
answer has no order, only identity.

*How much of it?* — an **ordinal** question. Bands that rank: this ground is
harder to cross than that ground.

*How sure is the observer?* — an **epistemic** question, which is not about
the world at all. It is about the character standing in it.

Three questions. The chart had two channels to answer them with — colour and
glyph — and one of those was already doing two jobs. A remembered cell drew
`,` where a sensed cell drew `.`, and `;` where a sensed cell drew `:`. That
substitution had a name in the source, `faded`, and it was quietly the most
consequential four characters on the chart.

While the land glyphs were an unordered set, the trick was harmless: `,` was
simply the memory-flavoured spelling of `.`. The moment the glyphs became a
**ladder**, it became a lie, because `,` and `;` sit *between* the rungs. A
remembered cell would render one step up the ordinal from what the character
had actually observed — on precisely the cells the character can least verify.
The hazard had been written down before this campaign existed, with a note
that the tempting move would be to reuse the substitution rather than replace
it.

What the note did not say, and what turned out to matter more, is that the
substitution was already wrong *before* any ladder existed. It was an
epistemic claim written in the substance channel. And the same construct had a
twin in the browser client, which had a weight channel — dimming — available
the entire time and was not using it.

So the rule this campaign ratifies is one sentence, and
[decision 0142](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md)
states it:

> **A rendering channel carries one measurement axis, and a client that lacks
> the channel loses the axis and says so.**

Colour takes the nominal axis. Glyph takes the ordinal. Weight takes
epistemic. And a surface that has no weight channel — the escape-free
rendering a screen reader gets — loses the epistemic axis *outright* and
prints a sentence saying so, rather than recovering it by borrowing a channel
that already means something else. Note what the old substitution was,
exactly: that forbidden recovery, performed because weight was unavailable.
Deleting it was right. Replacing it with an escape sequence in an escape-free
surface would have broken the same rule from the other side.

## Colour was reading the wrong layer

Colour, meanwhile, was answering the nominal question about the wrong thing.
It was the **bedrock**: the reflectance of the rock beneath a cell, lit by the
sun and filtered through the possessed creature's eye. Physically impeccable,
and not what anybody standing there would see. You do not see granite through
a metre of turf.

The fix is to compose a **surface mixture** instead — `bare`, `chlorophyll`,
`litter`, `snow`, `sand`, `silt` — weighted by what actually covers the ground
and integrated against the eye's sensitivity curves at the end rather than
partway through. The mineral blend survives as the `bare` endmember, so rock
still shows where rock is what there is.

The design as originally written could not be built. It put the composition in
the terrain domain, extending the function that turns a mineral mixture into a
reflectance — but that function would need the temperature and the biome
classification, which live in the climate domain, and a domain in this project
depends on the kernel and on nothing else. The composition moved up a layer to
where both are legitimately visible, and terrain's mineral mixture was left
untouched.

There was a second, subtler problem, and it was nearly fatal to the whole
exercise. Every input the design named — biome, snow fraction, temperature,
moisture — is decided per *canonical grid cell*, and a thirty-one-cell walk
band sits six refinement levels below that grid. Four thousand and ninety-six
rooms share one reading. Implemented literally, every room in the band would
have received an identical mixture, and the chart would have gone from one
flat colour to one flat colour. The measurement that caught this was taken
before any code was written: the band's *existing* colour count was **one**,
by construction rather than by coincidence.

What rescues it is the sub-cell micro-field, which is genuinely per-room:
climate sets the cover *regime* for the grid cell — what can grow, how much
snow — and openness, wetness and aspect vary the mixture *within* it. Three
distinct colours over thirty-one cells, up from one.

Three, and not thirty-one, is the point. A design that produced a unique
colour per room would have been noise wearing the costume of detail, and the
campaign's own guard against that was itself a lesson: the ceiling was
originally set at twenty, and when someone was asked to *demonstrate* rather
than assert that it would catch the rejected continuous design, the continuous
design measured **eighteen**. The guard would have passed, silently, on the
exact regression it was written to catch. It is now set at nine, with both
runs recorded.

## A snowline, not a peak

The design's illustrative example was that a peak reads white in winter
because its mixture changed. It is a good sentence and it is wrong about this
world.

Seed 42's highest cell is frozen at all thirty-two sampled points of the year,
minimum −17.9 °C. It is white in every season, and a seasonal hypothesis
tested there would pass trivially while proving nothing. What actually moves
with the seasons is the **snowline** — cells near the freezing margin, which
gain and lose their snow term across the year.

Finding that required overturning a null. The first probe sampled a marginal
cell eight times across the year and the readings declined monotonically,
which reads as clean evidence of no seasonal signal. It was not. A periodic
function does not decline monotonically across most of its period, and the
cell's reported annual mean sat near zero while every one of the eight
readings was above +1.7 °C — an arithmetic impossibility unless the minimum
lay in the unsampled tail. A denser resample found it: sixteen of thirty-two
days frozen, a true sampled minimum of −8.9 °C, and adjacent samples swinging
seventeen degrees.

The mechanism, once visible, is worth more than the result. There is a roughly
twenty-three-day oscillation riding on top of the annual trend, and the
original forty-six-day spacing **aliased** against it. The smooth monotone
decline was the beat frequency of a badly chosen sampling grid. The seasonal
colour term is confirmed — frozen days read 0.68 mean reflectance against 0.38
unfrozen — but the durable finding is that a null taken at eight points is a
statement about the sampling, not about the world.

## The seams, drawn

The chart used to be laid out on the lattice's own axes: a cell's box came
from its integer offset from the observer along the three axes of the
triangular grid. That works beautifully until the neighbourhood reaches across
an edge of the underlying icosahedron, where the surface genuinely bends and
no flat offset can say by how much.

Such cells carried a room, a state, and full semantics — and no coordinate.
They could not be placed. One of the three charts published in this book is
sited deliberately on such a seam: of its thirty-one cells, **twelve** lay
across a face edge, and the chart drew nineteen of them with a footer
disclosing the rest. The entire eastern side of the picture was blank.

A great-circle bearing and distance, however, are perfectly well defined
across a face seam. Each cell now carries its own polar coordinate about the
observer — an initial azimuth clockwise from north, and an angular distance —
and every renderer places from that. The chart is north-up: the top of the
picture is true north, not the direction the local triangle happens to point.

All thirty-one cells draw. The eastern half of that chart is visibly on a
different lattice from the western half, which is the honest picture, because
it is.

Rotating a projection creates a collision risk that the lattice layout did not
have — two cells can round into one character box — and the campaign measured
before it ruled. Seventy bands, 2,380 cells, three of them seam-crossing: **0
collisions** at the shipped scale, and **561** at a more compact scale that
would have preserved the chart's old footprint. The compact scale was rejected
for occluding four to eight cells per band purely to keep the picture the same
size. The tie-break rule was written anyway, and every test of it *forces* a
collision rather than trusting that none occurs. Its ordering settles a
question worth stating once: *salience ranks, weight inks, and neither becomes
the other.* A remembered cell holding the most salient thing in view wins its
box and draws dim.

## The wire ships two rungs of the ladder, and no new version

Between the light leaving a surface and a pixel on a screen there is a ladder:
reflectance, then the signal a particular eye produces, then a projection of
that signal onto three screen channels, then bytes. Every client has to cut
that ladder somewhere. Cutting it too high forces every client to implement
optics; cutting it too low means the sim has already decided what the picture
looks like.

So the wire now carries two adjacent rungs — the post-eye per-channel signal,
and the sim's own projection of it into bytes — and each client chooses. A
client happy with the sim's answer reads the bytes. A client with a wide-gamut
display, or an artistic opinion, reads the signal and projects it itself.

The design called for a new schema version to carry this. It did not get one,
and the reason is a small piece of engineering ethics. The browser client
accepts exactly one schema tag and refuses every other, deliberately — a
denial list would fail open on a schema nobody anticipated. Minting a version
would therefore darken that client's chart until every client updated in the
same breath. The fields were appended instead, at the end, where a consumer
that has never heard of them reads exactly the bytes it read before.

The near-miss here is the part worth recording. Reprojecting a signal takes
three things: which channels carry hue, which channel drives each of red,
green and blue, and the per-channel normalisers. The landing shipped the first
two. The third was dismissed during design on the grounds that it was
"already public" — public *in the kernel*, which is not the same as being on
the wire, and which is per-observer besides. A reviewer caught it before the
merge. Had it shipped, the schema would have published a field no client could
interpret, into a contract that cannot un-publish one.

With all three present, the claim is checkable, and it was checked: dividing
the signal by the normalisers, clamping, and applying the sRGB transfer
reproduces the carried colour **byte-for-byte**, computed from the document
alone with no reference to the code that wrote it, across all 186 coloured
cells in three regenerated examples. Zero mismatches.

One qualification ships with it, and it is deliberate. The eye's low-light
branch uses global constants that are not on the wire and are not
per-observer. No cell in anything published reaches that branch — including
one lit from 56 degrees below the horizon — so those constants were not
shipped, and the documentation says the reprojection guarantee covers the
daylight path rather than claiming more than it can.

## What did not happen, and it is the finding

A campaign spent on making colour mean something ends with a chart the player
will not see it on.

The flagship settlement's walk band is **100% river** — on seed 42, and on
seeds 13, 7, 1 and 100. That is not a quirk of one world; it is a property of
where settlements get sited, which is beside water, and the band a possession
opens on is the settlement's own. The colour lens withholds tint from every
glyph that is not drawing ground, because a river painted meadow-green is a
false claim and the honest response to one is to withhold it. So the first
outdoor chart a new player opens reads, in its own caption, `0 tinted, 31
withheld`.

Nothing is broken. Every part of this behaves exactly as designed, and the
sentence in the caption is true. But the entire colour recompose is invisible
at the view the game starts you on, and burying that would be the precise
species of overclaiming this campaign spent itself correcting. It goes further
than the default view: no page in this book reaches the colour lens either,
because the scripted rendering path runs with the eye off and the map command
defaults to the terrain lens. The colour work is real, measured, and currently
unphotographable.

The remedies are all live and none of them belonged to this campaign: an
affordance for looking at ground, a differently sited default observer, or an
accepted and documented limitation. It is documented now, which is the least
of those and the honest floor under the others.

A second qualification belongs beside it. The campaign preregistered that the
epistemic-as-weight change would hold across **three** renderers. It was
measured on one — the simulation's own text renderer, on real dry ground,
where a coloured band carries eleven distinct rendering units against the
monochrome rendering's four. The two browser renderers were a deliberate scope
cut. Reporting that as "confirmed" without the qualifier would have been an
overclaim against the campaign's own frozen prediction, so it is written here
with the qualifier attached.

## The glyphs cost nothing

The ordinal ladder is *impedance* — how hard the ground is to cross — composed
as the elevation band plus half the canopy closure plus half the terrain
roughness, each perturbation bounded so that vegetation and unevenness
together can raise a cell at most one rung above its bare relief.

It spends no new characters. Deleting the seven memory twins returned the
glyph vocabulary to eleven marks, which is where the design's legibility
budget said it had to stay, and the ladder reuses the five the relief bands
had already spent. That the budget balanced exactly was verified by grepping
every character literal the renderer can emit, rather than by counting the
ones anyone remembered.

## A closing note on instruments

This campaign found twenty-two defects. Essentially all of them originated in
the planning and dispatch text rather than in anything an implementer wrote,
which is a pattern this project has recorded before and keeps re-learning.

Twelve of them share one shape, and it is the shape worth naming here: a check
that reads as protection while not being pointed at what it claims. A ceiling
that passed at eighteen on the exact regression it was written for. A test
whose comment said it pinned a field it never checked. A guard whose test
scene could not reach the code path it guarded. A control that discriminated
nothing because both branches rounded to the same band. A published
declaration that colour cannot vary below grid resolution, still on the wire
after the campaign whose whole purpose was making it vary.

Every one was found by making the check fail on command, or by running the
system instead of reading it. None was found by review of the code, which is
not a criticism of review — a check that cannot fire is *correct as written*.
That is the whole difficulty.
