# The Sources

*A prospector's "source" is not a vein; it is a reason — the rock that
explains why ore collects where it does. This campaign wrote down six
reasons a buried place might have anything at all to eat, summed them, and
then found out what the sum actually looked like.*

Nothing underground had anything to eat. A prior campaign made the vocabulary
say the word *chemotroph* — a thing that lives on rock instead of light — but
no world's rock produced a number a chemotroph could live on, so the word sat
in the type system unused. This campaign built the number: for any place
underground, a scalar that answers "how much is there to eat here," derived
from the same buried lithology the world already computes for ore and
foundation stone. **A thing underground can eat the rock, and how much rock
there is to eat decides what the underground can hold.**

There was already a hand-authored answer to compare against. A small corpus
of named underworld places — a flooded sump gallery, a hall thick with
sulphur crusts, a cavern lit by its own chemistry — is scored on this exact
axis, on five levels running from *nothing arrives* to *a whole channel's
load at one point*, and its own commentary names two mechanisms explicitly:
**detrital import** — food washed down from the surface — feeds the shallow
end, and **chemolithotrophy** — eating the chemistry of a hot gradient —
feeds the deep end, so the level was expected to dip in the middle and rise
at both ends: a U in depth. That corpus was this campaign's target, not its
output, and testing three of its predictions against a derivation that runs
at every place rather than twenty-two named ones is most of what happened.
All three lost.

## What was measured

```
P1  Does per-place energy trough at the middle rung, the way the named
    corpus does?
      medians (shallow -> deep)
      0.1686 / 0.2008 / 0.2653 / 0.2814 / 0.2814              FALSIFIED
      (monotone rise; no trough anywhere)

P2  Do worlds differ from each other by at least a quarter of the spread a
    single world already contains internally? (the between-world median
    spread, over the typical within-world spread, frozen at a 0.25 bar
    before the derivation existed)
      measured separation  0.145                              FALSIFIED
      twelve per-world medians span 0.176-0.250

P3  Does the middle 80% of chambers AT A GIVEN DEPTH span at least a
    quarter of the ruler? (frozen the same way, after P1 failed)
      0.112 / 0.092 / 0.147 / 0.192 / 0.198                    FALSIFIED
      (shallow -> deep; the deepest rung comes closest, at 79% of the bar)

P4  Does more than one of the six-plus-one candidate mechanisms dominate,
    depending on where you are?
      confirmed at every depth, on every world sampled         CONFIRMED
```

Three predictions about *how much* energy a place has were wrong. One
prediction about *what kind* was right. That asymmetry is the finding, and it
reorganized the rest of the campaign's design around it.

## The trough that never arrived

The corpus's shape assumed two separate mechanisms handing off to each
other with depth — surface food fading out, chemistry taking over — with a
lean stretch in between where neither has fully arrived. The derivation
sums exactly those two mechanisms (plus four more keyed on the surrounding
rock) and does not trough. It rises, smoothly, from the shallowest occupied
depth to the deepest, and does not level off even at the bottom of the
ladder measured.

**Nothing was retuned to chase the corpus's shape**, and the standing
instruction not to is worth stating because the alternative was tempting: a
trough is a more interesting story than a ramp. What settled it instead was
a question about what the campaign was actually for. Nothing underground had
a productive base at all before this — a deep hall was fed, in the model,
by sunlight that could not reach it. A field that rises with depth means the
deep places now feed themselves, on rock, with no reference to the star,
which is the entire reason this campaign exists. A dip in the middle was
never the goal; it was a shape borrowed from twenty-two named exemplars and
mistaken for a target the whole world had to reproduce. The corpus and the
derivation still disagree about the middle depths, and that disagreement
stands unresolved — the corpus describes hand-picked, recognizable places,
and the derivation describes the median of every chamber at a depth, and a
flooded sump gallery being poor is not the same claim as the typical chamber
at that depth being poor. Comparing the two head-on may itself have been the
wrong comparison, a question left for whatever comes next.

## Worlds that would not tell each other apart

The second prediction was frozen before the derivation existed, specifically
because the project had been burned by this exact shape of assumption
before: an earlier campaign assumed a mineral field varied usefully across a
world's surface and found three-quarters of all land sitting inside a band
less than a hundredth wide. The same caution applied here. Twelve worlds,
picked from the seeds every existing measurement of this ground already
used, gave median energy values spread across barely more than a tenth of
the ruler — less than three-fifths of what one world's own chambers vary by
internally. A world with an unusually rich underworld and a world with an
unusually barren one are, on this measurement, more alike than a single
world's rich chamber is unlike its own poor one.

## A ruler with almost no headroom

The third prediction pushed the same question down to a single world, a
single depth: does the middle bulk of chambers spread out at all? None of
the five depths cleared the bar, the deepest coming closest at four-fifths
of it. A companion measurement sharpens why: **92 to 98 percent of all
chambers, at every depth, on every world sampled, sit inside one authored
band** — the second-lowest of five — and the top two bands, the ones the
corpus reserves for its richest named places, are never realized anywhere.
The richest chamber measured reached 0.424 on a ruler that runs to 1.0.
Whatever headroom the corpus imagined at the top of its own scale, nothing
in the derived world reaches it.

## What worked: composition instead of magnitude

The fourth measurement is the one that held, and it changed what the
campaign asked next. Averaging six-plus-one differently-shaped mechanisms
into one number was always going to compress their differences — a mean
smooths out exactly the thing that would otherwise be visible — and it did:
magnitude barely moves. But *which* mechanism supplies the largest share
does move, at every depth, on every world. The rock changes which of the
candidate sources wins; it essentially never changes how much arrives in
total.

The reason is structural, not incidental, and it was traced to its root.
The rock's chemistry sorts into three bands wide enough between them that
almost no sampled value leaves every mechanism below a meaningful floor —
there is close to nowhere on the axis where nothing is productive. And the
rock itself turns out to be far closer to a small set of categories than a
continuum: a karst chamber's carbonate content is close to a fixed value
regardless of which karst chamber you are in; a chamber cut by fracturing
reads a near-constant metamorphic signature; a lava-tube chamber is a
single value on every axis measured, full stop. What looked, pooled across
an entire world, like several independent continuous quantities is mostly
one categorical choice — *which kind of chamber is this* — with three
answers, each one nearly a point rather than a spread.

That result reoriented the design question for whatever comes after this
campaign: variety in the underworld cannot come from averaging more
continuous inputs together, because averaging is exactly what erases
variety. It has to be authored to survive composition — drawn per world,
kept as distinct qualities rather than folded into one number, so two
richly different chambers can share a magnitude and still be nothing alike.
That reframing, agreed after this measurement rather than before it, is
this campaign's largest single consequence for what gets built next.

## The correction this campaign owes its own founding document

The program that commissioned this campaign expected one more result for
free: that giving a hydrothermal vent the same derivation as a buried
chamber would demonstrate the mechanism is not special-cased to being
underground — real evidence, not merely a plausible assumption, since a
vent's chemistry (hot, mafic, water-saturated) sits squarely inside the
same reactions a chamber's does.

The vent unblock shipped, and it works: a vent's productivity is no longer
a bare, hand-picked number sitting in the code with a comment explaining
that nothing could be done about it yet. It is now derived from the same
seven-mechanism sum a chamber reads, from the vent's own rock and its own
thermal gradient, and it is measurably non-constant — every vent measured
differs slightly from every other, because the rock genuinely varies place
to place.

But the framing needs correcting, plainly, because the evidence is
stronger than advertised and points somewhere slightly different. A vent's
derived productivity, measured at 684 vents, has a median of **0.2139**. The
*rest of the open ocean floor* — 28,995 places with no vent at all, run
through the identical derivation — has a median of **0.2135**. The two are
0.19% apart. A vent is not distinguishable from the seafloor around it by
this mechanism, because the terrain model does not give a vent any special
chemistry or heat that the surrounding sea floor lacks: the whole ocean
floor sits at the same oceanic-maximum thermal gradient and the same
mafic-to-ultramafic rock chemistry that make the mechanism productive in the
first place. The free evidence was never "a vent is special and the model
found it." It is that **the entire ocean floor already sits in the regime
this mechanism rewards** — which is, if anything, a stronger and stranger
claim than the one inherited, and one the founding document did not make.

## What shipped

A new resource a creature's diet can weight now exists alongside the
world's other primary-production terms — ambient light, foraged plants,
prey, waste, ore — standing for ambient chemical production rather than
solar. One kind now draws on it: a stone-burrowing chemotroph that was,
until this campaign, declared able to eat rock and fed nothing at all. Its
capacity moved once given the real field — its ratio of what it can support
living underground against what the same conditions would support at the
surface rose from roughly even to nearly seventeen-to-ten in favor of
living underground. Two other buried kinds were checked against the same
change and did not move at all, because their own diets never touch the new
resource; their best available depth was already determined by other
qualities the derivation leaves untouched.

Getting a believable number at any one depth required a further piece that
had been quietly missing: every chamber in a column used to be scored once,
at its single deepest point, regardless of how many distinct depths a
buried creature might actually occupy. That is now resolved per depth band,
each read at the temperature difference that band's midpoint implies, with
the single deepest band kept exactly as it was before as a built-in check —
a value that must not move if nothing broke, and did not.

## What this leaves open

The underworld now has a productive base that answers to its own rock
rather than to the sun, which is what the whole arc exists to build toward.
What it does not yet have is a reason for two buried peoples differing only
by how deep they sit to actually differ from each other — the census of
occupied bands still has no dedicated vocabulary for "underground" the way
it does for tundra or rainforest, so a place's depth changes what it can
feed without yet changing what it *is*. And the variety this campaign found
— real, but living entirely in *which* mechanism wins, never in how much
arrives — is not yet something anything can be authored against. Both are
now measured rather than assumed, which is what a foundation is for.
