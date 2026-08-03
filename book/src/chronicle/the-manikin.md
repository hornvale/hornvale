# The Manikin

The species model measures its peoples against a reference, and for the whole
of its life that reference was one of the peoples. Every psychological,
social, perceptual and articulatory dimension a species carries is a bare
ratio in `[0, 1]`, and the number `0.5` on each of them meant, precisely,
*goblin*. This campaign lifted the reference out of the roster and replaced it
with a manikin: a named reference vector that belongs to no creature, has no
identifier, and can never be placed in a world.

Not one value changed. That was the design, and it was also the thing the
campaign froze as a prediction and then went and checked.

## The weld, and where it was pinned

The identity element was not merely *documented* as goblin's. It was defined
as goblin's, in code. `SocietyVector::baseline()` was a constant function
whose documentation read "Equal to the goblin's authored society dims," and a
test named for the equality asserted it. A second test asserted that every
goblin scalar sits at exactly `0.5`. Two of the four vector families carried
the claim only in prose — a doc comment declaring `0.5 ≡ the goblin baseline`
with nothing in code to cite, which is a convention that cannot be depended
on, tested, or deliberately moved.

Four separate things were sharing that one word. There is the **fallback
value** a consumer resolves when a solitary creature carries no society vector
at all. There is the **anchor** one people is authored at. There is the **test
contract** that adding a people is byte-neutral for the peoples already there.
And there is the **reader's calibration** — what a person should picture on
being told a dimension reads `0.5`. Only the last of these has any business
naming a people. The other three are properties of the frame.

The cost of leaving them welded is not hypothetical, and the shape of it is
already in this book. A number read against the wrong reference is not
slightly wrong; it is confidently wrong, and it looks like data all the way
down. The terrain stack paid for exactly that once, when an elevation compared
against the wrong datum made a documented highland stronghold unoccupiable on
most worlds — no formula in error, every value in range, the conclusion false.
A unit is not a frame. The species baseline was that same mistake one level
up, at psychology instead of altitude, waiting on a second people to bite.

## The repair that would have relocated the bug

The intuitive fix is to re-anchor on humans. Humans are the genre's default,
and a reader has a human body to calibrate against, so `0.5` would mean
something a person could feel.

It is wrong, and the reason is a fact about the roster rather than a
squeamishness about anthropocentrism. Humans are not average. Human night
vision is genuinely poor and sits *below* much of the bestiary this world
already holds; author humans at `0.5` on that dimension and the number stops
meaning "typical" and starts meaning "weak", silently re-scaling every keen-
eyed people's authored value against a feeble reference. The articulation
dimensions run the other way — the phonology envelope is built on a
human-calibrated inventory, so a human anchor is *better* founded there than a
goblin one. No single people is the right anchor for all four vectors, which
is the general statement of the problem: any exemplar is a reading in the same
table as the things it anchors, and re-anchoring only changes which reading
has been promoted.

The resolution is old and standard practice outside this project. The CIE
standard observer describes no actual pair of eyes; the ICRP's "standard man"
is a body nobody has. The sharper analogy is metrological: the redefinition of
the kilogram did not replace the platinum prototype with a *better* cylinder,
it stopped anchoring on an artefact at all.

## Why the manikin is a value and not a species

The obvious cheap implementation is a colourless species — a manikin people,
never placed anywhere, that the formulas point at. That was considered and
rejected on grounds that are checkable rather than aesthetic.

The world already refuses ghosts. A test over the roster asserts that no kind
has zero carrying capacity anywhere, and it holds that line with no exemption
list at all — which is the entirety of its value. A rostered manikin fails it
by construction, and the only repair is to introduce the first exemption into
a default-deny check whose whole strength is having none. It would further owe
a gloss, a family, a mass, a metabolic class, a resource vector and a climate
niche: six meaningless authored values, plus a special case in every consumer
that walks the roster.

A manikin that is a *value* cannot be a ghost, and the reason is physical
rather than procedural. Voidness is a property of a kind's carrying capacity;
the manikin has no mass and no niche, so it has no carrying capacity, so there
is no density at which it could be placed. It is not a kind declined
placement. It is a thing with nothing to place.

## The asymmetry, stated rather than dressed up

`0.5` is a principled neutral on a scalar: it is the middle of `[0, 1]` and it
means the reading leans neither way. Not every dimension is a scalar. Authority
shape, what earns standing, and a waking schedule are enumerations, and an
enumeration has no middle to occupy. The manikin carries a hierarchic
authority, standing by rank, and a diurnal schedule because those are what the
first people carried — and no argument makes them *neutral* rather than merely
*first*.

The campaign does not fix this, and deliberately does not pretend to. The
honest framing, which both the code and the book now use in these words, is
that the manikin is a neutral **midpoint** on the scalars and a designated
**default** on the enumerations. There is a third case, and it is the most
instructive: tonality *is* a scalar, so it does have a middle, and the
reference vector deliberately does not sit at it. It reads atonal, the value
every shipped people carries. A default can wear a scalar's clothes. Recording
that is the whole point of a campaign whose subject is a convention that had
quietly acquired the authority of a fact.

## A fourth family, found by the code and not by the plan

The campaign was scoped to a crate, and so to three vector families: mind,
society, perception. Execution found a fourth. The seven-dimension
articulation vector had moved to the language domain in an earlier
restructuring and carried the identical weld, word for word, in its own
documentation.

Shipping three of four de-welded, behind a chapter announcing that the
reference is nobody, would have produced a book that contradicts its own
engine — and would have forced the language chapter either to lie or to
advertise the gap. The scope was extended: articulation gained its own
reference vector, redeclared in the language domain rather than imported,
because a domain depends on the kernel and never on a sibling. Twenty-six
further sites across composition, culture, language and the walking layer that
stated the frame in terms of a people were swept with it.

## What the measurement said

The claim was checkable, so it was frozen before the code that could move it:
**zero artifact drift**. No authored value changes, so no derived value
changes, so no committed artifact changes. Falsification would have meant
something in the engine derived from *which people* was the reference rather
than from its values — a finding worth the campaign on its own, and grounds to
stop and re-specify rather than to re-pin the drifted artifact.

It held. Regenerating every committed artifact — the almanacs, the elevation
map, the registry and manifest dumps, the laboratory studies, the type audit —
reproduced the committed tree byte for byte, across all four watched paths.
The full gate passed.

What that proves is bounded, and worth being exact about. It proves the weld
was **documentary rather than structural**: no derivation anywhere ever read
which kind was the baseline, only what its numbers were. It does not prove the
weld was harmless. The harm is latent by construction — a frame error is
invisible while the frame has exactly one tenant, and becomes visible on the
day a second people is authored who disagrees with the first. Removing it was
free. Leaving it would not have stayed free.

## Goblin, afterwards

Goblin's authored row still sits at exactly the manikin's values on every
dimension of every vector. Nothing in the model requires that any longer, and
the coincidence is now pinned by characterization tests, one per vector
family, each naming in the test itself that goblin is *authored at* the
manikin rather than *defining* it. Characterising goblin on its own merits —
giving it a temperament that is its own rather than the origin's — is now a
decision about one people, arriving as a visible change to those tests.
Before this campaign it was a silent re-origining of sixteen axes (nine in
`hornvale-species`, seven in `hornvale-language`'s articulation vector) in
every world that has ever contained a goblin.

That is the whole deliverable: not a number, but the removal of a reason a
number could not be changed.
