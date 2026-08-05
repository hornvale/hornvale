# The Tense

Capacity was written in the eternal present. It was a pure function of
*present-day* climate, so the deep-history bake played two thousand years of
ice ages and interglacials across a map that never changed, and the only thing
an era could do to a cell was switch it off.

Everything the surrounding campaigns had been fighting over followed from that
one fact. The Fallow needed capacity to vary over time and invented a soil stock
to get it, because the field itself could not move. The Tilth's stages 6 and 7
argued about how a binary gate should behave, because the gate was the only
era-varying quantity in the model. And a world that fell under a glacial maximum
did not thin — it died, because a gate has no gradient to degrade along.

The repair is one parameter. `K(s, c)` becomes `K(s, c, e)`. Cold ground is not
gated; it is poor.

## Three words that were the same word

Before the change, Hornvale answered the question "is this habitable" with three
different oracles that had never been compared.

One was an era mask: land above the era's sea level, and mean temperature above
a thermal snowline of −10 °C. One was capacity: this species' carrying capacity
here is non-zero. The third rebuilt the climate wholesale to choose glacial
refugia, by a rule neither of the others used.

The first two disagreed **over roughly half of all land**, because they were
computed from two different climates. And the third selected the refugia the
bake then routed migrants toward. A world could route a people onto ground one
oracle called dead, a second called productive, and a third called a sanctuary.

Only one of the three could be made to answer the question properly, and the
reason is structural rather than a matter of taste. An era mask is a map from
cell to boolean. It has no species argument and cannot acquire one without
becoming a capacity field. Capacity already carries both indices. The collapse
had exactly one admissible direction, and after it there is one oracle:
**habitability is a relation between a people, a cell, and an era** (decision
0105).

## What a mask leaves behind when it dies

`Bake::factor` now gates on ice alone, and the ice mask is identically empty on
every production path. The habitability mask is inert — read in zero places by
the bake.

Two of the bake's own unit fixtures had built their test conditions out of that
mask. One placed a tributary community with a punishing overlord and nowhere
admissible to flee, to prove that a flight which finds no home is counted as a
death and never as a departure. The other blocked the two rings nearest a
displaced people, to prove that its search widens outward rather than scanning
once and giving up.

Both constructions evaporated. The vassal's road led somewhere after all, so the
fixture was measuring a successful flight while claiming to measure a fatal one.
The roller stopped in the first ring it was supposed to walk past. Neither test
was wrong about the rule; both had quietly stopped exercising it, and each was
green for exactly as long as it proved nothing.

They were repaired by changing the language rather than the claim — dead ground
is now expressed per-people, in capacity, because that is where habitability
lives. Every assertion is untouched. The general form is worth keeping: **when
an oracle is retired, the fixtures that spoke through it do not fail, they go
vacuous**, and a vacuous test is invisible to the thing that is supposed to
catch vacancy, which is the test suite.

## The dynamic range collapsed inward

The interesting result is not that worlds changed. It is the *direction*, read
from both ends of the distribution at once.

Seed 1234 had been a dead world for the entire arc — zero survivors, every time.
It now carries 36 living communities across 70 sites, with occupation columns
sixteen layers deep and recolonisation running through the thirteenth to
nineteenth centuries of its history.

Seed 42, the flagship, went the other way. It fell from **209 settlements to
122**, and its chief settlements lost between a third and a half of their
people: the bugbear seat from 88 souls to 67, the goblin village from 82 to 41,
the human town from 77 to 36.

That is one mechanism seen twice. A gate produces all-or-nothing worlds, because
ground is either admitted or annihilated. A continuous squeeze produces middling
ones. **Dead worlds live and rich worlds thin**, and the variance between worlds
compresses toward the middle.

Whether that middle is the right place to sit is a genuinely open question, and
this campaign does not answer it. It is a question about the scale constant and
the shape of the response curves, not about the collapse — which is the useful
thing to have separated, because the two were previously the same knob.

## What the world's own language reported

The change is legible in a place worth noticing: the peoples' vocabularies.

Hornvale derives each people's lexicon from what its settlements are exposed to
— a concept a people never encounters is a *gap*, not a word. When settlement
placement moved, the lexicons moved with it, and the frozen pre-campaign golden
recorded eighty-eight changed lines.

None of them were a word becoming a *different* word. Every change was a gap
becoming a root, a root becoming a gap, or a gap changing its stated reason.
That distinction is the whole diagnosis: had a root become a different root, the
phonology itself would have shifted and the byte-identity that keeps a settled
people's language stable would have broken. It did not. The languages are
unchanged; the peoples simply live beside different biomes now, and their
vocabularies are correctly reporting it.

Goblins gained a word for barley and lost one for marsh. Kobolds — authored as
highland specialists — traded their word for *hill* for a word for *valley*.
The last of those is not obviously right, and it is on the ledger as an open
question rather than an accepted result.

## Still ahead

The largest unstarted piece is that capacity reads each cell's **mean**
temperature. By Jensen's inequality that is wrong for any nonlinear response,
and a Gaussian is emphatically nonlinear: evaluating at the mean overestimates
near the optimum and underestimates in the tails. **The tails are where refugia
live.** This is a defect at any grid resolution — subdividing the globe shrinks
it without removing it, at four times the cost per level, and nothing coarser
than a scale Hornvale cannot afford resolves a sheltered valley. Integrating the
response over a within-cell distribution removes it outright, and the spread can
be derived from terrain the world already commits rather than drawn, which keeps
it out of the save format.

The two-tier constraint split — lethal gates multiplied by floored preference
modifiers — is specified and sits in the tree, wired to nothing.
