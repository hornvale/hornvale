# The Chorus

*The same world, and it does not believe the same thing twice.*

Until now The Book had one narrator: the god's-eye gazetteer, which knows
the star's spectral class and the day's length in standard days because it
is the voice of the ledger itself. This campaign — the metaplan's fourth —
gives every placed people its own account of the same ground truth, and
prints them side by side. Seed 1's volume now ends like this:

> **As the Vavako tell it.**
> The Vavako are goblins — ourselves.
> The Ddenke are hobgoblins — neighbors.
> Vebe is the earth.
>
> *In truth, Vebe is a planet with two moons, orbiting a yellow-white
> dwarf (F); its day lasts about 1.5 standard days.*
>
> **As the Ddenke tell it.**
> The Vavako are goblins — rivals.
> The Ddenke are hobgoblins — ourselves.
> Vebe is the earth.

Nothing in those paragraphs was authored per culture, and nothing was
drawn. A cultural account is ground truth passed through four filters —
lexicon, knowledge, ontology, valence — each a pure function of state the
world already computes. The goblins call the world *the earth* because no
culture holds the concept `planet`; their classification is not wrong so
much as theirs, the best carving their ontology affords. Neither people
mentions the moons, because a diurnal species with ordinary night vision
does not hold the sky attentively enough to count them — while on seed 2,
the nocturnal, sky-rapt kobolds keep "with one moon" in their own account
and their margin drops it, because they never lost it. The stances differ
because the psychologies differ: the goblins' wide in-group reads the
hobgoblins as *neighbors*; the hobgoblins' narrow, threat-forward
psychology reads the goblins as *rivals*. Two voices, one world, and they
disagree — in what they know, how they carve it, what they value — for
reasons the machinery can recount.

The italic margin is the second register: the Neverending-Story device,
the god's-eye correction that fires only where a culture's filters lost or
corrupted a fact. It is literally the set difference `ground truth \ emic
account`, so it stays sparse by construction — nothing a voice kept is
ever repeated at it. Together the two registers satisfy a law the suite
enforces on every volume: emic ∪ margin ⊇ ground truth, checked *through
the parser* — every chorus sentence, emic and margin alike, parses back
into facts and re-realizes byte-identically, and the recovered facts must
cover the ledger's. Nothing the world knows can silently vanish between
its narrators. A second law pins the whole mechanism to the gazetteer: the
identity filter reproduces the god's-eye volume byte-for-byte. The old
narrator was the chorus's degenerate case all along.

The campaign's real bet was not the rendering — it was whether any of
this could be *measured*. A chorus can fail two ways: **uncanny** (the
filters too weak; six voices saying ground truth in synonyms) or
**gibberish** (the filters too strong; no worldview, just noise). The
program's metaplan flagged this as its biggest open risk: if the
distinctiveness × recoverability dial could not separate good accounts
from bad, the chorus would fall back to a taste gate. So the campaign
built the dial as six Laboratory metrics computed on the *structured*
accounts — never the surface strings, so two voices that differ only in
vocabulary measure as identical — and put the bet to a preregistered
known-groups test: a null filter (the uncanny pole), the shipped
derivation, and a documented pathological filter (the gibberish pole),
with the separation criteria frozen before the readout.

The dial held. On every measured world the null pole scored exactly
uncanny (pairwise distinctiveness zero), the pathological pole scored
exactly gibberish (recoverability zero, against a shipped margin of at
least twice the required floor), and every shipped voice sat strictly
between the poles on loss while clearing the distinctiveness band — on
seed 1 purely from the stance asymmetry, on seed 2 more than five times
the floor once the kobolds' sky knowledge diverged. The bet the metaplan
was most prepared to lose is the one result this campaign can state
without hedging: what a culture understands is now a measured quantity.
[Study 012](../laboratory/study-012.md) is the population readout.

The measurement even corrected its own author once, which is the best
evidence it is real. The first draft of the pole-ordering criterion used
an aggregate that averaged loss, ordering, and framing — and the third
task measured it *false*: a culture that merely reorders facts by its own
salience (lossless, fully recoverable) out-scored total destruction on
that aggregate. The per-component decomposition localized the error in
minutes: ordering is distinctiveness, not damage. The criterion was
amended onto the theory's own two axes — transparently, with the
falsified draft preserved in the spec — and no threshold moved. The dial
is calibrated enough to catch a category error in its own gate.

One honest caveat rides with the numbers, and it has a name in the idea
registry: the roster is the chorus's diversity budget. Goblin and
hobgoblin perception sit near the same baseline, so a seed-1 chorus is
honestly quiet — its voices differ in stance and little else, and no
filter change could widen that without faking it. The Laboratory reports
the two numbers side by side (inter-account variance against the input
parameter spread) precisely so a quiet chorus can be told apart from a
vacuous one. Louder choruses come from authoring stranger peoples, not
from tuning filters.

What the chorus does not yet do is also deliberate. The accounts render
in Common — the ethnographer's translation — because worldview divergence
is exactly what survives translation; the tongues themselves deepen at
the seventh campaign. The causal filter (why the goblins think the day is
long) waits on the schema library at the fifth. Doctrine, institutions,
and the reader-relative Book wait at the sixth. The chorus, for now, is
what each people *holds*; the campaigns ahead are why they hold it, and
who taught them.
