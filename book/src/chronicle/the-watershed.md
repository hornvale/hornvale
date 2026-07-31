# The Watershed

The campaign set out to name the landscape — to give rivers and landmasses
identities, and then name settlements after them, so that a world's places
would be called after things that exist. It measured that plan before building
it, found the plan could not deliver what it promised, and shipped the
measurement as the result. What it did build instead was quieter and, on the
evidence, worth more: every word in every tongue became pronounceable.

## The plan, and the number that killed it

The Shibboleth had traded uniqueness for meaning. It removed the random stem
from settlement names, so `Vngoashshngaoshshngoogootao` became `Gootao` and a
name became a translatable description of its site. The trade was right and it
was measured: mean name length fell from 27 characters to about nine. But
56.5% of settlements still shared a name with another, and four separate
attempts to close that gap by adding descriptor concepts each landed within a
few points of the last.

The diagnosis was that descriptors are *types*. A taxonomy has tens of members,
and types are exactly what neighbouring places share. What was missing were
**particulars** — proper nouns, whose cardinality equals the number of things
rather than the size of a vocabulary. Real toponymy names the landscape first
and settlements after it: Newcastle-upon-**Tyne**, where *Tyne* is older than
the language using it.

So the design named rivers by their mouths in the flow forest and landmasses by
connected components, glossed rivers from percentile-ranked catchment lithology,
and predicted that settlements would inherit enough discrimination to bring
collisions below 15%.

The prediction was checked before the code was written. It does not survive.

Grouping every settlement by its full `(gloss, landmass, river)` discriminator
— and assuming naming were **perfectly injective** on that tuple, which no real
namer achieves — leaves a floor of **44.8%** collisions across an eight-seed,
1837-settlement battery. The claim was below 15%.

## Why: particulars escape one limit and not the other

The failure is not that the landscape layer is weak. It is that a settlement's
landscape is *shared by construction*.

Seed 42 has 14 named landmasses and 123 named rivers. Its settlements occupy
**five** and **thirteen**. A landmass is definitionally shared — being the same
landmass is what the concept means — and settlements gather on rivers because
that is why they are there at all. The discriminator's effective cardinality is
a tenth of its nominal cardinality.

The campaign's own §1.2 had argued that particulars escape the type-vocabulary
limit, and they do. What it did not distinguish is a second limit: **clustering**.
Persons and events have the opposite structure — one founder per settlement, one
killing per place — so their cardinality equals the settlement count by
construction. Those are the veins with the room, and both were non-goals.

An earlier pass had considered clustering and dismissed it, on the grounds that
the nine settlements named `Ka` sit on four different continents. That is true
and it is a different question. Colliding *names* being dispersed does not help
when the *discriminator* takes five distinct values.

## What shipped instead

**Sonority sequencing.** Onset and coda templates were drawn by picking manners
independently, so `[Nasal, Nasal]` was a legal onset — names opening `ngng-` —
and reverse-sonority clusters no language uses were equally legal. Ordering each
template by sonority (rising toward the nucleus, falling away in a coda, equal
neighbours collapsed) fixes it, and **sorting rather than rejecting keeps the
draw count identical**, so the constraint costs no entropy: it only decides what
the same draws mean.

The result is visible in the re-pinned names throughout the tree:

```
Wtoevvelqa  ->  Twoevave      a glide-then-stop onset no language uses,
                              reordered to the stop-then-glide one every
                              language has
Jpojjpo     ->  Pjojpjo
Ngkooqngto  ->  Dngooqtngo
Ngngoqjqobqotdo -> the `ngng` the work exists to remove, gone
```

**Predecessor peoples.** The deepest foreign occupation layer at a settlement's
cell becomes a concept its name may use, so a goblin steading raised on a gnoll
ruin can carry the gnolls. It moves collisions by 0.4 percentage points, which
is what the spec predicted, and it is here for meaning rather than for the
criterion.

## The instrument was lying, and nothing said so

Midway through, `exposure-sound` — the metric asserting that no word stands at a
concept a people was never exposed to — was reading **false on roughly three
quarters of all worlds**. The worlds were right. The instrument was wrong: the
Laboratory keeps a deliberate hand-maintained duplicate of the exposure rules,
so that a check calling the code under test would assert nothing, and that
duplicate had not learned the staple rules an earlier commit added.

It had happened before. The Wearing repaired the same duplicate eleven days
earlier; The Watershed's staples broke it again from a new direction. The
duplication is correct and should stay. What is wrong is that **omission is
invisible**: nothing reddens when a campaign adds an exposure rule and the
second opinion does not learn it, and every measurement taken in between is
untrustworthy.

Repaired, the census reads **1000 true, 0 false, both species**.

A side effect fell out of sonority that nobody predicted: the last
honorific-detector-blind world disappeared. Seed 400's blindness was a
front-divergence artifact of wear-then-repair, the committed form and the
honorific-free reference landing on different rungs of that ladder. Predictable
repair puts them on the same rung. The roster of blind worlds is now empty,
down from one and before that two.

## What the numbers did

Across 1000 worlds, after the epoch:

```
                       before     after
name-collision-rate     0.5688    0.5645
goblin name length      9.14      8.78
kobold name length      7.67      7.40
name-transparency       0.816     0.793
exposure-sound       233/1000   1000/1000
```

Transparency **fell**, and that is worth stating plainly because the campaign
first recorded it as rising. At seed 42 it rose, 202 of 329 glossable names to
216. At census scale it moved the other way. The single-world reading was wrong,
as it had been wrong three times before in this same campaign — about where the
arity cliff sits, about the river multiplier, and about whether predecessor
peoples helps at all.

## What the campaign is for, in the end

Decision 0024 ratified two campaigns ago that uniqueness is a reference-time
property: committed names may collide, and any surface that would be ambiguous
disambiguates from the entities' own site facts, exactly as Earth accepts its
forty-one Springfields. It said plainly that no future work fixes the collision
rate by adding entropy.

The Watershed's measurement is independent evidence for that decision, arriving
from the other side. The mint-time branch of the tree — every way a name can
carry its own distinction — runs into clustering long before it runs out of
facts. The reference-time branch, which 0024 chose, does not.

What the campaign leaves is a world whose words can be said aloud, a naming
layer that knows who held the ground before, an instrument that no longer lies,
and a measured account of why the obvious next step is the wrong one.
