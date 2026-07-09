# The Meeting of Seed 42

[The Gods of Seed 42](./the-gods-seed-42.md), the page this one succeeds, holds
one variable and moves it: the same seed under two skies, spinning against
tidally locked, watching a goblin pantheon's head deity swing from cyclic to
eternal as the sun stops setting. It was built to make a single comparison
legible — sky × perception — and the second pantheon it grew, a kobold warren
standing beside the goblin village, was the first hint that the seed had more
than one story to tell. This page tells the rest. It holds the *sky* fixed —
one seed, one generated spinning sky, the default unpinned seed 42, no rotation
pin anywhere — and moves the *people* instead. Two peoples stand on that one
globe, under that one sky, and they diverge anyway: they settle different
cells, grow different social ladders, crown differently-headed pantheons, and
speak those pantheons in unmistakably different voices. Nothing about the sky
explains any of it. The divergence is entirely a consequence of who is
standing under the sky — and every strand of it recounts, by name, to one of
the three authored vectors Year 2 spent its spine building: **psychology**
(who a people is disposed to be), **perception** (what a people's eyes weigh),
and **articulation** (what a people's mouth affords).

The whole demonstration turns on a single discipline shared by all three
vectors: goblin is the identity point. Every scalar sits at 0.5 and every
enumeration at its goblin variant, and every downstream formula is *constructed*
— not tuned — so that the goblin values reduce it to the plain constant the
world used before any species substrate existed (see
[Species](../domains/species.md)). Goblin is therefore the world as it always
was; kobold is the world's first genuine second reading. So when the two
diverge below, the goblin column is never the thing that moved. The kobold
column is, and it moved by exactly the authored distance its vector sits from
the baseline — no more, no less.

## Psychology — where they land, and who stands where

The two flagships, quoted verbatim from the committed
[`almanac-seed-42-sky.md`](./almanac-seed-42-sky.md):

```text
The chief goblin settlement, Fnabnget, holds 359 souls amid temperate-rainforest.
The chief kobold settlement, Rakrra, holds 425 souls amid temperate-rainforest.

The goblin village of **Fnabnget**, population 359.

Fnabnget lives by farming.
Its roles, lowest to highest: slave, farmer, artisan, shaman, chief.

The kobold warren of **Rakrra**, population 425.

Rakrra lives by farming.
Its roles, lowest to highest: digger, shaper, keeper, elders.
```

**Fnabnget** and **Rakrra** are not the same site read twice; they are two
different cells on the same globe, and the psychology vector chose each one.
Settlement scores every habitable cell for a species by a weighted sum whose
weights are read off that species' vector rather than fixed once for the world
(see [Settlement](../domains/settlement.md)): `freshwater = 0.45 × (0.5 +
time_horizon)`, `coast = 0.20 × (2 × in_group_radius)`, and a hostility penalty
`0.50 × (1.5 − threat_response)`, with temperance unweighted at 0.35. At the
goblin baseline those expressions collapse to the plain constants 0.45, 0.20,
and 0.50 — the pre-species weighting exactly — so Fnabnget lands where a
goblin's world always placed its flagship. Kobold's authored numbers bend the
same three terms in three directions at once: a time horizon of 0.8 raises the
freshwater weight to `0.585`, an in-group radius of 0.2 cuts the coast bonus
from 0.20 to `0.08` (an insular warren barely prices sea access), and a threat
response of 0.8 softens the hostility penalty from 0.50 to `0.35` (a people
entrenched behind its own traps discounts a hostile cell less than a people
that would flee it). Every `(cell, species)` pair is scored once, into one
combined ranked list, and each species' single global argmax becomes its
flagship — so Fnabnget and Rakrra land where they do because a goblin and a
kobold, offered the same globe, do not agree on what the best cell *is*. That
they both net out in temperate-rainforest is this seed's coincidence, not the
vector's doing; the cells themselves differ, and so do the populations, 359
against 425.

The two ladders diverge for reasons one step deeper in the same vector.
Culture's structure function is one reviewed rule table, unchanged for goblin,
modulated for kobold by three psychology inputs (see
[Culture](../domains/culture.md)). Goblin's rank status basis lets the full
stratified ladder form — `slave, farmer, artisan, shaman, chief` — the same
five rungs Year 1 grew. Kobold's status basis is *knowledge*, not rank, and
that single enumeration makes the rank-gated slave rung structurally
unreachable no matter how much surplus or scale a warren has. Its other rungs
are the same ladder goblin grows, wearing kobold's own vocabulary: `digger` is
the worker rung, `shaper` the artisan, `keeper` the shaman — the priestly rung
that is precisely what makes the warren's cult *organized* — and `elders` is
simply what a kobold calls the top rung a goblin calls `chief`, a relabel that
rides with the species rather than a ceiling its psychology imposes (the
`structure` function reads a status basis, a threat response, and a time
horizon, but never sociality). So Rakrra tops out at `digger, shaper, keeper,
elders` — a worker, an artisan, a priest, and a council — a ladder that cannot,
by the shape of the vector, hold a slave at the bottom, though its summit is
named for a council of `elders` rather than a lone `chief`. Neither ladder
grows the warrior rung this seed's low threat leaves unearned (kobold would
name it `warden`); the difference between them is not threat but authority, and
it recounts to `status-basis` by name.

## Perception — the same sky, weighed by two different eyes

Now the pantheons. From the same committed almanac's "The Gods" section, the
goblin block:

```text
An organized priesthood tends a pantheon:

> Nodvnotngak the Nebsxad returns every 0.88 days. So it was, so it is. *(who presides)*
>
> — derived from the phenomenon *celestial-body*

> Sxebzvetfnat the Sxodsfotsfot returns every 15.99 days. So it was, so it is.
>
> — derived from the phenomenon *celestial-body*

> Nokzhfod the Fngeksfadnadnad returns every 32.55 days. So it was, so it is.
>
> — derived from the phenomenon *celestial-body*
```

and the kobold block, on the same globe, under the same sky:

```text
In the warren of **Rakrra**, an organized priesthood tends its own pantheon:

> Rragratxok Ragxoq comes back every 15.99 days. That's how it's always been. That's how it's always been. Rragratxok is the highest among the gods. *(who presides)*
>
> — derived from the phenomenon *celestial-body*

> Rraqrrok Roxag comes back every 32.55 days. That's how it's always been. That's how it's always been. Rraqrrok is a great one among the gods.
>
> — derived from the phenomenon *celestial-body*

> Rrogxat Xo comes back every 0.88 days. That's how it's always been. That's how it's always been. Rrogxat is a great one among the gods.
>
> — derived from the phenomenon *celestial-body*
```

The goblin pantheon is headed by a 0.88-day deity — the sun, whose period is a
single rotation; the kobold pantheon is headed by a 15.99-day one — the first
moon. The sky itself never moved: the sun still leads the kobold list too, as
**Rrogxat Xo**, third and least, on the identical 0.88-day period. What changed
is only which pair of eyes did the ranking, and that recounts to the perception
vector (see [Perception](../domains/perception.md)). A species' lens is three
multiplicative weights built from three authored dimensions — activity cycle,
night vision, sky attention — and written, again, to reduce to identity at the
goblin baseline. Goblin's vector `(Diurnal, 0.5, 0.5)` yields the lens
`(1.0, 1.0, 1.0)`: no reweighting at all, so a goblin ranks the sky's raw
salience unchanged and crowns the sun, whose raw salience the almanac's sky
report lists at `[1.00]` against the vast moon's `[0.64]`. Kobold's vector
`(Nocturnal, 0.9, 0.8)` yields the lens `(0.52, 1.82, 0.70)` — a nocturnal
activity factor of 0.4 and a high sky attention drop the day sky to a 0.52
weight while high night vision and sky attention lift the night sky to 1.82. Run
the same two bodies through that lens and the order flips: the sun's raw 1.00
scales to `0.52`, the vast moon's raw 0.64 to `1.16`, and the moon clears the
weighted bar the sun no longer can. The kobold does not see a different sky —
it is handed the identical phenomena — it *weighs* the one it sees differently,
by exactly the distance `activity-cycle`, `night-vision`, and `sky-attention`
sit from the goblin baseline. Both pantheons carry the same provenance line,
*derived from the phenomenon celestial-body*, on every deity: religion never
learns that a lens exists, only that its ranked list came out in a certain
order.

## Articulation — the names, and the two voices telling them

The divergence in those two blocks is not only *which* body heads each
pantheon. It is every syllable, and it is the grammar of the telling — and
both recount to the third vector, articulation (see
[Language](../domains/language.md)). Read the names first. The goblin head is
**Nodvnotngak the Nebsxad**; the kobold head is **Rragratxok Ragxoq**. The
sounds themselves are drawn from each species' own phonology under its authored
articulation envelope: kobold's sibilance sits at 0.9 and its anatomy affords a
trill, so a warren's names hiss and roll (`Rakrra`, `Rragratxok`, the doubled
`rr`), where goblin's cluster around `fn` and `ng` (`Fnabnget`, `Nodvnotngak`)
and never trill. But the sharper tell is the *shape* of the title, which keys
not to the mouth but back to psychology's status basis. A rank-basis people
affixes a dominance honorific onto its gods' epithets — goblin's *the Nebsxad*,
a bound honorific title stacked onto the stem — while a knowledge-basis people
builds a plain descriptive compound with no dominance marking anywhere in it:
kobold's *Ragxoq* carries no honorific article, no title, just a second stem.
The god's name recounts to how its people organize authority, exactly as the
caste ladder does.

The voices do the same. The almanac renders each tenet at display time from
committed facts through one seam, `render_line`, under three voice knobs
derived from the same psychology vector. Goblin, rank-basis, renders formal and
honorific-dense: an archaic connective, *"So it was, so it is,"* no repetition,
and its presiding god's rank marked only by the terse `*(who presides)*`
annotation. Kobold, communal and knowledge-basis, renders repetitive and
descriptive: it echoes its refrain twice a line — *"That's how it's always
been. That's how it's always been."* — and it *names* its head deity's rank in
plain words, *"Rragratxok is the highest among the gods,"* where the goblin
telling would never spell it out. The formality and the stacked honorifics
trace to `status-basis`, the same dimension that already decided who stands
where on the ladder and how a god's title is shaped; the doubled refrain
traces to `sociality`, the dimension the ladder never reads at all. One
vector, status-basis, reaches two layers deep, from a settlement's castes to
a deity's title; a second, sociality, supplies the third — the rhythm of the
sentence that venerates it.

## What this proves

One seed, one sky, one globe, one body of code — settlement, culture,
perception, religion, and language, none of them edited between the goblin pass
and the kobold pass. Two peoples diverge across all three of Year 2's vectors
at once: they land in different cells because their psychology prices land
differently, grow ladders that top out at *chief* against *elders* because
their status basis gates the slave rung and each species names the same rungs
in its own vocabulary, crown a sun against a moon because
their lenses weigh the same sky differently, and tell those gods' stories in a
formal honorific voice against a repeating descriptive one because the same
psychology that built the ladder also tunes the telling. Every one of those
divergences is *recountable*: ask the world `why`, and `recount` replays the
committed facts and provenance behind each result, and the trail leads to a
named dimension of a named vector sitting a named distance from the goblin
baseline — never to an author's instinct, and never to a coincidence of the
dice.

That last clause is the one a skeptic presses on. Both peoples draw from the
seed on their own species-qualified stream labels, so a doubter could ask
whether the divergence is real structure or merely two different draws off the
same seed — stream noise wearing the costume of culture. The control that rules
it out is a distributional twin: a third people carrying the goblin's *exact*
vectors, placed beside the goblin, which must score at chance on every axis
above, because a vector identical to the baseline cannot move a formula
built to reduce to identity there. If the twin diverged anyway, the divergence
would be noise; that it does not is the proof that these formulas read the
vector and nothing else. [Study 009](../laboratory/study-009.md) runs that null
control at census scale. Seed 42 is the single legible instance of what that
census measures ten thousand times: same code, two vectors, one sky — two
cultures, and every difference between them traceable home.

See [Species](../domains/species.md) for the three closed vectors and the
SRD-derived kobold values, [Perception](../domains/perception.md) for the lens
derivation, [Language](../domains/language.md) for the phonology and the
content→render seam, and [Religion](../domains/religion.md) for how a
species-flagship pantheon is built through its own eyes. This page is the
Year-2 capstone [The Gods of Seed 42](./the-gods-seed-42.md) grew toward — the
same seed, now read for everything the two peoples are, not only for the sky
they share.
