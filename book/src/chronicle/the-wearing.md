# The Wearing

The chief kobold settlement of seed 42 was called **Roqrrarogxok**. The chief
gnoll settlement was called **Gzaadmzhooqdsootngsootqzhoof** — twenty-eight
characters, and not a name anyone could repeat after hearing it once. They are
now **Xoxa** and **Dzoxgzhofdzha**.

The defect had been measured for some time and nothing had failed. The
laboratory's `name-length-goblin` and `name-length-kobold` metrics declare
buckets running `[2 … 10]` characters; across a thousand generated worlds the
median world read **13.15** and **12.51**, and *every world in the census
overflowed the range its own metric declared*. A histogram with a permanently
saturated tail is a silent alarm — the intent was written down, the reality
never met it, and no test was watching the gap.

## The cause was stacking, not spelling

The obvious suspect is the orthography — the `ng`, `zh`, `sh` digraphs, the
doubled vowels. It is the wrong suspect, and the arithmetic says so. Sampled
across the committed gallery, the names ran **3.4 characters per syllable**,
which is unremarkable (*Bristol* 3.5, *Winchester* 3.3). What was abnormal was
the syllable count: **6.04 per name** at seed 42, against real toponyms that
run one to three.

The syllables came from stacking. A settlement's name was one or two
site-concept words — each possibly itself a lexicon compound — *plus* a
freshly drawn two-to-three-syllable stem unique to that settlement. Four to
eight syllables by construction. The stem was not decoration: it had been
added deliberately, one campaign earlier, as a collision fix, and it worked.
It also lengthened every name in the world to buy a uniqueness nobody was
reading.

## A site with two facts

The deeper limit was upstream of the naming code. When the composition root
asked a settlement what it *was*, the answer had exactly two parts: its biome,
and the sky phenomenon its presiding belief revered. A dozen biomes against a
handful of phenomena — which is why one word appeared verbatim in six
different committed names. The facts to say more were already in scope at the
call site (elevation, drainage, water kind, ocean adjacency, rock, cave), and
the vocabulary to say them did not exist. There was no word for *hill*.

Nineteen concepts were registered: `hill`, `river`, `valley`, `island`,
`ford`, `marsh`, `spring`, `coast`, `lake`, `high`, `low`, `great`, `little`,
`new`, `old`, `north`, `south`, `over`, `under`. What makes them interesting
is that nobody had to decide who gets which. The lexicon is exposure-gated: a
people holds a word for what its settlements actually touch. At seed 42 the
new vocabulary sorted itself:

| | bugbear | gnoll | goblin | hobgoblin | kobold |
|---|---|---|---|---|---|
| `river` | Doodo | Dzha | Neede | Neebe | Raarraa |
| `ford` | Daadoo | Dzoxgzhof | Naadee | Naabee | Xarra |
| `marsh` | Kotoa | Msoo | Kotoa | Gotoa | Xo |
| `spring` | Qobao | Gsa | — | Qebae | Rarao |
| `lake` | Doobaado | — | — | Noebaane | Xaarrooraoroo |
| `hill` | Daodo | — | — | — | — |
| `valley` | — | — | — | — | — |

One people on this world has a word for `hill`, because one people's flagship
sits on a strict local elevation maximum. Nobody has a word for `valley` or
`island`. Goblin has no word for `spring`. These are not authored exceptions;
they are what the terrain gates returned. Where the family tree already
existed, the new words inherited it: proto-goblinoid `*Neede` "river" descends
to bugbear *Doodo*, goblin *Neede*, hobgoblin *Neebe* — the same comparative
method the older vocabulary is measured by, now running over the words places
are named after.

So `Dzoxgzhofdzha` is the gnoll for *ford-river*, assembled from the gnoll
words for each. It is a name that says where it is.

## What it cost, measured

Two measurements, on different populations, agreeing.

**A thousand worlds.** Regenerated during the campaign, both species' medians
land inside the buckets that had never once contained them:

| | before | after |
|---|---:|---:|
| `name-length-goblin`, median | 13.15 | **9.33** |
| `name-length-kobold`, median | 12.51 | **7.40** |
| `name-syllables-goblin`, median | — | **2.80** |
| `name-syllables-kobold`, median | — | **2.22** |

The syllable metric is new, and it exists because character length alone
cannot distinguish *shorter words* from *the same words spelled tighter*, and
spelling was never the defect. It
counts maximal vowel runs in the committed surface, deriving its vowel set
from the language's own romanized inventory rather than from `aeiou`, and it
reads the ledger rather than the namer.

**One world, controlled.** Seed 42's almanac lists 334 named places. Because
settlement placement is untouched by any of this, the same page rendered
before and after is a clean two-arm comparison — same world, same places, only
the naming code differs:

```
mean 21.41 → 7.07 characters      median 20 → 5
mean  5.22 → 2.51 syllables       median  5 → 2
```

Three levers carried it, and their shares were separated rather than assumed.
Retiring the drawn stem carried the most. Drawing a per-culture *name shape* —
some peoples preferring the bare simplex, others the specific-plus-generic
compound, with the sharpening exponent read from each people's own social
traits — carried 0.59 characters at the mean. Replacing the language-wide
obligatory nucleus **count** with a nucleus **template set**, so that a
language which licenses diphthongs no longer puts one in *every* syllable,
carried 1.29 (measured against a consumption-identical control, so that the
reseed the change rode on could be told apart from the change).

## Transparency was exactly 1.0, and that was the defect

The campaign's least intuitive claim is that a number at 100% was wrong.

Before this work, **650 of 650** committed names contained, verbatim, the
modern citation form of every concept their own gloss named. Not 99%: every
one, by construction, because nothing in the pipeline could ever wear a
morpheme down. Uniform total transparency is a large part of what makes
generated names read as generated. No English speaker hears *hām* in
*Birmingham*; most real toponyms are opaque to the people who say them daily,
and the ones that are not are opaque to different degrees.

Transparency is now a distribution. Across the thousand worlds it runs
**0.247 to 1.000**, median **0.856**. The metric is registered as a witness and
never as a bound, and its comment says in both directions that a drift back
*up* toward 1.00 would be a regression. The instrument was proved against the
thing it measures: with both wear limbs disabled it reads exactly 1.0 at every
seed sampled — reproducing, from the outside, the constant the old pipeline
guaranteed from the inside.

## The wear that mostly did not happen

The campaign is named after toponymic wear, and toponymic wear is its weakest
result. Say it plainly: across four sampled worlds and 650 settlement names,
**14 names carry a surviving sound-change wear.** Two percent.

The bare number is the least useful thing about it, and publishing it alone
would tell the next person nothing. The funnel is what matters, and every rung
was measured on the real pipeline with an instrument verified byte-neutral
(all four instrumented worlds byte-identical to a clean build):

```
940  morphemes in settlement names
611  clear the frequency floor          (65%)  eligibility
 53  the drawn wear cascade alters      (8.7% of eligible)   <-- the bottleneck
 40  rejected by the survival guard     (all genuine deletions, zero epenthesis)
 13  names carry surviving wear         (14 after positional reduction landed)
```

The guard was the suspect, and the guard is innocent. It refuses a wear whose
morpheme would not survive phonotactic repair — because the alternative,
briefly shipped and caught in review, was settlements committing a *gloss*
naming a word their name did not contain. The worry was that it might be
rejecting reflexes that were merely *interrupted* by an inserted vowel rather
than destroyed. Of forty production rejections, **zero** are that case; all
forty are genuine deletions, and 58 of the 59 annihilated morphemes are not
even a subsequence of the repaired form. The stricter alternative rule was
built, run, and agreed with the shipped rule on all 690 production decisions.

The bottleneck is the rung above: the drawn wear cascade returns the morpheme
unchanged for 558 of 611 eligible morphemes. Part of that has a named,
measured cause. `Tonogenesis` — the rule that turns a lost consonant into
pitch — reads its conditioning from a merger earlier in the same cascade, so a
`Tonogenesis` in the *first* position has nothing to consume and is provably
the identity function. In a wear regime that draws only one or two rules, that
is expensive: across twenty production culture-cascades it appears in eight,
leads in seven, and in **three of twenty it is the entire cascade** — those
cultures cannot wear a morpheme on any seed. Repairing it means re-drawing
every cascade in every world, including the lexicon's, which is another
campaign's epoch to spend.

So the name cycle's opacification phase is *in progress*, not shipped. The
mechanism is correct — drawn, Neogrammarian-regular, keyed to a morpheme's
frequency in its own culture's name corpus so that the generic wears because
it recurs, which is how *hām* became *-ham*. It simply does not fire often
enough yet to claim the phase, and a status flipped to `shipped` on 2% would
assert a property of the world that the world does not have.

## Collisions rose, and that was the plan

Retiring the stem retired a collision fix. The rate went up: median
in-world collision **0.15 → 0.65**. Seed 42's 329 named settlements now render
as **95 distinct strings**.

This is governed, not accidental. A ratified decision holds that settlement-name
uniqueness is a property of a *reference*, not of a name — two taiga
settlements both wanting the ice word is the system telling the truth — and it
forbids fixing the rate by stuffing more drawn entropy into the string, which
is exactly what the stem did. The same decision named the remedy and deferred
it; this campaign built it.

Qualification happens at render time and touches nothing else. Seed 42's
`world.json` is byte-identical with the feature in and out. A ladder of rungs —
the people, the biome, the pair, the coordinate — is walked per rendered
document, and the first rung that separates *the whole group* is taken; a rung
that separates only part of the group is refused, and a group no rung can
separate stays bare rather than acquiring a counter. The result on seed 42's
almanac: of 334 land entries, **102 stay bare** and 232 carry a qualifier, and
**all 334 rendered lines are distinct**. The document stayed unambiguous while
the names collapsed to a quarter of their number.

One structural finding fell out of building it, and it is the most durable
thing the stage produced. Colliding names **agree on their descriptors by
construction**. The gloss *is* the site-descriptor set the name was built from,
so two settlements sharing a name necessarily share the facts that named them:
at seed 42, **all 51 colliding name-groups share a single gloss between them**,
without exception. Widening the descriptor vocabulary — the obvious lever —
therefore cannot separate them, because any fact fed *into* naming is a fact
the colliding names already agree on. Only facts *outside* the gloss can
separate a collision. That is why the coordinate rung carries almost all the
live qualifications, and why the better version of this feature (*the northern
Roa*, *Upper Roa* — same site fact, read as toponymy rather than as a chart
reference, and cheaper) is recorded as a follow-up rather than claimed here.

## What it did not do

The name cycle has four phases: baptism, conventionalization, opacification,
reanalysis. This campaign moved the system from the first to a partial third
and stopped there deliberately. A name still does not go stale when its
referent changes — Ice-Home keeps its name after the ice melts — and nothing
renames anything for conquest or commemoration.

And one honest limit sits inside the shortening itself. The world's longest
seed-42 name is still 33 characters, and its owner is gnoll, whose word for
`coast` is `Qsooxpshaopzhaamshoa` — twenty characters, and not a root at all
but a lexicon compound of `earth` + `water`. Two compounding layers stack
here: the lexicon's own recipes, and the name shape built on top of them.
Shortening syllables reaches the first layer and cannot reach the multiplier
sitting on it.

*The thousand-world figures above were measured on a census regenerated during
the campaign. The census goldens committed in the book are still the
pre-campaign ones: two regens were invalidated by concurrent work on the main
line and the third was deliberately deferred rather than paid a third time.
The naming rows of the calibration battery are held open, greppable, and owed.*
