# The Palimpsest

A palimpsest is a manuscript scraped and written over, with the earlier text
still showing through. The Retelling built a *boundary* model: a claim is
damaged where teller and hearer stand differently toward the event, and since
those positions are closed under descent, a story crosses at most one boundary
and is damaged at most once. This campaign builds the other physics —
accumulation, many rewritings, each leaving a trace — and gives the damage a
unit that varies by who is doing the remembering.

It also, at the readout, found that unit missing from its own arithmetic. That
is the longest section here, and it is reported rather than repaired.

## Memory gets a unit

A retelling's damage is the number of **generations** it spans:

```
gen_span(teller, hearer) =
    | founding-day(hearer) − founding-day(teller) |
    ────────────────────────────────────────────────
      generation-length(people(teller)) × year-days
```

One axis, both parties on it, a magnitude rather than a category. A story
handed down across three generations blurs more than one handed across half a
generation.

The denominator is the interesting half, because nothing about it is authored.
Generation length falls out of allometry — body mass and metabolic class —
and across this world's fifteen peoples it spans **6.75×** in three clean
tiers:

| tier | peoples | generation length |
|---|---|---|
| short | goblin, kobold, human, hobgoblin, bugbear, gnoll | 21.65 – 35.86 yr |
| middle | gully-dwarf, desert-dwarf, hill-dwarf | 117.83 – 121.46 yr |
| long | desert-elf, drow, high-elf, wood-elf, sea-elf, snow-elf | 139.57 – 146.08 yr |

A lifespan is derivable for all fifteen. Nothing in that table was written
down; it is read off a mass and a metabolic class the world already committed.

**Why the teller's generation length and not the hearer's.** They are always
the same, because inheritance never crosses a people boundary — The Retelling
measured zero of 780 such edges. That zero makes the choice free of
consequence *today*, and it means a lineage has one stable memory resolution.
It also makes the choice load-bearing the moment any future model lets a claim
travel along an edge that is not descent, which is why the suite pins it with
a deliberately artificial two-people case that production data can never
produce.

**Corrected by [The Parley](the-parley.md), which added the edge this
paragraph waits on.** The conditional has fired. A claim can now cross a
people boundary along a contact edge stamped on a raid, so teller and hearer
are no longer always of the same people, and the artificial two-people case
that production data "can never produce" is exactly what production data
produces on the contact arm — 4,112 of 23,594 endings over a forty-world
panel reach two or more peoples. The freeze itself is deliberately unchanged:
the amplitude still reads the *teller's* generation length. What changed is
that it is now a choice with consequences rather than one free of them.

**What the unit buys that a boundary model could not.** The same gap in days
is 0.18 generations for an elven lineage and 0.70 for a gnoll one. Two peoples
therefore remember the same event at different resolutions **with no contact
between them at all** — divergence the world can supply out of biology, where
the divergence that needs contact remains out of reach.

## The ladder gains two rungs that are not astronomical

The Retelling read its precision ladder from the world's committed sky — its
day, each of its moons, its year — sorted by real span. This campaign appends
two rungs read from committed allometry:

```
day → moon(s) → year → GENERATION → LIFESPAN
```

Per people, so an elven ladder is longer in absolute time than a goblin one,
and both new rungs are optional exactly as a lunar rung is: a kind with no
mass-derived life history contributes neither, as a moonless world contributes
no moon.

This changes a merged campaign's stated design, which said rungs come from
astronomy. What it preserves is the principle underneath — **rungs are derived
from the world, never authored.** An invented "century" would fail that test;
a generation does not. The non-nesting property survives and strengthens: a
generation divides a year no more evenly than a synodic month does.

Damage itself is now a continuous **width** carried with the claim, and the
reported rung is the coarsest rung whose span does not exceed that width,
computed when the claim is read and never snapped into the accumulator. That
is the project's quantize-at-emit discipline applied to a non-float quantity,
and it is what separates how *often* damage fires from how *deep* it goes —
The Retelling spent exactly one rung per firing, which welded the two together
and made its one-rung ceiling and its unreachable coarse rungs the same defect
seen from opposite ends. The surviving invariant is unchanged: width only
grows, so the rung index only rises. A claim is an interval that widens, not a
point that moves.

## Three rules, and no favourite

```
additive        w ← w + span
quadrature      w ← √(w² + span²)
multiplicative  w ← w × (1 + span)
```

All three ship, all three are reported, and **none is nominated as primary.**
That is disclosure rather than indecision: the substrate below was measured
before the rules were written down, so choosing one afterwards would be
selection on data already seen. Quadrature is the standard propagation rule
for independent contributions; multiplicative is the one commensurate with a
roughly geometric ladder; additive is the naive baseline. Each has an
argument; none has evidence.

## The substrate, and the warning it carried

Measured on seed 42 before the model was frozen, and reported as substrate
rather than as outcome.

The probe reproduced The Retelling's published ceiling exactly — **3,237 held
claims one rung below finest**, on a world 165 commits downstream of the one
that first reported it. A zero or a null below is therefore a real one.

One retelling spans a median of **0.804 generations** (p90 8.04, max 76.77).
Accumulated over a whole transmission path that becomes a median of 12.86
generations under the additive rule and 8.27 under quadrature. The longest
natural duration this world contains is a lifespan, at about 2.2 generations.

That is a prediction with a sharp edge on it: **a ladder built only from
durations the world contains must saturate at the median under any
accumulating rule.** The campaign froze its hypotheses knowing that its
headline could be a null, and wrote them to report one.

## The units did not agree, and the readout is what found it

The frozen model seeds the width from the finest rung — a length in **standard
days** — then increments it by an amplitude that is a **dimensionless count of
generations**, and finally compares the result against rung spans, which are
days again. Two different quantities were added and the sum read as a third.

**The defect is in the design, not the implementation.** The amplitude is
defined in generations, the rungs are defined in days, the two are compared,
and no part of the specification ever states a conversion. The code is
faithful to the text it was given.

What that costs is precise and worth stating, because it is not "the numbers
are wrong":

- With coherent units, a median step is ~9,131 days against a generation rung
  of ~11,362, so a path of 8–13 generations saturates the ladder — exactly the
  prediction above. Dimensionless, the width crawls from ~1 to ~50 against a
  ladder whose rung spans vary by an order of magnitude from world to world —
  seed 42's first three sit at 0.88 and 16–42 days, seed 0's at 1.645, 17.643
  and 210.224 — so how far the width gets varies with the sky it is read
  against. Under additive it clears the second rung on 11 of the 40 seeds, and
  a third on two of them: 18,253 of 413,216 pooled claims, 4.4%. What it never
  does on any seed is reach a people's `generation` or `lifespan` rung —
  additive's occupied labels are exhausted by day, moon 1–3 and year — and that
  is why the saturated fraction is exactly 0.0000 on all forty.
- So **additive and quadrature did not disconfirm the saturation prediction —
  they never tested it.** Their saturated fraction of exactly zero is a fact
  about units, not about worlds.
- Nothing was changed. Editing a model after seeing its readout is the move
  this project forbids, and the previous campaign set the precedent by
  reporting its own model/specification mismatch rather than repairing it.

A post-hoc **exploratory** re-measurement with the amplitude converted to days
is reported alongside, on the same panel and the same quantities, never merged
into the frozen numbers.

## Both readouts, each labelled

Forty seeds, the first forty of the census panel. Both readouts walk exactly
the same population — **413,216 held claims pooled per rule under both** — so
the only thing that moved between the columns is which rung a claim lands on.
Every figure was measured on this campaign's own tree; the tree it merges into
changed nothing that genesis reads, so unlike its predecessor this campaign
had no figures to restate.

| quantity | rule | **preregistered** | **exploratory** (post-hoc) |
|---|---|---|---|
| H1 ρ(generation, rung), median | additive | **−0.1948** (n=29, 11 undefined) | −0.1290 (n=40) |
| H1 ρ(generation, rung), median | quadrature | **−0.1157** (n=27, 13 undefined) | −0.1361 (n=40) |
| H1 ρ(generation, rung), median | multiplicative | **−0.1788** (n=40) | −0.0603 (n=40) |
| H2 distinct rungs, median (range) | additive | **2.0** (1–4) | 4.0 (4–4) |
| H2 distinct rungs, median (range) | quadrature | **2.0** (1–4) | 4.0 (4–4) |
| H2 distinct rungs, median (range) | multiplicative | **5.0** (2–7) | 4.0 (2–4) |
| saturated fraction, median | additive | **0.0000** | 0.7497 |
| saturated fraction, median | quadrature | **0.0000** | 0.7182 |
| saturated fraction, median | multiplicative | **0.1307** | 0.7899 |
| H3 ρ(width, variants), median | additive | **+0.0306** | +0.1971 |
| H3 ρ(width, variants), median | quadrature | **+0.0067** | +0.2211 |
| H3 ρ(width, variants), median | multiplicative | **+0.0489** | +0.2265 |

**The exploratory column corrects two rules and breaks the third differently.**
Multiplicative as frozen is `w × (1 + span)` with a dimensionless span — days
times a pure number is days, and that was already coherent. Substituting days
uniformly makes it `w × (1 + span_days)`, which adds a duration to a pure
number: a *third* differently-incoherent model, growing by ~10⁶⁶ over fifteen
steps. Its exploratory row is not a corrected counterpart to anything, and the
tell was in the numbers before it was in the argument — the erratum names
multiplicative as the one rule the defect does not touch, and its headline
correlation then moved further between the columns than either of the others'
(0.119, against 0.066 and 0.020). A prediction that specific, contradicted
that plainly, is worth more attention than it got.

## What survives, and it is the thing the campaign was for

**H1 is negative under all three rules in both readouts.** Long-generation
peoples retain finer precision for events of comparable age, on 27 of 29, 23
of 27 and 37 of 40 seeds where the correlation is defined under the frozen
units, and on 37, 37 and 32 of 40 under the exploratory ones. The panel's
dominant sign never flips, so the no-verdict branch is never reached.

It survives *because* of the defect rather than despite it. A unit mismatch
that rescales every people's width by the same factor cannot disturb a
correlation taken **between** peoples; it can only move where the whole
distribution sits. The magnitudes are modest — medians between −0.06 and −0.19
— and that is the honest size of the effect, not a rounding of something
larger.

One caveat belongs with it. Under the frozen units the correlation is
undefined on 11 and 13 of 40 seeds for additive and quadrature, because every
held claim on those seeds sits on a single rung and a rank correlation has no
variance to work with. The frozen medians are therefore taken over a smaller,
self-selected panel; the exploratory ones are defined on all forty.

**Saturation is confirmed** for the two rules where the correction genuinely is
one: 0.7497 and 0.7182 against 0.0000. The prediction that a ladder of natural
durations cannot hold an accumulating quantity was right, and the frozen units
are the reason it did not show. Part of the residual is structural rather than
interesting — witnesses hold at the finest rung by construction and are 14.9%
of the pooled population, capping the pooled fraction near 0.851 — so the
distance from 0.79 to 1.0 is smaller than it looks.

**H2 is confirmed, and its evidence is the frozen column and only the frozen
column.** The decision table set before the readout was explicit: confirmed if
the retained distribution puts mass at three or more distinct rungs *under at
least one accumulation rule*, falsified if it collapses to two. Multiplicative
gives a median of 5 and reaches 7, spanning the entire ladder from day to
lifespan, while additive and quadrature sit at 2. One rule clearing the bar is
what the table asked for, so the verdict is **confirmed** — the ceiling of one
that campaign 2 measured is broken. Read rule by
rule, one confirms and two read as the falsification — which the frozen units
explain. The exploratory column's constant 4 is not evidence for anything: it
is `min = max = 4.0` on all forty seeds, and the occupied set is exactly the
finest rung together with the top three, on eighty of eighty additive and
quadrature rows. A quantity that never varies carries no information about
worlds; it is a fact about an accumulator overshooting.

**H3 survives as a direction and barely as a magnitude.** The relationship
between an ending's maximum-antichain width and its variant count stays
positive — medians +0.03, +0.01, +0.05 frozen, +0.20 to +0.23 exploratory —
but the frozen figures are an order of magnitude weaker than the 0.662 the
previous campaign measured. That weakening is the reportable content. The
structural predictor of divergence does not vanish when content varies more
richly; it stops being the dominant term.

## The ceiling's cause was not what it was said to be

The Retelling attributed its one-rung ceiling to social position being an
absorbing partition. That is right about held claims and wrong as a statement
about paths, and the sharper version is what tells you where to push:
transmission is strictly parent → child on a single-parent founding tree, so
**any predicate over the teller/hearer lineage relation is constant across
every step by construction.** Position is not special; descent-closure is.

Measured: **209 paths cross social position twice** — bystander to perpetrator
is reachable when an attacker sits inside a witness's own subtree. Those paths
carry no held claim, because retention keeps the least-corrupted route per
holder, which is why the published ceiling of one was correct for what it
counted. An earlier draft of this campaign's design asserted the two-crossing
case was structurally impossible; its own control went red at two.

A second ceiling was predicted from that retention rule and does not exist: for
every candidate axis, the retained maximum equals the all-paths maximum.
Retention binds on exactly one axis, and only there.

**Corrected by [The Parley](the-parley.md), which turned both of this
section's premises into settings.** Transmission is strictly parent → child
only where the contact edge is off: a raid seam is a horizontal step between
two peoples, and no argument from lineage closure reaches it. And the stance
geometry is now two co-equal arms, of which the inherited one closes *all
three* labels under descent — which makes stance exactly the kind of
predicate this section says it is not, and stops the crossing maximum of two
following from the reason given for it. Descent-closure is still the right
cause of the ceiling on the descent arm under the singleton geometry, which
is the arm these 209 paths were counted on.

## What this leaves

The thing three campaigns have called a scarcity of contact is not one.
Every people shares a total common tongue — every registered concept has a
word in it, and that totality is enforced — so nothing linguistic blocks a
claim from crossing a people boundary. Contact itself is in the ledger
already: seventeen of 474 endings carry a witness of another people, which is
exactly the raid seam, and those witnesses hold claims at hop 0 — having
travelled no edge at all.

What is missing is neither language nor contact but an **edge**. Every
transmission model built so far walks parent to child down the founding tree
and nothing else, so a claim cannot reach another people no matter how much
contact the world contains. Corroboration and the ingroup/outgroup direction
of distortion are both waiting on one edge that nobody has added, not on a
world that has yet to supply something.

**Corrected by [The Parley](the-parley.md), which added the edge.** The gap
is closed: the transmission graph now carries a horizontal, undirected edge
for every ending that names an attacker, stamped with the ending's day and
traversable only forward in time. Accounts reach up to six peoples where
descent reaches three on exactly zero of 23,594 endings. What did not survive
is the expectation attached to that edge. Supplying contact was expected to
make the two sides *disagree*; two-sided divergence instead falls on every
accumulation rule (0.59×, 0.52×, 0.77×), because a seam is a channel in both
directions and each side keeps whichever of the other's tellings it can reach
least corrupted. Corroboration's precondition is supplied, and its motive is
weaker than this paragraph assumes.

And the amplitude is still symmetric: it does not care which party stands
further from the event. A signed version — damaging a claim more as it flows
*toward* the more removed party — is the derived form of ingroup and outgroup,
costs nothing to compute, and was held back only because varying it and the
accumulation rule at once would measure neither.

**Corrected by [The Undertow](the-undertow.md), on the second clause only.** The
amplitude is still symmetric and the signed version is still unbuilt — but it is
no longer *the* derived form of ingroup and outgroup, only one of them. A
crossing between two peoples now costs a penalty read from how much contact the
two actually have (`span(FINEST) / (1 + contact_edges)`), which produces a
preference for one's own line as an output of the walk rather than as a rule,
and does it through the accumulated width rather than through the amplitude. The
mechanism reaches its target — 94% of the near-stranger tercile demonstrably pay
it — and the aggregate this thread reports does not move at all. One fence
travels with that, because the claim is narrower than it sounds: **nothing yet
separates a magnitude read from the world's contact history from a well-chosen
constant**, so "derived" describes what the penalty is made of and not a
demonstrated advantage over a constant one.
