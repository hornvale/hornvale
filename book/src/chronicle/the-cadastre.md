# The Cadastre

A cadastre is the register of every parcel of land in a territory — not a
survey of some of them. The distinction is not thoroughness for its own sake.
A parcel's defining fact is where it *ends*, and a boundary is a relation
between two parcels, so the moment a register admits one parcel and omits its
neighbour, the boundary they share has nowhere to be written down. What
survives is a description of the parcel with the edge silently deleted, and it
reads exactly like a complete one.

That is the defect this campaign repairs, in a corpus rather than a territory.
[The Kiln](./the-kiln.md) froze forty-one technologies from a published chronology
of invention, scored them against Hornvale, and got no `present` rows at all.
Its forty-one items were drawn from a catalogue of nearly fifteen hundred, and
the catalogue carries its own dependency relation — every item naming what it
was built on. A corpus states its prerequisites in a field that may name only
items inside the corpus. So every prerequisite pointing outside the sample was
discarded at authoring time, before any code could see the loss, because the
field that would have carried it does not exist to carry it.

## A sample of a graph is not a subgraph of it

The corpus's central computed quantity is not its item list. Each item names
the one capability it introduces and the items it presupposes; the *demand
set* — what a world would actually need in order to grow that technology — is
the transitive closure over those edges, derived on read and never written
down. That derivation is the reason the family exists, and it is exactly what
the sample boundary truncates.

Measured over the forty-one: fifteen distinct items were named as a
prerequisite by some member and were not themselves members — `biped`,
`stone-tool`, `fire`, `copper`, `steel`, `plow`, `nation`, `university`,
`geometry`, `alchemy`, `artillery`, `crossbow`, `falling-motion`, `coke-iron`,
`heat-capacity`. Each one was filtered out at the *edge* rather than at the
item, so the file recorded no gap. The predecessor's own founding document
says so plainly and draws the right conclusion — the derived demand set
*under-describes every such item's real prerequisites* — but a corpus whose
own provenance states that its central quantity is known to be partial cannot
support the claims that quantity is meant to carry.

The repair is to close the population rather than filter the edges: whenever a
member's prerequisite is missing, admit the prerequisite, and repeat until
nothing more is added. The result is a population that satisfies the field's
constraint by *being large enough for it*, not by pruning.

| | the sample | the closed population |
|---|---|---|
| items | 41 | 301 |
| `presupposes` edges | 40 | 401 |
| roots (no prerequisites) | 10 | 1 |
| deepest demand set | 14 | 47 |
| mean demand set | 4.27 | 13.04 |

The single most legible row is the deepest. It is the same item both times —
an improved steam engine, unchanged in every field but one — and what a world
must hold in order to reach it rises from fourteen capabilities to
forty-seven. Nothing about the item changed. What changed is that its
ancestors are now in the register, so the chain no longer terminates wherever
the sample happened to stop. The ten roots collapse to one, `biped`, four
million years back, and that is the same fact stated from the other end: a
root in the sample was mostly an artifact of the sample's edge, not an item
with nothing behind it.

## Two blind rules, because either one alone is wrong

A selection rule for a corpus must be blind — applicable by someone who has
never read the code being measured — or the selection becomes the
measurement. Two blind rules were available and neither is sufficient.

*Close the arcs* is the rule the defect itself suggests: take the
predecessor's forty-one and add whatever they depend on. Measured, it yields
seventy-seven items. It also excludes pottery, along with irrigation, the
calendar, codified law, medicine, coinage, the arch, the aqueduct, the sickle
and fermentation — because no item in three arcs about knighthood, the
republic of letters and the diffusion of steam happens to be built on a pot.
The project's own speculative register names the pyrotechnology ladder —
ceramics gating storage and surplus, kiln temperature gating smelting — as one
of the two things it most explicitly plans for technology, and pottery is that
ladder's first rung. A rule that drops the capability the project most wants
is not a principled rule, whatever its pedigree.

*Take an era* is the obvious complement: admit everything the catalogue
attests before a chosen date. It reintroduces the truncation this campaign
exists to repair, since an era cut is just another sample boundary.

The two turn out to be very nearly independent, which is the finding that
decided it. Compared at the five-hundred-year cut where the comparison was
first run: closure alone seventy-seven, era alone ninety-eight, and their
**overlap only forty-one of a one-hundred-thirty-four-item union**. Each rule
is blind to most of what the other catches. So the rule is the union of both,
and then the closure again: the three named arcs, union everything attested
before 1700, closed under the catalogue's own prerequisite relation. That is
301 items, 401 edges, zero cycles, one root — and the closure step adds only
three items above the era cut, so the cut is very nearly self-closing.

The 1700 threshold is a judgement and is recorded as one. It was chosen
generously rather than tightly, and the reason is an instrument argument
rather than a historical one: **a corpus that stops exactly where the world is
thought to stop can never report that the world stops too early.** An `absent`
verdict on universal gravitation is a finding; excluding the Principia from
the register is a decision that leaves no trace in the output.

## The census that was specified, adopted, and withdrawn

The campaign's first ruling was to score the whole catalogue — all fifteen
hundred items. It did not survive contact with its own cost.

Scoring is the only real expense here, and the obvious economy is a keyword
sieve: match each item against the text of the project's speculative register,
adjudicate the hits by hand, and score the rest `absent`. The sieve was tested
before it was adopted, against the only answer key that exists — the
predecessor's forty-one hand-scored items and their six `deferred` verdicts.
Over 1,787 register rows it recovered **three of six**, while flagging
twenty-three of forty-one for adjudication: it bought almost no labour and
cost half the signal.

The disqualifying part is not the recall figure. It is the *direction* of the
misses. Every one of them scores `absent` where the truth is `deferred`, and a
high `absent` count is this family's flattering result — the campaign's own
thesis is that the world's technology model is impoverished. An instrument
whose error is unbiased is a noisy instrument; one whose error runs entirely
toward the self-serving answer is a broken one.

Tuning the sieve against those six was refused too, and that refusal is the
load-bearing half. Six positives is the only answer key this campaign will
ever have, and fitting a threshold to it consumes the control: a sieve tuned
to score six of six on the one set where the truth is known says nothing about
the fourteen hundred where it is not.

With no cheap method, the census failed a second test as well. The verdict
vocabulary has no value meaning *admitted but not yet examined* — `unmeasured`
means reach has already succeeded and only the trajectory is unscored, and it
requires a mechanism anchor, which is a positive claim. Every item in the file
carries a required verdict. **Admitting an item obliges a claim about it.**
There is no way to take in a whole catalogue and say a tenth of it has been
looked at, so a census costs a census's worth of judgement, and it was
withdrawn for a population that could be searched honestly.

## The instrument was pointed at ruins

Twice during design, and in two separate arguments, this campaign described
Hornvale's four-rung technology horizon as the world's model of what living
peoples can do, and derived a threshold from where that ladder tops out. It is
not that. Every consumer of the horizon is occupation- or vestige-side: it
labels an *abandoned* settlement's era, assigns a horizon by year alone to
date a past occupation, and stamps the ruins a world leaves behind. **There is
no living-world technology model at all.**

The correction strengthens rather than weakens the previous campaign's
finding. It is not merely that the shipped model cannot express loss; the
thing being modelled is ruin-dating, and a living world with no capability
state has nothing to lose in the first place. It also removes the last worry
about the era cut: since no item's verdict can depend on its date, the cut
changes *which* capabilities are examined and never *how well the world does
on them*.

What makes it worth a section is how many readers it caught. The controller
made the error twice and was corrected. Then two of the independent scoring
agents reproduced it exactly, from the code alone, having never seen the
correction — each scoring items `present` on the strength of a mechanism that
dates ruins. Three readers, in one campaign, misled in the same direction.
That is evidence about the code rather than about the readers: the enum's own
definition is domain-neutral, and only its call sites reveal what it is for.

## Twice, a confirming number measured the instrument rather than the world

**The catalogue holds 1,486 items, not the 1,484 every prior count reported.**
Two slugs are dropped by a slug pattern that admits only lowercase ASCII: one
containing a comma, one containing an `ö`. Broadening the capture recovers
both. Neither is in any arc, neither postdates the cut into the population,
and neither is named as a prerequisite anywhere in the closed graph, so the
register itself is unaffected — what was wrong was its stated denominator.

The way the error survived is the part worth keeping. The design offered, as
evidence that the catalogue was stable, that a third independent fetch had
reproduced 1,484 exactly. **The fetch was independent; the parser was not.**
Re-running the same pattern could only ever confirm the pattern, so the
agreement measured nothing about the catalogue at all.

The same shape recurred one level up, in the control. Forty-one arc items were
re-scored blind by an agent denied the corpus, the reports and the audits, as a
check on the method used for the rest. It returned exactly thirty-five
`absent` and six `deferred` — the target aggregate, precisely. It had read it:
the project's own speculative register carries a row restating the
predecessor's scored tally, and that file is one the scoring procedure
*requires* every scorer to read. The spoiler lives in the one place it cannot
be withheld from.

The aggregate is therefore worthless as a control and the item *set* is not,
because knowing there are six cannot tell you which six. The sets differ on
four. Twice in one campaign, agreement was produced by the instrument rather
than by the world, and both times the agreement was the thing that had been
offered as reassurance.

## The control accuses the register it was checking

Against the predecessor's six, the blind re-score recovered four —
domestication, the library, the turn-plough and writing, each on the same
anchor, exactly. It missed coal-mining and literature. And it proposed two
more: the alphabet and the horse, both scored `absent` in the frozen corpus.

Its reasoning for both is uncomfortable. Domesticating the horse *is* animal
domestication, and the control cites the identical register row the frozen
corpus already accepts as discharging domestication. For the alphabet it cites
the identical row the frozen corpus accepts as discharging writing, reporting
that the essay behind it names phonetic borrowing explicitly. If the control is
right on either, the frozen corpus is wrong in the flattering direction —
`absent` where the project's own plans already reach — which is the exact error
the family's disclosure discipline was built to prevent, found by the
instrument built to catch it.

**The forty-one verdicts were not changed**, and the reason is stronger than
consistency. Those verdicts are the control's answer key. Re-scoring them from
the control's own output would consume the control and make the measurement
unrepeatable. The two disagreements are recorded as an open finding for a
successor to rule on, and the cost if they are right is that the register
carries two verdicts that are probably too generous to the world, visibly
documented, for one campaign.

## A correction that cannot be made where it was written

[The Kiln](./the-kiln.md) considered closure and declined it, with a stated
reason: *the full transitive closure was not measured and is not proposed; it
would plainly exceed 80* — eighty being the ceiling its own selection rule set
itself. The first clause is exactly right and the second does not follow from
it. Measured now, the arcs' own transitive closure is **seventy-seven**,
inside the band, and the closure was therefore available under the very rule
that was cited for skipping it.

The estimate was not careless; it was simply never a measurement, and it sat
in a sentence that says so in its own first half. That is the durable shape
here: an unmeasured quantity named in the same breath as the decision not to
measure it reads, to every later reader, as though it had been. The
predecessor's provenance is merged, and this project's records are
append-only — including corrections of things that were false the day they
were written — so the correction lives here and in the new register's
provenance, never by editing the record that carries the claim.

## The reading

Three hundred and one items, rendered to
[`docs/audits/technology-coverage-asimov-1989.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/technology-coverage-asimov-1989.md):

| verdict | count | share |
|---|---|---|
| present | 9 | 3% |
| refused | 0 | 0% |
| deferred | 61 | 20% |
| absent | 231 | 77% |
| inapplicable | 0 | 0% |
| grown / flat / lost | 0 | 0% |
| unmeasured | 0 | 0% |

**The zero in the `inapplicable` row is a rule, not an observation.** Most of
this catalogue is science the world has no position on whatever, and
`inapplicable` was the tempting verdict for it: it means the world
*deliberately* lacks a precondition, which converts *we cannot do this* into
*we needn't*. Both available verdicts are self-serving in opposite directions,
so neither could be chosen on which one looked honest. It was decided on three
grounds instead — the family has never once used the value; nothing about
Hornvale's technology has been decided, so there is no deliberate lack to
report; and it is the one verdict the family's guards do not ratchet, so
routing the mass of a corpus through it puts the campaign's weight where
nothing watches.

The headline is a rate rather than the raw count, because `absent` rose mostly
because the denominator did. Both series are reported, and the forty-one arc
items keep their identifiers and their arc attribution precisely so the older
reading stays comparable:

- the forty-one arc items: **35 of 41 `absent`, 85.4%**, unchanged;
- the 260 newly admitted items: **196 of 260 `absent`, 75.4%**;
- the closed population: **231 of 301 `absent`, 76.7%**.

The fraction moved, by nine points, and downward. The honest reading is
modest: the arc sample was slightly less favourable to the world than the
closed population is, which is what one would expect from three arcs chosen
for narrative momentum — knighthood, letters, steam — against a population
that closure drags back through pottery, numbers, domestication and the
plough, where the project's own plans are thickest. It is not evidence that
the world does better than the predecessor said. It is evidence that a
narrower register asked harder questions.

The nine `present` rows are the first this instrument has ever produced, and
they are its weakest claim by construction: a mechanism anchor that resolves
is not proof a capability is met, only that something exists at that location.
They arrived under two rules adjudicated across every scoring batch, both of
which move items *toward* `absent` and so toward the flattering answer — that
a ruin-dating mechanism cannot support `present`, and that a register row
planning the acquisition *mechanism* names no capability of its own and so
cannot support `deferred` for anything. The asymmetry is stated rather than
argued away. Nineteen items were reopened by those two rules and were
independently re-scored rather than settled by the session that wrote them,
because rejecting an anchor reopens a verdict; it does not decide one.
Fourteen came back `deferred`.

## What it still does not claim

**The register is not a roadmap.** An instrument with a declared bias is never
a standard, and a 231-row `absent` column is a statement about the
intersection of two rosters — a mid-twentieth-century Western chronology of
invention, and a concept registry that grows only on demand — not a deficiency
score and not a list of things to build.

**The bias re-count is narrower than the old one, and says so.** The
predecessor counted geography: twenty-six of forty-one items attributed to
Europe, none to the Americas, sub-Saharan Africa or Oceania. That count is
unchanged and still exactly true of those forty-one, and it could not be
extended, because the catalogue's per-item attribution is not in the data this
campaign fetched. What could be re-counted across all 301 is the catalogue's
own topical tag, and it says something real but different: closure drags in a
wide band of numbers, geometry, logic and astronomy that three arcs about
knights, letters and steam never touched.

**And nothing about the world moved.** The ladder is untouched, the horizon
still dates ruins, and every trajectory verdict in this column remains
unscored because the model is monotone. What this campaign changed is the
sharpness of the falsifier a successor will have to survive: the register now
states, for each of three hundred and one capabilities, the full set of things
a world would need in order to hold it — and states it derived from the
source's own edges rather than from anyone's recollection of them.

A cadastre exists because you cannot tax, sell, inherit or defend a parcel
whose boundaries are only approximately known. The same is true of a claim.
