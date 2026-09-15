# Species

**Questions it answers:** What makes one people different from another, and
what has to stay fixed for that difference to be legible rather than
arbitrary?

Year 1 built one people. Every settlement in Hornvale, tier 0 through tier 1,
was goblin; its social structure grew from land and pressure, but its
psychology was never named because there was only one psychology to name.
Year 2's first campaign, The Peoples, gives the world a second people —
kobolds, nocturnal deep-dwellers — and, underneath both, the substrate that
makes the difference real rather than cosmetic: a new kernel-only domain,
`domains/species`, holding nothing but authored definitions of what a people
is disposed to do. Placement and social structure now diverge between
goblin and kobold for reasons a player can be told, not for reasons baked
separately into two copies of the same code.

**Species are data; the social grammar is code.** The domain follows a
rhyme already ratified elsewhere in the project — studies are data, metrics
are code — restated here as: species are data, the social grammar is code.
A species is a point in a small, closed parameter space, never a table of
rules of its own. The alternative was tried on paper and refused: a
per-species `(role, condition)` table living in data reads, at first, like
configuration, and becomes, by the second or third species, a bespoke
interpreter for a language nobody designed on purpose — the ontology trap
with a welcome mat out front. Species as code — one Rust type per people —
was refused for the opposite reason: every new people would be a code
change, and the Laboratory could never sweep a space it isn't a point in.
Culture's rule table stays exactly what it was, one reviewed function, and
grows a parameter; adding a third people later edits no code that goblins or
kobolds depend on, only a row in a registry.

**The closed vector, and the manikin.** What was authored as one closed
six-dimension psychology vector is, since *The Cloister* (Campaign 4 of the
Dragons program), two closed vectors cleaved along who carries them. The
**mind vector** — three scalars bounded in `[0, 1]` (threat response,
deliberation latency, time horizon) — is carried by every minded kind: the
twenty-five peoples and, since *The Eremite*, the three solitary dragons
too. The **society vector** — one scalar (in-group radius, `[0, 1]`) and two
enumerations (sociality mode: hierarchic or communal; status basis: rank,
knowledge, or generosity) — is carried by a minded kind that lives
*socially* (a `Gregarious` or `Settled` kind), and by no other; a `Solitary`
creature (a dragon) carries none. The gate is sociality, not settlement — a
nomadic band would carry a society without ever settling.

**That last clause stopped being hypothetical at [The
Tidemark](../chronicle/the-tidemark.md).** This passage used to add that "today
that set is exactly the fifteen settling peoples, since no `Gregarious` kind is
yet minded" — a vacancy *The Vacancy* deliberately left open and recorded
rather than filled, because a settlement-free people is unaudited everywhere
downstream of a settlement. The marine roster filled it: **merfolk** are minded
and `Gregarious`, so they carry a society row and place no settlement at all.
The gate has read *minded ∧ social* since decision 0068 and `Settled` was only
ever an extensional coincidence; the two sets now differ by exactly one, at
twenty-five society rows against twenty-four settling kinds. The correction
matters beyond the count, because anything that read "the settling peoples" as
"the kinds carrying a society row" was reading a coincidence — and two such
sites were found by being wrong, not by being audited.

Both vectors are read against a **manikin**: a reference vector belonging to
no creature. Every scalar is a bare ratio in `[0, 1]` whose `0.5` is the
manikin's **neutral midpoint** — the reading that leans neither way, and the
genuine middle of the interval. The three enumerations are a different case,
and the asymmetry is real rather than papered over: an authority shape and a
waking schedule have no middle, so `Hierarchic`, `Rank`, and `Diurnal` are
the manikin's **designated defaults** — chosen because something had to be,
not because they are neutral, average, or typical. The manikin carries no
identifier and no registry row, and it has neither mass nor niche: it cannot
be placed in a world, because there is no body to place. It is described at
length in [The Manikin](./manikin.md).

Goblin is currently authored at exactly the manikin's values — every scalar
at 0.5, every enumeration at the designated default. That is a fact about
goblin rather than a fact about the manikin: goblin was the first people
written down, and its row was never characterised so much as left where it
started. A characterization test pins the coincidence, so that giving goblin
a temperament of its own later arrives as a visible change to that test
rather than as a silent shift in every goblin-bearing world.

Every downstream formula that reads either vector is built so that the
manikin's values reproduce today's behavior — not tuned to match, but
constructed to match, by writing the formula as an expression like
`0.5 + time_horizon` or `1.5 − threat_response` that evaluates to today's
plain constant precisely when the vector reads the manikin. A consumer that
needs a society reading for a kind that carries none resolves the manikin's
society vector, so a solitary creature's absent society reads as the
reference figure's, never as an error or a zero. This is the mechanism, not
merely a claim, behind the campaign's keystone test: a world generated with
the species pin restricted to goblin must be byte-identical to a world
generated before species existed at all, because goblin — sitting where it
happens to sit — and "no species substrate" are, by construction, the same
input to every formula in the cascade. *The Cloister*'s own keystone test
extends the claim one level: cleaving the vector in two moves no byte of any
generated world or reference artifact, verified by full regeneration and
diff, not merely asserted.

**The society vector is a grid/group instrument, and since *The Tolerance* the
chapter says so.** Douglas's cultural theory arranges societies on two axes —
*grid*, how rule-bound a life is, and *group*, how bounded "us" is — into four
biases: hierarchy, egalitarian/sect, individualist, fatalist. Sociality mode is
this model's grid and in-group radius is its group, and reading them that way
is a commitment rather than an observation: it says that a people's cosmology,
its appetite for risk and its stance toward outsiders are things to be
**derived from where it sits on two axes**, not authored one people at a time.
Adding a people then becomes a matter of placing it, and the placement carries
predictions that can be wrong. The adoption is presently documentary — no
consumer reads a quadrant yet, because both axes are per-people constants and
wiring one into behaviour without a measurement attached would have been an
unpreregistered change. What is recorded is the reading, so the campaign that
wants it inherits an argument instead of inventing one.

**A people is authored as a location and a spread.** Every scalar above names
where a people sits; since *The Tolerance* a second number beside it names how
far its members scatter around that point, one dispersion per vector rather
than one per dimension. The frame is stated rather than left implicit — the
authored value is the **mean**, and the dispersion is a standard deviation
about it — because an unstated frame is the defect The Manikin removed one
level up. The family is stated for the same reason: the draw is a **uniform on
±√3σ**, clamped to the axis, the half-width chosen so that a uniform's variance
`a²/3` makes the realized standard deviation exactly the authored `σ`. A uniform
gives *spread* but not *rare extremes* — beyond its support the probability is
zero, not small — so a consumer that needs the exceptional member rather than
the merely varied one needs a different family, not a larger number. Zero
reproduces the older model exactly: a people every one of whose members is the
same member. The consequence is not decorative. A threshold
applied to a distribution is not the same operation as a threshold applied to
its mean, so a people authored *below* a behavioural gate no longer simply
lacks the behaviour; some fraction of it clears the gate, and which fraction is
a property of the spread its authors gave it.

**The authoring corpus.** Kobold's six numbers are not measured, drawn, or
fit — they are read. The project's method for authoring a new people is to
take the Dungeons & Dragons 5th Edition System Reference Document as a
corpus of decades of playtested lore and translate a race's flavor and
statistics into the six dimensions by hand: the same "models author, dice
roll" pattern already governing this project's other authoring work — a
person makes the judgment call once, in the open, and the simulation then
runs deterministically from what they wrote down. Every authored kobold
value carries a one-line derivation naming the piece of lore behind it, and
where the source material pulls in two directions the chapter follows one
coherent reading and says which. The corpus's value outlasts this campaign:
it is one consistent measurement frame that will eventually span peoples
and beasts alike — a future water buffalo or tyrannosaur sits in the same
kind of dimensions a kobold does — a vein the frontier map notes as still
to mine.

*Kobold parameters in this chapter are derived from the Dungeons & Dragons
5th Edition System Reference Document 5.1, available under the Creative
Commons Attribution 4.0 International License (CC-BY-4.0). No SRD text is
reproduced here — only parameter derivations, paraphrased to one line each.*

**The kobold mind card** (the three individual dims, carried by every minded
kind — including a solitary creature with no society of its own):

| Dimension | Type | Manikin | Kobold | 5E derivation |
|---|---|---|---|---|
| Threat response (flee ↔ stand) | scalar `[0,1]` | 0.5 | 0.8 | cowardly in the open field, but entrenched at home behind the traps and tunnels the warren has prepared |
| Deliberation latency | scalar `[0,1]` | 0.5 | 0.7 | communal decisions arrive by slow consensus |
| Time horizon | scalar `[0,1]` | 0.5 | 0.8 | generational works — tunnel complexes, egg-tending — that pay off only across years |

Goblin's authored mind currently coincides with the Manikin column exactly.

**The kobold society card** (the three community dims, carried only by a
`Settled` kind; a solitary creature carries none, and a consumer that needs a
society reading for one resolves the manikin's society vector instead):

| Dimension | Type | Manikin | Kobold | 5E derivation |
|---|---|---|---|---|
| In-group radius | scalar `[0,1]` | 0.5 | 0.2 | insular warrens; loyalty runs tight and does not extend past the pack |
| Sociality mode | enum | Hierarchic | Communal | pack tactics and communal egg-tending, not a chief's household |
| Status basis | enum | Rank | Knowledge | trap-cunning and craft esteemed over raw dominance |

Goblin's authored society coincides with the Manikin column here too. Note
that the two enumeration rows are designated defaults, not midpoints: only
the in-group radius row is a genuine middle.

Kobold's values are authored judgment, not a fixed translation — the
design's requirement is only that they land somewhere meaningfully unlike
the manikin, and that `0.5` always means the neutral midpoint rather than
any particular people's reading. A vocabulary rides alongside the vector,
one word per rung: kobold's
worker rung is always "digger" — fixed regardless of subsistence mode,
unlike goblin's, which still names the subsistence worker itself
(farmer/herder/fisher/forager) — and kobold's warrior, artisan, shaman, and
top rungs render as "warden," "shaper," "keeper," and "elders" where
goblin's render as "warrior," "artisan," "shaman," and "chief"; the
settlement itself is a "warren" rather than a "village." A structural
consequence rides along for free: kobold's knowledge-based status basis
(below) makes the rank-gated slave rung unreachable, so no kobold ladder
ever needs a word for it, and every kobold structure tops out at "elders,"
never "chief" (`domains/culture`'s structure function; see
[Culture](./culture.md)). These role words are common nouns, not proper
nouns, and stay English by convenience even after Campaign Y2-3 (The
Tongues) gave the world a generated tongue — that campaign gave *names* a
real sound (a settlement, a deity, a deity's epithet), deliberately drawing
the line at the lexicon a common noun like "digger" or "chief" would need;
see [Language](./language.md)'s bright line. This vocabulary keeps waiting,
now on a lexicon and a lens for common nouns, not merely a phonology.

**Idle by design; banked for later.** Not every dimension pulls its weight
the day it is authored. Deliberation latency is authored, differs
meaningfully between the two peoples, and this chapter's first campaign
declared it idle — consumed by no rule at all — banked for a future
salience or negotiation rule, once observers judge each other's
decisiveness directly; that rule still has not arrived. Campaign Y2-3 (The
Tongues) spent a first, narrower share of it without waiting for the
negotiation rule: `domains/language`'s `voice_params` folds deliberation
latency in, half and half with status basis, to derive the formality voice
knob (see [Language](./language.md)) — so the dimension is no longer idle
exactly as first banked, only not yet spent on the decisiveness-facing rule
it was banked for. A banked dimension can be spent gradually, on more than
one rule, rather than all at once; the full model card below tracks which
dimension is spent where. Kobold nocturnality was banked the same way
through this chapter's first campaign,
more literally: an activity-cycle dimension had no seat in this closed
vector, so nocturnality lived only as authored prose with nowhere to run.
Campaign 15 (The Eyes) spent it: `domains/species` now also holds a closed
three-dimension **perception vector** — activity cycle, night vision, sky
attention — a second component keyed to the same kind alongside the mind and
society vectors above, and kobold's authored nocturnality finally reaches a
formula, a species-specific characteristic hour and salience lens that
crowns the moons and the night sky over the sun. See
[Perception](./perception.md) for the vector, the lens derivation, and what
it let religion do with two peoples instead of one. Campaign Y2-3 (The
Tongues) added a third: a closed **articulation vector** —
labiality, vowel-space breadth, voicing contrast, sibilance, voice
loudness, a tonality the phonology epoch later added, and an exotic manner a
species' anatomy affords — again read against a manikin of its own, and read
from the same D&D 5E corpus for kobold. That manikin is not uniformly `0.5`:
tonality's `0.0` and the exotic manner's `None` are designated defaults, not
midpoints. Since
*The Dissolution* the articulation vector is owned by `domains/language`
itself — its true home, the domain that reads it to build every name — rather
than by `domains/species`, keyed to the same kind. See
[Language](./language.md) for the vector itself and what it let every name
in the world become.

**In-group radius went idle, then acquired two new readers.** It once scaled
the coastal term of the settlement-placement suitability formula, and *The
Gathering* retired that formula for a carrying-capacity field whose coastal
bonus is fixed rather than psychology-scaled. *The Tithe* later read the same
authored scalar as how much a vassal conceals from an outsider. *The Housemark*
now reads it again as threshold posture: inward, plain or outward. The vector's
shape never changed while its consumers did, which is the value of banking an
authored dimension instead of deleting it when one use disappears.

**The ontology-trap posture.** This closedness is a stance, not an
oversight. The frontier map warns, more than once, against traits that grow
without a designer choosing to grow them — an inheritance hierarchy here, an
extensible trait bag there — and this vector is where that warning is
finally enforced: six dimensions, no more, no per-individual variation, no
inheritance between species. A seventh dimension, or a psychology that
varies goblin-to-goblin rather than only goblin-to-kobold, is real work with
its own design principles; it does not arrive as a quiet addition to a
struct that happened to have room.

**The full species model card.** Since *The Dissolution* (the entity-component
program's third campaign) there is no `SpeciesDef` struct at all. A kind is a
**set of components**, each keyed by the kind's stable `KindId` label, each
authored and owned by the one domain that presents it, composed only at
worldgen. `domains/species` authors the universal **biosphere** component
(`BiosphereTraits` — mass, thermal strategy, trophic mode, resource niche,
condition niche, potency, social organization, life schedule — the row every
kind carries and the packer and habitat model read), plus a **sparse**
habitat-realm component, **fifteen rows today across three realms** — seven
`Subterranean` (the xorn and the rust monster, the drow, and the four
Underworld peoples), six `Marine` (the marine peoples
[The Tidemark](../chronicle/the-tidemark.md) authored), and two explicit
`Surface` rows — gated on whether the cell holds a cave at all, or holds a
water column at all,
and, since *The Cloister*, two psychology components where there used to be
one: the **mind** vector (3 — threat response, deliberation latency, time
horizon), carried by every minded kind, dragons included. Since *The Vigil*,
the **perception** (3) component is carried by every kind that *speaks* — a
chain, not a settlement gate: speech presupposes perception presupposes a
mind — so the three chromatic dragons carry it alongside the twenty-five
peoples, though nothing settles a dragon does. The **society** vector (3 —
sociality, status basis, in-group radius) stays gated differently, on
*sociality* rather than speech: only a minded kind that lives socially (a
`Gregarious` or `Settled` kind) carries it, which today is the twenty-five
peoples — twenty-four that settle plus the `Gregarious` merfolk, which do
not — while a `Solitary` dragon carries none.

**The habitat-realm gate gained a second dimension.** *The Sources* changed
how the three subterranean kinds' conditions are read, not how many of them
there are. A chamber used to be scored once, at the deepest point of its
column; it is now scored **per rung**, each read at that rung's own offset
from the surface temperature, and a subterranean kind is credited with
whichever rung its whole suitability score is best at, not the deepest one
by default. The same campaign gave the underworld its own productive term,
derived from the surrounding rock and thermal gradient rather than from
sunlight, and rewired the xorn's niche to draw on it — the one kind of the
three whose capacity moved, because the rust monster and the drow weight
nothing on that new term at all.

**And then a third realm ([The Tidemark](../chronicle/the-tidemark.md)).** The
enumeration is two-valued no longer: a `Marine` kind is scored at every band of
the sea's pelagic ladder and credited with the best of them, exactly as a
subterranean kind is scored at every rung of its column, behind an availability
mask that is `{0, 1}` on whether the vertex carries a water column at all
rather than a graded tolerance. Because a domain may not depend on a sibling,
this enumeration is the same axis climate already expresses as a realm and a
medium, so the two are pinned against each other by a **two-directional**
agreement test at the composition root — forward over the realms and backward
over the media, since a one-directional round trip cannot see a new realm that
reuses an existing medium. Adding the variant was deliberately done by letting
the compiler produce the site list rather than a grep, which found seven sites,
three of them in places nobody had enumerated.

Presence in this store consequently stopped meaning what it used to.
**Absence still means `Surface`; presence no longer implies non-`Surface`** —
the sea elf and the giant crocodile carry explicit `Surface` rows, not because
the default is wrong for them but because a reader meeting `Marine` for the
first time would reasonably assume a sea elf belongs to it, and the mask has no
middle value with which to be half right. The kuo-toa is the same shape read
from the other side: residence subterranean, reach aquatic, authored that way
by a parallel campaign that did not yet have the vocabulary.

Two limits of the new realm are measured rather than suspected, and both are
about the *ladder* rather than the gate. The five pelagic strata are
distinguished at placement **by depth alone**, except at the seabed band of a
vertex a vent is lighting: the column is handed one temperature per vertex,
moisture is a constant, and the light that does vary by band is read by
nothing. And **depth confines downward only** — every ocean column has a
surface band, so a shallow kind is near-optimal at *every* ocean vertex, and
the argmax among shallow kinds then falls to a fraction of a percent of
difference in the sovereignty floor. A kind whose identity is a pelagic *zone*
is expressible; a kind whose identity is "warmer water at depth" is not yet.

`domains/language` authors the **articulation** (6) and the social
**lexicon** — the speech a kind that speaks carries. Since *The Vigil*, a
dragon carries three of this table's four vectors — mind, perception,
articulation (and lexicon) — and is absent only from Society. The rest of
the menagerie's biosphere-only fauna (treant, xorn, …) still have no rows in
any of the peopled registries, and none of this table; their absence there
is expressed rather than declared, unlike a dragon's now-partial presence.
They are nonetheless *nameable*: since *The Actants* every kind the world
simulates carries a concept, so a treant is a thing the vocabulary knows of
even though nothing about a treant thinks, speaks, or lives socially. The two
are genuinely independent — being nameable is a fact about the world's
vocabulary, being minded is a fact about the creature — and conflating them
is precisely how twelve creatures went four campaigns with no name at all.
The one biosphere dimension the fauna carry
distinctly is **potency** — a creature's magical might — assayed, like `mass`,
from the D&D 5E corpus: it is the kind's adult Challenge Rating over thirty
(`CR/30`), nonzero only for the supernatural set (dragons, treant, xorn) and
zero for mundane beasts. It is the term that buys the mighty their rarity,
through the sovereignty floor the habitat model reads. A second universal
biosphere dimension is **social organization** (`SocialForm`: sessile, solitary,
gregarious, or settled) — orthogonal to the mind, perception, and speech
*capacities* a creature may or may not carry. Only the settling peoples build
settlements; and the capacities compose as a **nested lattice** rather than the
old all-or-none bundle: a **chain**, not a menu — speech presupposes perception
presupposes a mind — that a kind may stop climbing at any rung, but never skip
a rung above the one it stops at. A settling people carries the full cluster,
society included. A `Solitary` kind carries no `SocietyVector` at all, since
society gates on *sociality*, a different axis, not on how far up the
mind-perception-speech chain a kind climbs; the three chromatic dragons climb
it all the way — each carries a mind, perceives, and speaks — and are absent
only from Society. A consumer that needs a society reading for a solitary
kind resolves the manikin's society vector.

What the old `Option<PeopledTraits>`
once guaranteed by the shape of a type — the peopled traits together or none —
worldgen now enforces as this load-time nested-capacity check across the
registries. Every dimension here is still **authored**. Nothing in this table is
drawn, fit, or measured; species is data written once by a person reading a
corpus, the same posture each of the four sub-vector sections above keeps on
its own. The "consumer" column names the actual formula that reads each
dimension today, not the formula a dimension was originally authored for —
the two differ for one row, called out below the table.

| Vector | Dimension | Type | Goblin | Kobold | Consumer |
|---|---|---|---|---|---|
| Mind | Threat response (flee ↔ stand) | authored, scalar `[0,1]` | 0.5 | 0.8 | culture's warrior-rung threshold; demography's hostility factor (carrying-capacity field) |
| Mind | Deliberation latency | authored, scalar `[0,1]` | 0.5 | 0.7 | language's formality voice knob (partial — see below) |
| Mind | Time horizon | authored, scalar `[0,1]` | 0.5 | 0.8 | culture's artisan-rung threshold; demography's freshwater factor (carrying-capacity field) |
| Society | In-group radius | authored, scalar `[0,1]` | 0.5 | 0.2 | history's tribute concealment; vessel's inward/plain/outward housemark threshold |
| Society | Sociality mode | authored, enum | Hierarchic | Communal | language's repetition voice knob; vessel's command/common housemark authority |
| Society | Status basis | authored, enum | Rank | Knowledge | culture's slave-rung gate; language's formality/epithet-density knobs and honorific gate |
| Perception | Activity cycle | authored, enum | Diurnal | Nocturnal | perception's characteristic hour and lens activity factor (Crepuscular carried by `white-dragon` — see below) |
| Perception | Night vision | authored, scalar `[0,1]` | 0.5 | 0.9 | perception's night-sky lens weight |
| Perception | Sky attention | authored, scalar `[0,1]` | 0.5 | 0.8 | perception's day-sky/night-sky/ambient lens weights |
| Articulation | Labiality | authored, scalar `[0,1]` | 0.5 | 0.1 | language's labial-segment gate (envelope) |
| Articulation | Vowel-space breadth | authored, scalar `[0,1]` | 0.5 | 0.3 | language's permitted-vowel band (envelope) |
| Articulation | Voicing | authored, scalar `[0,1]` | 0.5 | 0.6 | language's voiced-segment gate (envelope) |
| Articulation | Sibilance | authored, scalar `[0,1]` | 0.5 | 0.9 | language's sibilant keep-probability bonus (drawn inventory) |
| Articulation | Voice loudness | authored, scalar `[0,1]` | 0.5 | 0.2 | language's high-sonority keep-probability penalty and exotic-manner down-weighting (banked to derive from a future body/frailty vector — see [Language](./language.md)) |
| Articulation | Exotic manner | authored, enum | None | Trill | language's exotic-segment gate (Click/Ejective idle — claimed by neither shipped people) |

Two rows are worth reading carefully rather than at face value. **Deliberation
latency**'s consumer is a genuine but partial one: Campaign Y2-3 folds it,
half and half with status basis, into language's formality knob, so the
dimension is spent on *how a myth is told*, not yet on the decisiveness-
facing salience or negotiation rule this chapter originally banked it for —
see "Idle by design; banked for later," above. **Activity cycle**'s
`Crepuscular` variant is authored into the closed enumeration and its lens
activity factor (0.7) is authored and ready in `domains/perception`'s
formula; it is no longer idle since *The Vigil* — `white-dragon` reads
`Crepuscular` off its own authored insolation optimum (0.05, polar,
twilight-dominated light), the first shipped kind to carry the value.
`red-dragon` (insolation optimum 0.20, open volcanic terrain) reads
`Diurnal` and `black-dragon` (0.10, shaded lowland swamp) reads `Nocturnal`
by the same rule — one ecological schedule per dragon, derived from each
kind's own already-authored niche rather than authored directly on the
perception vector, so the three dragons diverge in activity even though
they share one clade eye (`night_vision = 0.9`). Click and Ejective still
wait in the articulation vector's own enumeration for a species anatomy
that claims one.

**The two Society rows now meet again at a dwelling's threshold.** *The
Housemark* reads sociality as command versus common seating and bands
`in_group_radius` into inward, plain and outward threshold postures. The two
readings remain independent, producing six reachable chamber signatures over
the fifteen peoples that campaign measured. The roster is twenty-five now, and
the reachable count has not been re-measured against it. This is a
vessel-owned, seedless reading of the
authored vector; it does not move society into the vessel or make culture a
species-name table.

**A solitary dragon carries three of this table's four vectors, and none of
its Society row.** The three chromatic dragons are `Solitary`, not
`Settled`: each authors a mind vector (threat response 0.95, deliberation
latency 0.5, time horizon 0.90 — an apex that stands, a banked deliberation
dial, a centuries-long hoarder) and, since *The Vigil*, a perception vector
(night vision 0.9, the shared clade eye that lexicalizes exactly `dark`,
`light`, and `red` — see [Perception](./perception.md)) and speech (a frozen
Draconic tongue — see [Language](./language.md)) — but no society vector at
all. A consumer that needs a society reading for a dragon — there is none
among the unplaced-path consumers today, since dragons are not yet placed —
resolves the manikin's society vector, which the goblin column above
currently happens to match.

**No drawn parameter lives on a species.** Every cell in the table above is
authored; `stream_labels()` in `domains/species` returns an empty vector
because there is nothing in this domain a seed ever touches. The seeded
draws that make two peoples sound and settle differently are real, but they
belong to the domains that consume this vector, not to species itself:
`domains/language` draws a phoneme inventory and syllable phonotactics
*under* the articulation envelope above (see [Language](./language.md)'s own
model card), and `domains/demography` folds threat response and time
horizon into the carrying-capacity field each species reads (see
[Settlement](./settlement.md)) — no draw at all any more, since *The
Gathering* moved population from a per-site draw to a field readout, but
still a real per-species divergence these two psychology scalars produce. A
species is the fixed point those draws (and, now, this one field) are taken
with respect to.

That sentence survives *The Tolerance*; the clause that used to close it —
*"never a distribution of its own"* — does not.
`stream_labels()` here is still empty and every cell in the
table above is still authored — the dispersion beside each vector is authored
exactly like the location it accompanies, and the draw that reads the pair
lives at the composition root, where a settlement's founding site and year are
in scope. But a species is no longer *"never a distribution of its own"*, which
is what this chapter said until a people acquired a second authored number.
The distinction worth keeping is between where a distribution is **written
down** and where it is **sampled**: the first is here, the second is not, and
`domains/species` remains a domain a seed never touches.

**Threat response found a second, sharper consumer in *The Tumult*.** Where
demography folds it smoothly into a capacity term, the deep-history bake's
conflict rule reads it as a **gate**: a settlement whose threat response falls
below a fixed threshold does not raid at all, however strong it is on paper.
Two things follow that are worth recording here rather than only in
[Settlement](./settlement.md). First, the gate produces an **asymmetric**
aversion structure with no pairwise machinery whatever — each raider gates on
its *own* number, so one community can decline a fight its neighbour is happy
to pick. Second, and more pointed at the time: threat response was for one
campaign the **only** per-species input the bake received, so every
people-to-people asymmetry the deep past produced ran through a single scalar.

**The gate sorted *kinds* until *The Tolerance*, and now sorts *places*.** For
two campaigns the comparison read a species constant, so every settlement of a
people answered it identically and one could say flatly that the goblin and the
human — both authored at 0.5 — never raided while the other four might. That
sentence was only ever sayable because a people was a point. Once a people
became a distribution, the gate's input became a *draw*: each settlement takes
its own threat response from its people's authored mean and dispersion, keyed
on where and when it was founded. Every one of the six settling peoples the
campaign measured — the roster has since grown to twenty-five, which that
measurement does not cover — now has
settlements on both sides of the threshold — the assertive ones mostly above it
and the neutral ones mostly below, but none wholly either. Warlikeness became a
property of a place rather than of a kind, which is what makes an aversion
between two neighbouring towns of the same people expressible at all.

**That is no longer true, and the two scalars that joined it were already
authored and simply unread.** *The Tithe* gave the bake a standing tribute
relation, and both sides of the negotiation over it consult a different axis of
the same authored psychology. The vassal's **`SocietyVector.in_group_radius`**
sets its **concealment** — how much of what it owes it can hide from an
outsider — so the insular kobold at 0.2 withholds most and the expansive gnoll
at 0.7 least. The patron's **`MindVector.time_horizon`** is read as a
**discount rate** on the future stream its vassal represents: because a
community grows logistically, maximum sustainable yield sits at half of
capacity, so a generational patron holds its vassal near that peak while an
immediate one strips it. On the shipped roster the patrons order gnoll (0.2),
bugbear (0.3), goblin (0.5), hobgoblin (0.5), human (0.75), gully-dwarf (0.8),
kobold (0.8), hill-dwarf (0.85), desert-dwarf (0.9) — the dwarf family, whose
long life *The Delvers* authored, sitting at the generational end of it. Until
*The Tolerance* that list stopped at four: the goblin's and the human's threat
responses were both authored at 0.5, below the raid threshold, so as *kinds*
neither could ever take the initiative and neither could ever become a patron.
Now that threat response is drawn per settlement, a particular goblin or human
town can clear the gate, and the full ordering above is reachable —
and extraction rate and relation lifetime are both monotone in that order. It
is also the mechanism by which a subjugated people can be extinguished at all,
since extermination is optimal exactly when the discount rate exceeds intrinsic
growth, which makes extinction rare and concentrated on the short horizon.
Three authored scalars now reach the deep history where one did. The condition
niche still does not, which remains a real limit of the bake rather than a
property of the vector.

**The condition niche's four axes, and the frame each is said in.** The
biosphere component's `ConditionNiche` carries one response curve per
environmental axis — temperature in °C, moisture in the climate field's
`[0,1]` unit, insolation relative to the planet's global scalar, and
elevation in **metres above the world's own sea level**. That last frame is
newer than the others and worth stating plainly, because it was wrong until
*The Tumult* and wrong invisibly. Elevation had been scored against the raw
[reference datum](../chronicle/the-datum.md) — an *isostatic* zero, a
reference-thickness crust floating at equilibrium — which is the right frame
for a planet's crust and the wrong one for a creature. A world's sea level is
itself a value of that type, derived from the elevation field and different on
every world: across a hundred sampled seeds it ranges from −1723 m to
−3478 m. So an optimum authored as "2600" did not name an altitude at all; it
named 5200 m above sea level on one world and 5900 m on another, in both cases
at or above the highest land those worlds had. The kobold's authored highland
stronghold — the one axis meant to be its exclusive, uncontestable ground —
was not merely uncontested but **unoccupiable**, and its habitat fit ran
roughly an order of magnitude below every other people's *everywhere*, which
is precisely the signature of a niche that has been quietly deleted rather
than lost. The correction is one subtraction at the substrate boundary
(`elevation_at(cell) − sea_level`), and it makes the axis a fixed frame that
an authored number can mean something in. The optima are now stated against
the measured distribution of settleable land — median 1561 m above sea level,
quartiles 621 m and 2651 m, the 95th percentile at 4148 m — and the four
peoples of that roster tile it: bugbear's lowland at the 15th percentile,
hobgoblin's plains at the 24th, goblin's wide generalist curve centred on the
median, and kobold's stronghold at the 79th, with the giant goat's alpine
ceiling above it at the 91st. The three dwarves *The Delvers* added are
authored against the same distribution and fill in below the median —
gully-dwarf at 150 m, desert-dwarf at 700 m, hill-dwarf at 900 m — which is
the crowded end of it. Measured over the same seeds, the kobold is now the best-fit
people on every settleable cell above 3000 m and the worst below 500 m, which
is what "highlander" was always supposed to mean. The lesson generalizes past
this one axis: **a unit is not a frame.** Both quantities were honest metres;
only one of them was metres from a place a creature could care about.

The correction also exposed something the bug had been hiding, and the fix for
that is the second half of the same story. Under the old datum an ocean cell
sat some four kilometres from every authored optimum, so the elevation axis was
incidentally acting as a **land mask** — and two of the habitat model's supply
axes had never had one of their own. Photosynthate and plant forage ride the
carrying-capacity field, which is land-limited by construction (habitability
requires a cell above sea level); the ambient detritus term was a global
constant and the mineral term reads a prospectivity field, and both were
defined on the seafloor exactly as on a hillside. With the elevation axis
re-datumed and no longer excluding submerged cells by accident, the three kinds
that eat detritus or rock scored right across the ocean floor: at seed 42 the
otyugh's total suitability went from 0 % to 85 % submerged, the rust monster's
from 22 % to 86 %, the xorn's from 58 % to 74 %. No settlement landed there *at
the time* — every one of seed 42's 216 stack settlements then stood on land,
because every people's supply was land-limited — but a swamp detritivore whose
habitat is mostly seabed is not a claim this model should be making. That
"then" is load-bearing now, and the paragraph after next says why.

The repair is a **supply-term** one, not an elevation one, and where it was put
matters more than what it does. The blunt option was to multiply assembled
carrying capacity by a land mask, which would have stated "nothing lives in
water" as a law of the model — a law that would have to be *unstated* the day
an aquatic kind is authored. Instead each of the v1 resource-supply axes is
declared *terrestrial*: detritus becomes a field rather than a constant, zero
below sea level, and mineral supply is masked the same way, so all five axes
now carry the land limit that three of them always carried implicitly. A
species' habitat then follows from what it eats. No kind is forbidden the sea;
the roster **of that day** simply had nothing that could feed there — checked
kind by kind it was entirely terrestrial, and even the two wettest niches, the
otyugh's and the black dragon's, were swamps, which is wet *land*. All three
submerged shares return to 0 %, with every kind's land carrying capacity
byte-unchanged. An aquatic kind arrives by weighting a marine supply axis with
a field defined on water — an addition to the supply vocabulary rather than an
exemption from a rule.

**That last sentence is now history rather than description**, and the roster
sentence above it with it. The Vacancy authored the marine axis this paragraph
anticipated, exactly in the shape it predicted, and the sea is inhabited. The
Repose then measured the consequence at scale: over thirty seeds, 33,544 of
59,690 stack settlements stand on cells that are not settleable land, and the
giant squid alone holds 30,971 of them, 51.9% of the unfiltered total. A
reader taking "the roster is entirely terrestrial" as a live fact would
mis-scope any land-only statistic by more than half.

**And the sea is now *peopled*, not merely inhabited ([The
Tidemark](../chronicle/the-tidemark.md)).** Six marine peoples carry minds,
speech and — for the five that settle — society rows, and they place
settlements in the water column rather than on the shore. The same campaign
measured what the sea's roster does *not* have, before authoring the
subsistence roster its own design called for, and stopped on the answer:
**every marine kind resolves to trophic height exactly 1.000** — six peoples
and four fauna alike — and the food web omits all ten as keys, because the
model's predator test reads the animal-prey axis and marine niches spend
their weight on marine forage, photosynthate and chemosynthate instead. By
this model's own classifier a killer whale is not a heterotroph. Competition
is the half that *is* real, and it is saturated: pairwise niche overlap is
**1.0000 for eight of the ten**, identical niche vectors, so a 5,400 kg
whale and a merfolk settlement compete at exactly the coefficient two merfolk
would. The sea has one axis where the land has two and the underworld three,
which is a fact about the *vocabulary* rather than about the sea, and the same
readout shows six terrestrial detritivores pinned at height 1.000 for the same
underlying reason: one axis standing in for a structure.

*The Vacancy* took both of those unfinished halves. The marine axis is real,
its supply derived from what climate already computes (the marine biome class,
sea-surface temperature, depth through the euphotic zone), and nine of the ten
marine biomes now carry occupants. The animal-prey axis is real too, a
trophic-transfer fraction of forage — which mattered more than it sounds,
because a hard-coded zero there had meant the three chromatic dragons and the
owlbear, all obligate predators, had zero carrying capacity on every cell of
every world. They were in the registry and absent from creation, and had been
for four campaigns.

**The coverage table and the non-void rule.** That discovery is why the roster
now ships with two instruments beside it. A committed table names every declared
state of this model — each `ThermalStrategy`, `SocialForm`, `ActivityCycle`,
`StatusBasis`, and `LifeSchedule` variant, plus the trait *combinations* that
matter — and records
which kinds witness it, so a state cannot silently lose its witness or gain an
unintended one. It deliberately does not demand that every variant be witnessed:
an empty cell is a legitimate creature-design prediction, and a rule forbidding
them would only force junk into the roster. What it demands is that the record
match reality.

The second is a refusal rather than a record. **No kind may be void** — every
kind must achieve carrying capacity above the viability floor on at least one
cell of at least one world. A species can otherwise be authored, load, satisfy
every referential-integrity check here, and simply not exist anywhere, which is
what happened to the kobold's unoccupiable highland stronghold and to the four
obligate predators. That test costs almost nothing, runs in the commit gate, and
converts the whole failure class from something found by hand months later into
something that fails on arrival.

A companion measurement makes authored optima checkable at all: a committed
readout of where each kind actually lives, over thirty worlds. Condition optima
are measured-from-this-world values — they mean nothing except relative to the
land a world actually produces — so they perish whenever terrain or climate
moves beneath them, and the readout is what lets the next such move be
re-checked instead of merely hoped about.

**The peoples ahead:** deliberation latency's still-unspent half — the
salience or negotiation rule that would read a people's decisiveness
directly, now that language's formality knob has spent only the telling of
a myth, not the deciding of anything; a lexicon that would
give this chapter's role vocabulary (`digger`, `chief`, `elders`) a sound of
its own, the one stopgap Campaign Y2-3 (The Tongues) deliberately left
standing (it gave names a mouth, not common nouns a meaning); comparative
religion across more than a pair, once a third people exists to make
"comparative" mean something beyond two pantheons on one globe; drawn
variation, both per-species and eventually per-individual, in place of one
authored point per people; a physiology this vector does not yet have, so
habitat affinity and temperance stay shared rather than species-derived;
inter-species politics, trade, and conflict; and, past twenty-five, however
many further peoples the registry is asked to hold.

One limit was measured rather than suspected. Carrying capacity is a supply
term spanning orders of magnitude multiplied by a condition product bounded in
the unit interval, so an authored niche can only modulate the primary-production
signal, never select against it: a kind authored for a particular climate can be
genuinely present there and still rank behind kinds with no affinity for it at
all. The gnoll, authored for hot-arid desert, has none. Until that is addressed,
"centred on a biome" is not a thing this model can express.

**It was addressed, and the last sentence no longer holds.** The repair is not a
better curve but a different *place* in the pipeline: a **biome affinity** is a
per-kind mask over biome classes, applied *outside* the four-way tolerance
minimum rather than folded in as a fifth axis, so it cannot be discarded by the
minimum the way a climate curve is. [The Range](../chronicle/the-range.md) built
it and gave gnoll its desert back;
[The Radiation](../chronicle/the-radiation.md) authored six elves *entirely* in
that vocabulary and on no other route, and derived the one number the mechanism
had been guessing at.

Two things about it are worth carrying, because both were surprises.

**A row states a shape and never a level.** The rungs — stronghold, near,
marginal, elsewhere — are *preferences* in `[0, 1]`, mapped through the kind's own
sovereignty floor as `floor + (1 − floor) · preference`, so a row's default **is**
that floor and a stronghold is exactly `1.00` for every kind however heavy. That
is not a stylistic choice. The level had been an authored constant defended as
*gauge*, which is true of how a single kind ranks its own cells and false of
everything downstream: the same factor multiplies the capacity that becomes a
settlement's population. A uniform mask on one kind of thirty-nine removes 13.7%
of a world's facts (decision 0120).

**A `0.0` rung is a hard exclusion, not a strong dislike**, because the founding
pool filters on strictly positive capacity. Any zero in a row is a deliberate act
and the founder floor does not clear it.

What the mechanism does not yet express is *degree of centring*. A concentration
measured on the top rung alone reads a people that has moved entirely onto the
second and third rungs as a people that was merely thinned — which is exactly
what happened to the desert elf, and is the successor question rather than a
defect in the mask.
