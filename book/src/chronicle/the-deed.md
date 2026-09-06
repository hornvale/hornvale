# The Deed

A deed is a thing done and, in the other sense, the document that records it
having been done — the act and its instrument sharing one word. This campaign
is about the join between them. [The Tackle](./the-tackle.md) fitted the
harness while the animal stood still: an action layer in its own module, a word
returned to its rightful owner, one shared derivation of what a body weighs,
and every committed artifact byte-identical afterwards. Nothing there was
player-facing. This one drives. A possessed body's acts now cost it time and
leave facts behind, and the seed-42 possession transcript, which used to walk
its whole route at midnight of day zero, now comes out of the last doorway into
a golden morning.

## One suite, two moods, and a sigil that is a namespace

The Bridle — the program these arcs belong to, whose end is one action system
serving player and creature alike — put the mood on the *request* in its
earlier draft: `examine` and `!examine` were to be the same act invoked with
two authorities, differing only in whether a gate was consulted. Reading the
code refuted that before a line was written. `!map` draws the objective chart
where `map` draws the one coloured through this body's eyes; `!examine`
answers about a creature standing here in the dark that `examine` cannot see.
Those are not one act with two permissions. They are different acts wearing
similar syntax.

So being in character became a property of the **action** (decision 0169).
`Action::mood` is an exhaustive match over every variant with no wildcard arm,
which means a new act fails to compile until someone classifies it. There is
still one suite — the alternative, two parallel enums for creature and player,
stays rejected — but each member of it knows what it is.

The consequence that carries the most weight is at the parser, and it is the
difference between a namespace and a flag. The `!` is stripped *before* verb
lookup and routes to a separate table. A sigilled verb nobody classified is
therefore an ordinary unknown-verb refusal — `No verb '!xyzzy'` — and not a
silent fallthrough to the bare spelling. Had it fallen through, every
in-character verb in the language would have quietly acquired an
out-of-character twin that bypassed nothing, and the namespace would advertise
a capability it does not have. That is pinned by a test whose name is the
claim: an unclassified sigil verb refuses rather than aliasing to the bare
form.

Seven verbs lost their bare spelling entirely — `!why`, `!npcs`, `!help`,
`!eyes`, `!whoami`, `!provoke`, `!soothe`. These are operator instruments. None
of them names an act any creature could perform, so none of them has a
meaningful in-character counterpart, and an instrument that does not *look*
like one is how a verb came to be a side channel once already. The last two are
the surprising members of that list, because they commit facts; they are on it
anyway, and the reason is the more interesting half. A worldgen pin is already
an out-of-character act — `--plates 7` is the operator imposing a state the
simulation's own process did not choose — and `provoke` is the same category at
play-time instead of genesis-time. That reading is what makes the provenance
stamp on those facts load-bearing rather than hygienic: it is the only thing
that can tell operator-imposed world-state from simulation-produced
world-state inside a saved world.

Six verbs kept both forms, and each sigilled half had to earn its place by
**discriminating** from its bare twin somewhere. Four do it by taking a
renderer's own gating parameter to its permissive limit — the chart's observer
step declined, the chamber band's sight narrowing with nothing left to narrow.
The other two relax nothing at all: `!look` and `!knows` call the very same
functions their bare spellings call, and their whole difference is that the
body's state cannot refuse them. That difference did not exist when the four
shipped, which is why the two were withheld then and shipped later. The
omission had been recorded together with its reason, and the reason — not the
conclusion — is what a later reader re-checked.

## A gap that can never close

Every action keeps a **required** concept name; the rejected alternative,
letting an instrument answer "no concept", would have let the reconciliation
that audits acts against the registry silently shrink its own denominator. So
the suite needed fourteen new concepts, appended as a new accession cohort
rather than inserted into an existing one — proto-root assignment is a global
ordered walk, and a mid-alphabet insertion moves words derived from concepts
that come after it.

Seven of them name in-character acts: `chart`, `look`, `sense`, `know`, `wait`,
`write`, `read`. `examine` and `look` fold onto the single concept `look`,
differing only by scope — a focused look at a named thing against a survey of
the surroundings — which is the same fold that already puts
walking between rooms and stepping across one onto the single concept `move`,
differing only by scale. All eight of the going verbs reuse that one: a
language has a word for going, not eight words separated by how far.

The other seven name operator instruments, and those cannot have a word at all.
The language domain already models "this concept has no word here, for a
recountable reason" as an exposure gap carrying a reason, and it had three:
*experiential* (this culture never met the referent), *perceptual* (its senses
have not resolved it), and *unnameable* (the referent is real and objective and
no culture here has the concept). None of the three fits, and the reason they
do not is the interesting part rather than an inconvenience: **all three
describe gaps that could in principle close.** A people can meet a referent, a
sense can sharpen, a concept can arrive. An out-of-character gap can never
close, because there is nothing in the world for it to be about. `!why` is not
a thing no goblin has encountered. It is a thing no goblin could encounter.

Hence a fourth reason, *extradiegetic* (decision 0172), and it is this arc's one
change outside the possession window. Concepts carrying it are excluded from
the family-level
proto-root universe exactly as unnameable ones already were, so no culture ever
draws a root for a word it could never have needed; the published dictionary
pages now carry lines reading `gap (extradiegetic): help is an operator
instrument; it has no referent in the world`.

The classification is driven by membership of an explicit list, not by any
reading of the registry's own absence markers, and that is deliberate. The
closest existing marker says a referent is objectively real and unnamed — which
is exactly [The Vernacular](./the-vernacular.md)'s spectral classes, and
exactly the opposite of what is true of an instrument. From the marker alone
the two are indistinguishable, so a generic rule over the registry could not
have told them apart. Common keeps its word for all of them, and correctly:
Common is the author's register and has no speakers.

## The tariff belongs to the body

The Tackle's finding was that the action clock charges time as a function of
the act, the mass of the thing performing it, and the ground it is performed
on — and that the function has no driver parameter and nowhere to put one. It
had simply never had a player routed through it. This arc routes one. An
in-character act consults the gate, charges against the possessed body's own
mass through the shared derivation The Tackle extracted, and commits its fact
through `agent_at_fact` — the same constructor the drive tick uses for a
creature's step.

The observable is the day-zero transcript, and it is the cleanest evidence the
arc produced. `!whoami`'s day report — the room and chamber header itself no
longer carries one, since The Ken moved it there for good — used to read
`day 0` throughout. A step inside a chamber now costs 0.01172 days and a
walk-band step ten times that, and no two walk-band steps cost quite the
same, because the uphill factor between the two rooms is real and is read
before the position moves. By the time the body has gone into a building,
crossed it, come out and walked three cells, the sky has passed from *Night*
to *Twilight. The horizon glows gold.* to *The sun climbs the morning sky.*
Not one line of the sky model changed; the body simply stopped being free,
and the fraction of a day it now spends carries downstream — the 90-day wait
that closes the transcript lands somewhere else, on a different evening,
with six more creatures having stirred.

That the trail is a creature's trail is asserted on the envelope rather than on
a count. The player's committed facts and a creature's are compared field set
for field set, predicate for predicate, object arity for object arity — and the
provenance is checked for any word naming the driver. It names an errand
instead: *walked on (its own errand)*, *turned back the way it came*. A
creature's provenance names the drive that sent it; the keystone requires that
nothing in the trace reveal that a different mind chose, so a possessed body's
must name an errand too.

None of it is filtered on the way to disk. A saved played world holds the
player's steps exactly as it holds a creature's, and holds the operator's
interventions stamped — which makes such a world *auditable* for intervention
rather than merely contaminated by it. The alternative, filtering at the save
boundary, was refused for a reason stronger than taste: a body that had walked
would return from disk having been nowhere, while the same ledger still held
the creatures that turned hostile toward it. A world with someone who was
acted upon and never acted is a worse artifact than a long one.

## Time and facts are separate questions

The two-tier position law (decision 0069) says an entity's persisted position
is its room and any finer coordinate is never serialized. Three of its
consequences fall out of the representation rather than being defended, and one
of them is that entering a room, moving within it, and leaving cannot alter the
world.

So those three acts now charge time and commit **nothing**, and the arc had to
learn that those are two questions before it got them right. A first cut read
the law as one question and made the band changes free as well as silent:
stepping one cell inside a chamber cost time while walking through a doorway
into a different chamber cost none, and leaving and re-entering was an
unbounded free loop. The law's own text is what settles it — *the only thing
spent is turns* — a sentence about the ledger that says nothing about the
clock, and which rests on the free loop not existing. All three band changes
charge now; all three still commit nothing.

Four verbs stay genuinely free, and say so where they are dispatched rather
than leaving it to be discovered: `dive`, `surface`, `delve` and `climb` change
the vertical band, no dial prices a descent, and a swim down a hundred metres
of water column is not a tenth of a walk between rooms. Inventing a number for
it would be building a second cost model, which this arc was explicitly not
allowed to do. A gap that is named in the place a reader will stand is a
different object from the same gap left implicit.

## A table with one row

The body-state gate is a table from the first day it exists, though it has
exactly one row: asleep. Its justification is empirical rather than
anticipatory. This window already carries a per-verb perception gate that had
to be patched after a verb walked straight around a redaction the pane had
performed one verb earlier ([The Sighting](./the-sighting.md)), and a second,
still-open instance sits documented in a neighbouring method's own comment. A
table with an exhaustive match over `(state, mood)` and no wildcard arm turns
the next omission into a compile error instead of a bug report. Out of
character is permitted in every row; in character is refused only while asleep,
with a reason a player can read.

Its review round produced the sharpest lesson in the arc, and it is the same
lesson The Sighting learned in a different costume. **The gate stands in front
of acts, not in front of verb resolution.** Consulted on every bare token, a
sleeping body answered *"You cannot — you are asleep."* to `xyzzy`, and — worse
— to `whoami`, thereby telling the player that a retired spelling was a real
in-character verb merely blocked by body state. A refusal that discriminates is
an oracle, and this one was leaking the shape of the verb table. It now
consults a roster of what actually resolves to an act, held against the help
text in both directions.

Getting there needed one verb that did not exist. The acceptance criterion
requires a body that stops obeying, and none of the twenty-six could produce
one, so `sleep` was added — the only verb this arc mints. It mints nothing
else: it routes to the existing rest action, costs what lying down costs,
commits the same fact a creature's own rest commits, and wakes at the next
moment its species is awake, by the same scan a creature's rest jumps by.

And `sleep`'s reply names `!wait`, which is the whole argument for the
out-of-character namespace being a capability rather than a debug aid.
Observing a state you cannot act in requires a clock you can still advance;
without one, being asleep is indistinguishable from the game having hung. A
player who lies down and is told nothing has a body that refuses every verb and
no way to learn which one still answers.

## What a possessed body still is not

At the level of facts, the acceptance test holds and was verified rather than
assumed: one constructor, one field set, one predicate, no field naming the
driver, and no production code that parses a provenance value back — every such
parse in the tree is inside a test.

At the level of bodies it is false, and the types say so. A creature is an
`Npc` — entity, home, resource, activity cycle, temperature niche, the fields
the drives read. A possessed body is an `Agent` — id, species, perception,
position, village. There is no conversion between them anywhere in the tree: no
`From`, no accessor, no bridge of any kind. No drive can act on a possessed
body, because it has no niche to be uncomfortable in and no resource to seek.
It has no affect; `needs` reads other creatures' felt states and never its own
body's. It does not appear in occupancy, so nothing sees it as an occupant.
Creatures live in the world; a possessed body visits it.

The provenance strings are where that shows, and the temptation to fix the
symptom is worth recording because it was refused. A creature's provenance
always names a drive — thirst, hunger, fear — while a possessed body's says
*walked on (its own errand)*, which is precisely a refusal to name one. Read
against a criterion of *indistinguishability*, that is a tell to be papered
over. Read against the criterion that replaced it — that a driver is
interchangeable, keyboard or planner, and the creature is the subject in either
case — it is an accurate report: the body genuinely has no drive, because it is
not a creature yet. The candidate repair, having a possessed body borrow its
own dominant drive, cannot be built at all today for exactly that reason, and
discovering a fix is unbuildable is worth more than proposing it.

So the arc closes with a stated requirement rather than a closed one (decision
0167). A driver is a visitor over a creature; where the intent came from is a
parameter of the
visit and never a different kind of subject. Making that true means a possessed
body *is* a creature, so that every function taking one accepts it with no
special case, and that is a campaign rather than a task. It is recorded as a
must-fix, deferred deliberately, and the tests assert only what is actually
true today: the shared constructor, the identical envelope, and that no field
names the driver.
