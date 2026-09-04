# The Hand

A hand is the part of you that does the thing, and it is never the part that
decides. That separation is the whole of this campaign. [The
Deed](./the-deed.md) made a possessed body's acts cost it time and leave facts
behind, and closed with an honest limit: at the level of *bodies* a possessed
one was still not a creature. It was an `Agent` — id, species, perception,
position, village — where a creature was an `Npc` — entity, home, resource,
species, activity cycle, temperature niche, deliberation latency, time
horizon, metabolic class, diet niche, boldness, threat niche, mass, label. No
conversion existed anywhere in the tree, and twenty-three functions in the
creature layer took one of them and could not take the other.

That is why the arc the metaplan actually asked for — *swap controllers: a
creature on player input and a player body on GOAP* — was unbuildable rather
than merely unbuilt. You cannot swap the driver between two things that are
not the same kind of thing. This campaign makes one kind of thing.

## The twin that was holding the room up

`Npc` and `Agent` merge into `Body` (decision 0229), and the merge is cheaper
than it sounds because every field of both is derivable from the same two
inputs, a species and a settlement. Both constructors already had both.
`derive_npcs` reads the biosphere and psyche registries by species label and
takes home and resource from the settlement; `mint_flagship` starts from a
village. One constructor serves them, and what used to be two derivations
become two argument lists.

Merging them made a duplicate visible that had been there all along. Possession
used to *mint*: a separate seed-derived `AgentId` draw at session start, giving
the possessed body an identity of its own. Meanwhile the flagship settlement is
the most populous one and the derivation order hoists the home settlement to
index zero — so the minted body and derived creature number zero already shared
a settlement, a species and a home. They were one villager wearing two types.
Collapse the types and the villager stands next to themself.

So possession stops minting and starts *selecting* (decision 0227). A session
is a roster and an index, `Session { bodies, driven }`, and possessing any
creature is `driven = i`. The Arc II acceptance test needs no new mechanism at
all; it is an integer. `AgentId` goes, and the `vessel/agent` stream label with
it — a permanent contract, so its retirement is a deliberate act, though a
narrower one than an epoch: that draw was taken after genesis from its own
position-keyed stream, so no seed generates a different world, and genesis
byte-identity was checked rather than assumed at 12,534 facts either side.

**Deleting the duplicate is where the campaign learned something it had not
gone looking for.** The twin was not redundant. It was the only thing
guaranteeing that a fresh possession began beside another creature.
`derive_npcs` derives one creature per settlement, the player now *is* that
creature, and wild creatures are scattered independently — so a seed search
for a chamber the player enters with a creature already in it, which used to
succeed on roughly nineteen seeds in twenty-four, succeeded on **zero of
sixty-four**. Twenty-one tests that exercised standing next to somebody had
been passing on an artifact of the duplication rather than on anything the
world does.

That is a fidelity question wearing a refactor's clothes, and it was answered
as one: a settlement of eighty people that contains exactly one simulated
creature is a design decision, not a bug to be patched inside a type merge —
particularly since tick cost was measured superlinear in agent count. Twenty of
the twenty-one now state their co-location through a documented seam instead of
inheriting it from a duplicate that no longer exists. The twenty-first is
silenced, because the seam cannot serve it — it needs a position change
occurring *inside* a tick, which only the drive simulation itself produces — and
the population question is left standing, visible, for a campaign that will
measure it.

## Widening the trait until it could carry a decision

With one body type the controller becomes possible, and the first attempt at
it is instructive because it type-checked, compiled, passed, and did nothing.

The trait was `intend(body, view, mode)`. That is enough to reach the
thirst-only computation the drive layer began with, and nowhere near enough to
reach the arbitration that actually runs today: the real decision function
takes twenty threaded parameters — terrain, hazards, the alarm state, the
visited and frozen sets, a mesh memo, a home-navigation cache. A trait too
narrow to express the real decision cannot wrap it. It can only run a second,
simpler decision *beside* the real walk and throw the answer away, which is
what the first cut did: the returned intent was assigned to `_`, and deleting
the line changed nothing.

The remedy was to widen the trait rather than to keep the parallel pass.
`intend` now receives the `Resolution` the body's own arbitration already
produced this tick — its mode, its affect, and the intent arbitration itself
would have acted on. So `DefaultController` is `resolution.intent.clone()`: a
pass-through that is byte-identical to having no controller at all by
construction rather than by agreement, which is what leaves every existing
creature's committed trail exactly where it was. `PlayerController` is
`self.pending.take()`, and nothing pending is `Hold` — never a GOAP fallback,
because a driven body with nothing queued waits on the player rather than
quietly acting for itself.

Nothing in `advance_one` learns which one answered (decision 0228). The
controller says what happens; it never says what the body feels, because mode
and affect were settled before it was asked. That split is what will let an
*imposed* controller arrive in Arc III without disturbing anything here.

Verifying the pass-through is where the campaign's sharpest methodological
moment sits, and it belongs to a reviewer who checked its own instrument
first. The obvious way to ask whether the NPCs still move identically is to
compare the world JSON and the rendered snapshots. That reviewer did — and
then forced *every* catch-up intent to `Hold` and got byte-identical artifacts
anyway. The comparison it had been about to trust would have certified
anything. What discriminates is the decision stream itself, instrumented at
the site: across four seeds it found 224 decisions and 8 differences, and every
one of the eight was the possessed body's own.

## The host is in there

Running arbitration for a possessed body is not a performance detail. It is
the answer to the question the possession design had left open longest — while
you ride a body, what becomes of the person? — and the two candidates differ
by exactly this loop. **Displaced** means they are dormant and return
afterwards, which is what skipping the tick for the driven body would
implement. **Co-present** means they are in there and aware, which is what
running it implements.

The tempting shape is to skip. You are already deciding; why arbitrate? The
answer is that arbitration is *where a host's inner life is computed*, and a
host with nothing computed has nothing to be aware with. Every downstream
ambition — a host who refuses, a host who names you, affect that becomes
testimony, a vacated host who testifies — rests on that state existing. So
the cheap-looking option was the one that quietly foreclosed a published
branch of the design, and this arc chose co-presence on purpose and recorded
it as a decision rather than letting it happen (decision 0226).

What arrives with this is the substrate for the payoff, and it is worth being
exact about how much of the payoff that is. Every tick computes what the host
*wanted*, from its own drives, alongside what the body *did*, from you, and the
session keeps the host's commitment *mode* — the drive it was pursuing, or its
idleness. What it does not keep is the *action* arbitration chose: the intent
never leaves the decision function, which returns a bare boolean. So the
sentence one wants to be able to write — *you made it stay when it wanted to
run* — is one signature change away rather than already in hand. The inner life
is being computed and recorded, at the granularity of the drive; reading it back
as a refusal is the next arc's work, and it is small work, which is a different
claim from no work.

## A measurement that could not measure what it claimed

The spec predicted that routing a possessed body through the tick would not
change the ledger's growth rate, on the argument that a driven body *holds*
most ticks and holding commits nothing. Measured before and after: 0.25 facts
per body per tick, identical.

The prediction held and the reasoning behind it is not what happened. A driven
body's walk emits its facts into a value that `Session::wait` discards
**unconditionally** — so the rate is flat whatever the controller answers.
Force `intend` to return a real action and the guarding test stays green.

The discard is correct and deliberate. What the player types is what the body
*does*, and it commits by the ordinary verb path; the walk supplies only what
the host *wants*. Committing both would give one body two competing sources of
position. But correct or not, it means the spec's "commits on `Do`, nothing on
`Hold`" argument is **untestable in this design**, not confirmed by it — and
the honest reading of the flat number is the narrower claim the arc actually
needed: routing a possessed body through the tick costs no committed facts.

The campaign ran this measurement twice and it was vacuous both times, for two
entirely different reasons — first against an implementation that never
touched the ledger at all, then against one whose facts are discarded before
they reach it. A third run was declined rather than staged until it produced a
number that looked like agreement, and the spec was corrected instead.

**Corrected 2026-09-03, by [The Minute](./the-minute.md).** Three claims in
this section rest on the discard, and the discard itself was the defect. It
was not "correct and deliberate": the walk's facts were dropped at that call
site for every body, and it went unnoticed because the controller asked there
had always answered *hold*, so there had never been anything to drop. Once a
controller that acts was swapped in, the same line began throwing away a held
body's drinks and journeys. A held body's walk now commits what it does — so
the measurement above becomes testable rather than vacuous, and a body no
longer has two competing sources of position because the walk's own committed
move *is* the source the column follows.

## Who else is here, still answered the old way

`colocated_npcs` and `sensed_npcs` answer *who is here* by filtering the
roster. With the possessed body now genuinely in that roster, they would
include **you**, and the needs display would report your own felt state among
the others'.

This arc keeps today's meaning by excluding the driven index, and says in the
code that it is doing so and what supersedes it: a component-shaped exclusion —
creatures in the area lacking a scenery-style marker — served by an indexed
query rather than a linear filter. That is work of a different lineage and it
belongs there. A placeholder that admits it is one is a different object from a
design decision made by accident in the wrong campaign.
