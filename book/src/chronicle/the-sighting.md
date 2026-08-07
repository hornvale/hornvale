# The Sighting

[The Panes](./the-panes.md) shipped a map pane and said, in its own first
section, that the map ships uninhabited. The sentence it quoted to prove the
point was a module doc: *"No creature stands in a cell until The Sighting."*
This is that campaign, and it begins by naming the reason the sentence could
be written so confidently.

Hornvale has two fine spatial layers, and they had never met.

The **relational** layer is `Interior { anchors, adjacency }`, where an anchor
is a place in a room — a hearth, an alcove, a doorway's inner side — and
`within` is RCC's non-tangential proper part. It has containment and touching
and no coordinates at all. This is where creatures actually stand:
`liveness::Occupancy` maps an entity to a `(RoomAddr, AnchorId)` pair, is
repopulated every tick, and evaporates with the bubble.

The **metric** layer is `Lattice { extent, cells, doorways, dof }` — Cartesian
cells, the thing that gets drawn.

`Structure` is `{ threshold, chambers, links }` and knows no anchors.
`Lattice` never mentions `Interior`. And yet `CellKind::Wall`'s own doc had
asserted for two campaigns that "an alcove, a screen or a fireplace is an
anchor **at** one of these." The intent was written down. The mapping was
never built. `lattice/occupancy.rs` was the empty typed socket waiting for it.

## The keystone is an authority question, not a mechanism one

The campaign opened holding this as a choice between three mechanisms: embed
anchors into cells, place creatures into cells directly, or draw the map by
anchor and skip cells entirely. That framing was wrong, and being wrong about
it was the most useful thing that happened at design time.

Lifted, the problem is not about anchors or cells at all: **a relational
structure must be embedded into a metric space so that metric queries become
answerable, while the relational structure stays authoritative.** That is
graph drawing, and its other instances — molecular conformation, VLSI
place-and-route, multidimensional scaling, the London Tube map — all keep one
discipline. Topology is truth; geometry is a licensed convenience.

Once stated that way the mechanism follows from the authority, rather than the
authority being whatever the mechanism happens to imply. And the authority
question has a sharp criterion attached, because the repository already
instruments it. `Lattice.dof` is documented as "how many independent choices
the embedder made — one per stream draw it consumed," and the checker compares
that against what the anchor graph leaves free, because inventing "is the one
thing an embedder may not do."

**Anchors are discovered. Cell geometry is invented. `dof` counts the
invention.** So the line the campaign draws is a line about what the invented
half is allowed to be load-bearing *for*:

- Sight narrowing the `sensed` channel is **structural redaction**, and it
  must happen sim-side, because a client that decides what to withhold is a
  cheat pane. The embedding is load-bearing here, and that is correct.
- Sight gating `Knowledge` would make an agent's **belief** depend on the
  embedder's free draws. Decision 0069 permits the fine layer to "regenerate
  differently forever without corrupting a world" precisely because nothing
  stored points into it; a sight-derived fact would start pointing at it.

**The embedding may decide what a client is shown. It may not decide what an
agent comes to believe.** That is the campaign in one sentence, and the second
half of it is a deferral rather than a feature.

## `anchor_cells`, and what a faithful embedding costs

`anchor_cells(interior, lattice, chamber, seed)` places each of a chamber's
anchors at a lattice cell. **Faithful** means: anchors adjacent in
`Interior.adjacency` land at cells with a passable path between them that
crosses no third anchor's cell. The drawn plan may be one drawing among many,
but it may not lie about which places touch.

Faithfulness is stated over **walkable neighbours** — ordinary adjacency plus
containment — rather than over adjacency alone, which is strictly wider than
the design document asked for and for a reason discovered in the code.
Composition gives a *contained* anchor no adjacency edge at all, so a
neighbours-only check is vacuous for containment-only compositions: it passes
by having nothing to check.

**The keystone property only bites on a grown corpus.** Verified by mutation,
reproduced twice independently: removing the placement filter leaves the
rectilinear test suite green and reddens the grown one, at ten unfaithful
pairs against five. A property test asserting the campaign's central invariant
was, on the corpus first written for it, nearly a tautology.

The surprise came out of the data rather than out of anyone's prediction.
Everybody expected the filter to rescue **narrow** rooms — one-cell corridors,
where a placement has almost nowhere to go. Size is not the predictor.
**Connectivity is.** Three of the five rescued cases are large blobs of 59, 79
and 63 floor cells; a big room with a pinch point is harder to embed
faithfully than a small convex one.

Two admissions the derivation makes rather than hides. Some anchor graphs
cannot be placed in some chambers, so the scan carries a **stated** relaxation
rather than a silent one, and it fires 5 times in 256 on the grown corpus;
a surplus-unplaced branch fires 3 in 256. The consequence propagates: **an
`AnchorId` may resolve to no cell at all,** and everything downstream had to be
written knowing it.

And the derivation was redesigned for cost before it shipped, which is only
true because the campaign measured its clock first. The obvious implementation
ran a **1.469 ms median per call** — on its own larger than The Panes' 1.249 ms
figure for an entire snapshot, and this is called on every snapshot. The lazy
sweep that replaced it measures 42 µs median, 410 µs at p99, 437 µs at worst.

`room/layout/v1/anchors` is a new stream label, which makes it a save-format
contract: new, never a rename, with the stream manifest regenerated in the
same commit that minted it.

## Symmetric shadowcasting, with no floats in it

`Interior.adjacency` is a `BTreeSet` of pairs — symmetric by construction. The
relational layer gets symmetry for free. Ordinary recursive shadowcasting is
famously asymmetric: A sees B while B cannot see A. In a sim where perception
feeds belief that is a modelling commitment, and an asymmetry the relational
layer does not have would be exactly the artifact of the embedding that the
authority decision exists to prevent.

So the campaign ships Ford's symmetric variant over four quadrants. The
recursion is the ordinary one — scan a row, split the slope band at every
floor/wall transition, recurse a row deeper — with one change that is the
whole point: a passable cell is lit only when its **centre** lies inside the
surviving slope band, never merely when the band clips its square.
Centre-in-band is a relation between two cell centres and the fabric on the
segment between them, and nothing in that relation distinguishes its two ends.

Every slope is an exact rational `num / den` and every comparison is a
cross-multiplication. There are **no floats in the module**; the only `f64` in
the file sits inside a comment explaining why there are none. A last-ULP
difference in a slope comparison would not be a rounding cosmetic — it would
decide which cells are lit, which is a determinism defect. Chebyshev radius
falls out of the scan structure rather than being imposed on it, since a scan
at depth *d* only ever emits columns in `-d..=d`.

Walls are the deliberate exception: a wall is lit whenever the band touches it
at all, so a room's fabric draws as fabric instead of a ragged fringe of gaps.
**The exception is therefore any pair involving an impassable cell**, not
merely wall-to-wall pairs — the first attempt at documenting it named the
narrower set and was wrong. The narrowing is safe for the reason the authority
decision cares about: nothing stands in a wall, so a wall's visibility can
never reach belief.

Symmetry here is measured and not merely tested. Counting asymmetric ordered
pairs across five fixtures gives **floor-to-floor asymmetry of exactly zero
everywhere**, while the asymmetry that does exist is dominated by mixed
floor/wall pairs at three to seven times the wall-to-wall count — 3624 mixed
against 842 on one fixture, 5714 against 780 on its grown twin.

## The honest headline: the occlusion term is dead in production

State this plainly, because softening it would be the kind of claim this
project exists not to make.

A probe over **400 built structures** counted every ordered pair of co-located
creatures and asked why each was not seen:

```
in-radius pairs      2,975,196
occluded                     0
beyond-radius pairs  2,741,474
```

**No wall in any shipped world hides anyone.** The chain is short and each
link is checkable: `structure_at` returns `None` unless the brief says the
site is built; built sites route to `allocate`; `allocate` draws **convex**
chambers; and every co-located creature is placed in the possession's own
chamber. A convex room has no shadow.

The entire narrowing that ships today is `SIGHT_RADIUS`, which is
`CHAMBER_SIDE / 2` — four cells, Chebyshev. Forty-eight per cent of pairs fall
beyond it. That is a real rule and it does real work; it is a **distance** rule
and not an **occlusion** rule.

The shadowcaster itself is correct, symmetric, integer-exact, and pinned by
mutations that discriminate — a symmetry mutation reddens only the symmetry
test, and a return-everything control reddens only the blocking test. It will
bite the day chambers stop being convex, and building it now is what makes
that day cheap. But the campaign shipped a capability its worlds do not yet
exercise, and the correct summary is *sight has a radius*, not *sight through
apertures*.

## What the map actually shows

The campaign's own design document says the pane "draws only lit cells." **That
was never built and cannot be**, and the error is in the specification rather
than in anything that shipped.

There is no unlit concept for a cell at the wire. `plan_of` emits the whole
wall/floor/threshold grid unconditionally, and `CellKind` is closed at three
variants with an explicit warning in its own doc against widening it. What is
gated by sight is `marks`, and only `marks`.

**So: the map shows the full floor plan; sight hides creatures, not geometry.**
You always see the shape of the room you are standing in. What you may not see
is who else is in it. The implementer declined to build blank-rendering for a
wire value that cannot occur and flagged the specification instead of shipping
unreachable code, which is the correct order of operations.

`vessel/plan/v1` gains `marks: Vec<PlanMark>`, each `{x, y, noun, kind, datum,
salience}` — the surrounds chart's `Mark` shape plus a cell, chosen
deliberately to be the focalizer's `Focalized.nouns` shape so that a creature
on the map and a creature in the prose are **one examinable thing** rather than
two things that agree. The field is the one The Panes omitted on the reasoning
that a field nothing writes cannot be seen to be wrong; this campaign is its
first writer, and additivity was verified at the byte level — exactly one key
added, last in key order, no pre-existing key moved.

Marks sort ascending by `(salience, noun)` so that the bytes do not depend on
discovery order, and removing the sort reddens a test. Extent validation is a
`debug_assert!` rather than a panic or a silent filter, and the choice is
argued: a silent filter would hide a visibility bug from the negative control
built to catch it, and a panic would crash a turn over an upstream defect.
`plan.rs` itself stays pure and entirely sight-unaware, which is what makes
the negative control mean anything.

On the client, a mark draws as the first letter of its **noun** — `b` for a
bugbear — and not of its kind. Every creature carries the kind `agent` since
the predicate was unified, so a kind-derived glyph would draw an entire
menagerie as `a`.

```
###################
#..b.......#......#
#.....@....+......#
###################
```

## One predicate, and the four leaks that found it

The narrowing is a single predicate, `sensed_npcs`, and the campaign's product
shape is best read as a table:

| a creature that is… | present | examinable | has needs | provokable | drawn |
| --- | --- | --- | --- | --- | --- |
| placed and lit | yes | yes | yes | yes | **yes** |
| placed and unlit | no | no | no | no | no |
| unplaced | yes | yes | yes | yes | **no** |

The third row is deliberate and it is the interesting one. If the embedding
could not place a creature, we cannot claim that **sight** hid it — the
placement scan failed, and a failed drawing is not a shadow. Presence is the
conservative default, and keeping it there is what preserves the authority
line: presence must not depend on the embedder's free draws; only drawing may.

Getting to one predicate took five fix rounds and four separate leaks, which
the retrospective treats as the campaign's central process lesson. The product
consequence worth recording here is what the four had in common. Every one of
them — `examine`, `needs`, `provoke`/`soothe`, and the motion narration inside
`wait` — was a surface that **narrated** a creature rather than one that
**returned** one. Gating at the verb rather than at the data read is therefore
the right layer and not a convenient one, because `narration.prose` reads no
field of any creature, appears in no grep for one, and is nonetheless how every
verb's text reaches the client.

The departure narration deserves its own note, because the obvious gate there
would have been a bug whose symptom is absence. `wait` narrates who arrived and
who left. Gating departures against the *current* sensed roster is not a
preference but a **set identity**: the roster derives from a filter that is
"is here", and a departure is "is here is false", so the intersection is
empty by construction. That gate would have deleted every departure line in the
game while looking like a working one. So `wait` captures the sensed roster
*before* the day advances, beside the population it is comparing against. Out
of doors both guards are provable tautologies and outdoor narration is
byte-identical.

## A withheld creature is indistinguishable from an absent one

At every verb. `examine` answers as it would for a creature that is not there;
`provoke` refuses as it would for a name that names nobody; the needs report
lists no need.

This is deliberate, and the reasoning is the same reasoning that put the
narrowing sim-side. A distinct refusal — *"you sense something you cannot
see"* — is itself a **positive, pollable presence oracle**. A player who can
tell "hidden" from "absent" can sweep a room by trying to examine each name in
turn, which recovers exactly the information the redaction removed, and does
it through the redaction's own error path.

The cost is real and should be recorded rather than discovered later: **you
cannot swing at what you heard.** "Strike the thing you sense but cannot see"
is a perfectly good mechanic, and this campaign forecloses the accidental
version of it, not the designed one. A future hearing or scent model wants a
channel of its own, with its own narration and its own decision about what it
discloses — not the residue of a lookup nobody filtered.

## Knowledge is not gated, and it is safe by construction

The campaign's central negative control perturbs the embedding and asserts that
what is **drawn** moves while what is **known** does not.

It passes, and it passes for a stronger reason than the one it was designed to
check. `Knowledge` is not gated by sight because the knowledge path never
consults the session's creature roster at all: absorption reads the frozen
world through `observable()`, and `self.npcs` is not on that path. So the
control confirms an **architectural property** rather than creating one. That
is a weaker claim than "we built a gate and the gate works" — and worth pinning
precisely because nothing else states it. Nothing prevents a later campaign
from committing a sight-derived fact; the constraint lives in the module docs
where a knowledge campaign will read it, and in the registry.

## Cost, and a ratio that had been wrong for eleven days

The campaign's first task was a measurement it did not need but the repository
did: **nothing had ever timed a turn through the wasm ABI.** Every browser
figure in this repository was a native measurement multiplied by an
extrapolated 3.6–3.8× ratio, which is the precise practice the project's own
clock inventory forbids in as many words — *re-measure, do not extrapolate.*

Measured, and reproduced three times independently:

```
turn through the wasm ABI   3.70 – 3.77 ms
the same turn, native       2.08 – 2.35 ms
ratio                       1.57 – 1.78x        (assumed: 3.6 - 3.8x)
```

The wasm penalty is roughly **half** what every derived figure in the
repository assumed, and the measured figure is an upper bound besides, because
the native benchmark stops its timer before serializing the snapshot while the
ABI figure includes it.

The archaeology is the better half of the finding. The 3.6–3.8× figure was not
a guess: it was a valid handle-to-handle measurement, correctly taken and
correctly recorded. It was **superseded ninety-six minutes later** by a commit
that moved snapshot construction inside the timed handle, and nobody
re-measured for eleven days. A number can be right when it is taken, wrong an
hour later, and cited as authority for a fortnight, and no gate in this
repository watches for that.

One ratio stays unsettled and is recorded as unsettled: world genesis through
the ABI read 1.48×, then roughly parity, then 1.83× across three attempts on a
contended box. The committed text names the contention instead of picking a
number.

The turn-cost story on the sim side has a shape nobody predicted either. The
plan proposed ratcheting the cost gate by **verb class** — moving verbs are
expensive, verbs that only look are cheap. A probe that pooled samples by
sequence position overturned it: `out` and `enter` are both moving verbs and
differ fourfold; `map` and `look` are both non-moving and differ fourfold by
band. **The cost axis is indoors versus outdoors**, because indoors is where a
sighting must be derived at all.

That correction was not cosmetic. **Twenty of fifty pooled samples already
exceeded the 8.0 ms turn budget and the gate passed**, because the pooled
ceiling could not see a subpopulation. A moving-class ceiling would have been
blind to the two indoor non-moving samples that were already over. The new
`INDOOR_SNAPSHOT_BUDGET_MS` of 18.0 is keyed on the *band*, and it gates
`snapshot() + json` alone — where the indoor/outdoor spread is six per cent —
rather than the combined turn, where it drops to four and becomes noise. Its
basis is three quiet runs at 8.910, 8.503 and 8.530 ms, doubled at the slowest.

Deriving a sighting costs **~8.5 ms in dev and ~3.7 ms in release**, per call.
A committed doc had claimed 42 µs, which was the anchor-placement sweep alone —
one line of the derivation — quoted as the cost of the whole. Understated by
about 195×, and caught by re-measuring rather than by reading.

Memoizing it is **deferred deliberately, with the numbers written down.**
`wait` derives a sighting twice inside one handler, and the two calls straddle
the day advance *on purpose*: that is what makes the departure narration
possible. A memo keyed wrong would silently collapse them and reintroduce the
bug two fix rounds went into removing — a bug whose symptom is absence. The
gain would be about 7.4 ms of release time for the pair, roughly 12.6 ms
through the ABI, against a turn a human already cannot perceive.

The payload barely moved. The walk-band snapshot is unchanged at 12,273 bytes;
the chamber band grew from 4,802 to 4,813 — **eleven bytes** for the marks
field and its contents in the fixture. The per-band snapshot cost for moving
verbs went from 1.259 to 3.706 ms, a 2.94× rise that is entirely the sighting
derivation and entirely indoors.

## Two smaller things, both latent defects on main

The client's chart reader validated no schema at all. It had survived the
`scene/surrounds/v1` → `v2` migration by luck — it reads only the fields it
needs, and `v2` kept them. A version that renamed a field would have degraded
to a blank pane, which is acceptable; one that **reused** a name with a new
meaning would have silently mis-drawn, which is the failure class this campaign
is least able to detect, since the campaign is entirely about what is drawn.
The reader now validates a positive allowlist, so an unanticipated schema fails
**closed**. Six synthetic fixtures turned out to carry no schema field at all
and had been passing silently.

And a third unguarded dereference surfaced in the client's plan pane — the
first of the three caught *before* review rather than by it. The pre-existing
`@`-placement check is `&&`-chained and degrades safely on a `NaN` coordinate;
the new bounds check for marks is `||`-chained and does not, so a malformed
extent would have let a row index throw instead of refusing the pane. It was
found by a mandated sibling audit of every dereference in the file, not by
looking at the code the fix touched.

## What this leaves

The shadowcaster is a capability waiting on a world that exercises it.
Non-convex chambers — an L-shaped hall, a pillared room, a chamber with an
interior screen — would make occlusion live in a single generation change, and
the instrument that would then measure it already exists: the same probe that
counted zero occluded pairs would count them.

Knowledge gated by sight is named, deferred, and registered, and the deferral
is the design rather than an omission. Taking it up means deciding what it
means for a belief to point into a layer licensed to regenerate differently
forever — which is an epoch-adjacent question, not a feature.

And the possession still cannot leave its body. Hopping from one creature to
another was carved off this campaign at the opening fork because `release` ends
the session today; hopping needs release-as-detach, a live locale context, and
a decision about what becomes of a vacated body. The map now shows who else is
in the room, which makes that the obvious next question rather than a
speculative one.
