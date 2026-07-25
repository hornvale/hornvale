# The Snapshot

*The sim was already telling the truth. It was only saying it out loud in a
form nothing but a reader could use.*

[The Casement](./the-casement.md) put the possession loop in the browser, and
the interface it exposed was the one the native binary had always had: command
bytes in, rendered text out — a paragraph per turn, and not one word of it
addressable. So every pane the client might want next — a map, a status
readout, a list of who is here, a view of what the agent knows — was blocked,
and the blockage was in an unexpected place. It was not that the data did not
exist. `Session` had held it in Rust since [The Seam](./the-seam.md):
the focalized room, the compass exits, the agent's identity, the accumulated
knowledge projection, the labels of co-located creatures, each one's grievance
and whether it had crossed into hostility. All of it was structured, typed, and
one method call away. What was missing was a way for it to *leave*.

## Why prose cannot feed a pane

The tempting shortcut is to parse the prose. A client that reads

> You stand in tropical seasonal forest — buttressed canopy — in the lands of
> Qvooshtvoagootao. Ways on: SE, N, SW.

could plausibly recover three compass directions and a biome name from it, and
draw something. It must never do that, and the reason is not fragility — it is
authority. Prose is a *rendering*: a projection of the world through one
agent's perception, composed by the sim, which owns the interpretation. A
client that parses prose back into data has appointed itself the second
authority on what that rendering meant, and the two authorities will disagree.
They will disagree the first time a comma moves. More importantly they will
disagree *silently*, because a parser that recovers "SE, N, SW" from a sentence
has no way to notice that the sentence was about something else.

So the emit had to come from the producer, and this campaign built it:
`vessel/session/v1`, one structured document per committed turn, joining the
schema families under the same epoch discipline as the room and the scene —
additive changes are free, a change of meaning mints a `v2`, and nothing is
ever renamed. Two new exports carry it out of the WebAssembly module beside the
five that carry the prose, and the browser client's transcript pane was
refactored to render *from the snapshot* rather than from the text. The visible
result of that refactor is nothing at all: the pane prints exactly what it
printed before, which is the whole point. The seam was proved by moving the one
existing pane onto it, not by adding a second.

## Channels, not tags

The design question worth recording is how provenance is carried. A page that
withholds is a page that must know, for every datum it holds, *how the agent
came to hold it* — sensed here and now, known from having been told, felt,
committed to the ledger. The obvious answer is to tag each field with its
provenance and have each pane check the tags.

The snapshot does something stronger: it is **grouped by channel**, and there
are no tags at all. `sensed` holds what evaporates when the agent leaves the
room. `known` holds the accumulated projection that does not. `social` holds
committed, entity-keyed, placeless standing, which is why it is *not* nested
under `sensed` — a grievance survives walking away, and filing it under
presence would imply the opposite. `narration` holds the sim's own rendering.
`self` holds identity.

The difference between grouping and tagging is the difference between a
discipline and a structure. A pane that reads one channel does not *decline* to
look outside it; it **cannot**, because the fields it would need are not in the
object it was handed. The redaction boundary stops being a rule that a future
pane's author might forget and becomes a property of the data's shape. This is
the same move the trace protocol made when it declined to let a consumer learn
which system produced a phenomenon: not "please don't look", but "there is
nothing there to look at."

## Tying the newest thing to the oldest

The snapshot's `narration` channel duplicates the prose verbatim, and that
duplication is deliberate: the client never re-derives prose from structure,
because the rendering is the sim's. Duplication invites drift, so it needs a
check, and the check available here is unusually strong.

The gallery has held [a frozen possession transcript](../gallery/possession-seed-42.md)
since The Seam — seed 42's opening, walked by the native binary, committed
verbatim and drift-checked by CI like any other artifact. So the test asserts
that the snapshot's narration, over a fixed script, is **byte-identical to the
lines of that published chapter**. The newest channel in the newest schema is
pinned to the oldest golden in the book. Nothing weaker was on offer: a
self-consistency test would only prove the snapshot agrees with itself, and a
hand-written expectation would only prove it agrees with whoever wrote the
expectation. Agreement with a document a reader can open is a different order
of claim. Beside it sits a committed byte pin of the whole `vessel/session/v1`
document for the same seed and script, so a change of meaning cannot land
quietly — it lands as a reviewable diff in a fixture whose regeneration is
itself the decision to mint a version.

## Four things the design got wrong

The spec for this campaign was written before the code, and implementation
corrected it in four places. The corrections are recorded in the spec itself,
with the original wording still visible, because a campaign that corrected its
own design is a more useful record than one that appears to have been right
from the start.

The first two were the same error twice: the design described data that did not
exist. It gave the snapshot a top-level `felt` channel on the assumption that
the possessed agent has an affect to read — and it does not. The player has no
drive layer and no affect layer at all. What the design mistook for the
player's interior was the mechanism [The Temperament](./the-temperament.md)
shipped for *other* creatures: a read of a co-located NPC's felt state, gated
on standing in the same room as it. That is a sense datum about somebody else,
so it lives inside the presence-gated channel, one felt string per creature
present. A player interior remains a later campaign, and when it exists, a
top-level channel is the right home for it.

The second was subtler and more instructive, because the design contradicted
itself. It had already decided — correctly — that the room would be embedded
*verbatim* as the existing room schema, one schema with one owner, so that a
future room version would be carried rather than re-described. Three paragraphs
later it added a `ways` field listing the compass exits. But the embedded room
already carries its exits; the session's own method is not a source of truth
about them, only a filter (keep the edges, keep the compass directions). Two
representations of one truth in one document is exactly the drift the embed
decision existed to prevent, so the shipped schema emits the exits once and the
client applies the same two predicates. The principle was right in the
paragraph that stated it and violated in the example that followed it.

The third would have been a visible bug. The design's example showed the
narration carrying the focalized room description. In fact the room description
is produced for only four commands — look, go, back, and the opening — while
the session records the text of *each* turn's own response. Had the snapshot
emitted the room block unconditionally, the transcript pane would have printed
the room description on a turn where the player asked who they were. Checked
against the thirteen-turn walker script, eight of those turns return text that
is not the room block. So the prose channel carries what the player was just
told, which is what a transcript is.

The fourth was a misidentified convention rather than a mistake about the
world. The design called for hand-rolled serialization "in the house style".
The house style is not hand-rolled: it is a derived serializer with a
quantizing attribute on each float field, which is what the scene window
already does. The constitutional requirement was never that the JSON be written
by hand — it is that floats quantize at the emit boundary and nowhere else, and
an attribute declared on the field it governs discharges that more legibly than
a writer that could forget one.

## A negative zero that had been sitting there for months

Serializing a value is a stricter demand than comparing one, and the difference
surfaced a latent bug the moment this campaign made the first demand.

An NPC's grievance toward the player is an additive fold over its committed
disposition-shift facts, shipped by [The First Mark](./the-first-mark.md). It
was written with `Iterator::sum`, and Rust's `sum` for floating point folds
from **`-0.0`**, not `0.0`. That is a deliberate and defensible choice by the
standard library: `-0.0` is the better additive identity, because `-0.0 + x`
equals `x` for every `x` including `-0.0`, whereas starting from `0.0` would
destroy the sign of a lone negative-zero addend. The consequence here is that
an NPC with no committed grievance facts — every NPC in an unprovoked world —
folded an empty sequence and got `-0.0`.

For months that was harmless, because the value was only ever compared against
the hostility threshold, and in IEEE 754 arithmetic `-0.0 == 0.0`. Nothing
could see the sign. This campaign is the first thing in the project to *write
a grievance down*, and `-0.0` and `0.0` are different bytes. Every unprovoked
NPC in the pinned fixture would have carried a negative zero, permanently, in a
save-format-class document.

The instructive part is that the existing test could not have caught it, and no
strengthening of that test could have. It asserted that an unprovoked NPC's
grievance is zero, which is *true*: `-0.0` is zero, and the assertion passes.
The bug was invisible to equality by construction, and became visible only when
the value crossed a boundary where identity, not equality, is the question. The
fix is one line — fold from an explicit `0.0` — and the general lesson is worth
more than the fix: the first serialization of any quantity is an audit of it.

## What did not move

No new seed draws, so the stream manifest is unchanged. No new facts, no new
verbs, no new concepts, no epoch, no census. The prose interface is retained
untouched, because it is the constitutional primary and the book chapter
depends on it; the snapshot is taken after a turn commits, by a caller who asks
for it, so the command-line path that never asks costs exactly what it cost
before. The live pane in the gallery renders the same bytes it rendered
yesterday.

One incidental repair: the browser client's smoke gate had been failing,
because it asserted that a particular hard-coded seed could be possessed and
that seed's world happens to contain no settlements at all. A test that depends
on a geography accident is testing the accident. It now scouts for a
possessable seed, so it exercises the teardown path it meant to exercise.

What this opens is a pane count. There is still exactly one pane, and the
campaign's claim is that the second one now costs no interface surface at
all — a pure function over a document that already exists, unit-testable with
no simulation in the room. The claim is cheap to make and will be tested the
ordinary way, by writing the second pane.
