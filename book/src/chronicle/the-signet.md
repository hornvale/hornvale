# The Signet

**August 2026 · outcome: merged — an entity's identity becomes its derivation
path rather than its position in mint order, and the test written to prove it
could not, at first, see the failure it existed to catch**

## What was attempted

Every entity in a world was minted with the next number in a sequence. That
number was its identity, and its identity was therefore a fact about the whole
build rather than about the entity: insert a stage anywhere in the pipeline and
everything downstream of it renumbers.

The complaint is measurable, and the measurement is what opened the campaign.
Promoting a hundred and seventeen persons into the build moved six committed
fixtures:

```
  session-seed-42                537-543 -> 654-660   52589 -> 52589 bytes
  snapshot-seed-42-walk          537-543 -> 654-660   13400 -> 13400 bytes
  snapshot-seed-42-chamber       537-543 -> 654-660    8513 ->  8513 bytes
  snapshot-seed-1-chamber-occ.   505-511 -> 637-643    7795 ->  7795 bytes
  session-seed-42-turn-0         537-542 -> 654-659   13231 -> 13231 bytes
  session-seed-42-chamber        537-542 -> 654-659    8306 ->  8306 bytes
```

Byte length identical in all six, every entity displaced by exactly the number
of persons minted ahead of it. Equal bytes with shifted numbers is the signature
of a positional identifier doing a stable identity's job.

The Signet is the last of three campaigns on that defect. The Scaffold split the
bake's private handle from the ledger's permanent one; The Salt stopped derived
prose from reading a number for its value, so that this campaign's diff would
contain only renumbering and not also the consequences of renumbering. This one
changes the derivation itself, which is the only one of the three that removes
the churn rather than relocating it.

## An accession number is not a call number

The distinction the campaign turns on is archival. An **accession number**
records the order in which things arrived: it is arbitrary, it is never reused,
and it says nothing about the object. A **call number** is derived from
provenance: where the thing sits in a classification, which is a property of the
thing itself. The defect stated in one sentence is that the ledger used its
accession number as its call number.

Both survive, with their jobs separated. `next_entity` stays and counts — how
many entities exist, never which one this is. `EntityId` becomes derived:

```
  id : NonZeroU64
       +-------------------------------- 48 bits ------------------+-- 16 --+
       |  path hash: derive(parent_id, role_label)                 | ordinal|
       +-----------------------------------------------------------+--------+
```

The high forty-eight bits hash the entity's **parent** and the **role** it fills
for that parent, through the kernel's existing labelled-derivation primitive on
a leg of its own (`entity/identity/v1`) so that the space is namespaced away
from any other consumer deriving off a seed whose value happens to equal an
entity id. The low sixteen are the **ordinal** — which sibling this is among
that parent's children in that role — supplied by the caller, because the caller
is the code that knows "this is the third occupation of this settlement" and the
ledger has no semantic basis for guessing.

Entities with no parent hash a fixed root constant against the role label alone.
**The world seed is deliberately not an input.** That is not a regression but
the preservation of a property that already held: every world numbered its
entities 1, 2, 3, so ids have never been world-unique. Excluding the seed also
keeps the ledger free of it — minting needs only (parent, role, ordinal), so no
mint path has to thread a seed it does not otherwise hold.

Siblings therefore share their high bits and differ only in the low sixteen,
which makes a lineage legible on sight. Four wild creatures derived off one
parent, as the possession transcript now prints them:

```
  9630022852472602624   a wild rust-monster
  9630022852472602625   a wild otyugh
  9630022852472602626   a wild xorn
  9630022852472602627   a wild giant-goat
```

Collisions cannot happen between siblings, which differ by construction in the
ordinal. Across parents, forty-eight bits over the ~10³ entities a world carries
gives about 1.8 × 10⁻⁹ per world, and the residual case is converted from silent
corruption into a reproducible panic: the ledger asserts at mint that the id it
derived is not already in use, and the message names the lineage that collided.

## Why the derivation reads no material fact

The obvious derivation — hash what the entity *is* — is wrong here, and a test
that predates the campaign says so. Two occupations whose material facts agree
in every particular must still be two occupations. A world contains such pairs
by construction: on the witness world the keystone test builds, **3 groups of
occupations share a material core (6 records), and 33 share a founding key (66
records)**. A material hash would give the members of each group one id and
silently merge them.

Deriving from the path instead means an id is a function of structure and of
nothing else — no people, no site, no founding day, no population. The keystone
test's own anti-vacuity assert, which demands at least one colliding group
before it will claim to have proved anything, is what would catch a violation of
that rule; and running the old counter and the new derivation against the same
worlds returns the identical group counts, so the derivation demonstrably reads
nothing the grouping reads.

## The defect the counter had been hiding

Under a counter, a freshly minted id is always new, because "new" means "one
larger than last time". That property makes an entire class of mistake
invisible.

Opening a session on a world that had already been played and saved re-runs the
derivation that creates its inhabitants, against a ledger that already carries
them. Under the counter this quietly minted a **second** creature for the same
settlement on every reload, indefinitely, and nothing could notice: each
duplicate had a number nobody else had. Under lineage-derived ids the same code
path derives the same id twice and the mint-time assert fires immediately.

The repair is a second, explicitly narrower minting operation: one that returns
the existing entity when a lineage has already been minted, and mints only when
it has not. It is documented as the **idempotent-derivation** form and pointedly
not as a softer mint — the strict form still panics, and a unit test proves that
reusing a lineage leaves the collision visible to it. Re-derivation on load is
now a fixed point rather than an accumulation.

## An id that a player was expected to type

The vessel printed each derived creature's raw id beside its label, and that
number was not decoration: it was accepted as input, the way a player names who
to ask about. A nineteen-digit number is unusable for that, and the epoch made
the problem concrete in a published transcript rather than theoretical.

The printed number is now a **session-local ordinal** — 1..n over the listing,
short-lived, stable within a session, and explicitly not an identity:

```
  [1] bugbear of Googo            [5] a wild otyugh
  [2] hobgoblin of Nenagabo       [6] a wild xorn
  [3] hobgoblin of Toa            [7] a wild giant-goat
  [4] a wild rust-monster
```

The same handle resolves everywhere the old number did, with one asymmetry
preserved deliberately: the verbs that act on a creature resolve the handle
against the listing and then re-filter it through what the possession can
currently *see*, so a short handle cannot address someone the sight model is
withholding. That withholding invariant predates this campaign, and a bare
positional lookup would have silently reopened it.

## Crossing into a language with 53-bit integers

A full-width sixty-four-bit id exceeds the range in which JavaScript represents
integers exactly. Any document carrying one as a bare number is therefore
lossy at the boundary — silently, since rounding produces a plausible number
rather than an error.

The session document already had the answer in it: an agent handle, itself a
derived sixty-four-bit value, was already serialized as a decimal string in that
very schema. Creature ids now follow that precedent. Because a field changing
from number to string is a change of meaning rather than an addition, the schema
minted `vessel/session/v2`.

The bump is exactly one schema deep, and the reasoning is worth recording
because it runs in both directions. The session document is an envelope that
embeds three other versioned documents, each announcing its own version on the
wire; an earlier campaign had already established that an embedded document
bumping does not move the envelope. The converse holds for the same reason — a
document is identified by the tag it carries, not by its container's — and
nothing embeds the session envelope, since it is the outermost document. No
scene schema carries an entity id at all, so no cross-repo contract moved.

One boundary was left deliberately untouched: the world save itself still
serializes ids as bare numbers. It is a Rust round-trip format with no consumer
in a browser, and re-encoding it would be a save-format epoch of a different
order of magnitude. Recorded as a decision rather than an oversight — it becomes
live the day a client loads a world document directly.

## The instrument that could not see its own failure

The campaign's acceptance test builds a world twice — once as it stands, once
with one extra entity minted ahead of the stage under test — and claims that
every id outside the inserted entity's lineage is unmoved. The claim was frozen
in advance, in the form that reads most naturally: the set of ids the
unperturbed run produced, minus the set the perturbed run produced, must be
empty.

**That formulation survives the exact defect it exists to catch.** Under a
counter the inserted entity takes the next number and every later id shifts up
by one; the unperturbed run's set is then a strict *subset* of the perturbed
run's, and nothing reads as missing. Restoring the counter and running the test
produced a green result on its headline claim, twice, independently. The only
assertion that fired at all was a subsidiary guard noticing that an entity which
should not exist in one arm was present in it.

The shipped test compares the minted ids **elementwise, in derivation order**,
which a uniform shift breaks on the first element. Under the restored counter
both arms of the test then fail directly on the claim, at both minting seams:

```
  left:  [EntityId(7), EntityId(8), EntityId(9), ... EntityId(524)]
  right: [EntityId(8), EntityId(9), EntityId(10), ... EntityId(525)]
```

The set difference is kept beneath the elementwise comparison, because it still
catches an id that *vanished* rather than moved, and it is labelled as the weak
form. The general statement is worth more than the instance: **a set-difference
formulation of "nothing moved" cannot see a monotone renumbering.**

## The prediction that failed

The campaign predicted, on the strength of The Salt's sweep, that exactly two
prose files would move when every id in the world changed: the two possession
transcripts, where numbers are printed as numbers.

**Three moved.** The third prints two ids in a sentence:

```
- Entities: 1 (the vale), 2 (the village). Facts: 2. That is everything.
+ Entities: 9222568859608023040 (the vale), 3707018031091679232 (the village).
```

That is a number read for its value, in exactly the sense The Salt's rule
forbids, in a channel The Salt's sweep did not reach. Nothing was retuned to
rescue the prediction; the file was re-pinned with the rest of the epoch and the
falsification is the finding.

The sharper form of the finding is about the guard rather than the file. The
scan that enforces the rule walks a **hand-maintained list of four source
files**. The channel that moved is a fifth, and no list contains it. A guard
whose population is authored by hand can only ever be as complete as the sweep
that authored it, and this one was demonstrably one file short from the day it
shipped — which is precisely why the prediction was written down in advance with
"anything else that moves is a channel that was missed" attached to it.

## The limit, stated in advance and confirmed

Deriving an id from a lineage contains the blast radius of a renumbering to that
lineage. It does not abolish it. **Reordering entities within one lineage still
moves that lineage's ids**, because the ordinal is part of the id — and that is
correct behaviour rather than a residual defect: a campaign that reorders a
settlement's own occupations has changed which occupation is third.

The campaign supplied its own worked example, which is more instructive than the
prediction. A people's collective entity took its ordinal from the list of
peoples that had *placed a settlement*, rather than from the full roster. Both
are deterministic; only one is stable. Under the placed-subset ordinal, a later
change to settlement survival — era-varying capacity, a raid gate, a fifth
settling people, all of which this world has seen — makes one people stop
placing, and every people after it in the list slides down one, re-keying every
fact about entities that did not change at all. That is the campaign's own
defect class, reintroduced at a single site by choosing the wrong index.

Keying the ordinal on the full roster instead turned dense consecutive indices
into sparse roster positions, with the gaps that prove it, and moved nothing
else. **Choosing an ordinal's source is choosing what an id is invariant to**,
and it is the one judgment the derivation cannot make on the caller's behalf.

## What the shape leaves reserved

Two entity families root rather than descend, because the thing they belong to
is not an entity at all: an occupation's site is a cell and its people a kind,
and a wild concentration is a species and a position. Their ordinals are
therefore positions in a baked order, and they carry the residual churn the
previous section describes. Giving those parents entities of their own would
move both families under a stable key.

The ordinal is sixteen bits, so a parent may have 65,536 children in one role.
Every production site casts to that width, and past the ceiling the cast wraps
and the collision assert fires — loudly, which is the acceptable failure mode,
but the ceiling is nowhere written down. The largest world measured here carries
roughly 650 entities in total.

And ids are now wide enough to carry information they do not carry: a domain
tag, a format epoch, a schema version in the spare bits, so that an id printed
in isolation says what kind of thing it names and which era minted it. That is a
different campaign's ambition, deliberately declined here so that this one's
diff stayed readable.
