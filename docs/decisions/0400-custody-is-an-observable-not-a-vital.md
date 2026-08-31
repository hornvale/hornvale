# 0400. Custody is an observable, not a vital — carried things ride the `self` channel

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot; the
question routed to a decision record because putting carried things on the
wire contradicts a standing statement in shipped client code, and the
project's rule is that contradicting a written statement is a record rather
than an edit) ·
**Amends:** `clients/game/core/src/endpaper.rs`'s module doc, which said
`Snapshot` "carries no player vitals at all — no hit points, stamina, hunger,
or inventory — by design (The Quire spec §6)" ·
**Relates:** [0070](0070-wounds-commit-health-folds.md) (the ratified
taxonomy this record should have cited from the start — see the correction
below), [0022](0022-sim-emits-data-clients-render.md) (the sim emits
data and the client renders it),
[0117](0117-the-client-re-derives-nothing-the-sim-emits.md)
(one datum, one place on the wire),
[0126](0126-fact-day-is-a-typed-world-time.md) (a committed fact's shape)
· [The Chattel](../superpowers/plans/2026-08-28-the-chattel.md)

In the context of putting a possessed body's carried things on
`vessel/session/v2`, we decided that **custody is an ordinary observable and
not a vital, so it rides the `self` channel as `self.carrying` — additively,
with no version bump** — accepting that a shipped client module's doc has to
be corrected rather than obeyed.

## Context

The Chattel shipped six verbs — `open`, `close`, `take`, `drop`, `put`,
`carrying` — and the fold behind them: `thing::held_by` reads every entity
whose latest `located-in` names the body as its object. A player can pick a
key up in a storeroom, walk out of the building, come back in and set it down
in another room, and the key is there on the next entry. That is the
campaign's headline, and until this record it existed only in the prose the
CLI printed. No pane could see it.

The obstacle was a sentence in `clients/game/core/src/endpaper.rs`:

> `Snapshot` carries no player vitals at all — no hit points, stamina,
> hunger, or inventory — by design (The Quire spec §6)

**Its citation is false.** `grep -c inventory
docs/superpowers/specs/2026-08-08-the-quire-design.md` returns **0**, and that
spec's §6 is the drive-across-the-linker / read-across-the-serializer seam —
it has no opinion about inventories. The rule was never written in a spec at
all; it was written in the module that obeys it, and then cited to an
authority that does not carry it. Task 13's own plan text repeated the
citation, which is how a false pointer gets a second reader.

**Its classification is false too, and that is the half worth a record.** The
four items are not one kind of thing, and "inventory" was swept into the list
by category resemblance: these are all things a roguelike HUD shows.

**THE DISCRIMINATING PREMISE THIS RECORD FIRST OFFERED WAS FALSE IN THE VERY
CRATE THAT BUILDS THE SNAPSHOT, and it is corrected here rather than quietly
dropped** (The Chattel, Task 13 fix round). The original paragraph read: *"Hit
points, stamina and hunger name quantities the sim does not commit — so a
strip printing one would be inventing it."* Two of the three are folds over
committed events, in `windows/vessel/src/liveness.rs`:

```text
liveness.rs:2273  fatigue_at(…)   "FATIGUE == FOLD", over `rested`
liveness.rs:2456  hunger_at(…)    "HUNGER == FOLD", folding `eaten`
liveness.rs:2171  pub const RESTED: &str = "rested"
liveness.rs:2348  pub const EATEN:  &str = "eaten"
```

So hunger and fatigue are *exactly* the shape this record admits custody on:
a fold over dated postings, re-derivable by any reader of the ledger. A test
that admitted custody and excluded them would have to be a different test
from the one stated. Only **hit points** genuinely lack a model, and
[0070](0070-wounds-commit-health-folds.md) is why: *"injuries are committed
facts and vitality is a fold over them … no stored, mutable health value may
exist anywhere."*

**Worse, 0070 already ruled the taxonomy and this record did not cite it.**
Its own consequence names the family — *"drive, belief, affect, **inventory**,
health"* — putting custody in the same class as the three items the original
argument split it from. The project had settled this in 2026-07; a record
minted a month later argued it from scratch, got the discriminating fact
wrong, and reached the right answer anyway.

**The conclusion does not move, and the reason it does not is that rules 2
and 3 below never depended on rule 1.** Rule 2 is a positive claim about
custody's own shape — a committed `located-in` fact with an entity on each end
— and rule 3 is a placement argument from `SelfChannel.room`'s precedent.
Neither needs anything to be true about hit points. Rule 1 is rewritten to say
what it can support.

## The rule

**1. The test is whether a client would have to invent the number, and the
list the endpaper inherited is not that test.** A datum belongs off the wire
when nothing in the ledger determines it. **Hit points** fail it at the
source, by [0070](0070-wounds-commit-health-folds.md)'s own construction:
there is no such quantity to serialize, at any fidelity, so the only way to
draw one is to make it up. **Hunger and stamina do not fail it** —
`liveness.rs`'s `hunger_at` and `fatigue_at` are folds over committed `eaten`
and `rested` postings — so the reason the endpaper draws neither is a claim
about that STRIP (it is one row, identity only) and never a claim about the
wire. Conflating the two is how a rule about invention became a rule about
categories, and how "inventory" ended up inside it.

**2. Custody passes it, on the ledger's own terms — and this is the clause
the decision actually rests on.** A carried thing is a
`located-in` fact whose subject is the thing and whose object is the **body**
(`thing::located_in_holder_fact`). Both ends are entities, the fact is
committed, `possess --out` saves it, and a reload re-derives the same custody
from the same ledger. Reading it invents nothing; it is the same kind of
observable as `sensed.present` (who is standing here) or `spatial` (where
here is). [0070](0070-wounds-commit-health-folds.md) had already put it in
that class by name: *"drive, belief, affect, inventory, health"* is one
pattern with five instances, and this record is one of them reaching a wire.

**3. So it rides `self`, and the precedent is the field above it.**
`SelfChannel` already carries `room` — the body's own STATE, not its identity
— so the channel is not "the identity channel" and never was. Custody is
body-relative in exactly `room`'s way: it is a fact about the body that
outlives the room the body is standing in. It is not presence-gated (it
survives walking out, which is what `sensed` means), not knowledge (a body
needs no inference to know its own hands), and not a standing toward anyone.
None of the other four channels is its home, and inventing a fifth top-level
channel would assert a redaction boundary that does not exist.

**4. `CarriedEntry` carries two fields, `entity` and `noun`, and the id
crosses as a decimal string.** `entity` is the thing's identity across turns —
the key that leaves a storeroom is the same key that is set down four chambers
away, and nothing else on this wire would carry that. `noun` is the word the
verbs take, matched by `Session::carried_named` against the whole noun,
article and all, so a completion-capable client has the exact string `drop`
and `put` accept. The id is emitted as decimal text for the **fourth** time on
this wire, for the reason the three before it already do: a lineage-derived
`EntityId` exceeds the 2^53 a JavaScript `number` holds losslessly. **This
clause said "third" and named two of the three**, and two other sites copied
the miscount with a different pair each. The roster is every
`serialize_with = "u64_as_decimal_string"` in `windows/vessel/src/snapshot.rs`:
`SelfChannel::agent` (:91), `CarriedEntry::entity` (:145),
`PresentEntry::entity` (:175), `SocialEntry::entity` (:223).

**5. Additive, so no version moves.** `vessel/session/v2` gains a key; a
mirror that does not know it ignores it, which is the schema discipline's own
free case. `clients/game/core`'s mirror does not add the field, on the mirror
rule's terms — no component draws it yet — exactly as it already omits
`self.room`.

## Consequences

- **The endpaper's doc is corrected in place, loudly, rather than deleted.** A
  record that outlives its subject produces wrong answers from readers acting
  in good faith, and this one already had: it is what made "carried things on
  the snapshot" look like a rule violation instead of a design question. The
  module doc now states the false citation, the false classification, and the
  reason the strip still draws nothing of it — which is its own (one row,
  identity only), not the wire's. `draw_carries_no_vitals` keeps `"inventory"`
  in its forbidden-word list, re-scoped by a doc comment to a claim about the
  ROW.

- **Four committed byte-goldens moved, additively and only additively.**
  `windows/vessel/tests/fixtures/{session-seed-42,snapshot-seed-42-walk,
  snapshot-seed-42-chamber,snapshot-seed-0-chamber-occupied}.json` gained
  seven insertions of `,"carrying":[]` between them and changed nothing else
  — verified by differencing the changed lines rather than by reading the
  diff, since these are single-line JSON documents where an inspection by eye
  proves little. The client fixtures under
  `clients/game/core/tests/fixtures/` moved the same way.

- **A byte-golden cannot hold this field, and a third fixture is what does.**
  Every seed-42 fixture records a possession that never typed `take`, so all
  of them carry `"carrying":[]` — which is also what a neutralised fold
  emits. `the_client_fixtures_are_current` stayed GREEN through the mutation
  that replaces `Session::snapshot`'s fold with `Vec::new()`; the only red was
  `strongbox_reachability::the_snapshot_carries_what_the_body_holds`. So the
  campaign also commits `session-seed-1-carrying.json`, taken from the seed-1
  possession that walks to the strongbox — the one committed artifact of this
  wire in which the field is not empty.

  **The figure first published for that mutation was taken from a CANCELLED
  run** (`823/865 tests run: 822 passed, 1 failed, 3 skipped`, 2026-08-30):
  823 of 865 means nextest stopped at the failure and 42 tests never
  executed, so it could not establish "the only red". Re-taken the same day
  in the fix round with `--no-fail-fast`: `868 tests run: 867 passed, 1
  failed, 3 skipped`. The conclusion is unchanged and now has a denominator.

- **Nothing renders it yet, and that is stated rather than implied.** This
  record puts the datum on the wire; no pane in `clients/game` draws it. The
  next client campaign inherits a field with a committed non-empty witness and
  a `noun` that already matches what the verbs accept.
