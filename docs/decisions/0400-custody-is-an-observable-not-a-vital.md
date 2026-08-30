# 0400. Custody is an observable, not a vital — carried things ride the `self` channel

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot; the
question routed to a decision record because putting carried things on the
wire contradicts a standing statement in shipped client code, and the
project's rule is that contradicting a written statement is a record rather
than an edit) ·
**Amends:** `clients/game/core/src/endpaper.rs`'s module doc, which said
`Snapshot` "carries no player vitals at all — no hit points, stamina, hunger,
or inventory — by design (The Quire spec §6)" ·
**Relates:** [0022](0022-sim-emits-data-clients-render.md) (the sim emits
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
four items are not one kind of thing. Hit points, stamina and hunger name
quantities the sim does not commit — `CLIENT-vitality-folds` states the rule
directly ("wounds commit; health folds. There is no HP counter anywhere") —
so a strip printing one would be *inventing* it, which is exactly what
decision 0022's split forbids a client to do. The list is a rule about
**invention**, and "inventory" was swept into it by category resemblance:
these are all things a roguelike HUD shows.

## The rule

**1. The test is whether a client would have to invent the number.** A datum
belongs off the wire when nothing in the ledger determines it. Hit points fail
that test at the source: there is no such quantity to serialize, at any
fidelity, so the only way to draw one is to make it up.

**2. Custody passes it, on the ledger's own terms.** A carried thing is a
`located-in` fact whose subject is the thing and whose object is the **body**
(`thing::located_in_holder_fact`). Both ends are entities, the fact is
committed, `possess --out` saves it, and a reload re-derives the same custody
from the same ledger. Reading it invents nothing; it is the same kind of
observable as `sensed.present` (who is standing here) or `spatial` (where
here is).

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
and `put` accept. The id is emitted as decimal text for the third time on this
wire, for the reason `SelfChannel::agent` and `PresentEntry::entity` already
do: a lineage-derived `EntityId` exceeds the 2^53 a JavaScript `number` holds
losslessly.

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
  `strongbox_reachability::the_snapshot_carries_what_the_body_holds`
  (`823/865 tests run: 822 passed, 1 failed, 3 skipped`, 2026-08-30). So the
  campaign also commits `session-seed-1-carrying.json`, taken from the seed-1
  possession that walks to the strongbox — the one committed artifact of this
  wire in which the field is not empty.

- **Nothing renders it yet, and that is stated rather than implied.** This
  record puts the datum on the wire; no pane in `clients/game` draws it. The
  next client campaign inherits a field with a committed non-empty witness and
  a `noun` that already matches what the verbs accept.
