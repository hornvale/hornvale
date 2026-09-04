# 0618. A descent key's identity is a plan position, so the plan grammar is a save-format contract

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0069](0069-fine-position-is-never-serialized.md) (fine position is
never serialized), [0400](0400-custody-is-an-observable-not-a-vital.md) (custody
is an observable), [0099](0099-worlds-are-version-locked.md) (worlds are version-locked
to the code that generated them),
[0566](0566-a-place-is-a-graph-before-it-is-a-map.md),
[0616](0616-a-gate-is-a-requirement-on-a-way.md) ·
[The Brattice](../../book/src/chronicle/the-brattice.md)

In the context of a descent key becoming a Thing a possession can pick up,
facing the question of what identity that Thing wears when every plan, gate and
level is derived on entry and discarded on exit, we decided that **a descent
key's identity is its PLAN POSITION, and therefore that from the first world
saved holding a descent key the plan grammar is a save-format contract: a later
change to `underworld/plan/v1`'s draws or to `underworld/gate/v1/pattern`'s
selection is an EPOCH — `underworld/plan/v2`, `underworld/gate/v2` — and never
a silent edit** — accepting that a campaign wanting a different plan grammar
must pay for a stream epoch, and accepting that a world saved before such an
epoch names keys the new grammar does not place.

## Context

Decision 0566's plan is `FRAME`-tier under 0069: derived from `(seed, vertex,
rungs, kind, character)` on entry, discarded on exit, never serialized. The
Crosscut's own brief could therefore say "any new draw changes plan bytes —
allowed, no world reads them."

A world does now. `take` on a descent key commits a custody fact whose
**subject** is the key's derived identity, and that identity is a function of
where the plan put the key. As shipped:

```text
role    thing@descent/<vertex>/<level>/<col>.<row>/key          (ordinal 0)

subject    derive_entity_id(Lineage { parent: None, role, ordinal: 0 })
predicate  located-in
object     Value::Entity(<the body holding it>)
day        the session's WorldTime
```

preceded, when the key is still latent, by the `instance-of` fact the promotion
commits for the same subject. `drop` posts the same predicate with
`Value::Text("descent/<vertex>/<level>/<col>.<row>")` — the REGION, never the
cell, so no fine position of anything is serialized and 0069 is obeyed in the
letter. A door's identity is the same shape over its edge's two nodes,
`thing@descent/<vertex>/<level>/<col>.<row>-<col>.<row>/door`, lesser cell
first, so one door has one name however you cross it.

Move a draw and those subjects move with it.

The alternative — descent custody as session-only state, lost on save — was
considered and refused. Decision 0400 makes custody an observable on the
session snapshot, and two custody rules for one `key` kind would be exactly the
verb × object table the object system exists to avoid.

## Decision

The plan grammar and the gate grammar join the save-format contract list. A
change to either is a deliberate regeneration under an epoch suffix —
`underworld/plan/v2`, `underworld/gate/v2` — never a rename and never a silent
edit, which is the project's standing rule for any save-format contract. What
changed is that it is no longer the *empty* epoch a grammar with no readers
would have been.

## Consequence

- **One repair was taken now because it could never again be free.** The
  Crosscut deferred a fix to its lengthening move (it tested its capability
  invariant against the passage set as it stood *before* the move). That fix
  changes which moves land, and therefore the plan. It was taken in this
  campaign, with the four Crosscut readouts moving once and every verdict word
  unchanged, attributed by revert: with the single call reverted the committed
  panel is byte-identical to its baseline. After this record, the same fix would
  have cost an epoch.
- **An orphaned custody fact is harmless** in the way a strongbox key's is: the
  thing it names has no location and appears nowhere.
- **The three role spellings are themselves contracts**, and are written out as
  literals in a test so a spelling change cannot be rebaselined — the discipline
  the room-thing and cave-mouth role spellings already carry for their own
  namespaces. The `descent` prefix is neither a decimal facet id nor
  `passage`, so the three namespaces feeding one derivation cannot collide.
- **This is not the campaign's only committed-artifact movement, and the two are
  different in kind.** Registering the `door` thing-kind appended concept-registry
  accession epoch 19 and moved the seed-42 world golden, the proto-root tables
  and the derived corpora — additively, nothing re-sorted. That is the accession
  discipline working, and it is what every new thing-kind costs; it is not this
  contract.
- Everything else this campaign adds is additive: one stream leg, three palette
  kinds on a wire whose reader draws an unknown kind as rock, one thing kind, no
  new predicate, and no existing stream's consumption order changed.

## See also

- [The Brattice design](../superpowers/specs/2026-09-02-the-brattice-design.md)
  §5, §3.7.
- [The Brattice ledger](../superpowers/ledgers/2026-09-02-the-brattice.md) #10
  (ruling C), #13.
- `windows/vessel/src/descent_thing.rs` — the module doc states this cost where
  the spellings are.
- [The Brattice chronicle](../../book/src/chronicle/the-brattice.md).
