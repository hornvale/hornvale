# 0516. A reachable lock implies a reachable key is left empirical, deliberately

**Status:** Accepted (2026-08-30) · **Decider:** Nathan · **Campaign:** The Tenant

## Context

The Chattel shipped a lock that wants a key. To make the lock reachable at
all, it placed `the-key-by-the-door` in `Role::Threshold` — the chamber `enter`
lands in, which `role_for(0, …)` returns unconditionally. That made **"a
reachable lock implies a reachable key" a theorem**: provable from the grammar
for every brief, without inspecting a world.

The Custodian moved the key to `Role::Loomroom` because a lock is only a lock
if the key might be somewhere else, and one key per doorway makes finding a key
a formality. Availability halved (48/48 → 24/48 seeds carry a takeable key)
while the campaign's whole sequence survived (10 of 48 seeds reach a strongbox;
all 10 still open it).

**The theorem did not survive the move.** `role_for(2, brief)` is `Role::Store`
for a brief with no `Function`, so a three-chamber structure of that shape
composes a strongbox and no key at all. No swept seed is that shape. Nothing
forbids one. `windows/vessel/src/interior/pattern.rs` states this in the test's
own doc and asserts only the agrarian-brief clause it can prove.

## The rule

**The gap stays open, and restoring the theorem is the wrong fix.**

Three responses were considered:

1. **Restore it as a rule** — require a structure composing a strongbox to
   compose a key. Safe, and it returns the old guarantee.
2. **Declare it fine** — a locked box nobody can open is an honest thing for a
   world to contain.
3. **Wait for the residents.** ← chosen

The reason is that the question is mis-posed at the grammar layer. A key on a
floor is a **placeholder for the person who would hold it**
(`PLAY-key-placement-stands-in-for-a-resident`), and the custody mechanism it
is standing in for **already exists and is already general**:
`thing::located_in_holder_fact(thing, holder: EntityId)` and
`thing::held_by(ledger, holder: EntityId)` take any entity, not only a player.
What is missing is the resident — `SOC-one-creature-per-settlement`, where a
settlement of eighty holds exactly one simulated creature, blocked in turn on
agent-count cost measured superlinear at 2.17 across 100-200 agents.

Once a building has someone living in it, "where is the key" stops being a
question about floors and becomes **"who has it"**. A chest that cannot be
opened is then not a gap in the grammar but a person who will not hand the key
over — which is a better object than either remedy above produces.

## Consequences

- **A future campaign meeting the weakened clause must not strengthen it back**
  without reopening this decision. That is the specific act this record exists
  to prevent: the test's doc says the universal claim is gone, which reads like
  a defect to fix, and fixing it would quietly undo a deliberate choice and
  re-ubiquitise the key.
- **The guarantee is a measurement now, and a measurement is a claim with a
  date.** 10 of 48 swept seeds, 2026-08-30. If a later change moves the roles
  table, that number is stale and wants re-running before it is quoted.
- **A player can, in principle, meet an unopenable chest.** Accepted. Nothing
  crashes; the box is simply shut.
- **This decision expires by being dissolved rather than superseded.** When
  residents exist and a key is drawn onto a person, the question this record
  answers stops being asked, and the row that carries it should say so.
