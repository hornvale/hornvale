# 0167. A driver is interchangeable, and a possessed body is a creature

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Relates:**
[0069](0069-fine-position-is-never-serialized.md) (what a body's acts may and
may not commit) · **Implementation deferred:** to a future campaign; this
record states the requirement and measures the gap, and does not close it

In the context of The Deed giving a possessed body in-character acts that
charge time and commit facts, we decided that **keyboard input and a planner
must be interchangeable drivers of one creature — the visitor pattern, where
the creature is the subject and the source of intent is a parameter** —
accepting that the codebase does not satisfy this today and that closing the
gap is a campaign of its own.

## Context

The Deed's keystone is that *the effect of an act belongs to the body, not the
driver* (spec §3.1). At the **fact** level Task 7 delivers exactly that, and it
was verified rather than assumed: a player's `agent-at` is built by
`liveness::agent_at_fact`, the same constructor the drive tick uses; the field
set, predicate and object arity are identical; no field names the driver; and
**no production code reads a provenance value back** — every parse of one is
inside `#[cfg(test)]`.

**At the body level it is false, and the types say so.** A creature is an
`Npc` (`liveness.rs`): entity, home, resource, activity cycle, temperature
niche — the fields drives read. A possessed body is an `Agent` (`agent.rs`):
id, species, perception, position, village. **There is no conversion between
them anywhere in the tree** — no `From`, no `as_npc`, no bridge of any kind —
and 21 functions in the creature layer take `&Npc` or `&[Npc]`, none of which
a possessed body can be passed to.

Measured consequences, all current:

- No drive can act on a possessed body: it has no niche to be uncomfortable
  in and no resource to seek.
- A possessed body has no affect. Nothing can ask whether it is afraid or
  hungry; `needs` reads *other* creatures' feelings, never its own body's.
- It does not appear in occupancy, so creatures do not see it as an occupant.
- It commits `agent-at` and `rested`. A creature also commits `drank`,
  `eaten`, `turned-hostile`.

**Creatures live in the world; a possessed body visits it.**

## The requirement

A driver — a keyboard, a GOAP planner, a script, a future network peer — is a
**visitor** over a creature. The creature is the subject in every case; where
the intent came from is a parameter of the visit, never a different type of
subject. Concretely, the target state is that a possessed body **is** an `Npc`
(or that both collapse into one type), so every function taking `&Npc` accepts
it without a special case.

This restates The Tackle's own finding, which is why that finding is now
promoted rather than left in a followup register: *"an act branched by where
intent came from occupies one leaf of six; player command and GOAP plan are
two values of ONE leaf, siblings not opposites."*

## Consequences

- **This is a MUST FIX, deferred deliberately.** Implementation is a campaign,
  not a task, and it must not land on top of an unfinished acceptance test.
  `PLAY-driver-substitutability` carries the work.
- **"Indistinguishable" is retired as the acceptance criterion, and
  substitutability replaces it.** The distinction is not pedantic: an audit
  found a possessed body's provenance strings are 100% separable from a
  creature's by value — the creature vocabulary always names a drive
  (*hunger*, *thirst*, *fear*), while a possessed body's says *"walked on (its
  own errand)"*, which is precisely a refusal to name one. Under
  "indistinguishable" that reads as a defect to paper over. Under
  substitutability it reads as **an accurate report**: the body genuinely has
  no drive, because it is not an `Npc`. The string is a symptom; changing it
  would hide the gap and narrow it not at all.
- The candidate repair considered and **rejected as unbuildable** — have a
  possessed body's provenance borrow its own dominant drive — cannot be
  implemented at all today, for the same reason: an `Agent` has no drives to
  borrow. A fix that cannot be built is a useful thing to have discovered
  before proposing it, and it is recorded here so it is not re-proposed.
- Tests assert what is actually true and worth protecting: the shared
  constructor, the identical field set, and that no field names the driver.
  They must not assert value-level identity of provenance, which is false by
  design until this is closed.
