# 0338. A possession state is relational, not intrinsic

**Status:** Accepted (2026-08-26) · **Decider:** Nathan (autopilot, spec §8,
amended G4) · **Relates:** [0336](0336-possession-by-another-is-possession.md);
[0069](0069-fine-position-is-never-serialized.md) (the other row `body_state`
derives rather than stores) · [The Coercion](../../book/src/chronicle/the-coercion.md)

In the context of adding the first gate row that is not simply true or false
of a body regardless of who is asking, we decided that **`BodyState::
PossessedByAnother` names a relation, and the variant name carries that
relation because the derivation itself cannot — it is computed with no asker
parameter at all.**

## Context

Every existing gate row is intrinsic: `Awake` and `Asleep` are true of a body
whoever asks about it. A possession state is not — a body held by another is
refused in-character *to the player it is being withheld from*, not refused
in some absolute sense. The spec's first draft (§3.1) modelled this literally,
defining `possessed-by-another(body, asker)` and comparing the possessor
against the asking party.

**That signature could not be built, and finding out why is what this record
actually settles.** `Session::agent_entity` (`windows/vessel/src/session.rs`)
*is* `self.driven_body().entity` — the player does not have a ledger identity
separate from the body it drives, because nothing commits one (the player's
own possession is the session's premise, never a world fact). So `asker`
would have needed a value that does not exist, and the plausible-looking
placeholder — comparing the possessor against the driven body's own entity —
is degenerate: it returns true whenever a body is possessed at all, including
by the player's own possession, and refuses the player's in-character acts on
the body they are driving. That is exactly the bug §2.3 was written to
prevent, reintroduced by its own proposed remedy.

## The rule

`possessor_of(&Ledger, EntityId) -> Option<EntityId>` (`windows/vessel/src/
session.rs`) takes only the body. An open `possessed-by` fact with no
matching `possession-ended` **always** means someone other than the player
holds the body — there is no other party it could mean, because the player's
own possession leaves no comparable fact. `Session::body_state` calls it as a
flat `&self` read, the same shape `Awake`/`Asleep` already used, and the
relation lives entirely in the variant's name: `PossessedByAnother`, not the
shorter `Possessed` the other rows' naming convention would otherwise suggest.

## Consequences

- The state is relational **in principle** — a possession fact is inherently
  about who holds whom — and **degenerate in practice**: today's one asker
  (the player) makes the derivation a simple presence check. The distinction
  is deliberate, not a simplification lost along the way, and §2.3 states it
  as such.
- A later campaign that gives the player-soul its own ledger identity (the
  vacated-host-as-witness thread wants exactly this) is the one that would
  need to add an asker parameter back — and the variant is already named
  correctly for that day, so the rename this record's first draft would have
  needed does not have to happen twice.
- `possessor_of`'s single-pass, ledger-order fold (open sets, close clears,
  last word wins) is the only place the open/close pair from decision 0336's
  sibling mechanism is read; nothing else in `windows/vessel` re-derives
  possession state by any other route.
